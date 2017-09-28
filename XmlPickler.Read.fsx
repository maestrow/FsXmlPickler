module XmlPickler.Read

#load "XmlPickler.Sequence.fsx"
#load "XmlPickler.Processors.fsx"
#load "XmlPickler.Conditions.fsx"

open XmlPickler.Sequence
open XmlPickler.Processors
open XmlPickler.Conditions

open System
open System.Reflection
open FSharp.Reflection
open System.Xml
open System.IO
open System.Collections.Generic

module Private = 
  let createListProcessor t =
    typedefof<ListProcessor<_>>.MakeGenericType([|t|])
    |> Activator.CreateInstance
    |> unbox : ISequenceProcessor

open Private

type Deserializer = (Type->bool) * (Reader->Type->obj)

and Reader (enm: IEnumerator<XmlElem>, deserializers: Deserializer list) = 
  
  let moveUntilClose tagName = 
    let isTagClosed () = enm.Current |> function
      | EndElement name when name = tagName -> true
      | _ -> false
    match enm.MoveNext () with
    | false -> false
    | _ -> isTagClosed () |> not
    
  let move () =
    if not <| enm.MoveNext () then failwith "Unexpected end"

  member this.GetElementName () = 
    match enm.Current with
    | Element (name, _)
    | EmptyElement (name, _) -> name
    | _ -> failwith "Current item must be an Element"

  member this.Enumerator = enm
  
  member this.Read (t: Type) : obj =
    deserializers
    |> List.find (fst >> (fun f -> f t))
    |> snd
    |> fun d -> d this t

  /// t - Type of elements in array
  member this.DeserializeArray (t: Type): obj seq = 
    let parent = this.GetElementName ()
    seq {
      while moveUntilClose parent do
        yield this.Read t
    }

  member this.DeserializeMembers (members: (string * Type) list) = 
    let memberNames = members |> List.map fst
    let parent = this.GetElementName ()
    seq {
      while moveUntilClose parent do
        match enm.Current with
        | Text v -> failwith "Unexpected Text Element"
        | Element (name, _)
        | EmptyElement (name, _) -> 
            if memberNames |> List.contains name then  
              let mt = members 
                        |> List.find (fst >> name.Equals)
                        |> snd
              yield name, this.Read mt
        | _ -> ()
    }

  member this.ReadScalar () = 
    let parent = this.GetElementName ()
    match enm.Current with
    | Element (name, _) ->
        move ()
        match enm.Current with
        | Text v -> 
            move ()
            match enm.Current with
            | EndElement parent -> v
            | _ -> failwith "Expected closing tag"
        | EndElement parent -> String.Empty
        | _ -> failwith "Expected Text or closing tag"
    | EmptyElement _ -> String.Empty
    | _ -> failwith "Unexpected element"
    
module Deserializers = 
  // order values corresponding to fields order in UnionCase
  let private orderValues (fields: list<string * Type>) (memberValues: seq<string * obj>) = 
    fields
    |> List.map (fun (n, _) -> memberValues |> Seq.find (fst >> n.Equals) |> snd)
    |> Array.ofList
  
  let list (reader: Reader) (t: Type) =
    let itemsType = t.GetGenericArguments().[0]
    let listProc  = createListProcessor itemsType
    reader.DeserializeArray itemsType
    |> listProc.FromSequence

  let union (reader: Reader) (t: Type) =
    let case = 
      FSharpType.GetUnionCases t
      |> Array.find (fun i -> i.Name = reader.GetElementName ())
    let fields = case.GetFields () |> List.ofArray |> List.map (fun f -> f.Name, f.PropertyType)
    let memberValues = reader.DeserializeMembers fields 
    let values = orderValues fields memberValues
    FSharpValue.MakeUnion (case, values)

  let record (reader: Reader) (t: Type) =
    let fields = 
      FSharpType.GetRecordFields t 
      |> List.ofArray
      |> List.map (fun pi -> pi.Name, pi.PropertyType)
    let memberValues = reader.DeserializeMembers fields
    let values = orderValues fields memberValues
    FSharpValue.MakeRecord (t, values)

  let scalarDeserializers : Deserializer list = 
    [
      typedefof<bool>          , fun s -> bool.Parse           |> box
      typedefof<DateTime>      , fun s -> DateTime.Parse       |> box
      typedefof<DateTimeOffset>, fun s -> DateTimeOffset.Parse |> box
      typedefof<decimal>       , fun s -> Decimal.Parse        |> box
      typedefof<float>         , fun s -> Double.Parse         |> box
      typedefof<int>           , fun s -> Int32.Parse          |> box
      typedefof<int64>         , fun s -> Int64.Parse          |> box
      typedefof<float32>       , fun s -> Single.Parse         |> box
      typedefof<string>        , box
    ]
    |> List.map (fun (t, f) -> 
        let k = fun typ -> t = typ
        let v = fun (r: Reader) (t: Type) -> r.ReadScalar () |> f
        k, v
      )

  let deserializers = 
    scalarDeserializers @
    [
      isList, list
      isUnion, union
      isRecord, record
    ]