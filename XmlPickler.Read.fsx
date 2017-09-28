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

type CallStackEntry = 
  | Begin of deserializerName: string * typeName: string
  | MoveTo of XmlElem
  | ReadScalar of string
  | End of deserializerName: string * typeName: string

type Deserializer = { Name: string; Condition: Type->bool; Fn: Reader->Type->obj }
  with
    static member Create n c f = { Name = n; Condition = c; Fn = f }

and Reader (enm: IEnumerator<XmlElem>, deserializers: Deserializer list) = 
  
  let callStack = new Stack<CallStackEntry>()
    
  let moveNext () = 
    let result = enm.MoveNext ()
    if result then callStack.Push (MoveTo enm.Current)
    result

  let move () =
    if not <| moveNext () then failwith "Unexpected end"

  let moveUntilClose tagName = 
    let isTagClosed () = enm.Current |> function
      | EndElement name when name = tagName -> true
      | _ -> false
    match moveNext () with
    | false -> false
    | _ -> isTagClosed () |> not

  member this.MoveNext () = moveNext ()

  member this.GetElementName () = 
    match enm.Current with
    | Element (name, _)
    | EmptyElement (name, _) -> name
    | _ -> failwith "Current item must be an Element"

  member this.Enumerator = enm
  
  member this.CallStack with get () = callStack

  member this.Read (t: Type) : obj =
    deserializers
    |> List.find (fun d -> d.Condition t)
    |> fun d -> 
      callStack.Push (Begin (d.Name, t.Name))
      let r = d.Fn this t
      callStack.Push (End (d.Name, t.Name))
      r

  /// t - Type of elements in array
  member this.DeserializeArray (t: Type): obj seq = 
    callStack.Push (Begin ("DeserializeArray", t.Name))
    let parent = this.GetElementName ()
    seq {
      while moveUntilClose parent do
        yield this.Read t
      callStack.Push (End ("DeserializeArray", t.Name))
    }

  member this.DeserializeMembers (members: (string * Type) list) = 
    callStack.Push (Begin ("DeserializeMembers", "<members>"))
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
        | EndElement name -> failwithf "Unexpected closing '%s' element" name
      callStack.Push (End ("DeserializeMembers", "<members>"))
    }

  member this.ReadScalar () = 
    let parent = this.GetElementName ()
    let r = 
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
    callStack.Push (ReadScalar r)
    r

    
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
    if fields.Length = 1 then
      let memberType = fields |> List.head |> snd
      FSharpValue.MakeUnion (case, [|reader.Read memberType|])
    elif fields.Length > 0 then
      let memberValues = reader.DeserializeMembers fields 
      let values = orderValues fields memberValues
      FSharpValue.MakeUnion (case, values)
    else
      FSharpValue.MakeUnion (case, [||])

  let record (reader: Reader) (t: Type) =
    let fields = 
      FSharpType.GetRecordFields t 
      |> List.ofArray
      |> List.map (fun pi -> pi.Name, pi.PropertyType)
    let memberValues = reader.DeserializeMembers fields |> List.ofSeq
    let values = orderValues fields memberValues
    FSharpValue.MakeRecord (t, values)

  let scalarDeserializers : Deserializer list = 
    [
      typedefof<bool>          , bool.Parse           >> box
      typedefof<DateTime>      , DateTime.Parse       >> box
      typedefof<DateTimeOffset>, DateTimeOffset.Parse >> box
      typedefof<decimal>       , Decimal.Parse        >> box
      typedefof<float>         , Double.Parse         >> box
      typedefof<int>           , Int32.Parse          >> box
      typedefof<int64>         , Int64.Parse          >> box
      typedefof<float32>       , Single.Parse         >> box
      typedefof<string>        , box
    ]
    |> List.map (fun (t, f) -> 
        let k = fun typ -> t = typ
        let v = fun (r: Reader) (t: Type) -> r.ReadScalar () |> f
        Deserializer.Create t.Name k v
      )

  let deserializers = 
    scalarDeserializers @
    ([
      "list"   , isList   , list 
      "union"  , isUnion  , union
      "record" , isRecord , record
    ]
    |> List.map (fun (n, c, v) -> Deserializer.Create n c v))