open System
open System.Xml
open FSharp.Reflection
open System.Reflection
open System.IO

type Host 
  (
    writer: XmlTextWriter,
    valueProcessors: ((Type->bool) * (Host->obj->unit)) list,
    memberProcessors: ((MemberInfo->bool) * (Host->MemberInfo->obj->unit)) list)
    = 
    let valueProcessors = valueProcessors
    let memberProcessors = memberProcessors

    member this.ProcessValue (value: obj) = 
      let p = valueProcessors |> List.find (fst >> (fun f -> f <| value.GetType())) |> snd
      p this value
    
    member this.ProcessMember (mi: MemberInfo) (value: obj) = 
      let p = memberProcessors |> List.find (fst >> (fun f -> f <| mi)) |> snd
      p this mi value
      
    member this.Writer = writer


type ISequenceProcessor =
  abstract member ToSequence : obj -> seq<obj>
  abstract member FromSequence : seq<obj> -> obj

type ListProcessor<'T>() =
  interface ISequenceProcessor with
    member this.ToSequence (x: obj) = Seq.map box (x :?> list<'T>)
    member this.FromSequence (s: seq<obj>) = box [for x in s -> x :?> 'T]


module Picklers =
  
  let listCond (t:Type) = t.IsGenericType && (t.GetGenericTypeDefinition() = typedefof<list<_>>)
  let list (host: Host) value = 
    let w = host.Writer
    let args = value.GetType().GetGenericArguments()
    let arg = args.[0]
    let sP =
      typedefof<ListProcessor<_>>.MakeGenericType(args)
      |> Activator.CreateInstance
      |> unbox : ISequenceProcessor
    sP.ToSequence value |> Seq.iter host.ProcessValue

  FSharpType.IsUnion
  let union (host: Host) value = 
    let w = host.Writer
    let (info, fieldValues) = FSharpValue.GetUnionFields (value, value.GetType())
    w.WriteStartElement (info.Name)
    Seq.iter2 host.ProcessMember (info.GetFields()) fieldValues
    w.WriteEndElement ()

  let memberP (host: Host) (mi: MemberInfo) value = 
    let w = host.Writer
    w.WriteStartElement (mi.Name)
    host.ProcessValue value
    w.WriteEndElement ()

  let record (host: Host) value = 
    let w = host.Writer
    let fieldInfos = FSharpType.GetRecordFields <| value.GetType()
    let fieldValues = FSharpValue.GetRecordFields value
    Seq.iter2 host.ProcessMember fieldInfos fieldValues

  let scalarPicklers = 
    let writers: (Type * (XmlTextWriter -> obj -> unit)) list =  
      [
        typedefof<bool>          , fun w v -> w.WriteValue (unbox v : bool)
        typedefof<DateTime>      , fun w v -> w.WriteValue (unbox v : DateTime)
        typedefof<DateTimeOffset>, fun w v -> w.WriteValue (unbox v : DateTimeOffset)
        typedefof<decimal>       , fun w v -> w.WriteValue (unbox v : decimal)
        typedefof<float>         , fun w v -> w.WriteValue (unbox v : float)
        typedefof<int>           , fun w v -> w.WriteValue (unbox v : int)
        typedefof<int64>         , fun w v -> w.WriteValue (unbox v : int64)
        typedefof<float32>       , fun w v -> w.WriteValue (unbox v : float32)
        typedefof<string>        , fun w v -> w.WriteValue (unbox v : string)
      ]
    writers 
    |> List.map (fun (t, f) -> 
      let key = fun typ -> t = typ
      let value = fun (host: Host) value -> f host.Writer value
      key, value
    )

  let valuePicklers = 
    scalarPicklers @
    [
      listCond, list
      FSharpType.IsUnion, union
      FSharpType.IsRecord, record
    ]
  let memberPicklers = 
    [
      (fun mi -> true), memberP
    ]

//

type Document = Block list

and Block = 
  | Text of string
  | FoldedText of FoldedText

and FoldedText = 
  {
    Caption : string
    Content : Document
    Level : int
  }

let text = 
  [
    Text "owejfowiejf owiejf oiwje foiwje foij oierjg oiwj gwef. "
    FoldedText 
      { 
        Caption = "oeigj"
        Content = [Text "oweifj"] 
        Level = 0
      }
  ]

let createWriter () =
  let sw = new StringWriter()
  let writer = new XmlTextWriter(sw)
  writer.Formatting <- Formatting.Indented
  sw, writer

let createHost () = 
  let (sw, writer) = createWriter ()
  let host = Host(writer, Picklers.valuePicklers, Picklers.memberPicklers)
  host, sw

let (host, sw) = createHost ()
host.ProcessValue text

sw.ToString() |> printfn "%s"
