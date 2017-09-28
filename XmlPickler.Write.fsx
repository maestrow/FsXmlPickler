module XmlPickler.Write

#load "XmlPickler.Processors.fsx"

open XmlPickler.Processors

open System
open System.Xml
open FSharp.Reflection
open System.Reflection
open System.IO

type WriterHost (writer: XmlTextWriter,
                 valueProcessors: ((Type->bool) * (WriterHost->obj->unit)) list,
                 memberProcessors: ((MemberInfo->bool) * (WriterHost->MemberInfo->obj->unit)) list) = 
                 
    let valueProcessors = valueProcessors
    let memberProcessors = memberProcessors

    member this.ProcessValue (value: obj) = 
      let p = valueProcessors |> List.find (fst >> (fun f -> f <| value.GetType())) |> snd
      p this value
    
    member this.ProcessMember (mi: MemberInfo) (value: obj) = 
      let p = memberProcessors |> List.find (fst >> (fun f -> f <| mi)) |> snd
      p this mi value
      
    member this.Writer = writer




module Writers =
  
  let listCond (t:Type) = t.IsGenericType && (t.GetGenericTypeDefinition() = typedefof<list<_>>)
  let list (host: WriterHost) value = 
    let w = host.Writer
    let args = value.GetType().GetGenericArguments()
    let arg = args.[0]
    let sP =
      typedefof<ListProcessor<_>>.MakeGenericType(args)
      |> Activator.CreateInstance
      |> unbox : ISequenceProcessor
    sP.ToSequence value |> Seq.iter host.ProcessValue

  let union (host: WriterHost) value = 
    let w = host.Writer
    let (info, fieldValues) = FSharpValue.GetUnionFields (value, value.GetType())
    let fields = info.GetFields()
    if fields.Length = 0 then
      w.WriteElementString (info.Name, String.Empty)
    else 
      w.WriteStartElement (info.Name)
      if fields.Length = 1 then
        host.ProcessValue fieldValues.[0]
      else
        Seq.iter2 host.ProcessMember fields fieldValues
      w.WriteEndElement ()

  let memberP (host: WriterHost) (mi: MemberInfo) value = 
    let w = host.Writer
    w.WriteStartElement (mi.Name)
    host.ProcessValue value
    w.WriteEndElement ()

  let record (host: WriterHost) value = 
    let w = host.Writer
    let fieldInfos = FSharpType.GetRecordFields <| value.GetType()
    let fieldValues = FSharpValue.GetRecordFields value
    Seq.iter2 host.ProcessMember fieldInfos fieldValues

  let scalarWriters = 
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
      let value = fun (host: WriterHost) value -> f host.Writer value
      key, value
    )

  let valuePicklers = 
    scalarWriters @
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




let createWriter () =
  let sw = new StringWriter()
  let writer = new XmlTextWriter(sw)
  writer.Formatting <- Formatting.Indented
  sw, writer

let createHost () = 
  let (sw, writer) = createWriter ()
  let host = WriterHost(writer, Writers.valuePicklers, Writers.memberPicklers)
  host, writer, sw

