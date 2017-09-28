#load "../XmlPickler.Sequence.fsx"
#load "../XmlPickler.Read.fsx"
#load "Fixtures.fsx"

open XmlPickler.Sequence
open XmlPickler.Read
open Tests.Fixtures

open System
open System.Xml
open System.Collections.Generic

let printStack (stack: Stack<CallStackEntry>) = 
  let indent level = Seq.init level (fun i -> "  ") |> String.Concat
  stack.ToArray () 
  |> List.ofArray
  |> List.rev
  |> List.fold (fun state item -> 
      let level = fst state
      let res = snd state
      let (newLevel, indentation) = 
        match item with
        | Begin _ -> level + 1, indent level
        | End _   -> level - 1, indent (level-1)
        | _       -> level    , indent level
      let str = sprintf "%s%A" indentation item
      (newLevel, str::res)
    ) (0, [])
  |> snd
  |> List.rev
  |> List.iter (fun i -> printfn "%s" i)


let enm = __SOURCE_DIRECTORY__ + "/data/data3.xml" |> getReader |> xmlReaderToSeq |> (fun s -> s.GetEnumerator ())

let reader = Reader (enm, Deserializers.deserializers)

reader.MoveNext()

let result = 
  try 
    (reader.Read typeof<Document>) :?> Document
  with 
  | Exception as ex ->
      printfn "\nCallStack\n"
      reader.CallStack |> printStack
      //printfn "\nProcessedElements\n"
      //reader.ProcessedElements |> printStack
      printfn "\nException\n%A" ex
      []

result |> printfn "%A"

//Seq.iter (printfn "%A") sequence

