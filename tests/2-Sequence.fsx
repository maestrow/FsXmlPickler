module Tests.Sequence

#load "../XmlPickler.Sequence.fsx"
#load "Fixtures.fsx"

open XmlPickler.Sequence
open Tests.Fixtures
open System.Collections.Generic

let getSeq () = __SOURCE_DIRECTORY__ + "/data/data2.xml" |> getReader |> xmlReaderToSeq

let readAll () = 
  let sequence = getSeq ()
  Seq.iter (printfn "%A") sequence

let readOneByOne () = 
  let sequence = getSeq ()
  let enm = sequence.GetEnumerator()
  while enm.MoveNext() do 
    enm.Current |> printfn "%A"
   

getSeq () |> Seq.length |> printfn "Count: %d"
readAll ()
readOneByOne ()

//Seq.takeWhile (fun i -> i.Value <> "1") sequence |> doWork
//printfn "%s" "oooow maaan"
//Seq.takeWhile (fun i -> i.Value <> "4") sequence |> doWork