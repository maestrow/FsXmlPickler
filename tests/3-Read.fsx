module Tests.Read

#load "../XmlPickler.Sequence.fsx"
#load "../XmlPickler.Read.fsx"
#load "Fixtures.fsx"

open XmlPickler.Sequence
open XmlPickler.Read
open Tests.Fixtures

open System.Xml
open System.Collections.Generic


let enm = __SOURCE_DIRECTORY__ + "/data/data3.xml" |> getReader |> xmlReaderToSeq |> (fun s -> s.GetEnumerator ())

let reader = Reader (enm, Deserializers.deserializers)

reader.Enumerator.MoveNext()

let result = reader.Read typeof<Document>

result |> printfn "%A"

//Seq.iter (printfn "%A") sequence

