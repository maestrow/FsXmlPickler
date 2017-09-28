open System.Xml
open System.IO

type Attribute = { Name: string; Value: string }

type XmlElement = 
  | Element of Name: string * Attributes: Attribute list
  | EmptyElement of Name: string * Attributes: Attribute list
  | Text of string
  | EndElement of string

let createEl (reader: XmlTextReader) = 
  match reader.NodeType with
  | XmlNodeType.Element -> 
      if reader.IsEmptyElement then
        EmptyElement (reader.Name, []) |> Some
      else
        Element (reader.Name, []) |> Some
  | XmlNodeType.Text -> reader.Value |> Text |> Some
  | XmlNodeType.EndElement -> reader.Name |> EndElement |> Some
  | _ -> None

let getReader (file: string) = 
  let reader = new XmlTextReader(file)
  reader.WhitespaceHandling <- WhitespaceHandling.None
  reader

let read (reader: XmlTextReader) = 
  seq { 
    while reader.Read () do
      let el = createEl reader
      if (el.IsSome) then
        yield el.Value 
    }

let sequence = __SOURCE_DIRECTORY__ + "/../../tests/data/data2.xml" |> getReader |> read

let doWork s = Seq.iter (printfn "%A") s

sequence |> doWork

//Seq.takeWhile (fun i -> i.Value <> "1") sequence |> doWork
//printfn "%s" "oooow maaan"
//Seq.takeWhile (fun i -> i.Value <> "4") sequence |> doWork

