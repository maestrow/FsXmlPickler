module XmlPickler.Sequence

open System.Xml
open System.IO

type XmlAttr = { Name: string; Value: string }

type XmlElem = 
  | Element of Name: string * Attributes: XmlAttr list
  | EmptyElement of Name: string * Attributes: XmlAttr list
  | Text of string
  | EndElement of string

let isElement = function Element _ -> true | _ -> false
let isEmptyElement = function EmptyElement _ -> true | _ -> false
let isText = function Text _ -> true | _ -> false
let isEndElement = function EndElement _ -> true | _ -> false

module Implementation = 
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

open Implementation

let xmlReaderToSeq (reader: XmlTextReader) = 
  seq { 
    while reader.Read () do
      let el = createEl reader
      if (el.IsSome) then
        yield el.Value 
    }

