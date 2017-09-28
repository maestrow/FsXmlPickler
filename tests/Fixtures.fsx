module Tests.Fixtures

open System.Xml

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

let getReader (file: string) = 
  let reader = new XmlTextReader(file)
  reader.WhitespaceHandling <- WhitespaceHandling.None
  reader