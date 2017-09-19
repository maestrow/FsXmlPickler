open System.Xml

let filename = "data.xml"
let reader = new XmlTextReader(filename)
reader.WhitespaceHandling <- WhitespaceHandling.None

while reader.Read() do 
  match reader.NodeType with
  | XmlNodeType.Element -> 
    match reader.IsEmptyElement with
    | true -> printfn "<%s/>" reader.Name 
    | false -> printfn "<%s>" reader.Name 
  | XmlNodeType.Text -> printfn "%s" reader.Value
  | XmlNodeType.CDATA -> printfn "<![CDATA[%s]]>" reader.Value
  | XmlNodeType.ProcessingInstruction -> printfn "<?%s %s?>" reader.Name reader.Value
  | XmlNodeType.Comment -> printfn "<!--%s-->" reader.Value
  | XmlNodeType.XmlDeclaration -> printfn "%s" "<?xml version='1.0'?>"
  | XmlNodeType.Document -> ()
  | XmlNodeType.DocumentType -> printfn "<!DOCTYPE %s [%s]" reader.Name reader.Value
  | XmlNodeType.EntityReference -> printfn "%s" reader.Name
  | XmlNodeType.EndElement -> printfn "</%s>" reader.Name
  //| _ -> printfn "!!!"