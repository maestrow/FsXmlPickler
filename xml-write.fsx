open System.Xml
open System.IO

let sw = new StringWriter()
let writer = new XmlTextWriter(sw)
writer.Formatting <- Formatting.Indented

writer.WriteStartElement("Stock")
writer.WriteAttributeString("Symbol", "MSFT")
writer.WriteElementString("Price", XmlConvert.ToString(2.28))
writer.WriteElementString("Change", XmlConvert.ToString(3.98))
writer.WriteElementString("Volume", XmlConvert.ToString(123123213))
writer.WriteEndElement()

writer.Close()

printfn "%s" <| sw.ToString()