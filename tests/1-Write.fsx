#load "../XmlPickler.Write.fsx"
#load "Fixtures.fsx"

open XmlPickler.Write
open Tests.Fixtures

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

let (host, writer, sw) = createHost ()
writer.WriteStartElement("Document")
host.ProcessValue text
writer.WriteEndElement()

sw.ToString() |> printfn "%s"
