open System
open System.Collections.Generic
open System.Xml

let x: (Type * (XmlTextWriter -> obj -> unit)) list = 
  [
    typedefof<int>   , fun w v -> w.WriteValue (unbox v : int)
    typedefof<string>, fun w v -> w.WriteValue (unbox v : string)
  ] 

x
  |> List.map (fun (t, f) -> t, f)


let d = Dictionary<Type, XmlTextWriter->'T->unit>()

d.Add (typedefof<string>, x1)
d.Add (typedefof<int>, x2)

