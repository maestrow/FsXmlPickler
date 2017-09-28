module XmlPickler.Conditions

open System
open FSharp.Reflection

let isList (t:Type) = t.IsGenericType && (t.GetGenericTypeDefinition() = typedefof<list<_>>)
let isUnion = FSharpType.IsUnion
let isRecord = FSharpType.IsRecord

