open FSharp.Reflection

open FSharp.Reflection

type TheUnionCase =
    | EmptyCase
    | IntCase of int
    | StringCase of string
    | TupleNamedCase of age:int * name:string
    | TupleCase of bool * float * string
    
let val1 = TupleNamedCase (1, "asd")
let val2 = EmptyCase
    
let tagReader = FSharpValue.PreComputeUnionTagReader typedefof<TheUnionCase>

printfn "%d" <| tagReader val1
printfn "%d" <| tagReader val2

val1