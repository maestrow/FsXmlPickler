type A =
    | TheA
    | TheB of string
    
let a = TheA

printfn "%A" a

printfn "%A" a.IsTheA