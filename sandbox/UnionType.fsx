#load "JournalTools.fsx"

open JournalTools
open FSharp.Reflection

type TheUnionCase =
  | EmptyCase
  | IntCase of int
  | StringCase of string
  | TupleNamedCase of age:int * name:string
  | TupleCase of bool * float * string

module GetUnionCases = 
  printHeader "Get Union Cases with fields"
  typeof<TheUnionCase> 
  |> FSharpType.GetUnionCases
  |> Array.map (fun i -> 
      let fields = 
        i.GetFields () 
        |> Array.map (fun f -> sprintf "%s : %s" f.Name f.PropertyType.Name)
        |> String.concat "; "
      sprintf "%s = [%s]" i.Name fields
      )
  |> Array.iter (printfn "%s")

module PreComputeUnionTagReader = 
  printHeader "PreComputeUnionTagReader"
  let val1 = TupleNamedCase (1, "asd")
  let val2 = EmptyCase
  let tagReader = FSharpValue.PreComputeUnionTagReader typedefof<TheUnionCase>
  printfn "%d" <| tagReader val1
  printfn "%d" <| tagReader val2


module MakeUnion = 
  printHeader "MakeUnion"
  let cases = FSharpType.GetUnionCases typeof<TheUnionCase>
  let caseInfo = cases |> Array.find (fun i -> i.Name = "IntCase")
  let value = FSharpValue.MakeUnion (caseInfo, [|box 1|])
  value |> printfn "%A"