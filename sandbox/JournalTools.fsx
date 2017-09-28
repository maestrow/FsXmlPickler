module JournalTools

open System

let printHeader (text: string) = 
  let underline = 
    fun i -> "="
    |> Seq.init text.Length 
    |> String.Concat
  printfn "\n%s\n%s" text underline
