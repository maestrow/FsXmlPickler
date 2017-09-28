open System
open System.Collections
open System.Collections.Generic
open System.IO

module Sequence = 
  type Item = 
    | Text of string
    | BeginTag of string
    | EndTag

module Xml = 
  type Tag = 
    { 
      Name: string
      Content: ContentItem list
    }

  and ContentItem = 
    | Value of string
    | Tag of Tag

open Sequence

let testSeq = 
  [
    BeginTag "a"   // <a>         
    Text "1"       //   1   
    BeginTag "b"   //   <b>       
    Text "2"       //     2   
    EndTag         //   </b> 
    BeginTag "c"   //   <c>       
    Text "3"       //     3   
    EndTag         //   </c> 
    BeginTag "d"   //   <d>       
    BeginTag "e"   //     <e>       
    Text "4"       //       4   
    EndTag         //     <e> 
    BeginTag "f"   //     <f>       
    Text "5"       //       5   
    EndTag         //     <f> 
    EndTag         //   </d> 
    EndTag         // </a> 
  ]
  |> Seq.ofList

open Xml


let rec makeTree (enm: IEnumerator<Item>) (stack: ContentItem list) = 
  if not <| enm.MoveNext () then
    stack
  else 
    match enm.Current with
    | BeginTag name -> makeTree enm (Tag { Name = name; Content = makeTree enm [] }::stack)
    | EndTag -> List.rev stack
    | Text value -> makeTree enm ((Value value)::stack)
            

let rec toXml (tree: ContentItem list) (level: int) (res: TextWriter) = 
  let indent = Seq.init level (fun i -> "  ") |> String.Concat
  let iterator i = 
    match i with
    | Value v -> res.WriteLine (sprintf "%s%s" indent v)
    | Tag t -> 
        res.WriteLine (sprintf "%s<%s>" indent t.Name)
        toXml t.Content (level+1) res
        res.WriteLine (sprintf "%s</%s>" indent t.Name)
  List.iter iterator tree

let enm = testSeq.GetEnumerator()
let tree = makeTree enm []

printfn "%A" tree


let tw = new StringWriter()
toXml tree 0 tw

tw.ToString() |> printfn "%s"
