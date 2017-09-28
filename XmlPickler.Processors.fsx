module XmlPickler.Processors

type ISequenceProcessor =
  abstract member ToSequence : obj -> seq<obj>
  abstract member FromSequence : seq<obj> -> obj

type ListProcessor<'T>() =
  interface ISequenceProcessor with
    member this.ToSequence (x: obj) = Seq.map box (x :?> list<'T>)
    member this.FromSequence (s: seq<obj>) = box [for x in s -> x :?> 'T]

