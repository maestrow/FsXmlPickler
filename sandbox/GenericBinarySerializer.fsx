// Source: http://www.fssnip.net/6u

module Serialization.Binary

exception EncodingError

exception NoEncoding of System.Type with
    override this.ToString() =
        sprintf "Failed to derive a binary encoding for type: %O" this.Data0

type E = (string -> int) -> System.IO.BinaryWriter -> obj -> unit
type D = (int -> string) -> System.IO.BinaryReader -> obj
type S = D * E

type Dictionary<'T1,'T2> = System.Collections.Generic.Dictionary<'T1,'T2>

let inline Basic<'T> (rd: System.IO.BinaryReader -> 'T) 
                     (wr: System.IO.BinaryWriter -> 'T -> unit) : S =
    let dec dS r = rd r :> obj
    let enc eS w (x: obj) = wr w (x :?> 'T)
    (dec, enc)

let inline Add<'T> (rd: System.IO.BinaryReader -> 'T)
                   (wr: System.IO.BinaryWriter -> 'T -> unit)
                   (d: Dictionary<_,_>) =
    d.[typeof<'T>] <- Basic rd wr

let Serializers =
    let d = Dictionary()
    Add (fun r -> r.ReadChar()) (fun w -> w.Write) d
    Add (fun r -> r.ReadByte()) (fun w -> w.Write) d
    Add (fun r -> r.ReadSByte()) (fun w -> w.Write) d
    Add (fun r -> r.ReadInt16()) (fun w -> w.Write) d
    Add (fun r -> r.ReadInt32()) (fun w -> w.Write) d
    Add (fun r -> r.ReadInt64()) (fun w -> w.Write) d
    Add (fun r -> r.ReadUInt16()) (fun w -> w.Write) d
    Add (fun r -> r.ReadUInt32()) (fun w -> w.Write) d
    Add (fun r -> r.ReadUInt64()) (fun w -> w.Write) d
    Add (fun r -> r.ReadSingle()) (fun w -> w.Write) d
    Add (fun r -> r.ReadDouble()) (fun w -> w.Write) d
    Add (fun r -> r.ReadDecimal()) (fun w -> w.Write) d
    Add (fun r -> r.ReadBoolean()) (fun w -> w.Write) d
    d.[typeof<string>] <-
        let decString : D = fun dS r -> dS (r.ReadInt32()) :> obj
        let encString : E = fun eS w x -> w.Write (eS (string x))
        (decString, encString)
    d

type FST = Reflection.FSharpType
type FSV = Reflection.FSharpValue

let TupleEncoder dE (t: System.Type) : E =
    let e = Array.map dE (FST.GetTupleElements t)
    let r = FSV.PreComputeTupleReader t
    fun eS w o -> Array.iter2 (fun e x -> e eS w x) e (r o)

let TupleDecoder dD (t: System.Type) : D =
    let e = Array.map dD (FST.GetTupleElements t)
    let c = FSV.PreComputeTupleConstructor t
    fun dS r -> c (Array.map (fun e -> e dS r) e)

let ArrayEncoder (dE: System.Type -> E) (t: System.Type) : E =
    let e = dE (t.GetElementType())
    fun eS w o ->
        let o = o :?> System.Array
        w.Write o.Length
        for x in o do
            e eS w x

let ArrayDecoder (dD: System.Type -> D) (t: System.Type) : D =
    let eT = t.GetElementType()
    let e  = dD eT
    fun dS r ->
        let k = r.ReadInt32()
        let res = System.Array.CreateInstance(eT, k)
        for i in 0 .. k - 1 do
            res.SetValue(e dS r, i)
        res :> obj

let Flags =
    System.Reflection.BindingFlags.Public
    ||| System.Reflection.BindingFlags.NonPublic

let UnionEncoder dE (t: System.Type) : E =
    let tR = FSV.PreComputeUnionTagReader(t, Flags)
    let cs =
        FST.GetUnionCases(t, Flags)
        |> Array.map (fun c ->
            let r = FSV.PreComputeUnionReader(c, Flags)
            let fs =
                c.GetFields()
                |> Array.map (fun f -> dE f.PropertyType)
            (r, fs))
    fun wS w o ->
        let tag = tR o
        w.Write (byte tag)
        let (r, fs) = cs.[tag]
        Array.iter2 (fun e x -> e wS w x) fs (r o)

let UnionDecoder dD (t: System.Type) : D =
    let cs =
        FST.GetUnionCases(t, Flags)
        |> Array.map (fun c ->
            let mk = FSV.PreComputeUnionConstructor(c, Flags)
            let fs =
                c.GetFields()
                |> Array.map (fun f -> dD f.PropertyType)
            (mk, fs))
    let k = cs.Length
    fun dS r ->
        let tag = int (r.ReadByte())
        let (mk, fs) = cs.[tag]
        fs
        |> Array.map (fun f -> f dS r)
        |> mk

let RecordEncoder dE (t: System.Type) : E =
    let fs =
        FST.GetRecordFields(t, Flags)
        |> Array.map (fun f ->
            let r = FSV.PreComputeRecordFieldReader f
            (fun eS w o -> dE f.PropertyType eS w (r o)))
    fun eS w o -> Array.iter (fun f -> f eS w o) fs

let RecordDecoder dD (t: System.Type) : D =
    let mk = FSV.PreComputeRecordConstructor(t, Flags)
    let fs =
        FST.GetRecordFields(t, Flags)
        |> Array.map (fun f -> dD f.PropertyType)
    fun dS r ->
        fs
        |> Array.map (fun dec -> dec dS r)
        |> mk

type IDictionaryProcessor =
    abstract member ToSequence : obj -> seq<obj*obj>
    abstract member FromSequence : seq<obj*obj> -> obj

type ISequenceProcessor =
    abstract member ToSequence : obj -> seq<obj>
    abstract member FromSequence : seq<obj> -> obj

type DictionaryProcessor<'T1,'T2 when 'T1 : comparison>() =
    interface IDictionaryProcessor with
        member this.ToSequence (map: obj) =
            (map :?> Dictionary<'T1,'T2>)
            |> Seq.map (fun (KeyValue (k, v)) -> (box k, box v))
        member this.FromSequence (seq: seq<obj*obj>) =
            let d = Dictionary()
            for (k, v) in seq do
                d.[k :?> 'T1] <- v :?> 'T2
            box d

type MapProcessor<'T1,'T2 when 'T1 : comparison>() =
    interface IDictionaryProcessor with
        member this.ToSequence (map: obj) =
            (map :?> Map<'T1,'T2>)
            |> Seq.map (fun (KeyValue (k, v)) -> (box k, box v))
        member this.FromSequence (seq: seq<obj*obj>) =
            seq
            |> Seq.map (fun (k, v) -> (k :?> 'T1, v :?> 'T2))
            |> Map.ofSeq
            |> box

type ListProcessor<'T>() =
    interface ISequenceProcessor with
        member this.ToSequence (x: obj) = Seq.map box (x :?> list<'T>)
        member this.FromSequence (s: seq<obj>) = box [for x in s -> x :?> 'T]

type SetProcessor<'T when 'T : comparison>() =
    interface ISequenceProcessor with
        member this.ToSequence (x: obj) = Seq.map box (x :?> Set<'T>)
        member this.FromSequence (s: seq<obj>) =
            s
            |> Seq.map (fun x -> x :?> 'T)
            |> Set.ofSeq
            |> box

let DictionaryDecoder (dP: IDictionaryProcessor) dD kT vT : D =
    let kD = dD kT
    let vD = dD vT
    fun dS r ->
        let k = r.ReadInt32()
        Array.init k (fun _ ->
            let key = kD dS r
            let value = vD dS r
            (key, value))
        |> dP.FromSequence

let DictionaryEncoder (dP: IDictionaryProcessor) dE kT vT : E =
    let kE = dE kT
    let vE = dE vT
    fun eS w x ->
        let s = dP.ToSequence x
        w.Write (Seq.length s)
        for (k, v) in s do
            kE eS w k
            vE eS w v

let SequenceDecoder (sP: ISequenceProcessor) dD eT : D =
    let eD = dD eT
    fun dS r ->
        let k = r.ReadInt32()
        Array.init k (fun _ -> eD dS r)
        |> sP.FromSequence

let SequenceEncoder (sP: ISequenceProcessor) dE eT : E =
    let eE = dE eT
    fun dS w x ->
        let s = sP.ToSequence x
        w.Write (Seq.length s)
        for e in s do
            eE dS w e

let inline GetEncoding scalar array tuple union record dict seq
                       (cache: Dictionary<_,_>) =
    let recurse t =
        lock cache <| fun () ->
            cache.[t] <-
                Choice1Of2 (fun i v ->
                    match cache.TryGetValue t with
                    | true, Choice1Of2 f -> f i v
                    | _ -> raise (NoEncoding t))
    let rec get (t: System.Type) =
        let derive dD =
            try
                let r =
                    if t.IsGenericType then
                        let d = t.GetGenericTypeDefinition()
                        let a = t.GetGenericArguments()
                        if d = typedefof<Map<_,_>> then
                            let dP =
                                typedefof<MapProcessor<_,_>>
                                    .MakeGenericType(a)
                                |> System.Activator.CreateInstance
                                |> unbox : IDictionaryProcessor
                            Some (dict dP dD a.[0] a.[1])
                        elif d = typedefof<Dictionary<_,_>> then
                            let dP =
                                typedefof<DictionaryProcessor<_,_>>
                                    .MakeGenericType(a)
                                |> System.Activator.CreateInstance
                                |> unbox : IDictionaryProcessor
                            Some (dict dP dD a.[0] a.[1])
                        elif d = typedefof<list<_>> then
                            let sP =
                                typedefof<ListProcessor<_>>
                                    .MakeGenericType(a)
                                |> System.Activator.CreateInstance
                                |> unbox : ISequenceProcessor
                            Some (seq sP dD a.[0])
                        elif d = typedefof<Set<_>> then
                            let sP =
                                typedefof<SetProcessor<_>>
                                    .MakeGenericType(a)
                                |> System.Activator.CreateInstance
                                |> unbox : ISequenceProcessor
                            Some (seq sP dD a.[0])
                        else
                            None
                    else
                        None
                if r.IsSome then 
                    Choice1Of2 r.Value
                else
                    if t.IsArray && t.GetArrayRank() = 1 then
                        Choice1Of2 (array dD t)
                    elif FST.IsTuple t then
                        Choice1Of2 (tuple dD t)
                    elif FST.IsUnion (t, Flags) then
                        recurse t
                        Choice1Of2 (union dD t)
                    elif FST.IsRecord (t, Flags) then
                        recurse t
                        Choice1Of2 (record dD t)
                    else
                        Choice2Of2 t
            with NoEncoding t ->
                Choice2Of2 t
        if t = null then Choice2Of2 t else
            match Serializers.TryGetValue t with
            | true, x ->
                Choice1Of2 (scalar x)
            | _ ->
                let d =
                    match cache.TryGetValue t with
                    | true, d -> Some d
                    | _ -> None
                match d with
                | Some d -> d
                | None ->
                    let dD t =
                        match get t with
                        | Choice1Of2 d -> d
                        | Choice2Of2 d -> raise (NoEncoding t)
                    let d = derive dD
                    cache.[t] <- d
                    d
    get

[<Sealed>]
type Encoding(t: System.Type, d: D, e: E) =

    member this.Decode stream =
        let mode = System.IO.Compression.CompressionMode.Decompress
        use reader =
            new System.IO.BinaryReader(
                new System.IO.Compression.GZipStream(stream, mode))
        try
            if reader.ReadString() <> t.AssemblyQualifiedName then
                raise EncodingError
            let dS = Dictionary()
            for i in 0 .. reader.ReadInt32() - 1 do
                let s = reader.ReadString()
                dS.[i] <- s
            d (fun x -> dS.[x]) reader
        with _ ->
            raise EncodingError

    member this.Encode stream (value: obj) =
        let mode   = System.IO.Compression.CompressionMode.Compress
        use memory = new System.IO.MemoryStream()
        use actual = new System.IO.Compression.GZipStream(stream, mode)
        use wM     = new System.IO.BinaryWriter(memory)
        use wA     = new System.IO.BinaryWriter(actual)
        try
            let eS = Dictionary()
            let encS x =
                match eS.TryGetValue x with
                | true, y -> y
                | _ ->
                    let y = eS.Count
                    eS.[x] <- y
                    y
            e encS wM value
            wA.Write t.AssemblyQualifiedName
            wA.Write eS.Count
            for v in eS.Keys do
                wA.Write v
            memory.WriteTo actual
        with _ ->
            raise EncodingError

    member this.Type = t

[<Sealed>]
type EncodingProvider() =

    let Decoders = Dictionary()
    let Encoders = Dictionary()

    let GetDecoder (t: System.Type) =
        GetEncoding fst ArrayDecoder TupleDecoder
            UnionDecoder RecordDecoder
            DictionaryDecoder SequenceDecoder
            Decoders t

    let GetEncoder (t: System.Type) =
        GetEncoding snd ArrayEncoder TupleEncoder
            UnionEncoder RecordEncoder
            DictionaryEncoder SequenceEncoder
            Encoders t

    member this.DeriveEncoding t =
        match GetEncoder t, GetDecoder t with
        | Choice1Of2 e, Choice1Of2 d ->
            Encoding (t, d, e)
        | Choice2Of2 t, _ | _, Choice2Of2 t ->
            raise (NoEncoding t)

    static member Create() =
        EncodingProvider()