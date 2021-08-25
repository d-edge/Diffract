namespace Diffract

#nowarn "40"

open System
open TypeShape.Core

module Differ =

    type DiffFunc<'T> = 'T -> 'T -> Diff option

    type FieldDiffFunc<'T> = 'T -> 'T -> FieldDiff option

    let inline wrap<'a, 'T> (p : DiffFunc<'a>) =
        unbox<DiffFunc<'T>> p

    let inline simpleEquality< ^a, 'T when ^a : equality> : DiffFunc<'T> =
        wrap (fun (x1: ^a) (x2: ^a) -> if x1 = x2 then None else Some (ValueDiff (x1, x2)))

    let inline private failwith (msg: string) = raise (DiffConstructionFailedException(msg))

    let rec diff<'T> : DiffFunc<'T> =
        match shapeof<'T> with
        | Shape.Unit -> wrap (fun () () -> None)
        | Shape.Bool -> simpleEquality<bool, 'T>
        | Shape.Byte -> simpleEquality<byte, 'T>
        | Shape.SByte -> simpleEquality<sbyte, 'T>
        | Shape.Int16 -> simpleEquality<int16, 'T>
        | Shape.UInt16 -> simpleEquality<uint16, 'T>
        | Shape.Int32 -> simpleEquality<int32, 'T>
        | Shape.UInt32 -> simpleEquality<uint32, 'T>
        | Shape.Int64 -> simpleEquality<int64, 'T>
        | Shape.UInt64 -> simpleEquality<uint64, 'T>
        | Shape.Single -> simpleEquality<single, 'T>
        | Shape.Double -> simpleEquality<double, 'T>
        | Shape.Decimal -> simpleEquality<decimal, 'T>
        | Shape.String -> simpleEquality<string, 'T>
        | Shape.Char -> simpleEquality<char, 'T>
        | Shape.Guid -> simpleEquality<Guid, 'T>
        | Shape.DateTime -> simpleEquality<DateTime, 'T>
        | Shape.DateTimeOffset -> simpleEquality<DateTimeOffset, 'T>
        | Shape.TimeSpan -> simpleEquality<TimeSpan, 'T>
        | Shape.BigInt -> simpleEquality<bigint, 'T>
        | Shape.Uri -> simpleEquality<Uri, 'T>
        | Shape.Tuple (:? ShapeTuple<'T> as t) -> diffFields t.Elements RecordDiff
        | Shape.Enumerable e -> diffEnumerable<'T> e
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as r) -> diffFields r.Fields RecordDiff
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as u) ->
            let tagNames = u.UnionCases |> Array.map (fun c -> c.CaseInfo.Name)
            let tagDiffs = u.UnionCases |> Array.map (fun c ->
                diffFields c.Fields (fun d -> UnionFieldDiff (c.CaseInfo.Name, d)))
            fun (x1: 'T) (x2: 'T) ->
                let t1 = u.GetTag x1
                let t2 = u.GetTag x2
                if t1 = t2 then
                    tagDiffs.[t1] x1 x2
                else
                    Some (UnionCaseDiff (tagNames.[t1], tagNames.[t2]))
        | Shape.Enum e ->
            { new IEnumVisitor<DiffFunc<'T>> with
                member _.Visit<'t, 'u when 't : enum<'u> and 't : struct and 't :> ValueType and 't : (new : unit -> 't)>() =
                    wrap (fun (x1: 't) (x2: 't) ->
                        // Can't do better without 't : equality? :(
                        if (x1 :> obj).Equals(x2) then None else Some (ValueDiff (x1, x2))) }
            |> e.Accept
        | Shape.Equality e ->
            { new IEqualityVisitor<DiffFunc<'T>> with
                member _.Visit<'t when 't : equality>() =
                    wrap (fun (x1: 't) (x2: 't) ->
                        if x1 = x2 then None else Some (ValueDiff (x1, x2))) }
            |> e.Accept
        | _ -> failwith $"Don't know how to diff values of type {typeof<'T>.AssemblyQualifiedName}"

    and diffFields (members: IShapeMember<'T>[]) wrap =
        let fields =
            members
            |> Array.map (fun f ->
                { new IMemberVisitor<'T, FieldDiffFunc<'T>> with
                    member _.Visit(shape) =
                        let fieldDiff = diff<'Field>
                        let name = shape.MemberInfo.Name
                        fun x1 x2 ->
                            fieldDiff (shape.Get x1) (shape.Get x2)
                            |> Option.map (fun diff -> { Name = name; Diff = diff }) }
                |> f.Accept)
        fun x1 x2 ->
            match fields |> Seq.choose (fun f -> f x1 x2) |> List.ofSeq with
            | [] -> None
            | diffs -> Some (wrap diffs)

    and diffEnumerable<'T> (e: IShapeEnumerable) : DiffFunc<'T> =
        { new IEnumerableVisitor<DiffFunc<'T>> with
            member _.Visit<'Enum, 'Elt when 'Enum :> seq<'Elt>>() =
                wrap (fun (s1: 'Enum) (s2: 'Enum) ->
                    let s1 = Seq.cache s1
                    let s2 = Seq.cache s2
                    let l1 = Seq.length s1
                    let l2 = Seq.length s2
                    if l1 <> l2 then
                        Some (CollectionCountDiff (l1, l2))
                    else
                        match
                            (s1, s2)
                            ||> Seq.mapi2 (fun i e1 e2 ->
                                diff<'Elt> e1 e2
                                |> Option.map (fun diff -> { Name = string i; Diff = diff }))
                            |> Seq.choose id
                            |> List.ofSeq
                            with
                        | [] -> None
                        | diffs -> Some (CollectionContentDiff diffs)) }
        |> e.Accept
