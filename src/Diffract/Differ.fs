namespace Diffract

#nowarn "40"

open System
open TypeShape.Core

module Differ =

    let inline wrap<'Outer, 'Inner> (f: 'Inner -> 'Inner -> Diff option) =
        unbox<IDiffer<'Outer>> { new IDiffer<'Inner> with member _.Diff(x, y) = f x y }

    let inline simpleEquality<'Outer, ^Inner when ^Inner : equality> : IDiffer<'Outer> =
        wrap< ^Outer, ^Inner> (fun x1 x2 -> if x1 = x2 then None else Some (ValueDiff (x1, x2)))

    let inline private failwith (msg: string) = raise (DifferConstructionFailedException(msg))

    let rec diffWith<'T> (custom: ICustomDiffer) : IDiffer<'T> =
        let (|Custom|_|) shape = custom.GetCustomDiffer(shape)
        match shapeof<'T> with
        | Custom d -> d
        | Shape.Unit -> wrap<'T, unit> (fun () () -> None)
        | Shape.Bool -> simpleEquality<'T, bool>
        | Shape.Byte -> simpleEquality<'T, byte>
        | Shape.SByte -> simpleEquality<'T, sbyte>
        | Shape.Int16 -> simpleEquality<'T, int16>
        | Shape.UInt16 -> simpleEquality<'T, uint16>
        | Shape.Int32 -> simpleEquality<'T, int32>
        | Shape.UInt32 -> simpleEquality<'T, uint32>
        | Shape.Int64 -> simpleEquality<'T, int64>
        | Shape.UInt64 -> simpleEquality<'T, uint64>
        | Shape.Single -> simpleEquality<'T, single>
        | Shape.Double -> simpleEquality<'T, double>
        | Shape.Decimal -> simpleEquality<'T, decimal>
        | Shape.String -> simpleEquality<'T, string>
        | Shape.Char -> simpleEquality<'T, char>
        | Shape.Guid -> simpleEquality<'T, Guid>
        | Shape.DateTime -> simpleEquality<'T, DateTime>
        | Shape.DateTimeOffset -> simpleEquality<'T, DateTimeOffset>
        | Shape.TimeSpan -> simpleEquality<'T, TimeSpan>
        | Shape.BigInt -> simpleEquality<'T, bigint>
        | Shape.Uri -> simpleEquality<'T, Uri>
        | Shape.Tuple (:? ShapeTuple<'T> as t) -> diffFields custom t.Elements RecordDiff
        | Shape.Enumerable e -> diffEnumerable custom e
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as r) -> diffFields custom r.Fields RecordDiff
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as u) ->
            let tagNames = u.UnionCases |> Array.map (fun c -> c.CaseInfo.Name)
            let tagDiffs = u.UnionCases |> Array.map (fun c ->
                diffFields custom c.Fields (fun d -> UnionFieldDiff (c.CaseInfo.Name, d)))
            wrap<'T, 'T> (fun x1 x2 ->
                let t1 = u.GetTag x1
                let t2 = u.GetTag x2
                if t1 = t2 then
                    tagDiffs.[t1].Diff(x1, x2)
                else
                    Some (UnionCaseDiff (tagNames.[t1], tagNames.[t2])))
        | Shape.Enum e ->
            { new IEnumVisitor<IDiffer<'T>> with
                member _.Visit<'Enum, 'Underlying when 'Enum : enum<'Underlying> and 'Enum : struct and 'Enum :> ValueType and 'Enum : (new : unit -> 'Enum)>() =
                    wrap<'T, 'Enum> (fun x1 x2 ->
                        // Can't do better without 'Enum : equality? :(
                        if (x1 :> obj).Equals(x2) then None else Some (ValueDiff (x1, x2))) }
            |> e.Accept
        | Shape.Equality e ->
            { new IEqualityVisitor<IDiffer<'T>> with
                member _.Visit<'Actual when 'Actual : equality>() =
                    wrap<'T, 'Actual> (fun x1 x2 ->
                        if x1 = x2 then None else Some (ValueDiff (x1, x2))) }
            |> e.Accept
        | _ -> failwith $"Don't know how to diff values of type {typeof<'T>.AssemblyQualifiedName}"

    and diffFields<'T> (custom: ICustomDiffer) (members: IShapeMember<'T>[]) (wrapFieldDiffs: FieldDiff list -> Diff) : IDiffer<'T> =
        let fields =
            members
            |> Array.map (fun f ->
                { new IMemberVisitor<'T, 'T -> 'T -> FieldDiff option> with
                    member _.Visit<'Field>(shape) =
                        let fieldDiff = diffWith<'Field> custom
                        let name = shape.MemberInfo.Name
                        fun x1 x2 ->
                            fieldDiff.Diff(shape.Get x1, shape.Get x2)
                            |> Option.map (fun diff -> { Name = name; Diff = diff }) }
                |> f.Accept)
        wrap<'T, 'T> (fun x1 x2 ->
            match fields |> Seq.choose (fun f -> f x1 x2) |> List.ofSeq with
            | [] -> None
            | diffs -> Some (wrapFieldDiffs diffs))

    and diffEnumerable<'T> (custom: ICustomDiffer) (e: IShapeEnumerable) : IDiffer<'T> =
        { new IEnumerableVisitor<IDiffer<'T>> with
            member _.Visit<'Enum, 'Elt when 'Enum :> seq<'Elt>>() =
                let diffItem = diffWith<'Elt> custom
                wrap<'T, 'Enum> (fun s1 s2 ->
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
                                diffItem.Diff(e1, e2)
                                |> Option.map (fun diff -> { Name = string i; Diff = diff }))
                            |> Seq.choose id
                            |> List.ofSeq
                            with
                        | [] -> None
                        | diffs -> Some (CollectionContentDiff diffs)) }
        |> e.Accept

    let simple<'T> = diffWith<'T> (NoCustomDiffer())
