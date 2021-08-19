namespace Diffract

open System
open TypeShape.Core

module Differ =

    let inline private wrap (p : 'a -> 'a -> Diff option) = unbox<'T -> 'T -> Diff option> p

    let inline private simpleEquality< ^a, 'T when ^a : equality> : 'T -> 'T -> Diff option =
        wrap (fun (x1: ^a) (x2: ^a) -> if x1 = x2 then None else Some (ValueDiff (x1, x2)))

    let rec diff<'T> : ('T -> 'T -> Diff option) =
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
            { new IEnumVisitor<'T -> 'T -> Diff option> with
                member _.Visit() = fun x1 x2 -> None }
            |> e.Accept
        | _ -> failwith $"Don't know how to diff values of type {typeof<'T>.AssemblyQualifiedName}"

    and diffFields (members: IShapeMember<'T>[]) wrap =
        let fields =
            members
            |> Array.map (fun f ->
                { new IMemberVisitor<'T, 'T -> 'T -> FieldDiff option> with
                    member _.Visit(shape) =
                        let fieldDiff = diff<'Field>
                        let name = shape.MemberInfo.Name
                        fun x1 x2 ->
                            fieldDiff (shape.Get x1) (shape.Get x2)
                            |> Option.map (fun diff -> name, diff) }
                |> f.Accept)
        fun x1 x2 ->
            match fields |> Array.choose (fun f -> f x1 x2) with
            | [||] -> None
            | diffs -> Some (wrap diffs)
