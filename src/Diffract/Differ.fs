namespace Diffract

#nowarn "40"

open System
open System.Collections.Generic
open TypeShape.Core
open Diffract.ReadOnlyDictionaryShape
open Diffract.DictionaryShape

module Differ =

    type private Cache = Dictionary<Type, IDifferFactory>
    type private CachedDiffer<'T> = Cache -> IDiffer<'T>

    let private addToCache (differ: CachedDiffer<'T>) : CachedDiffer<'T> =
        let ty = typeof<'T>
        fun cache ->
            match cache.TryGetValue(ty) with
            | false, _ ->
                let r = ref Unchecked.defaultof<IDiffer<'T>>
                cache.Add(ty, { new IDifferFactory with
                    member _.GetDiffer<'U>() =
                        { new IDiffer<'U> with
                            member _.Diff(x1, x2) =
                                (unbox<IDiffer<'U>> !r).Diff(x1, x2) } })
                let differ = differ cache
                r := differ
                differ
            | true, differ ->
                differ.GetDiffer<'T>()

    let private wrap<'Outer, 'Inner> (f: 'Inner -> 'Inner -> Diff option) =
        addToCache (fun _cache -> unbox<IDiffer<'Outer>> { new IDiffer<'Inner> with member _.Diff(x, y) = f x y })

    let inline private simpleEquality<'Outer, ^Inner when ^Inner : equality> : CachedDiffer<'Outer> =
        wrap< ^Outer, ^Inner> (fun x1 x2 -> if x1 = x2 then None else Some (Diff.Value (x1, x2)))

    let inline private failwith (msg: string) = raise (DifferConstructionFailedException(msg))

    let rec diffWith<'T> (custom: ICustomDiffer) (cache: Cache) : IDiffer<'T> =
        let getCached = { new IDifferFactory with
            member _.GetDiffer<'U>() =
                match cache.TryGetValue(typeof<'U>) with
                | true, d -> d.GetDiffer<'U>()
                | false, _ -> diffWith<'U> custom cache }
        let (|Custom|_|) shape = custom.GetCustomDiffer(getCached, shape)
        match shapeof<'T> with
        | Custom d -> d
        | Shape.Unit -> wrap<'T, unit> (fun () () -> None) cache
        | Shape.Bool -> simpleEquality<'T, bool> cache
        | Shape.Byte -> simpleEquality<'T, byte> cache
        | Shape.SByte -> simpleEquality<'T, sbyte> cache
        | Shape.Int16 -> simpleEquality<'T, int16> cache
        | Shape.UInt16 -> simpleEquality<'T, uint16> cache
        | Shape.Int32 -> simpleEquality<'T, int32> cache
        | Shape.UInt32 -> simpleEquality<'T, uint32> cache
        | Shape.Int64 -> simpleEquality<'T, int64> cache
        | Shape.UInt64 -> simpleEquality<'T, uint64> cache
        | Shape.Single -> simpleEquality<'T, single> cache
        | Shape.Double -> simpleEquality<'T, double> cache
        | Shape.Decimal -> simpleEquality<'T, decimal> cache
        | Shape.String -> simpleEquality<'T, string> cache
        | Shape.Char -> simpleEquality<'T, char> cache
        | Shape.Guid -> simpleEquality<'T, Guid> cache
        | Shape.DateTime -> simpleEquality<'T, DateTime> cache
        | Shape.DateTimeOffset -> simpleEquality<'T, DateTimeOffset> cache
        | Shape.TimeSpan -> simpleEquality<'T, TimeSpan> cache
        | Shape.BigInt -> simpleEquality<'T, bigint> cache
        | Shape.Uri -> simpleEquality<'T, Uri> cache
        | Shape.Tuple (:? ShapeTuple<'T> as t) -> addToCache (diffFields custom t.Elements Diff.Record) cache
        | Shape.ReadOnlyDictionary d -> addToCache (diffReadOnlyDict custom d) cache
        | Shape.Dictionary d -> addToCache (diffDict custom d) cache
        | Shape.Enumerable e -> addToCache (diffEnumerable custom e) cache
        | Shape.FSharpRecord (:? ShapeFSharpRecord<'T> as r) -> addToCache (diffFields custom r.Fields Diff.Record) cache
        | Shape.FSharpUnion (:? ShapeFSharpUnion<'T> as u) ->
            let tagNames = u.UnionCases |> Array.map (fun c -> c.CaseInfo.Name)
            let tagDiffs = u.UnionCases |> Array.map (fun c ->
                diffFields custom c.Fields (fun d -> Diff.UnionField (c.CaseInfo.Name, d)) cache)
            wrap<'T, 'T> (fun x1 x2 ->
                let t1 = u.GetTag x1
                let t2 = u.GetTag x2
                if t1 = t2 then
                    tagDiffs.[t1].Diff(x1, x2)
                else
                    Some (Diff.UnionCase (tagNames.[t1], tagNames.[t2])))
                cache
        | Shape.Enum e ->
            { new IEnumVisitor<IDiffer<'T>> with
                member _.Visit<'Enum, 'Underlying when 'Enum : enum<'Underlying> and 'Enum : struct and 'Enum :> ValueType and 'Enum : (new : unit -> 'Enum)>() =
                    wrap<'T, 'Enum> (fun x1 x2 ->
                        // Can't do better without 'Enum : equality? :(
                        if (x1 :> obj).Equals(x2) then None else Some (Diff.Value (x1, x2)))
                        cache }
            |> e.Accept
        | Shape.Poco (:? ShapePoco<'T> as p) ->
            let members = p.Properties |> Array.filter (fun p -> p.IsPublic)
            addToCache (diffReadOnlyFields<'T> custom members Diff.Record) cache
        | Shape.Equality e ->
            { new IEqualityVisitor<IDiffer<'T>> with
                member _.Visit<'Actual when 'Actual : equality>() =
                    wrap<'T, 'Actual> (fun x1 x2 ->
                        if x1 = x2 then None else Some (Diff.Value (x1, x2)))
                        cache }
            |> e.Accept
        | _ -> failwith $"Don't know how to diff values of type {typeof<'T>.AssemblyQualifiedName}"

    and diffReadOnlyFields<'T> (custom: ICustomDiffer) (members: IShapeReadOnlyMember<'T>[]) (wrapFieldDiffs: IReadOnlyList<FieldDiff> -> Diff) (cache: Cache) : IDiffer<'T> =
        let fields =
            members
            |> Array.map (fun f ->
                { new IReadOnlyMemberVisitor<'T, 'T -> 'T -> FieldDiff option> with
                    member _.Visit<'Field>(shape) =
                        let fieldDiff = diffWith<'Field> custom cache
                        let name = shape.MemberInfo.Name
                        fun x1 x2 ->
                            fieldDiff.Diff(shape.Get x1, shape.Get x2)
                            |> Option.map (fun diff -> { Name = name; Diff = diff }) }
                |> f.Accept)
        { new IDiffer<'T> with
            member _.Diff(x1, x2) =
                match fields |> Seq.choose (fun f -> f x1 x2) |> List.ofSeq with
                | [] -> None
                | diffs -> Some (wrapFieldDiffs diffs) }

    and diffFields<'T> (custom: ICustomDiffer) (members: IShapeMember<'T>[]) (wrapFieldDiffs: IReadOnlyList<FieldDiff> -> Diff) (cache: Cache) : IDiffer<'T> =
        diffReadOnlyFields custom (unbox<IShapeReadOnlyMember<'T>[]> members) wrapFieldDiffs cache

    and diffEnumerable<'T> (custom: ICustomDiffer) (e: IShapeEnumerable) (cache: Cache) : IDiffer<'T> =
        { new IEnumerableVisitor<IDiffer<'T>> with
            member _.Visit<'Enum, 'Elt when 'Enum :> seq<'Elt>>() =
                let diffItem = diffWith<'Elt> custom cache
                { new IDiffer<'Enum> with
                    member _.Diff(s1, s2) =
                        let s1 = Seq.cache s1
                        let s2 = Seq.cache s2
                        let l1 = Seq.length s1
                        let l2 = Seq.length s2
                        match
                            (s1, s2)
                            ||> Seq.mapi2 (fun i e1 e2 ->
                                diffItem.Diff(e1, e2)
                                |> Option.map (fun diff -> { Name = string i; Diff = diff }))
                            |> Seq.choose id
                            |> List.ofSeq
                            with
                        | [] when l1 = l2 -> None
                        | diffs -> Some (Diff.Collection (l1, l2, diffs)) }
                |> unbox<IDiffer<'T>> }
        |> e.Accept

    and diffReadOnlyDict<'T> (custom: ICustomDiffer) (d: IShapeReadOnlyDictionary) (cache: Cache) : IDiffer<'T> =
        { new IReadOnlyDictionaryVisitor<IDiffer<'T>> with
            member _.Visit<'Dict, 'K, 'V when 'K : equality and 'Dict :> IReadOnlyDictionary<'K, 'V>>() =
                let diffItem = diffWith<'V> custom cache
                { new IDiffer<'Dict> with
                    member _.Diff(d1, d2) =
                        let seen = HashSet<'K>()
                        let struct (keysInX1, common) =
                            (struct ([], []), d1)
                            ||> Seq.fold (fun (struct (keysInX1, common) as state) (KeyValue (k, v1)) ->
                                seen.Add(k) |> ignore
                                match d2.TryGetValue(k) with
                                | true, v2 ->
                                    match diffItem.Diff(v1, v2) with
                                    | Some d -> struct (keysInX1, { Name = string k; Diff = d } :: common)
                                    | None -> state
                                | false, _ -> struct (string k :: keysInX1, common))
                        let keysInX2 =
                            d2
                            |> Seq.choose (fun (KeyValue (k, _)) ->
                                if seen.Contains(k) then None else Some (string k))
                            |> List.ofSeq
                        match keysInX1, keysInX2, common with
                        | [], [], [] -> None
                        | _ -> Some (Diff.Dictionary (keysInX1, keysInX2, common)) }
                |> unbox<IDiffer<'T>> }
        |> d.Accept

    and diffDict<'T> (custom: ICustomDiffer) (d: IShapeDictionary) (cache: Cache) : IDiffer<'T> =
        { new IDictionaryVisitor<IDiffer<'T>> with
            member _.Visit<'Dict, 'K, 'V when 'K : equality and 'Dict :> IDictionary<'K, 'V>>() =
                let diffItem = diffWith<'V> custom cache
                { new IDiffer<'Dict> with
                    member _.Diff(d1, d2) =
                        let seen = HashSet<'K>()
                        let struct (keysInX1, common) =
                            (struct ([], []), d1)
                            ||> Seq.fold (fun (struct (keysInX1, common) as state) (KeyValue (k, v1)) ->
                                seen.Add(k) |> ignore
                                match d2.TryGetValue(k) with
                                | true, v2 ->
                                    match diffItem.Diff(v1, v2) with
                                    | Some d -> struct (keysInX1, { Name = string k; Diff = d } :: common)
                                    | None -> state
                                | false, _ -> struct (string k :: keysInX1, common))
                        let keysInX2 =
                            d2
                            |> Seq.choose (fun (KeyValue (k, _)) ->
                                if seen.Contains(k) then None else Some (string k))
                            |> List.ofSeq
                        match keysInX1, keysInX2, common with
                        | [], [], [] -> None
                        | _ -> Some (Diff.Dictionary (keysInX1, keysInX2, common)) }
                |> unbox<IDiffer<'T>> }
        |> d.Accept

    let rec simple<'T> = diffWith<'T> (NoCustomDiffer()) (Dictionary())
