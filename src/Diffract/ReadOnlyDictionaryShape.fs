namespace Diffract.ReadOnlyDictionaryShape

open System
open System.Collections.Generic
open TypeShape.Core

type IReadOnlyDictionaryVisitor<'R> =
    abstract Visit<'Dict, 'K, 'V when 'K : equality and 'Dict :> IReadOnlyDictionary<'K, 'V>> : unit -> 'R

type IShapeReadOnlyDictionary =
    abstract Key : TypeShape
    abstract Value : TypeShape
    abstract Accept : IReadOnlyDictionaryVisitor<'R> -> 'R

type private ShapeReadOnlyDictionary<'Dict, 'K, 'V when 'K : equality and 'Dict :> IReadOnlyDictionary<'K, 'V>>() =
    interface IShapeReadOnlyDictionary with
        member _.Key = shapeof<'K> :> _
        member _.Value = shapeof<'V> :> _
        member _.Accept(v) = v.Visit<'Dict, 'K, 'V>()

module Shape =

    let (|GenericInterface|_|) (fullName: string) (s: TypeShape) =
        match s.Type.GetInterface(fullName) with
        | null ->
            match s.ShapeInfo with
            | Generic (td, ta) when td.FullName = fullName -> Some ta
            | _ -> None
        | iface ->
            Some (iface.GetGenericArguments())

    let (|ReadOnlyDictionary|_|) (s: TypeShape) =
        match s with
        | GenericInterface "System.Collections.Generic.IReadOnlyDictionary`2" ta ->
            Activator.CreateInstanceGeneric<ShapeReadOnlyDictionary<_, _, _>>(Array.append [|s.Type|] ta)
            :?> IShapeReadOnlyDictionary
            |> Some
        | _ -> None
