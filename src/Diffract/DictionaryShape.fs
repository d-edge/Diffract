namespace Diffract.DictionaryShape

open System
open System.Collections.Generic
open TypeShape.Core

type IDictionaryVisitor<'R> =
    abstract Visit<'Dict, 'K, 'V when 'K : equality and 'Dict :> IDictionary<'K, 'V>> : unit -> 'R

type IShapeDictionary =
    abstract Key : TypeShape
    abstract Value : TypeShape
    abstract Accept : visitor: IDictionaryVisitor<'R> -> 'R

type private ShapeDictionary<'Dict, 'K, 'V when 'K : equality and 'Dict :> IDictionary<'K, 'V>>() =
    interface IShapeDictionary with
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

    let (|Dictionary|_|) (s: TypeShape) =
        match s with
        | GenericInterface "System.Collections.Generic.IDictionary`2" ta ->
            Activator.CreateInstanceGeneric<ShapeDictionary<_, _, _>>(Array.append [|s.Type|] ta)
            :?> IShapeDictionary
            |> Some
        | _ -> None
