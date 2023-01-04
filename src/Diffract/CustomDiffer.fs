namespace DEdge.Diffract

open System
open DEdge.Diffract

[<Sealed; AbstractClass>]
type CustomDiffer<'T> =

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="buildDiffFunction">Builds the diff function for this type.</param>
    static member Build(buildDiffFunction: Func<IDifferFactory, Func<'T, 'T, Diff option>>) =
        { new ICustomDiffer with
            member this.GetCustomDiffer<'U>(differFactory, shape) =
                if shape.Type = typeof<'T> then
                    let diffFunction = buildDiffFunction.Invoke(differFactory)
                    { new IDiffer<'T> with
                        member _.Diff(x1, x2) = diffFunction.Invoke(x1, x2) }
                    |> unbox<IDiffer<'U>>
                    |> Some
                else
                    None }

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="buildDiffFunction">Builds the diff function for this type.</param>
    static member Build(buildDiffFunction: IDifferFactory -> 'T -> 'T -> Diff option) =
        { new ICustomDiffer with
            member this.GetCustomDiffer<'U>(differFactory, shape) =
                if shape.Type = typeof<'T> then
                    let diffFunction = buildDiffFunction differFactory
                    { new IDiffer<'T> with
                        member _.Diff(x1, x2) = diffFunction x1 x2 }
                    |> unbox<IDiffer<'U>>
                    |> Some
                else
                    None }

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="diffFunction">The diff function for this type.</param>
    static member Build(diffFunction: Func<'T, 'T, Diff option>) =
        CustomDiffer.Build(fun _ -> diffFunction)

    /// <summary>
    /// Create a custom differ for a specific type by mapping it to a diffable type.
    /// </summary>
    /// <param name="mapFunction">The mapping function.</param>
    /// <typeparam name="T">The type for which a custom differ is being created.</typeparam>
    /// <typeparam name="U">The type used to actually perform the diff.</typeparam>
    static member Map<'U>(mapFunction: Func<'T, 'U>) =
        CustomDiffer<'T>.Build(fun differFactory ->
            let differ = differFactory.GetDiffer<'U>()
            fun x1 x2 -> differ.Diff(mapFunction.Invoke(x1), mapFunction.Invoke(x2)))

[<Sealed; AbstractClass>]
type CustomDiffer =

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="buildDiffFunction">Builds the diff function for this type.</param>
    static member Build(buildDiffFunction: Func<IDifferFactory, Func<'T, 'T, Diff option>>) =
        CustomDiffer<'T>.Build(buildDiffFunction)

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="buildDiffFunction">Builds the diff function for this type.</param>
    static member Build(buildDiffFunction: IDifferFactory -> 'T -> 'T -> Diff option) =
        CustomDiffer<'T>.Build(buildDiffFunction)

    /// <summary>
    /// Create a custom differ for a specific type.
    /// </summary>
    /// <param name="diffFunction">The diff function for this type.</param>
    static member Build(diffFunction: Func<'T, 'T, Diff option>) =
        CustomDiffer<'T>.Build(diffFunction)

    /// <summary>
    /// Create a custom differ for a leaf type using default comparison and a custom display format.
    /// </summary>
    /// <param name="format">The display format.</param>
    static member Leaf<'T when 'T : equality> (format: Func<'T, string>) =
        CustomDiffer.Build<'T>(fun x y ->
            if x = y then
                None
            else
                Diff.Value(format.Invoke(x), format.Invoke(y))
                |> Some)

    /// <summary>
    /// Combine multiple custom differs.
    /// </summary>
    /// <param name="differs">The custom differs.</param>
    static member Combine (differs: seq<ICustomDiffer>) =
        CombinedCustomDiffer(differs) :> ICustomDiffer

    /// <summary>
    /// Combine multiple custom differs.
    /// </summary>
    /// <param name="differs">The custom differs.</param>
    static member Combine ([<ParamArray>] differs: ICustomDiffer[]) =
        CustomDiffer.Combine(differs :> seq<_>)
