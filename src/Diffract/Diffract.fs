namespace Diffract

open System
open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices

[<AbstractClass; Sealed>]
type Diffract private () =

    static let assertPrintParams : PrintParams =
        {
            indent = "  "
            x1Name = "Expect"
            x2Name = "Actual"
            neutralName = "Value"
        }

    static let defaultPrintParams p =
        if obj.ReferenceEquals(p, null) then assertPrintParams else p

    static let defaultDiffer d =
        if obj.ReferenceEquals(d, null) then Differ.simple else d

    /// The default print parameters for assertions.
    static member AssertPrintParams = assertPrintParams

    /// <summary>Get a differ for a specific type.</summary>
    /// <param name="customDiffers">Custom differs to handle specific types.</param>
    static member GetDiffer<'T>([<ParamArray>] customDiffers: ICustomDiffer[]) =
        match customDiffers with
        | null | [||] -> Differ.simple<'T>
        | [| customDiffer |] -> Differ.diffWith<'T> customDiffer (Dictionary())
        | _ -> Differ.diffWith<'T> (CombinedCustomDiffer(customDiffers)) (Dictionary())

    /// <summary>Compute the diff between two values.</summary>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="differ">The differ to use. If null, use <see cref="GetDiffer">GetDiffer&lt;T&gt;()</see>.</param>
    /// <returns>The diff between the two objects, or None if they are found equal.</returns>
    static member Diff<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>) =
        let differ = defaultDiffer differ
        differ.Diff(expected, actual)

    /// <summary>Throw <see cref="AssertionFailedException"/> if a diff is non-empty.</summary>
    /// <param name="diff">The diff to check.</param>
    /// <param name="param">The printing parameters used to generate the exception message.</param>
    static member Assert(diff: Diff option, [<Optional>] param: PrintParams) =
        let param = defaultPrintParams param
        if Option.isSome diff then
            DiffPrinter.toString param diff
            |> AssertionFailedException
            |> raise

    /// <summary>Throw <see cref="AssertionFailedException"/> if a diff is non-empty.</summary>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="differ">The differ to use. If null, use <see cref="GetDiffer">GetDiffer&lt;T&gt;()</see>.</param>
    /// <param name="param">The printing parameters used to generate the exception message.</param>
    static member Assert<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.Assert(diff, param)

    /// <summary>Print a diff to a string.</summary>
    /// <param name="diff">The diff to print.</param>
    /// <param name="param">The printing parameters.</param>
    static member ToString(diff: Diff option, [<Optional>] param: PrintParams) =
        let param = defaultPrintParams param
        DiffPrinter.toString param diff

    /// <summary>Print a diff to a string.</summary>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="differ">The differ to use. If null, use <see cref="GetDiffer">GetDiffer&lt;T&gt;()</see>.</param>
    /// <param name="param">The printing parameters.</param>
    static member ToString<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.ToString(diff, param)

    /// <summary>Print a diff to a TextWriter.</summary>
    /// <param name="diff">The diff to print.</param>
    /// <param name="writer">The writer to print to. If null, use standard output.</param>
    /// <param name="param">The printing parameters.</param>
    static member Write(diff: Diff option, [<Optional>] writer: TextWriter, [<Optional>] param: PrintParams) =
        let writer = if isNull writer then stdout else writer
        let param = defaultPrintParams param
        DiffPrinter.write param writer diff

    /// <summary>Print a diff to a TextWriter.</summary>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="differ">The differ to use. If null, use <see cref="GetDiffer">GetDiffer&lt;T&gt;()</see>.</param>
    /// <param name="writer">The writer to print to. If null, use standard output.</param>
    /// <param name="param">The printing parameters.</param>
    static member Write<'T>(expected: 'T, actual: 'T, [<Optional>] writer: TextWriter, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.Write(diff, writer, param)
