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

    static member AssertPrintParams = assertPrintParams

    static member GetDiffer<'T>([<ParamArray>] customDiffers: ICustomDiffer[]) =
        match customDiffers.Length with
        | 0 -> Differ.simple<'T>
        | 1 -> Differ.diffWith<'T> customDiffers.[0] (Dictionary())
        | _ -> Differ.diffWith<'T> (CombinedCustomDiffer(customDiffers)) (Dictionary())

    static member Diff<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>) =
        let differ = defaultDiffer differ
        differ.Diff(expected, actual)

    static member Assert(diff: Diff option, [<Optional>] param: PrintParams) =
        let param = defaultPrintParams param
        if Option.isSome diff then
            DiffPrinter.toString param diff
            |> AssertionFailedException
            |> raise

    static member Assert<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.Assert(diff, param)

    static member ToString(diff: Diff option, [<Optional>] param: PrintParams) =
        let param = defaultPrintParams param
        DiffPrinter.toString param diff

    static member ToString<'T>(expected: 'T, actual: 'T, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.ToString(diff, param)

    static member Write(diff: Diff option, [<Optional>] writer: TextWriter, [<Optional>] param: PrintParams) =
        let writer = if isNull writer then stdout else writer
        let param = defaultPrintParams param
        DiffPrinter.write param writer diff

    static member Write<'T>(expected: 'T, actual: 'T, [<Optional>] writer: TextWriter, [<Optional>] differ: IDiffer<'T>, [<Optional>] param: PrintParams) =
        let diff = Diffract.Diff(expected, actual, differ)
        Diffract.Write(diff, writer, param)
