namespace Diffract

module Diffract =

    let assertPrintParams : PrintParams =
        {
            indent = "  "
            x1Name = "Expect"
            x2Name = "Actual"
            neutralName = "Value"
        }

    let assertDiffWith param d =
        if Option.isSome d then
            DiffPrinter.toString param d
            |> AssertionFailedException
            |> raise

    let assertDiff d =
        assertDiffWith assertPrintParams d

    let assertEqualWith param (differ: IDiffer<'T>) x1 x2 =
        differ.Diff(x1, x2)
        |> assertDiffWith param

    let assertEqual differ x1 x2 =
        assertEqualWith assertPrintParams differ x1 x2

    let toStringWith param (differ: IDiffer<'T>) x1 x2 =
        differ.Diff(x1, x2)
        |> DiffPrinter.toString param

    let toString differ x1 x2 =
        toStringWith assertPrintParams differ x1 x2

    let printWith param (differ: IDiffer<'T>) x1 x2 =
        differ.Diff(x1, x2)
        |> DiffPrinter.write param stdout

    let print differ x1 x2 =
        printWith assertPrintParams differ x1 x2
