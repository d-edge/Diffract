namespace Diffract

type AssertionFailedException(diff: string) =
    inherit System.Exception(diff)

module Diffract =

    let assertPrintParams : DiffPrinter.PrintParams =
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

    let assertEqualWith param x1 x2 =
        Differ.diff x1 x2
        |> assertDiffWith param

    let assertEqual x1 x2 =
        assertEqualWith assertPrintParams x1 x2

    let toStringWith param x1 x2 =
        Differ.diff x1 x2
        |> DiffPrinter.toString param

    let toString x1 x2 =
        toStringWith assertPrintParams x1 x2

    let printWith param x1 x2 =
        Differ.diff x1 x2
        |> DiffPrinter.toStream param stdout

    let print x1 x2 =
        printWith assertPrintParams x1 x2
