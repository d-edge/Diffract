namespace Diffract

open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension; AbstractClass; Sealed>]
type Extensions private () =

    static let defaultParam p =
        if obj.ReferenceEquals(p, null) then Diffract.assertPrintParams else p

    [<Extension>]
    static member Assert(differ: IDiffer<'T>, x1: 'T, x2: 'T, [<Optional>] param: PrintParams) =
        let param = defaultParam param
        Diffract.assertEqualWith param differ x1 x2

    [<Extension>]
    static member ToString(differ: IDiffer<'T>, x1: 'T, x2: 'T, [<Optional>] param: PrintParams) =
        let param = defaultParam param
        Diffract.toStringWith param differ x1 x2

    [<Extension>]
    static member Write(differ: IDiffer<'T>, x1: 'T, x2: 'T, writer: TextWriter, [<Optional>] param: PrintParams) =
        let param = defaultParam param
        differ.Diff(x1, x2)
        |> DiffPrinter.write param writer

    [<Extension>]
    static member Print(differ: IDiffer<'T>, x1: 'T, x2: 'T, [<Optional>] param: PrintParams) =
        differ.Write(x1, x2, stdout, param)

    [<Extension>]
    static member GetDiffer<'T>(custom: ICustomDiffer) =
        Differ.diffWith<'T> custom
