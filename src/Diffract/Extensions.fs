namespace Diffract

open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension; AbstractClass; Sealed>]
type Extensions private () =

    [<Extension>]
    static member Assert(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] param: PrintParams) =
        Diffract.Assert(expected, actual, differ, param)

    [<Extension>]
    static member ToString(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] param: PrintParams) =
        Diffract.ToString(expected, actual, differ, param)

    [<Extension>]
    static member Write(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] writer: TextWriter, [<Optional>] param: PrintParams) =
        Diffract.Write(expected, actual, writer, differ, param)

    [<Extension>]
    static member GetDiffer<'T>(custom: ICustomDiffer) =
        Differ.diffWith<'T> custom (Dictionary())

    [<Extension>]
    static member Unwrap<'T, 'U>(differ: IDiffer<'T>) =
        Some (unbox<IDiffer<'U>> differ)
