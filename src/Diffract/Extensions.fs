namespace Diffract

open System.Collections.Generic
open System.IO
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<Extension; AbstractClass; Sealed>]
type Extensions private () =

    /// <summary>Throw <see cref="AssertionFailedException"/> if a diff is non-empty.</summary>
    /// <param name="differ">The diffing engine to use.</param>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="param">The printing parameters used to generate the exception message.</param>
    [<Extension>]
    static member Assert(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] param: PrintParams) =
        Diffract.Assert(expected, actual, differ, param)

    /// <summary>Print a diff to a string.</summary>
    /// <param name="differ">The diffing engine to use.</param>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="param">The printing parameters.</param>
    [<Extension>]
    static member ToString(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] param: PrintParams) =
        Diffract.ToString(expected, actual, differ, param)

    /// <summary>Print a diff to a TextWriter.</summary>
    /// <param name="differ">The diffing engine to use.</param>
    /// <param name="expected">The first value to diff.</param>
    /// <param name="actual">The second value to dif.</param>
    /// <param name="writer">The writer to print to. If null, use standard output.</param>
    /// <param name="param">The printing parameters.</param>
    [<Extension>]
    static member Write(differ: IDiffer<'T>, expected: 'T, actual: 'T, [<Optional>] writer: TextWriter, [<Optional>] param: PrintParams) =
        Diffract.Write(expected, actual, writer, differ, param)

    /// <summary>Get a diffing engine with support for specific types.</summary>
    /// <param name="custom">A custom differ to handle specific types.</param>
    [<Extension>]
    static member GetDiffer<'T>(custom: ICustomDiffer) =
        Differ.diffWith<'T> custom (Dictionary())

    /// <summary>Use in a custom differ to cast a differ for one type into a differ for another type.</summary>
    /// <seealso href="https://github.com/eiriktsarpalis/TypeShape">TypeShape documentation</seealso>
    [<Extension>]
    static member Unwrap<'T, 'U>(differ: IDiffer<'T>) =
        tryUnbox<IDiffer<'U>> differ
