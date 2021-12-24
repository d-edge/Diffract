namespace Diffract

open System.Collections.Generic
open System.IO
open System.Runtime.InteropServices
open TypeShape.Core

/// A computed diff between two objects.
type Diff =
    /// The objects are leaf values and are different.
    | Value of x1: obj * x2: obj
    /// The objects are records or plain objects and some of their fields differ.
    | Record of fields: IReadOnlyList<FieldDiff>
    /// The objects are F# unions with different cases.
    | UnionCase of caseName1: string * caseName2: string
    /// The objects are F# unions with the same case but some of their fields differ.
    | UnionField of case: string * fields: IReadOnlyList<FieldDiff>
    /// The objects are collections and their lengths and/or some of their items differ.
    | Collection of count1: int * count2: int * items: IReadOnlyList<FieldDiff>
    /// The objects are dictionaries and some items are only present in one of them and/or some of their items differ.
    | Dictionary of keysInX1: IReadOnlyList<string> * keysInX2: IReadOnlyList<string> * common: IReadOnlyList<FieldDiff>
    /// The objects are considered different by a custom differ.
    | Custom of ICustomDiff

    static member MakeCustom(printer: System.Func<_, _, _, _, _, _>) =
        Custom { new ICustomDiff with member _.WriteTo(w, p, i, pa, r) = printer.Invoke(w, p, i, pa, r) }

/// A computed diff between two values of a field.
and [<Struct>] FieldDiff =
    {
        /// The name of the field.
        Name: string
        /// The diff between the values.
        Diff: Diff
    }

/// Parameterize the display of a diff.
and PrintParams =
    {
        /// The string used to indent items. Default: "  "
        indent: string
        /// The name given to the first object. Default: "Expect"
        x1Name: string
        /// The name given to the second object. Default: "Actual"
        x2Name: string
        /// The common name given to both objects. Default: "Value"
        neutralName: string
        /// Ensure that Expect and Actual remain aligned even if there is text before the first line
        /// by prepending a newline if the diff is a single Value.
        /// Default: true for Assert(), false for Write() and ToString().
        ensureFirstLineIsAligned: bool
    }

/// A custom computed diff that.
and ICustomDiff =
    /// <summary>Print this diff.</summary>
    /// <param name="writer">The writer to write the diff to.</param>
    /// <param name="param">The printing parameters.</param>
    /// <param name="indent">The current indentation level. Should always be a replication of param.indent.</param>
    /// <param name="path">The drilled-down path to access the currently diffed values from the root objects.</param>
    /// <param name="recur">The function to call to recursively print an inner diff.
    ///     Takes indent, path and the inner diff to print as arguments.</param>
    abstract WriteTo : writer: TextWriter * param: PrintParams * indent: string * path: string * recur: (string -> string -> Diff -> unit) -> unit

/// A differ for a specific type.
type IDiffer<'T> =
    /// Diff two values.
    abstract Diff : x1: 'T * x2: 'T -> Diff option

/// Generates a differ for any given type.
type IDifferFactory =
    /// Get the differ for a given type.
    abstract GetDiffer<'T> : unit -> IDiffer<'T>

/// Generates a differ for a specific type or set of types.
type ICustomDiffer =
    /// <summary>Get the differ for this type.</summary>
    /// <param name="differFactory">The factory to use to get differs for nested values.</param>
    /// <param name="shape">The TypeShape for the current type.</param>
    /// <returns>A differ for this type, or None if this custom differ doesn't handle this type.</returns>
    abstract GetCustomDiffer<'T> : differFactory: IDifferFactory * shape: TypeShape<'T> -> IDiffer<'T> option

type NoCustomDiffer() =
    interface ICustomDiffer with
        member _.GetCustomDiffer(_, _) = None

type CombinedCustomDiffer(customDiffers: seq<ICustomDiffer>) =
    interface ICustomDiffer with
        member _.GetCustomDiffer(differ, shape) =
            customDiffers |> Seq.tryPick (fun customDiffer -> customDiffer.GetCustomDiffer(differ, shape))

/// Thrown when the differ found differences between two objects.
type AssertionFailedException(diff: string) =
    inherit System.Exception(diff)

/// Thrown when the differ factory couldn't build a differ for a type.
type DifferConstructionFailedException(message: string, [<Optional>] innerException: exn) =
    inherit System.Exception(message, innerException)
