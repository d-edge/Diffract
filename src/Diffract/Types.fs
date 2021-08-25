namespace Diffract

open System.IO
open System.Runtime.InteropServices
open TypeShape.Core

type Diff =
    | ValueDiff of x1: obj * x2: obj
    | RecordDiff of fields: FieldDiff list
    | UnionCaseDiff of caseName1: string * caseName2: string
    | UnionFieldDiff of case: string * fields: FieldDiff list
    | CollectionCountDiff of count1: int * count2: int
    | CollectionContentDiff of items: FieldDiff list
    | CustomDiff of ICustomDiff

and [<Struct>] FieldDiff =
    {
        Name: string
        Diff: Diff
    }

and PrintParams =
    {
        indent: string
        x1Name: string
        x2Name: string
        neutralName: string
    }

and ICustomDiff =
    abstract WriteTo : writer: TextWriter * param: PrintParams * indent: string * path: string * recur: (string -> string -> Diff -> unit) -> unit

type IDiffer<'T> =
    abstract Diff : 'T * 'T -> Diff option

type ICustomDiffer =
    abstract GetCustomDiffer<'T> : TypeShape<'T> -> IDiffer<'T> option

type [<Struct>] NoCustomDiffer =
    interface ICustomDiffer with member _.GetCustomDiffer(_) = None

type AssertionFailedException(diff: string) =
    inherit System.Exception(diff)

type DifferConstructionFailedException(message: string, [<Optional>] innerException: exn) =
    inherit System.Exception(message, innerException)
