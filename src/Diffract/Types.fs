namespace Diffract

open System.IO
open System.Runtime.InteropServices

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

type AssertionFailedException(diff: string) =
    inherit System.Exception(diff)

type DiffConstructionFailedException(message: string, [<Optional>] innerException: exn) =
    inherit System.Exception(message, innerException)
