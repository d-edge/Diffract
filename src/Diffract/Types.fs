namespace Diffract

open System.IO
open System.Runtime.InteropServices
open TypeShape.Core

type Diff =
    | Value of x1: obj * x2: obj
    | Record of fields: FieldDiff list
    | UnionCase of caseName1: string * caseName2: string
    | UnionField of case: string * fields: FieldDiff list
    | CollectionCount of count1: int * count2: int
    | CollectionContent of items: FieldDiff list
    | Dictionary of keysInX1: string list * keysInX2: string list * common: FieldDiff list
    | Custom of ICustomDiff

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
    abstract Diff : x1: 'T * x2: 'T -> Diff option

type IDifferFactory =
    abstract GetDiffer<'T> : unit -> IDiffer<'T>

type ICustomDiffer =
    abstract GetCustomDiffer<'T> : differFactory: IDifferFactory * shape: TypeShape<'T> -> IDiffer<'T> option

type NoCustomDiffer() =
    interface ICustomDiffer with
        member _.GetCustomDiffer(_, _) = None

type CombinedCustomDiffer(customDiffers: seq<ICustomDiffer>) =
    interface ICustomDiffer with
        member _.GetCustomDiffer(differ, shape) =
            customDiffers |> Seq.tryPick (fun customDiffer -> customDiffer.GetCustomDiffer(differ, shape))

type AssertionFailedException(diff: string) =
    inherit System.Exception(diff)

type DifferConstructionFailedException(message: string, [<Optional>] innerException: exn) =
    inherit System.Exception(message, innerException)
