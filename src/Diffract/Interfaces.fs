namespace Diffract

open System.Collections.Generic

type Diff =
    | ValueDiff of x1: obj * x2: obj
    | RecordDiff of fields: IReadOnlyList<FieldDiff>
    | UnionCaseDiff of caseName1: string * caseName2: string
    | UnionFieldDiff of case: string * fields: IReadOnlyList<FieldDiff>

and FieldDiff = string * Diff
