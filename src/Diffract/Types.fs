namespace Diffract


type Diff =
    | ValueDiff of x1: obj * x2: obj
    | RecordDiff of fields: FieldDiff list
    | UnionCaseDiff of caseName1: string * caseName2: string
    | UnionFieldDiff of case: string * fields: FieldDiff list
    | CollectionCountDiff of count1: int * count2: int
    | CollectionContentDiff of items: FieldDiff list

and [<Struct>] FieldDiff =
    {
        Name: string
        Diff: Diff
    }
