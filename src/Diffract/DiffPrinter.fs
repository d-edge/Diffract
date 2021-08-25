module Diffract.DiffPrinter

open System.IO

type PrintParams =
    {
        indent: string
        x1Name: string
        x2Name: string
        neutralName: string
    }

let toStreamImpl param (w: TextWriter) (d: Diff) =
    let rec loop (indent: string) (path: string) (d: Diff) =
        match d with
        | ValueDiff (x1, x2) ->
            w.WriteLine($"%s{indent}%s{param.x1Name}%s{path} = %A{x1}")
            w.WriteLine($"%s{indent}%s{param.x2Name}%s{path} = %A{x2}")
        | RecordDiff fields when fields.Count = 1 ->
            let field = fields.[0]
            loop indent $"%s{path}.%s{field.Name}" field.Diff
        | RecordDiff fields ->
            w.WriteLine($"%s{indent}%s{param.neutralName}%s{path} differs by %i{fields.Count} fields:")
            let indent = indent + param.indent
            for field in fields do
                loop indent $"%s{path}.%s{field.Name}" field.Diff
        | UnionCaseDiff (caseName1, caseName2) ->
            w.WriteLine($"%s{indent}%s{param.neutralName}%s{path} differs by union case:")
            let indent = indent + param.indent
            w.WriteLine($"%s{indent}%s{param.x1Name}%s{path} is %s{caseName1}")
            w.WriteLine($"%s{indent}%s{param.x2Name}%s{path} is %s{caseName2}")
        | UnionFieldDiff (case, fields) ->
            w.WriteLine($"%s{indent}%s{param.neutralName}%s{path} differs by union case %s{case} fields:")
            let indent = indent + param.indent
            for field in fields do
                loop indent $"%s{path}.%s{field.Name}" field.Diff
    loop "" "" d

let toStream (param: PrintParams) (w: TextWriter) (d: Diff option) =
    match d with
    | None -> w.WriteLine($"No differences between {param.x1Name} and {param.x2Name}.")
    | Some d -> toStreamImpl param w d

let toString param d =
    use w = new StringWriter()
    toStream param w d
    w.ToString()
