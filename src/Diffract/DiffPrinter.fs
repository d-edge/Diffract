module Diffract.DiffPrinter

open System.IO

let toStreamImpl param (w: TextWriter) (d: Diff) =
    let addPathField path field = if path = "" then field else (path + "." + field)
    let addPathIndex path index = path + "[" + index + "]"
    let indentLike str = String.replicate (String.length str) " "
    let displayPath path = if path = "" then param.neutralName else path

    let rec loop (indent: string) (path: string) (d: Diff) =
        match d with
        | Diff.Value (x1, x2) ->
            let dpath = if path = "" then "" else path + " "
            w.WriteLine($"%s{indent}%s{dpath}%s{param.x1Name} = %A{x1}")
            w.WriteLine($"%s{indent}%s{indentLike dpath}%s{param.x2Name} = %A{x2}")
        | Diff.Record [field] ->
            loop indent (addPathField path field.Name) field.Diff
        | Diff.Record fields ->
            w.WriteLine($"%s{indent}%s{displayPath path} differs by %i{List.length fields} fields:")
            let indent = indent + param.indent
            for field in fields do
                loop indent (addPathField path field.Name) field.Diff
        | Diff.UnionCase (caseName1, caseName2) ->
            w.WriteLine($"%s{indent}%s{displayPath path} differs by union case:")
            let indent = indent + param.indent
            w.WriteLine($"%s{indent}%s{param.x1Name} is %s{caseName1}")
            w.WriteLine($"%s{indent}%s{param.x2Name} is %s{caseName2}")
        | Diff.UnionField (_case, [field]) ->
            loop indent (addPathField path field.Name) field.Diff
        | Diff.UnionField (case, fields) ->
            w.WriteLine($"%s{indent}%s{displayPath path} differs by union case %s{case} fields:")
            let indent = indent + param.indent
            for field in fields do
                loop indent (addPathField path field.Name) field.Diff
        | Diff.CollectionCount (c1, c2) ->
            w.WriteLine($"%s{indent}%s{displayPath path} collection differs by count:")
            let indent = indent + param.indent
            w.WriteLine($"%s{indent}%s{param.x1Name} = %i{c1}")
            w.WriteLine($"%s{indent}%s{param.x2Name} = %i{c2}")
        | Diff.CollectionContent diffs ->
            w.WriteLine($"%s{indent}%s{displayPath path} collection differs by content:")
            let indent = indent + param.indent
            for item in diffs do
                loop indent (addPathIndex path item.Name) item.Diff
        | Diff.Custom cd ->
            cd.WriteTo(w, param, indent, path, loop)
        | Diff.Dictionary (keysInX1, keysInX2, common) ->
            w.WriteLine($"%s{indent}%s{displayPath path} dictionary differs:")
            let indent = indent + param.indent
            for k in keysInX1 do
                w.WriteLine($"%s{indent}%s{param.x2Name}[%s{k}] is missing")
            for k in keysInX2 do
                w.WriteLine($"%s{indent}%s{param.x1Name}[%s{k}] is missing")
            for item in common do
                loop indent (addPathIndex path item.Name) item.Diff
    loop "" "" d

let write (param: PrintParams) (w: TextWriter) (d: Diff option) =
    match d with
    | None -> w.WriteLine($"No differences between {param.x1Name} and {param.x2Name}.")
    | Some d -> toStreamImpl param w d

let toString param d =
    use w = new StringWriter(NewLine = "\n")
    write param w d
    w.ToString()
