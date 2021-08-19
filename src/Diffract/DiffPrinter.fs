module Diffract.DiffPrinter

open System.IO

let toStreamImpl (w: TextWriter) (d: Diff) =
    ()

let toStream (w: TextWriter) (d: Diff option) =
    match d with
    | None -> w.WriteLine("No differences.")
    | Some d -> toStreamImpl w d
