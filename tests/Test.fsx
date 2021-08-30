#r "nuget: TypeShape"
#load "../src/Diffract/Types.fs"
#load "../src/Diffract/ReadOnlyDictionaryShape.fs"
#load "../src/Diffract/Differ.fs"
#load "../src/Diffract/DiffPrinter.fs"
#load "../src/Diffract/Diffract.fs"
#load "../src/Diffract/Extensions.fs"

open Diffract

type Foo = { x: int; y: float }
type U =
    | U1 of int
    | U2 of x: int * y: int
type Bar = { a: Foo; b: U }

Differ.simple.Print(
    [ { a = { x = 2; y = 1. }
        b = U2 (2, 1) }
      { a = { x = 2; y = 1. }
        b = U2 (2, 1) } ],
    [ { a = { x = 2; y = 1. }
        b = U2 (2, 1) }
      { a = { x = 1; y = 1. }
        b = U2 (2, 3) } ])

Differ.simple.Print(
    Map [(1, 3); (2, 2); (3, 1)],
    Map [(2, 2); (3, 2); (4, 1)])
