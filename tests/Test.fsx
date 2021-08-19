#r "nuget: TypeShape"
#load "../src/Diffract/Interfaces.fs"
#load "../src/Diffract/Differ.fs"

open Diffract

type Foo = { x: int; y: float }
type Bar = { a: Foo }

Differ.diff
    { a = { x = 2; y = 2. } }
    { a = { x = 2; y = 1. } }
