module Tests

#nowarn "40"

open Xunit
open FsCheck
open FsCheck.Xunit
open Diffract

type Foo = { x: int; y: bool }
type U =
    | U1 of int
    | U2 of x: int * y: int
type Bar = { a: Foo; b: U }

[<Property>]
let ``No exception for equal values`` (x: Bar) =
    Differ.simple.Assert(x, x)

[<Property>]
let ``Exception for non-equal values`` (x: Bar) (y: Bar) =
    x <> y ==> lazy
        Assert.Throws<AssertionFailedException>(fun () ->
            Differ.simple.Assert(x, y))
        |> ignore

[<Property>]
let ``List diff`` (l1: int list) (l2: int list) =
    let d = Differ.simple.Diff(l1, l2)
    if l1 = l2 then
        d = None
    elif l1.Length <> l2.Length then
        d = Some (Diff.CollectionCount (l1.Length, l2.Length))
    else
        let expectedDiffs =
            (l1, l2)
            ||> Seq.mapi2 (fun i x1 x2 -> Differ.simple.Diff(x1, x2) |> Option.map (fun d -> { Name = string i; Diff = d }))
            |> Seq.choose id
            |> List.ofSeq
        d = Some (Diff.CollectionContent expectedDiffs)

[<Fact>]
let ``Example output`` () =
    Assert.Equal("Value differs by 2 fields:\n  Expect.a.y = true\n  Actual.a.y = false\n  Value.b differs by union case:\n    Expect.b is U1\n    Actual.b is U2\n",
        Differ.simple.ToString(
            { a = { x = 1; y = true }
              b = U1 1 },
            { a = { x = 1; y = false }
              b = U2 (1, 2) }))

[<Fact>]
let ``Example error message`` () =
    let ex = Assert.Throws<AssertionFailedException>(fun () ->
        Differ.simple.Assert(
            { a = { x = 1; y = true }
              b = U1 1 },
            { a = { x = 1; y = false }
              b = U2 (1, 2) }))
    Assert.Equal("Value differs by 2 fields:\n  Expect.a.y = true\n  Actual.a.y = false\n  Value.b differs by union case:\n    Expect.b is U1\n    Actual.b is U2\n",
        ex.Message)

type CustomDiffable = { x: string }

module rec MyDiffModule =

    let differ<'T> = CustomDiffer().GetDiffer<'T>()

    type CustomDiffer() =
        interface ICustomDiffer with
            member this.GetCustomDiffer<'T>(getDiffer, _shape) =
                if typeof<'T> = typeof<CustomDiffable> then
                    let differ = getDiffer.GetDiffer<string>()
                    { new IDiffer<CustomDiffable> with
                        member _.Diff(x1, x2) = differ.Diff(x1.x, x2.x) }
                    |> unbox<IDiffer<'T>>
                    |> Some
                else
                    None

type MyDiffType<'T>() =
    static member val Differ = MyCustomDiffer().GetDiffer<'T>()

and MyCustomDiffer() =
    interface ICustomDiffer with
        member this.GetCustomDiffer<'T>(getDiffer, _shape) =
            if typeof<'T> = typeof<CustomDiffable> then
                let differ = getDiffer.GetDiffer<string>()
                { new IDiffer<CustomDiffable> with
                    member _.Diff(x1, x2) = differ.Diff(x1.x, x2.x) }
                |> unbox<IDiffer<'T>>
                |> Some
            else
                None

[<Fact>]
let ``Custom differ`` () =
    Assert.Equal("Expect.x = \"a\"\nActual.x = \"b\"\n",
        Differ.simple.ToString({ x = "a" }, { x = "b" }))
    Assert.Equal("Expect = \"a\"\nActual = \"b\"\n",
        MyDiffModule.differ.ToString({ x = "a" }, { x = "b" }))
    Assert.Equal("Expect = \"a\"\nActual = \"b\"\n",
        MyDiffType.Differ.ToString({ x = "a" }, { x = "b" }))

type Rec = { xRec: Rec option }

[<Fact>]
let ``Recursive type`` () =
    let x1 = { xRec = Some { xRec = None } }
    let x2 = { xRec = Some { xRec = Some { xRec = None } } }
    Assert.Equal("Value.xRec.Value.xRec differs by union case:\n  Expect.xRec.Value.xRec is None\n  Actual.xRec.Value.xRec is Some\n",
        Differ.simple.ToString(x1, x2))
