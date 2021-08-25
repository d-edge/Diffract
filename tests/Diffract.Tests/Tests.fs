module Tests

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
        d = Some (CollectionCountDiff (l1.Length, l2.Length))
    else
        let expectedDiffs =
            (l1, l2)
            ||> Seq.mapi2 (fun i x1 x2 -> Differ.simple.Diff(x1, x2) |> Option.map (fun d -> { Name = string i; Diff = d }))
            |> Seq.choose id
            |> List.ofSeq
        d = Some (CollectionContentDiff expectedDiffs)

[<Fact>]
let ``Example output`` () =
    Assert.Equal("\
Value differs by 2 fields:
  Expect.a.y = true
  Actual.a.y = false
  Value.b differs by union case:
    Expect.b is U1
    Actual.b is U2
",
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
    Assert.Equal("\
Value differs by 2 fields:
  Expect.a.y = true
  Actual.a.y = false
  Value.b differs by union case:
    Expect.b is U1
    Actual.b is U2
",
        ex.Message)

type CustomDiffable = { x: string }

module CustomDiff =

    type Custom() =
        interface ICustomDiffer with
            member this.GetCustomDiffer<'T>(_shape) =
                if typeof<'T> = typeof<CustomDiffable> then
                    let diff = this.GetDiffer<string>()
                    { new IDiffer<CustomDiffable> with
                        member _.Diff(x1, x2) = diff.Diff(x1.x, x2.x) }
                    |> unbox<IDiffer<'T>>
                    |> Some
                else
                    None

    let differ<'T> = Custom().GetDiffer<'T>()

[<Fact>]
let ``Custom differ`` () =
    Assert.Equal("\
Expect.x = \"a\"
Actual.x = \"b\"
",
        Differ.simple.ToString({ x = "a" }, { x = "b" }))
    Assert.Equal("\
Expect = \"a\"
Actual = \"b\"
",
        CustomDiff.differ.ToString({ x = "a" }, { x = "b" }))

//type Rec = { xRec: Rec option }
//
//[<Fact>]
//let ``Recursive type`` () =
//    Assert.Equal("\
//Value.xRec.Value.xRec differs by union case:
//  Expect.xRec.Value.xRec is None
//  Actual.xRec.Value.xRec is Some
//",
//        Differ.simple.ToString({ xRec = Some { xRec = None } }, { xRec = Some { xRec = Some { xRec = None } } }))
