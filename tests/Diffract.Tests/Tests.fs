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
    Diffract.assertEqual x x

[<Property>]
let ``Exception for non-equal values`` (x: Bar) (y: Bar) =
    x <> y ==> lazy
        Assert.Throws<AssertionFailedException>(fun () ->
            Diffract.assertEqual x y)
        |> ignore

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
        Diffract.toString
            { a = { x = 1; y = true }
              b = U1 1 }
            { a = { x = 1; y = false }
              b = U2 (1, 2) })

[<Fact>]
let ``Example error message`` () =
    let ex = Assert.Throws<AssertionFailedException>(fun () ->
        Diffract.assertEqual
            { a = { x = 1; y = true }
              b = U1 1 }
            { a = { x = 1; y = false }
              b = U2 (1, 2) })
    Assert.Equal("\
Value differs by 2 fields:
  Expect.a.y = true
  Actual.a.y = false
  Value.b differs by union case:
    Expect.b is U1
    Actual.b is U2
",
        ex.Message)
