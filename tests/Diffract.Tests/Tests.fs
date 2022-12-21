module Tests

#nowarn "40"

open System.Collections.Generic
open Xunit
open FsCheck
open FsCheck.Xunit
open DEdge.Diffract

type Foo = { x: int; y: bool }

type U =
    | U1 of int
    | U2 of x: int * y: int

type Bar = { a: Foo; b: U }

[<Property>]
let ``No exception for equal values`` (x: Bar) = Differ.Assert(x, x)

[<Property>]
let ``Exception for non-equal values`` (x: Bar) (y: Bar) =
    x <> y
    ==> lazy
        Assert.Throws<AssertionFailedException>(fun () -> Differ.Assert(x, y)) |> ignore

[<Property>]
let ``List diff`` (l1: int list) (l2: int list) =
    let d = Differ.Diff(l1, l2)

    if l1 = l2 then
        d = None
    else
        let expectedDiffs =
            (l1, l2)
            ||> Seq.mapi2 (fun i x1 x2 -> Differ.Diff(x1, x2) |> Option.map (fun d -> { Name = string i; Diff = d }))
            |> Seq.choose id
            |> List.ofSeq

        d = Some(Diff.Collection(l1.Length, l2.Length, expectedDiffs))

[<Fact>]
let ``Example output`` () =
    Assert.Equal(
        "Value differs by 2 fields:\n  a.y Expect = true\n      Actual = false\n  b differs by union case:\n    Expect is U1\n    Actual is U2\n",
        Differ.ToString(
            { a = { x = 1; y = true }; b = U1 1 },
            { a = { x = 1; y = false }
              b = U2(1, 2) }
        )
    )

[<Fact>]
let ``Example error message`` () =
    let ex =
        Assert.Throws<AssertionFailedException>(fun () ->
            Differ.Assert(
                { a = { x = 1; y = true }; b = U1 1 },
                { a = { x = 1; y = false }
                  b = U2(1, 2) }
            ))

    Assert.Equal(
        "Value differs by 2 fields:\n  a.y Expect = true\n      Actual = false\n  b differs by union case:\n    Expect is U1\n    Actual is U2\n",
        ex.Message
    )

[<Fact>]
let ``Ensure first line is aligned`` () =
    let ex = Assert.Throws<AssertionFailedException>(fun () -> Differ.Assert(12, 13))
    Assert.Equal("\nExpect = 12\nActual = 13\n", ex.Message)
    Assert.Equal("Expect = 12\nActual = 13\n", Differ.ToString(12, 13))

[<Fact>]
let ``Example collection`` () =
    Assert.Equal(
        "x collection differs:\n  x.Count Expect = 2\n          Actual = 3\n  x[1] Expect = 3\n       Actual = 2\n",
        Differ.ToString({| x = [ 1; 3 ] |}, {| x = [ 1; 2; 3 ] |})
    )

type CustomDiffable = { x: string }

module MyDiffModule =

    type CustomDiffer() =
        interface ICustomDiffer with
            member this.GetCustomDiffer<'T>(differFactory, shape) =
                if shape.Type = typeof<CustomDiffable> then
                    let differ = differFactory.GetDiffer<string>()

                    { new IDiffer<CustomDiffable> with
                        member _.Diff(x1, x2) = differ.Diff(x1.x, x2.x) }
                    |> unbox<IDiffer<'T>>
                    |> Some
                else
                    None

    let differ<'T> = CustomDiffer().GetDiffer<'T>()

type MyDiffer(differFactory: IDifferFactory) =
    let stringDiffer = differFactory.GetDiffer<string>()

    interface IDiffer<CustomDiffable> with
        member _.Diff(x1, x2) = stringDiffer.Diff(x1.x, x2.x)

type MyCustomDiffer() =
    interface ICustomDiffer with
        member this.GetCustomDiffer<'T>(differFactory, shape) =
            if shape.Type = typeof<CustomDiffable> then
                MyDiffer(differFactory).Unwrap<CustomDiffable, 'T>()
            else
                None

type MyDiffType<'T>() =
    static member val Differ = MyCustomDiffer().GetDiffer<'T>()

[<Fact>]
let ``Custom differ`` () =
    Assert.Equal("x Expect = \"a\"\n  Actual = \"b\"\n", Differ.ToString({ x = "a" }, { x = "b" }))
    Assert.Equal("Expect = \"a\"\nActual = \"b\"\n", Differ.ToString({ x = "a" }, { x = "b" }, MyDiffModule.differ))
    Assert.Equal("Expect = \"a\"\nActual = \"b\"\n", Differ.ToString({ x = "a" }, { x = "b" }, MyDiffType.Differ))

module ``Custom differ with custom diff output`` =

    type MyCustomDiffer() =
        interface ICustomDiffer with
            member this.GetCustomDiffer<'T>(_, shape) =
                if shape.Type = typeof<CustomDiffable> then
                    { new IDiffer<CustomDiffable> with
                        member _.Diff(x1, x2) =
                            if x1.x = x2.x then
                                None
                            else
                                Diff.MakeCustom(fun writer param indent path recur ->
                                    if param.ensureFirstLineIsAligned then
                                        writer.WriteLine()

                                    let indentLike str =
                                        String.replicate (String.length str) " "

                                    let dpath = if path = "" then "" else path + " "
                                    writer.WriteLine($"{indent}{dpath}{param.x1Name} __is__ {x1.x}")
                                    writer.WriteLine($"{indent}{indentLike dpath}{param.x2Name} __is__ {x2.x}"))
                                |> Some }
                    |> unbox<IDiffer<'T>>
                    |> Some
                else
                    None

    type MyDiffType<'T>() =
        static member val Differ = MyCustomDiffer().GetDiffer<'T>()

    [<Fact>]
    let ``Assert with immediate value adds newline`` () =
        let ex =
            Assert.Throws<AssertionFailedException>(fun () -> Differ.Assert({ x = "a" }, { x = "b" }, MyDiffType.Differ))

        Assert.Equal("\nExpect __is__ a\nActual __is__ b\n", ex.Message)

    [<Fact>]
    let ``Assert with nested value doesn't add newline`` () =
        let ex =
            Assert.Throws<AssertionFailedException>(fun () ->
                Differ.Assert({| i = { x = "a" } |}, {| i = { x = "b" } |}, MyDiffType.Differ))

        Assert.Equal("i Expect __is__ a\n  Actual __is__ b\n", ex.Message)

    [<Fact>]
    let ``ToString with immediate value doesn't add newline`` () =
        let diff = Differ.ToString({ x = "a" }, { x = "b" }, MyDiffType.Differ)
        Assert.Equal("Expect __is__ a\nActual __is__ b\n", diff)

    [<Fact>]
    let ``ToString with nested value doesn't add newline`` () =
        let diff =
            Differ.ToString({| i = { x = "a" } |}, {| i = { x = "b" } |}, MyDiffType.Differ)

        Assert.Equal("i Expect __is__ a\n  Actual __is__ b\n", diff)

type Rec = { xRec: Rec option }

[<Fact>]
let ``Recursive type`` () =
    let x1 = { xRec = Some { xRec = None } }
    let x2 = { xRec = Some { xRec = Some { xRec = None } } }

    Assert.Equal(
        "xRec.Value.xRec differs by union case:\n  Expect is None\n  Actual is Some\n",
        Differ.ToString(x1, x2)
    )

[<Fact>]
let ``Anonymous record`` () =
    Assert.Null(Differ.Diff({| x = 1; y = "2" |}, {| x = 1; y = "2" |}))
    Assert.Equal("x Expect = 1\n  Actual = 2\n", Differ.ToString({| x = 1; y = "2" |}, {| x = 2; y = "2" |}))

[<Fact>]
let ``Both IEnumerable are null`` () =
    Differ.Assert({| x = Unchecked.defaultof<IEnumerable<int>> |}, {| x = Unchecked.defaultof<IEnumerable<int>> |})

[<Fact>]
let ``One IEnumerable is null but not the other`` () =
    let diff =
        Differ.ToString({| x = Unchecked.defaultof<IEnumerable<int>> |}, {| x = Seq.empty<int> |})

    Assert.Equal("x Expect = <null>\n  Actual = seq []\n", diff)

[<Fact>]
let ``Both IDictionary are null`` () =
    Differ.Assert(
        {| x = Unchecked.defaultof<IDictionary<int, int>> |},
        {| x = Unchecked.defaultof<IDictionary<int, int>> |}
    )

[<Fact>]
let ``One IDictionary is null but not the other`` () =
    let diff =
        Differ.ToString(
            {| x = Unchecked.defaultof<Dictionary<int, int>> |},
            {| x = System.Collections.Generic.Dictionary(Map.empty) |}
        )

    Assert.Equal("x Expect = <null>\n  Actual = seq []\n", diff)
