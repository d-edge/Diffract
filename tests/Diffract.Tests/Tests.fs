module Tests

#nowarn "40"

open Xunit
open FsCheck
open FsCheck.Xunit
open DEdge.Diffract

type Foo = { xxxxx: int; yyyyy: bool }
type U =
    | U1 of int
    | U2 of x: int * y: int
type Bar = { aaa: Foo; bbb: U }

let assertStr (expected: string, actual: string) =
    Assert.Equal(expected.Replace("\r\n", "\n"), actual)

[<Property>]
let ``No exception for equal values`` (x: Bar) =
    Differ.Assert(x, x)

[<Property>]
let ``Exception for non-equal values`` (x: Bar) (y: Bar) =
    x <> y ==> lazy
        Assert.Throws<AssertionFailedException>(fun () ->
            Differ.Assert(x, y))
        |> ignore

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
        d = Some (Diff.Collection (l1.Length, l2.Length, expectedDiffs))

[<Fact>]
let ``Example output`` () =
    assertStr("\
Value differs by 2 fields:
  aaa.yyyyy Expect = true
            Actual = false
  bbb differs by union case:
    Expect is U1
    Actual is U2
",
        Differ.ToString(
            { aaa = { xxxxx = 1; yyyyy = true }
              bbb = U1 1 },
            { aaa = { xxxxx = 1; yyyyy = false }
              bbb = U2 (1, 2) }))

[<Fact>]
let ``Example error message`` () =
    let ex = Assert.Throws<AssertionFailedException>(fun () ->
        Differ.Assert(
            { aaa = { xxxxx = 1; yyyyy = true }
              bbb = U1 1 },
            { aaa = { xxxxx = 1; yyyyy = false }
              bbb = U2 (1, 2) }))
    assertStr("\
Value differs by 2 fields:
  aaa.yyyyy Expect = true
            Actual = false
  bbb differs by union case:
    Expect is U1
    Actual is U2
",
        ex.Message)

[<Fact>]
let ``Ensure first line is aligned`` () =
    let ex = Assert.Throws<AssertionFailedException>(fun () -> Differ.Assert(12, 13))
    assertStr("
Expect = 12
Actual = 13
", ex.Message)
    assertStr("\
Expect = 12
Actual = 13
", Differ.ToString(12, 13))

[<Fact>]
let ``Example collection`` () =
    assertStr("\
xxx collection differs:
  xxx.Count Expect = 2
            Actual = 3
  xxx[1] Expect = 3
         Actual = 2
",
        Differ.ToString(
            {| xxx = [1; 3] |},
            {| xxx = [1; 2; 3] |}))

type CustomDiffable = { xxx: string }

module MyDiffModule =

    type CustomDiffer() =
        interface ICustomDiffer with
            member this.GetCustomDiffer<'T>(differFactory, shape) =
                if shape.Type = typeof<CustomDiffable> then
                    let differ = differFactory.GetDiffer<string>()
                    { new IDiffer<CustomDiffable> with
                        member _.Diff(x1, x2) = differ.Diff(x1.xxx, x2.xxx) }
                    |> unbox<IDiffer<'T>>
                    |> Some
                else
                    None

    let differ<'T> = CustomDiffer().GetDiffer<'T>()

module MyDiffWithCombinators =

    let customDiffer = CustomDiffer<CustomDiffable>.Build(fun factory ->
        let stringDiffer = factory.GetDiffer<string>()
        fun x1 x2 -> stringDiffer.Diff(x1.xxx, x2.xxx))

    let differ<'T> = customDiffer.GetDiffer<'T>()

type MyDiffer(differFactory: IDifferFactory) =
    let stringDiffer = differFactory.GetDiffer<string>()

    interface IDiffer<CustomDiffable> with
        member _.Diff(x1, x2) = stringDiffer.Diff(x1.xxx, x2.xxx)

type MyCustomDiffer() =
    interface ICustomDiffer with
        member this.GetCustomDiffer<'T>(differFactory, shape) =
            if shape.Type = typeof<CustomDiffable> then
                MyDiffer(differFactory).Unwrap<CustomDiffable, 'T>()
            else
                None

type MyDiffType<'T>() =
    static member val Differ = MyCustomDiffer().GetDiffer<'T>()

type MyDiffTypeWithCombinators<'T>() =
    static let customDiffer = CustomDiffer<CustomDiffable>.Build(fun factory ->
        let stringDiffer = factory.GetDiffer<string>()
        fun x1 x2 -> stringDiffer.Diff(x1.xxx, x2.xxx))

    static member val Differ = customDiffer.GetDiffer<'T>()

[<Fact>]
let ``Custom differ`` () =
    assertStr("\
xxx Expect = \"a\"
    Actual = \"b\"
",
        Differ.ToString({ xxx = "a" }, { xxx = "b" }))
    assertStr("\
Expect = \"a\"
Actual = \"b\"
",
        Differ.ToString({ xxx = "a" }, { xxx = "b" }, MyDiffModule.differ))
    assertStr("\
Expect = \"a\"
Actual = \"b\"
",
        Differ.ToString({ xxx = "a" }, { xxx = "b" }, MyDiffWithCombinators.differ))
    assertStr("\
Expect = \"a\"
Actual = \"b\"
",
        Differ.ToString({ xxx = "a" }, { xxx = "b" }, MyDiffType.Differ))
    assertStr("\
Expect = \"a\"
Actual = \"b\"
",
        Differ.ToString({ xxx = "a" }, { xxx = "b" }, MyDiffTypeWithCombinators.Differ))

module ``Custom differ with custom diff output`` =

    let myCustomDiffer = CustomDiffer<CustomDiffable>.Build(fun x1 x2 ->
        if x1.xxx = x2.xxx then
            None
        else
            Diff.MakeCustom(fun writer param indent path recur ->
                if param.ensureFirstLineIsAligned then writer.WriteLine()
                let indentLike str = String.replicate (String.length str) " "
                let dpath = if path = "" then "" else path + " "
                writer.WriteLine($"{indent}{dpath}{param.x1Name} __is__ {x1.xxx}")
                writer.WriteLine($"{indent}{indentLike dpath}{param.x2Name} __is__ {x2.xxx}"))
            |> Some)

    let differ<'T> = myCustomDiffer.GetDiffer<'T>()

    [<Fact>]
    let ``Assert with immediate value adds newline`` () =
        let ex = Assert.Throws<AssertionFailedException>(fun () ->
            Differ.Assert({ xxx = "a" }, { xxx = "b" }, differ))
        assertStr("
Expect __is__ a
Actual __is__ b
", ex.Message)

    [<Fact>]
    let ``Assert with nested value doesn't add newline`` () =
        let ex = Assert.Throws<AssertionFailedException>(fun () ->
            Differ.Assert({| iiiii = { xxx = "a" } |}, {| iiiii = { xxx = "b" } |}, differ))
        assertStr("\
iiiii Expect __is__ a
      Actual __is__ b
", ex.Message)

    [<Fact>]
    let ``ToString with immediate value doesn't add newline`` () =
        let diff = Differ.ToString({ xxx = "a" }, { xxx = "b" }, differ)
        assertStr("\
Expect __is__ a
Actual __is__ b
", diff)

    [<Fact>]
    let ``ToString with nested value doesn't add newline`` () =
        let diff = Differ.ToString({| iiiii = { xxx = "a" } |}, {| iiiii = { xxx = "b" } |}, differ)
        assertStr("\
iiiii Expect __is__ a
      Actual __is__ b
", diff)

type Rec = { xRec: Rec option }

[<Fact>]
let ``Recursive type`` () =
    let x1 = { xRec = Some { xRec = None } }
    let x2 = { xRec = Some { xRec = Some { xRec = None } } }
    assertStr("\
xRec.Value.xRec differs by union case:
  Expect is None
  Actual is Some
",
        Differ.ToString(x1, x2))

[<Fact>]
let ``Anonymous record`` () =
    Assert.Null(Differ.Diff({| xxx = 1; yyy = "2" |}, {| xxx = 1; yyy = "2" |}))
    assertStr("\
xxx Expect = 1
    Actual = 2
",
        Differ.ToString({| xxx = 1; yyy = "2" |}, {| xxx = 2; yyy = "2" |}))
