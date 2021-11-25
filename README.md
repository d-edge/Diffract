<br />

<p align="center">
    <img src="https://raw.githubusercontent.com/d-edge/diffract/main/diffract.png" alt="diffract logo" height="140">
</p>

<p align="center">
        <a href="https://github.com/d-edge/diffract/actions" title="actions"><img src="https://github.com/d-edge/diffract/actions/workflows/build.yml/badge.svg?branch=main" alt="actions build" /></a>
    <a href="https://www.nuget.org/packages/diffract/" title="nuget"><img src="https://img.shields.io/nuget/vpre/diffract" alt="version" /></a>
    <a href="https://www.nuget.org/stats/packages/diffract?groupby=Version" title="stats"><img src="https://img.shields.io/nuget/dt/diffract" alt="download" /></a> 
    <a href="https://raw.githubusercontent.com/d-edge/diffract/main/LICENSE" title="license"><img src="https://img.shields.io/github/license/d-edge/diffract" alt="license" /></a>
</p>

<br />

Diffract is a .NET library that displays a readable diff between two objects.
It is particularly useful for unit testing complex objects.
Here is an example:

```csharp
record User(string Name, int Age, string[] Pets);

var expected = new User("Emma", 42, new[] { "Oscar", "Fluffy", "Tibbles" });
var actual = new User("Andy", 42, new[] { "Oscar", "Sparky" });

Diffract.Assert(expected, actual);
```

The above throws an `AssertionFailedException` with the following message:

```
Value differs by 2 fields:
  Name Expect = "Emma"
       Actual = "Andy"
  Pets collection differs:
    Pets.Count Expect = 3
               Actual = 2
    Pets[1] Expect = "Fluffy"
            Actual = "Sparky"
```

Diffract can drill down many composite types:
* POCOs;
* C# records;
* F# records and anonymous records;
* F# unions;
* enumerables (`IEnumerable<T>`);
* dictionaries (`IDictionary<K, V>`, `IReadOnlyDictionary<K, V>`);
* value and reference tuples.

Values of any other equatable type (like `string` and `int` in the above example) are treated as leaves that can be tested for equality.

## Example outputs

POCO or record with multiple field differences:

```csharp
record User(string Name, int Age, bool IsActive);

var expected = new User("Emma", 42, true);
var actual = new User("Andy", 35, true);
```

```
Value differs by 2 fields:
  Name Expect = "Emma"
       Actual = "Andy"
  Age Expect = 42
      Actual = 35
```

F# union where the case is different:

```fsharp
type Contact =
  | Email of address: string
  | Phone of number: string
  
let expected = Email "user@example.com"
let actual = Phone "555-123-456"
```

```
Value differs by union case:
  Expect is Email
  Actual is Phone
```

F# union where the case is the same and the value is different:

```fsharp
type Contact =
  | Email of address: string
  | Phone of number: string
  
let expected = Email "user@example.com"
let actual = Email "someone@example.com"
```

```
Value differs by union case Email fields:
  address Expect = "user@example.com"
          Actual = "someone@example.com"
```

Enumerables show the counts if they differ, followed by the diffs per item:

```csharp
var expected = new string[] { "first", "second" };
var actual = new string[] { "first", "2nd", "third" };
```

```
Value collection differs:
  Count Expect = 2
        Actual = 3
  [1] Expect = "second"
      Actual = "2nd"
```

Dictionaries show the keys missing on either side, followed by the diffs per item that exists in both:

```csharp
var expected = new Dictionary<string, int>
{
    { "first", 1 },
    { "second", 2 },
    { "third", 3 },
};
var actual = new Dictionary<string, int>
{
    { "first", 1 },
    { "third", 2 },
    { "fourth", 4 },
};
```

```
Value dictionary differs:
  ["second"] Actual is missing
  ["fourth"] Expect is missing
  ["third"] Expect = 3
            Actual = 2
```

## API

Diffract provides the following methods:

```csharp
void Diffract.Assert<T>(T expected, T actual, IDiffer<T> differ = null, PrintParams param = null)
```

Computes the diff between two objects and, if it is not empty, throws an `AssertionFailedException` with the diff as message.

```csharp
string Diffract.ToString<T>(T expected, T actual, IDiffer<T> differ = null, PrintParams param = null)
```

Prints the diff between two objects to a string.

```csharp
void Diffract.Write<T>(T expected, T actual, TextWriter writer = null, IDiffer<T> differ = null, PrintParams param = null)
```

Prints the diff between two objects to the given TextWriter (or to standard output if not provided).

```csharp
FSharpOption<Diff> Diffract.Diff<T>(T expected, T actual, IDiffer<T> differ = null)
```

Computes the diff between two objects. Returns `None` if the objects are found to be equal.
