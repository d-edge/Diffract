using Microsoft.FSharp.Core;
using TypeShape.Core;
using Xunit;

namespace DEdge.Diffract.CSharp.Tests
{
    public class CustomDiffers
    {
        [Fact]
        public void NoCustomDiffer()
        {
            var expectedDiff = @"DDDDD.XXX Expect = ""a""
          Actual = ""b""
";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = Differ.ToString(expected, actual);
            AssertStr(expectedDiff, actualDiff);
        }

        [Fact]
        public void CustomDiffer()
        {
            var expectedDiff = @"DDDDD Expect = ""a""
      Actual = ""b""
";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = MyDiffer.Get<Container>().ToString(expected, actual);
            AssertStr(expectedDiff, actualDiff);
        }

        [Fact]
        public void CustomDifferWithCombinators()
        {
            var expectedDiff = @"DDDDD Expect = ""a""
      Actual = ""b""
";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = MyDifferWithCombinators.Get<Container>().ToString(expected, actual);
            AssertStr(expectedDiff, actualDiff);
        }

        [Fact]
        public void CustomDifferWithMap()
        {
            var expectedDiff = @"DDDDD Expect = ""a""
      Actual = ""b""
";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = MyDifferWithMap.Get<Container>().ToString(expected, actual);
            AssertStr(expectedDiff, actualDiff);
        }

        [Fact]
        public void CustomDifferWithMapToAnonymousObject()
        {
            var expectedDiff = @"DDDDD.v Expect = ""a""
        Actual = ""b""
";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = MyDifferWithMapToAnonymousObject.Get<Container>().ToString(expected, actual);
            AssertStr(expectedDiff, actualDiff);
        }

        public record CustomDiffable(string XXX);

        public record Container(CustomDiffable DDDDD);

        public class MyDiffer : IDiffer<CustomDiffable>
        {
            private readonly IDiffer<string> _stringDiffer;

            public MyDiffer(IDifferFactory differFactory)
            {
                _stringDiffer = differFactory.GetDiffer<string>();
            }

            public FSharpOption<Diff> Diff(CustomDiffable x1, CustomDiffable x2) =>
                _stringDiffer.Diff(x1.XXX, x2.XXX);

            public static IDiffer<T> Get<T>() => Singleton<T>.Instance;

            private static class Singleton<T>
            {
                public static readonly IDiffer<T> Instance = new CustomDiffer().GetDiffer<T>();
            }

            private class CustomDiffer : ICustomDiffer
            {
                public FSharpOption<IDiffer<T>> GetCustomDiffer<T>(IDifferFactory differFactory, Core.TypeShape<T> shape) =>
                    shape.Type == typeof(CustomDiffable)
                        ? new MyDiffer(differFactory).Unwrap<CustomDiffable, T>()
                        : null;
            }
        }

        public static class MyDifferWithCombinators
        {
            public static IDiffer<T> Get<T>() => Singleton<T>.Instance;

            private static class Singleton<T>
            {
                public static readonly IDiffer<T> Instance = CustomDiffer.GetDiffer<T>();
            }

            private static readonly ICustomDiffer CustomDiffer =
                CustomDiffer<CustomDiffable>.Build(factory =>
                {
                    var stringDiffer = factory.GetDiffer<string>();
                    return (x1, x2) => stringDiffer.Diff(x1.XXX, x2.XXX);
                });
        }

        public static class MyDifferWithMap
        {
            public static IDiffer<T> Get<T>() => Singleton<T>.Instance;

            private static class Singleton<T>
            {
                public static readonly IDiffer<T> Instance = CustomDiffer.GetDiffer<T>();
            }

            private static readonly ICustomDiffer CustomDiffer =
                CustomDiffer<CustomDiffable>.Map(x => x.XXX);
        }

        public static class MyDifferWithMapToAnonymousObject
        {
            public static IDiffer<T> Get<T>() => Singleton<T>.Instance;

            private static class Singleton<T>
            {
                public static readonly IDiffer<T> Instance = CustomDiffer.GetDiffer<T>();
            }

            private static readonly ICustomDiffer CustomDiffer =
                CustomDiffer<CustomDiffable>.Map(x => new { v = x.XXX });
        }

        private void AssertStr(string expected, string actual)
        {
            Assert.Equal(expected.Replace("\r\n", "\n"), actual);
        }
    }
}