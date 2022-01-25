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
            var expectedDiff = "D.X Expect = \"a\"\n    Actual = \"b\"\n";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = Differ.ToString(expected, actual);
            Assert.Equal(expectedDiff, actualDiff);
        }

        [Fact]
        public void CustomDiffer()
        {
            var expectedDiff = "D Expect = \"a\"\n  Actual = \"b\"\n";
            var expected = new Container(new CustomDiffable("a"));
            var actual = new Container(new CustomDiffable("b"));
            var actualDiff = MyDiffer.Get<Container>().ToString(expected, actual);
            Assert.Equal(expectedDiff, actualDiff);
        }

        public record CustomDiffable(string X);

        public record Container(CustomDiffable D);

        public class MyDiffer : IDiffer<CustomDiffable>
        {
            private readonly IDiffer<string> _stringDiffer;

            public MyDiffer(IDifferFactory differFactory)
            {
                _stringDiffer = differFactory.GetDiffer<string>();
            }

            public FSharpOption<Diff> Diff(CustomDiffable x1, CustomDiffable x2) =>
                _stringDiffer.Diff(x1.X, x2.X);

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
    }
}