using System;
using System.Collections.Generic;
using Xunit;

namespace Diffract.CSharp.Tests
{
    public class Tests
    {
        [Fact]
        public void Poco()
        {
            var expected = new MyPoco { Item = new MyInnerPoco(1, "a", 1) };
            var actual = new MyPoco { Item = new MyInnerPoco(2, "a", 2) };
            Assert.Equal("Item.X Expect = 1\n       Actual = 2\n",
                Diffract.ToString(expected, actual));
        }

        [Fact]
        public void Record()
        {
            var expected = new MyRecord(1, "a");
            var actual = new MyRecord(2, "a");
            Assert.Equal("X Expect = 1\n  Actual = 2\n",
                Diffract.ToString(expected, actual));
        }

        public class MyInnerPoco
        {
            public int X { get; }
            public string Y { get; }
            private int Z { get; }

            public MyInnerPoco(int x, string y, int z)
            {
                X = x;
                Y = y;
                Z = z;
            }
        }

        public class MyPoco
        {
            public MyInnerPoco Item { get; init; }
        }

        public record MyRecord(int X, string Y);
    }
}
