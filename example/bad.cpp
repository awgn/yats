/******************************************************************************
 *
 * The MIT License (MIT)
 *
 * Copyright (c) 2011-15 Nicola Bonelli <nicola@pfq.io>
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 ******************************************************************************/


#include "yats.hpp"

using namespace yats;

namespace group_context
{
    // Example: a class which is copyable
    //

    struct NonCopyable
    {
        NonCopyable() {}
    };

    // StaticErrors that fail:
    //

    StaticError(NonCopyable x = NonCopyable(),       "Non copy-constructible class")
    StaticError(NonCopyable x; NonCopyable y; x = y, "Non assignable class")

    auto bad = Group("bad")

        ////////////////// tests that fail

        .Single("test_0", []
        {

            Assert(std::vector<int>().empty(), is_false());
        })

        .Single("test_1", []
        {
            Assert(!std::vector<int>().empty(), is_true());
        })

        .Single("test_2", []
        {
            Assert(1, is_greater(2));
        })

        .Single("test_3", []
        {
            Assert(1, is_greater_equal(2));
        })

        .Single("test_4", []
        {
            Assert(2, is_less(2));
        })

        .Single("test_5", []
        {
            Assert(2, is_less_equal(1));
        })

        .Single("test_6", []
        {
            Assert(42, is_equal_to(39));

        })

        .Single("test_7", []
        {
            Assert(42, is_not_equal_to(42));
        })

        ////////////////// exceptions

        .Single("test_8", []
        {
            AssertNoThrow(throw 0);
        })

        .Single("test_9", []
        {
            int n = 0;
            AssertThrow(n++);
        })

        .Single("test_10", []
        {
            AssertThrowAs(std::logic_error("bad"), throw std::runtime_error("error"));
        })

        .Single("test_11", []
        {
            AssertThrowAs(std::runtime_error("not ok"), throw std::runtime_error("error"));
        })

        ////////////////// generic predicate

        .Single("test_12", []
        {
            Assert(11, make_predicate<int>("is_even", [](int n) -> bool { return !(n&1); }));
        })

        ////////////////// unexpected exception

        .Single("test_13", []
        {
            throw std::runtime_error("unexpected exception");
        })

        /////////////////  boolean combinator:

        .Single("test_14", []
        {
            Assert(42, is_less(0) and is_greater(0));
        })

        .Single("test_15", []
        {
            Assert(1, !is_greater(0));
        });

}


int
main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}

