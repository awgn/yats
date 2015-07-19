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

#include <random>
#include "yats.hpp"

using namespace yats;

namespace group_context
{
    std::mt19937 rand_engine;

    // Example: noncopyable class...
    //

    struct NonCopyable
    {
        NonCopyable() {}
        NonCopyable(const NonCopyable&) = delete;
        NonCopyable& operator=(const NonCopyable&) = delete;
    };

    // StaticError goes here (needs runtime YATS)
    //

    StaticError( NonCopyable x = NonCopyable(),       "non copyable class!")
    StaticError( NonCopyable x; Noncopyable y; x = y, "non assignable class!")

    auto good = Group("good")

        .Setup([]
        {
            std::cout << "[*] Setup!" << std::endl;
        })

        .Teardown([]
        {
            std::cout << "[*] Teardown!" << std::endl;
        })

        .Prolog([]
        {
            std::cout << "+ Prolog!" << std::endl;
        })

        .Epilog([]
        {
            std::cout << "- Epilog!" << std::endl;
        })

        .Single( "test_0", []
        {
            Assert(std::vector<int>().empty());
        })

        .Single( "test_1", []
        {
            Assert(!std::vector<int>().empty(), is_false());
        })

        .Single( "test_2", []
        {
            Assert(2, not is_greater(2) and not is_less(2));
        })

        .Single("test_3", []
        {
            Assert(2, is_greater_equal(2));
        })

        .Single("test_4", []
        {
            Assert(1, is_less(2));
        })

        .Single("test_5", []
        {
            Assert(0, is_less_equal(1));
        })

        .Single("test_6", []
        {
            Assert(42, is_equal_to(42));
        })

        .Single("test_7", []
        {
            Assert(42, is_not_equal_to(39) and is_not_equal_to(11));
        })

        ////////////////// exceptions

        .Single("test_8", []
        {
            int n = 0;
            AssertNoThrow(n++);
        })

        .Single("test_9", []
        {
            AssertThrow( throw std::runtime_error("ok") );
        })

        .Single("test_10", []
        {
            AssertThrow( throw std::logic_error("ok"), std::logic_error("ok"));
        })

        .Single("test_11", []
        {
            Assert(std::vector<int>{1,2,3}.size() == 3);
            Assert(std::vector<int>{1,2,3}.size(), is_equal_to(3) and is_greater(1));
        })

        .Repeat("test_12", []
        {
            std::uniform_int_distribution<int> u(1, 6);
            std::lognormal_distribution<double> l(0, 0.5);

            auto x = u(rand_engine);
            auto y = l(rand_engine);

            Assert( x , is_greater_equal(1));
            Assert( x , is_less_equal(6));
            Assert( y,  is_greater_equal(0.0));
        })

        .Repeat( "test_13", []
        {
            std::uniform_int_distribution<int> u(1, 6);
            std::lognormal_distribution<double> l(0, 0.5);

            auto x = u(rand_engine);
            auto y = l(rand_engine);

            Assert( x , is_greater_equal(1));
            Assert( x , is_less_equal(6));
            Assert( y,  is_greater_equal(0.0));
        });
}


int
main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}

