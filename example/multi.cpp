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


auto g = Group("Multiple Throw")

        .Single( "test_0", []
        {
            Assert(false);
            Assert(true);
        })

        .Single( "test_1", []
        {
            Assert(false);
            Assert(false);
        })
        .Single( "test_2", []
        {
            Assert(false);
            throw 0;
            Assert(false); // this assert is never checked!
        })
        .Single( "test_3", []
        {
            for(int i = 0; i < 10; i++)
                AssertId(i, i == 0);
        })
        ;

int
main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}

