/* $Id$ */
/*
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <bonelli@antifork.org> wrote this file. As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return. Nicola Bonelli
 * ----------------------------------------------------------------------------
 */

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

        .Single("test_0", [](YATS_DEFAULT_TASK) {

            Assert(std::vector<int>().empty(), is_false());
        })

        .Single("test_1", [](YATS_DEFAULT_TASK) {

            Assert(!std::vector<int>().empty(), is_true());

        })

        .Single("test_2", [](YATS_DEFAULT_TASK) {

            Assert(1, is_greater(2));
        })

        .Single("test_3", [](YATS_DEFAULT_TASK) {

            Assert(1, is_greater_equal(2));
        })

        .Single("test_4", [](YATS_DEFAULT_TASK) {

            Assert(2, is_less(2));
        })

        .Single("test_5", [](YATS_DEFAULT_TASK) {

            Assert(2, is_less_equal(1));
        })

        .Single("test_6", [](YATS_DEFAULT_TASK) {

            Assert(42, is_equal_to(39));

        })

        .Single("test_7", [](YATS_DEFAULT_TASK) {

            Assert(42, is_not_equal_to(42));
        })

        ////////////////// exceptions

        .Single("test_8", [](YATS_DEFAULT_TASK) {

            AssertNoThrow(throw 0);
        })

        .Single("test_9", [](YATS_DEFAULT_TASK) {

            int n = 0;
            AssertThrow(n++);
        })

        .Single("test_10", [](YATS_DEFAULT_TASK) {

            AssertThrowAs(std::logic_error("bad"), throw std::runtime_error("error"));
        })

        .Single("test_11", [](YATS_DEFAULT_TASK) {

            AssertThrowAs(std::runtime_error("not ok"), throw std::runtime_error("error"));
        })

        ////////////////// generic predicate

        .Single("test_12", [](YATS_DEFAULT_TASK) {

            Assert(11, make_predicate<int>("is_even", [](int n) -> bool { return !(n&1); }));
        })

        ////////////////// unexpected exception

        .Single("test_13", [](YATS_DEFAULT_TASK) {

            throw std::runtime_error("unexpected exception");
        })

        /////////////////  boolean combinator:

        .Single("test_14", [](YATS_DEFAULT_TASK) {

            Assert(42, is_less(0) and is_greater(0));
        })

        .Single("test_15", [](YATS_DEFAULT_TASK) {

            Assert(1, !is_greater(0));
        });

}


int
main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}

