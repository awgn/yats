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

        .Setup([] (YATS_TASK) {

               std::cout << "[*] Setup!" << std::endl;
        })

        .Teardown([] (YATS_TASK) {

               std::cout << "[*] Teardown!" << std::endl;
        })

        .Prolog([](YATS_TASK) {

            std::cout << "+ Prolog!" << std::endl;

        })

        .Epilog([](YATS_TASK) {

            std::cout << "- Epilog!" << std::endl;

        })

        .Single("test_0", [](YATS_DEFAULT_TASK) {

                Assert(std::vector<int>().empty());
        })

        .Single("test_1", [](YATS_DEFAULT_TASK) {

            Assert(!std::vector<int>().empty(), is_false());
        })

        .Single("test_2", [](YATS_DEFAULT_TASK) {

            Assert(2, not is_greater(2) and not is_less(2));
        })

        .Single("test_3", [](YATS_DEFAULT_TASK) {

            Assert(2, is_greater_equal(2));
        })

        .Single("test_4", [](YATS_DEFAULT_TASK) {

            Assert(1, is_less(2));
        })

        .Single("test_5", [](YATS_DEFAULT_TASK) {

            Assert(0, is_less_equal(1));
        })

        .Single("test_6", [](YATS_DEFAULT_TASK) {

            Assert(42, is_equal_to(42));
        })

        .Single("test_7", [](YATS_DEFAULT_TASK) {

            Assert(42, is_not_equal_to(39) and is_not_equal_to(11));
        })

        ////////////////// exceptions

        .Single("test_8", [](YATS_DEFAULT_TASK) {

            int n = 0;
            AssertNoThrow(n++);
        })

        .Single("test_9", [](YATS_DEFAULT_TASK) {

            AssertThrow( throw std::runtime_error("ok") );
        })

        .Single("test_10", [](YATS_DEFAULT_TASK) {

            AssertThrow( throw std::logic_error("ok"), std::logic_error("ok"));
        })

        .Single("test_11", [](YATS_DEFAULT_TASK) {

            Assert(std::vector<int>{1,2,3}.size() == 3);
            Assert(std::vector<int>{1,2,3}.size(), is_equal_to(3) and is_greater(1));
        })

        .Repeat("test_12", [] (YATS_REPEAT_TASK, int) {

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

