/* $Id$ */
/*
 * ----------------------------------------------------------------------------
 * "THE BEER-WARE LICENSE" (Revision 42):
 * <bonelli@antifork.org> wrote this file. As long as you retain this notice you
 * can do whatever you want with this stuff. If we meet some day, and you think
 * this stuff is worth it, you can buy me a beer in return. Nicola Bonelli
 * ----------------------------------------------------------------------------
 */

#include <yats.hpp>

using namespace yats;

Context(good_context)
{
    // Example: noncopyable class...
    //
    
    struct NonCopyable
    {
        NonCopyable() {};
        NonCopyable(const NonCopyable&) = delete;
        NonCopyable& operator=(const NonCopyable&) = delete;
    };

    // StaticError goes here (needs runtime YATS)
    //
    
    StaticError( NonCopyable x = NonCopyable(),       "non copyable class!");
    StaticError( NonCopyable x; Noncopyable y; x = y, "non assignable class!");


    Test(test_0)
    {
        Assert(std::vector<int>().empty());
    }

    Test(test_1)
    {
        Assert(!std::vector<int>().empty(), is_false());
    }

    Test(test_2)
    {
        Assert(2, is_greater(1));
    }
    
    Test(test_3)
    {
        Assert(2, is_greater_equal(2));
    }

    Test(test_4)
    {
        Assert(1, is_less(2));
    }           

    Test(test_5)
    {
        Assert(0, is_less_equal(1));
    }
    
    Test(test_6)
    {
        Assert(42, is_equal_to(42));
    }

    Test(test_7)
    {
        Assert(42, is_not_equal_to(39));
    }
    
    ////////////////// exceptions

    Test(test_8)
    {
        int n = 0;
        AssertNothrow(n++);
    }

    Test(test_9)
    {
        AssertThrow( throw std::runtime_error("ok") );
    }    

    Test(test_10)
    {
        AssertThrow( throw std::logic_error("ok"), std::logic_error("ok"));
    }
}


int
main(int argc, char *argv[])
{
    return yats::run(argc, argv);
}
 
