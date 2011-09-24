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


Context(bad_context)
{
    // Example: a class which is copyable
    //
    
    struct NonCopyable
    {
        NonCopyable() {};
    };
                   
    // StaticErrors that fail:
    //
    
    StaticError(NonCopyable x = NonCopyable(),       "Non copyable class!");
    StaticError(NonCopyable x; NonCopyable y; x = y, "Non copyable class!");


    Setup(init)
    {
        std::cout << "[*] Starting bad_context tests.. __________________________" << std::endl;
    }

    Teardown(fini)
    {
        std::cout << "[*] bad_context tests finished. ___________________________" << std::endl;
    }
    
    ////////////////// tests that fail
    //  
    
    Test(test_0)
    {
        Assert(std::vector<int>().empty(), is_false());
    }

    Test(test_1)
    {
        Assert(!std::vector<int>().empty(), is_true());
    }

    Test(test_2)
    {
        Assert(1, is_greater(2));
    }

    Test(test_3)
    {
        Assert(1, is_greater_equal(2));
    }

    Test(test_4)
    {
        Assert(2, is_less(2));
    }

    Test(test_5)
    {
        Assert(2, is_less_equal(1));
    }

    Test(test_6)
    {
        Assert(42, is_equal_to(39));
    }

    Test(test_7)
    {
        Assert(42, is_not_equal_to(42));
    }
    
    ////////////////// exceptions

    Test(test_8)
    {
        AssertNothrow(throw 0);
    }
    Test(test_9)
    {
        AssertThrow(0);
    }
 
    Test(test_10)
    {
        AssertThrow(throw std::runtime_error("error"), std::logic_error("bad"));
    }

    Test(test_11)
    {
        AssertThrow(throw std::runtime_error("error"), std::runtime_error("not ok"));
    }

    ////////////////// generic predicate

    bool is_even(int n)
    {
        return !(n&1);
    }

    Test(test_12)
    {
        Assert(11, generic_predicate<int>("is_even", is_even));
    }
    
    ////////////////// unexpected exception
    
    Test(test_13)
    {
        throw std::runtime_error("unexpected exception");
    }   

}

int
main(int argc, char *argv[])
{
    return yats::run();
}
 
