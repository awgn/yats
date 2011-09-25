#!/bin/sh
set -x

/usr/bin/ghc -Wall runtime/yats.hs -o /usr/local/bin/yats
/usr/bin/g++ yats.hpp -std=c++0x -O0 -o /usr/local/include/yats.hpp.gch

/bin/cp yats.hpp /usr/local/include/

