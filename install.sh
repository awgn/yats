#!/bin/bash

# Copyright (c) 2011 Bonelli Nicola <bonelli@antifork.org>
# 
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.

set -e

BOLD='\e[1m'
NC='\e[0m'

ASS="${BOLD}[YATS]${NC}"

echo -e "${ASS} Installing YATS suite:"

echo -e "${ASS} Compiling Haskell binaries..."

/usr/bin/ghc -O -Wall runtime/yats.hs -o /usr/local/bin/yats

echo -e "${ASS} Copying YATS header..."

/bin/cp yats.hpp /usr/local/include/

if [ -x /usr/bin/g++ ]; then
    echo -e "${ASS} Precompiling YATS header for g++..."
    /usr/bin/g++ yats.hpp -std=c++0x -O0 -o /usr/local/include/yats.hpp.gch
fi


if [ -x /usr/bin/clang++ ]; then
    echo -e "${ASS} Precompiling headers for clang++..."
    /usr/bin/clang++ yats.hpp -std=c++0x -O0 -D_GLIBCXX_DEBUG -Wall -Wextra -x c++-header -o /usr/local/include/yats.hpp.pch
fi

echo -e "${ASS} done."
