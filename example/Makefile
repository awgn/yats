# (C) 2011-13 Nicola Bonelli <nicola.bonelli@antifork.org>

CXX      = g++
CXXFLAGS = -I ../ -std=c++11 -g -Wall -Wextra -pedantic 


all: test-good test-bad

test-good: test-good.o
		$(CXX) $(CXXFLAGS) -o $@ $^

test-bad: test-bad.o
		$(CXX) $(CXXFLAGS) -o $@ $^

%.o: %.c 
		$(CXX) $(CXXFLAGS) -c $<

.PHONY: clean 

clean:
		@rm -rf *.o test-good test-bad


