CXXFLAGS=-std=c++20 -Wall -Wextra -pedantic
CXX=g++

all: a.out

a.out: cstring_parse.cpp constexpr_vector.hpp type_set.hpp static_string.hpp
	$(CXX) $(CXXFLAGS) $<

.PHONY: clean
clean:
	rm a.out
