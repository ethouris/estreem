#CXX=g++ -std=c++11
CXX=clang++ -std=c++11 -stdlib=libstdc++

#GCCFLAGS=-fno-default-inline -fno-implicit-inline-templates -fno-eliminate-unused-debug-symbols


CXXFLAGS=-Wall -O0 -ggdb -g3 ${GCCFLAGS}
main: main.cc
