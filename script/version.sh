#!/bin/bash
if [ $(type gcc 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(gcc --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "GCC                  ${VERSION}"
fi
if [ $(type clang 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(clang --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Clang                ${VERSION}"
fi
if [ $(type nasm 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(nasm --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "NASM                 ${VERSION}"
fi
if [ $(type gfortran 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(gfortran --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "GFortran             ${VERSION}"
fi
if [ $(type rustc 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(rustc --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Rust                 ${VERSION}"
fi
if [ $(type fpc 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(fpc -iV)
    echo "Free Pascal Compiler ${VERSION}"
fi
if [ $(type mcs 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(mcs --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    TARGET=$(mcs --help | grep VERSION | grep -o -E '[0-9.]+ \(default\)' | grep -o -E '[0-9.]*' | head -n 1)
    echo "MCS                  ${VERSION}, Target ${TARGET}"
fi
if [ $(type dotnet 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(dotnet --version | head -n 1)
    echo ".NET                 ${VERSION}"
fi
if [ $(type javac 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(javac --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Java                 ${VERSION}"
fi
if [ $(type python 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(python --version | grep -o -E 'Python [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
    echo "Python               ${VERSION}"
fi
if [ $(type pypy 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(pypy --version | grep -o -E 'PyPy [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
    TARGET=$(pypy --version | grep -o -E 'Python [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
    echo "PyPy                 ${VERSION}, Target ${TARGET}"
fi
if [ $(type ghc 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(ghc --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "GHC                  ${VERSION}"
fi
if [ $(type node 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(node --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Node                 ${VERSION}"
fi
if [ $(type lua 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(lua -v | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Lua                  ${VERSION}"
fi
if [ $(type luajit 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(luajit -v | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "LuaJIT               ${VERSION}"
fi
if [ $(type matlab 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(matlab -batch 'disp(version)' | head -n 1)
    echo "Matlab               ${VERSION}"
fi
VERSION=$(uname -r)
echo Kernel "              ${VERSION}"
