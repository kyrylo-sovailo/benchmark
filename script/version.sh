#!/bin/bash
if [ $(type gcc 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(gcc --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "GCC                  ${VERSION}"
fi
if [ $(type clang 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(clang --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "Clang                ${VERSION}"
fi
if [ $(type gfortran 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(gfortran --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
    echo "GFortran             ${VERSION}"
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
if [ $(type python 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(python --version | grep -o -E 'Python [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
    echo "Python               ${VERSION}"
fi
if [ $(type pypy3 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(pypy3 --version | grep -o -E 'PyPy [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
    TARGET=$(pypy3 --version | grep -o -E 'Python [0-9]+\.[0-9]+\.[0-9]+' | head -n 1 | cut -d ' ' -f 2)
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
if [ $(type matlab 2>/dev/null | wc -l) -gt 0 ]; then
    VERSION=$(matlab -batch 'disp(version)' | head -n 1)
    echo "Matlab               ${VERSION}"
fi
VERSION=$(uname -r)
echo Kernel "              ${VERSION}"
