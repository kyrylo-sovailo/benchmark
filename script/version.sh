#!/bin/bash
VERSION=$(gcc --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
echo GCC "     ${VERSION}"
VERSION=$(clang --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
echo Clang "   ${VERSION}"
VERSION=$(gfortran --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
echo GFortran "${VERSION}"
VERSION=$(mcs --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
TARGET=$(mcs --help | grep VERSION | grep -o -E '[0-9.]+ \(default\)' | grep -o -E '[0-9.]*' | head -n 1)
echo MCS "     ${VERSION}, Target ${TARGET}"
VERSION=$(python --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
echo Python "  ${VERSION}"
VERSION=$(ghc --version | head -n 1 | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+' | head -n 1)
echo GHC "     ${VERSION}"
VERSION=$(node --version | grep -o -E '[0-9]+\.[0-9]+\.[0-9]+')
echo Node "    ${VERSION}"
VERSION=$(matlab -batch 'disp(version)')
echo Matlab "  ${VERSION}"
VERSION=$(uname -r)
echo Kernel "  ${VERSION}"
