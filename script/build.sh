#!/bin/bash
SOURCE="$(dirname $(dirname $(readlink -f $0)))/source"
BUILD="$(dirname $(dirname $(readlink -f $0)))/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

#compile_debug(compiler, source, destination)
compile_debug()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            FLAGS="-std=c++11 -g -Wall -Wextra -pedantic"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            FLAGS="-std=c11 -g -Wall -Wextra -pedantic"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 ]; then #C#
            FLAGS="-debug -platform:x64"
            echo $1 $FLAGS "$2" -out:"$3"
            $1 $FLAGS "$2" -out:"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            FLAGS="-std=f95 -g -fcheck=all"
            echo $1  "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.hs$' | wc -l) -gt 0 ]; then #Haskell
            FLAGS="-rtsopts"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        else
            exit 1
        fi
    fi
}

#compile_release(compiler, source, destination, optimizations)
compile_release()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            FLAGS="-std=c++11 -O3 -DNDEBUG -fno-rtti -flto -march=native -Drestrict="
            if [ ! -z "$4" ]; then
                FLAGS="${FLAGS} -DENABLE_MAPPING -DENABLE_MULTITHREADING -lstdc++"
            fi
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            FLAGS="-std=c11 -O3 -DNDEBUG -flto -march=native"
            if [ ! -z "$4" ]; then
                FLAGS="${FLAGS} -DENABLE_MAPPING -DENABLE_MULTITHREADING"
            fi
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 ]; then #C#
            FLAGS="-optimize+ -platform:x64"
            echo $1 $FLAGS "$2" -out:"$3"
            $1 $FLAGS "$2" -out:"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            FLAGS="-std=f95 -O3 -flto -march=native"
            echo $1  "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.hs$' | wc -l) -gt 0 ]; then #Haskell
            FLAGS="-O2 -optc-O3"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        else
            exit 1
        fi
    fi
}

#create_benchmark(executable, benchmark)
create_benchmark()
{
    if [ "$1" -nt "$2" ]; then
        echo "$1"
        "$1" || exit 1
    fi    
}

#copy(source, destination)
copy()
{
    if [ "$1" -nt "$2" ]; then
        echo cp "$1" "$2"
        cp "$1" "$2" || exit 1
    fi
}

# C++
compile_debug g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_debug" || exit 1
compile_release g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_release" || exit 1
compile_debug clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_debug" || exit 1
compile_release clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_release" || exit 1

# C
compile_debug gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_debug" || exit 1
compile_release gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_release" || exit 1
compile_debug clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_debug" || exit 1
compile_release clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_release" || exit 1

# Extras
compile_release clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_release_opt" yes || exit 1 #Cheating C
compile_release clang "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_release_opt" yes || exit 1 #Cheating C++
copy "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c.cpp" || exit 1
compile_release clang++ "$BUILD/dijkstra_c.cpp" "$BUILD/dijkstra_c_clang_release_cpp" || exit 1 #As C++

# C#
copy "$SOURCE/dijkstra_csharp.cs" "$BUILD/dijkstra_csharp_debug.cs" || exit 1
compile_debug mcs "$BUILD/dijkstra_csharp_debug.cs" "$BUILD/dijkstra_csharp_mcs_debug.exe" || exit 1
copy "$SOURCE/dijkstra_csharp.cs" "$BUILD/dijkstra_csharp_release.cs" || exit 1
compile_release mcs "$BUILD/dijkstra_csharp_release.cs" "$BUILD/dijkstra_csharp_mcs_release.exe" || exit 1

# Fortran
compile_debug gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gfortran_debug" || exit 1
compile_release gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gfortran_release" || exit 1

# Haskell
copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_debug.hs" || exit 1
compile_debug ghc "$BUILD/dijkstra_haskell_debug.hs" "$BUILD/dijkstra_haskell_ghc_debug" || exit 1
copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_release.hs" || exit 1
compile_release ghc "$BUILD/dijkstra_haskell_release.hs" "$BUILD/dijkstra_haskell_ghc_release" || exit 1

# Matlab
copy "$SOURCE/dijkstra_matlab.m" "$BUILD/dijkstra_matlab.m" || exit 1

# Testing
compile_release g++ "$SOURCE/create_dijkstra.cpp" "$BUILD/create_dijkstra" || exit 1
create_benchmark "$BUILD/create_dijkstra" "$BUILD/dijkstra.txt" || exit 1
