#!/bin/sh
AFFINITY=1
SOURCE="$(dirname $(readlink -f $0))"
BUILD="$SOURCE/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

#compile_debug(compiler, source, destination)
compile_debug()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            echo $1 -std=c++11 -g "$2" -o "$3"
            $1 -std=c++11 -g "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            echo $1 -std=c11 -g "$2" -o "$3"
            $1 -std=c11 -g "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            echo $1 -std=f95 -g -fcheck=all "$2" -o "$3"
            $1 -std=f95 -g -fcheck=all "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.hs$' | wc -l) -gt 0 ]; then #Haskell
            echo $1 -rtsopts "$2" -o "$3"
            $1 -rtsopts "$2" -o "$3" || exit 1
        else
            exit 1
        fi
    fi
}

#compile_release(compiler, source, destination)
compile_release()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            echo $1 -std=c++11 -O3 -DNDEBUG -fno-rtti -flto -march=native "$2" -o "$3"
            $1 -std=c++11 -O3 -DNDEBUG -fno-rtti -flto -march=native "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            echo $1 -std=c11 -O3 -DNDEBUG -flto -march=native "$2" -o "$3"
            $1 -std=c11 -O3 -DNDEBUG -flto -march=native "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            echo $1 -std=f95 -O3 -flto -march=native "$2" -o "$3"
            $1 -std=f95 -O3 -flto -march=native "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.hs$' | wc -l) -gt 0 ]; then #Haskell
            echo $1 -O2 -optc-O3 "$2" -o "$3"
            $1 -O2 -optc-O3 "$2" -o "$3" || exit 1
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

#run_benchmark(executable, benchmark, log)
run_benchmark()
{
    if [ "$1" -nt "$3" -o "$2" -nt "$3" ]; then
        echo "{ { taskset -c 1 sh -c \"time \\\"$1\\\"\"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee \"$3\"; } 2>&1"
        { { taskset -c 1 sh -c "time \"$1\""; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
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

compile_release g++ "$SOURCE/create_dijkstra.cpp" "$BUILD/create_dijkstra" || exit 1
create_benchmark "$BUILD/create_dijkstra" "$BUILD/dijkstra.txt" || exit 1

compile_debug g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_debug" || exit 1
compile_release g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_release" || exit 1
compile_debug clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_debug" || exit 1
compile_release clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_release" || exit 1

compile_debug gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_debug" || exit 1
compile_release gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_release" || exit 1
compile_debug clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_debug" || exit 1
compile_release clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_release" || exit 1

compile_debug gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gcc_debug" || exit 1
compile_release gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gcc_release" || exit 1

copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_debug.hs" || exit 1
compile_debug ghc "$BUILD/dijkstra_haskell_debug.hs" "$BUILD/dijkstra_haskell_ghc_debug" || exit 1
copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_release.hs" || exit 1
compile_release ghc "$BUILD/dijkstra_haskell_release.hs" "$BUILD/dijkstra_haskell_ghc_release" || exit 1

run_benchmark "$BUILD/dijkstra_cpp_gcc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_gcc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_gcc_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_clang_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_clang_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_clang_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_clang_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_gcc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_gcc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_gcc_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_clang_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_clang_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_clang_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_clang_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_fortran_gcc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_fortran_gcc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gcc_release.txt" || exit 1
#run_benchmark "$BUILD/dijkstra_haskell_ghc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_debug.txt" || exit 1
#run_benchmark "$BUILD/dijkstra_haskell_ghc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_release.txt" || exit 1
run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_python.txt" || exit 1