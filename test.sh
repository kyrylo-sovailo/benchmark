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
        if [ $( echo $1 | grep -e '++' | wc -l) -gt 0 ]; then
            echo $1 -std=c++11 -g "$2" -o "$3"
            $1 -std=c++11 -g "$2" -o "$3" || exit 1
        elif [ $( echo $1 | grep -e 'fortran' | wc -l) -eq 0 ]; then
            echo $1 -std=c11 -g "$2" -o "$3"
            $1 -std=c11 -g "$2" -o "$3" || exit 1
        else
            echo $1 -std=f2003 -g -fcheck=all "$2" -o "$3"
            $1 -std=f2003 -g -fcheck=all "$2" -o "$3" || exit 1
        fi
    fi
}

#compile_release(compiler, source, destination)
compile_release()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo $1 | grep -e '++' | wc -l) -gt 0 ]; then
            echo $1 -std=c++11 -O3 -DNDEBUG -fno-rtti -flto -march=native "$2" -o "$3"
            $1 -std=c++11 -O3 -DNDEBUG -fno-rtti -flto -march=native "$2" -o "$3" || exit 1
        elif [ $( echo $1 | grep -e 'fortran' | wc -l) -eq 0 ]; then
            echo $1 -std=c11 -O3 -DNDEBUG -flto -march=native "$2" -o "$3"
            $1 -std=c11 -O3 -DNDEBUG -flto -march=native "$2" -o "$3" || exit 1
        else
            echo $1 -std=f2003 -O3 -flto -march=native "$2" -o "$3"
            $1 -std=f2003 -O3 -flto -march=native "$2" -o "$3" || exit 1
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

#run_benchmark(executable, log)
run_benchmark()
{
    if [ "$1" -nt "$2" ]; then
        echo "{ { taskset -c 1 sh -c \"time \\\"$1\\\"\"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee \"$2\"; } 2>&1"
        { { taskset -c 1 sh -c "time \"$1\""; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$2"; } 2>&1 || exit 1
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

run_benchmark "$BUILD/dijkstra_cpp_gcc_debug" "$BUILD/dijkstra_cpp_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_gcc_release" "$BUILD/dijkstra_cpp_gcc_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_clang_debug" "$BUILD/dijkstra_cpp_clang_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_clang_release" "$BUILD/dijkstra_cpp_clang_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_gcc_debug" "$BUILD/dijkstra_c_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_gcc_release" "$BUILD/dijkstra_c_gcc_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_clang_debug" "$BUILD/dijkstra_c_clang_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_c_clang_release" "$BUILD/dijkstra_c_clang_release.txt" || exit 1
run_benchmark "$BUILD/dijkstra_fortran_gcc_debug" "$BUILD/dijkstra_fortran_gcc_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_fortran_gcc_release" "$BUILD/dijkstra_fortran_gcc_release.txt" || exit 1
run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra_python.txt" || exit 1