#!/bin/bash
SOURCE="$(dirname $(dirname $(readlink -f $0)))/source"
BUILD="$(dirname $(dirname $(readlink -f $0)))/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

"$(dirname $(readlink -f $0))/build.sh" || exit 1

#run_benchmark(executable, benchmark, log, cores)
run_benchmark()
{
    if [ "$1" -nt "$3" -o "$2" -nt "$3" ]; then
        if [ $( echo "$1" | grep -e '.*.exe$' | wc -l) -gt 0 ]; then
            LAUNCH="mono"
        elif [ $( echo "$1" | grep -e '.*.js$' | wc -l) -gt 0 ]; then
            LAUNCH="node"
        else
            LAUNCH=""
        fi
        if [ "$4" == "all" ]; then
            CORES="1,2,3,4"
        elif [ "$4" == "one_physical" ]; then
            CORES="1,2"
        else
            CORES="1"
        fi
        RUNS=5
        echo "{ { taskset -c ${CORES} bash -c \"for i in \\\$(seq 1 ${RUNS}); do time ${LAUNCH} \\\"$1\\\"\"; done } 3>&2 2>&1 1>&3 | grep -v -e "^\$" | tee \"$3\"; } 2>&1"
        { { taskset -c ${CORES} bash -c "for i in \$(seq 1 ${RUNS}); do time ${LAUNCH} \"$1\"; done"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
    fi
}

#run_benchmark_matlab(script, benchmark, log, cores)
run_benchmark_matlab()
{
    if [ "$1" -nt "$3" -o "$2" -nt "$3" ]; then
        if [ "$4" == "all" ]; then
            CORES="1,2,3,4"
        elif [ "$4" == "one_physical" ]; then
            CORES="1,2"
        else
            CORES="1"
        fi
        RUNS=5
        echo "{ { taskset -c ${CORES} bash -c \"for i in \\\$(seq 1 ${RUNS}); do time matlab -batch 'm = dijkstra_matlab(); m.main();'; done } 3>&2 2>&1 1>&3 | grep -v -e "^\$" | tee \"$3\"; } 2>&1"
        { { taskset -c ${CORES} bash -c "for i in \$(seq 1 ${RUNS}); do time matlab -batch 'm = dijkstra_matlab(); m.main();'; done"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
    fi
}

# Testing
if [ $(type g++ 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_cpp_gcc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_gcc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_cpp_gcc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_gcc_release.txt" || exit 1
fi
if [ $(type clang++ 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_cpp_clang_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_clang_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_cpp_clang_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_cpp_clang_release.txt" || exit 1
fi
if [ $(type gcc 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_c_gcc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_gcc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_c_gcc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_gcc_release.txt" || exit 1
fi
if [ $(type clang 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_c_clang_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_clang_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_c_clang_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_clang_release.txt" || exit 1
fi
if [ $(type mcs 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_csharp_mcs_debug.exe" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_csharp_mcs_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_csharp_mcs_release.exe" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_csharp_mcs_release.txt" || exit 1
fi
if [ $(type gfortran 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_fortran_gfortran_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gfortran_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_fortran_gfortran_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gfortran_release.txt" || exit 1
fi

# Testing extras
if [ $(type g++ 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_c_gcc_release_cpp" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_gcc_release_cpp.txt" || exit 1
fi
if [ $(type clang++ 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_c_clang_release_cpp" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_c_clang_release_cpp.txt" || exit 1
fi

# Testing slow programs
if [ $(type python 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_python.txt" || exit 1
fi
if [ $(type node 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$SOURCE/dijkstra_js.js" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_js_node.txt" || exit 1
fi
if [ $(type ghc 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_haskell_ghc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_haskell_ghc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_release.txt" || exit 1
fi
if [ $(type matlab 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark_matlab "$BUILD/dijkstra_matlab.m" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_matlab_matlab.txt" || exit 1
fi
