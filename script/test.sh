#!/bin/bash
SOURCE="$(dirname $(dirname $(readlink -f $0)))/source"
BUILD="$(dirname $(dirname $(readlink -f $0)))/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

"$(dirname $(readlink -f $0))/build.sh" || exit 1

#run_benchmark(executable, benchmark, log, cores, interpreter)
run_benchmark()
{
    if [ "$1" -nt "$3" -o "$2" -nt "$3" ]; then
        if [ ! -z "$5" ]; then
            LAUNCH="$5"
        elif [ $( echo "$1" | grep -e '.*.exe$' | wc -l) -gt 0 ]; then
            LAUNCH="mono"
        elif [ $( echo "$1" | grep -e '.*.dll$' | wc -l) -gt 0 ]; then
            LAUNCH="dotnet"
        elif [ $( echo "$1" | grep -e '.*.class$' | wc -l) -gt 0 ]; then
            LAUNCH="java"
        elif [ $( echo "$1" | grep -e '.*.js$' | wc -l) -gt 0 ]; then
            LAUNCH="node"
        elif [ $( echo "$1" | grep -e '.*.lua$' | wc -l) -gt 0 ]; then
            LAUNCH="lua"
        elif [ $( echo "$1" | grep -e '.*.py$' | wc -l) -gt 0 ]; then
            LAUNCH="python"
        elif [ $( echo "$1" | grep -e '.*.m$' | wc -l) -gt 0 ]; then
            LAUNCH="matlab"
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
        if [ "$LAUNCH" == "matlab" ]; then
            echo "{ { taskset -c ${CORES} bash -c \"for i in \\\$(seq 1 ${RUNS}); do time matlab -batch 'm = dijkstra_matlab(); m.main();'; done } 3>&2 2>&1 1>&3 | grep -v -e "^\$" | tee \"$3\"; } 2>&1"
            { { taskset -c ${CORES} bash -c "for i in \$(seq 1 ${RUNS}); do time matlab -batch 'm = dijkstra_matlab(); m.main();'; done"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
        elif [ "$LAUNCH" == "java" ]; then
            DIRECTORY=$(dirname "$1")
            CLASS=$(basename "$1" | sed 's/.class$//g')
            echo "{ { taskset -c ${CORES} bash -c \"cd \\\"$DIRECTORY\\\"\"; for i in \\\$(seq 1 ${RUNS}); do time ${LAUNCH} \\\"$CLASS\\\"\"; done } 3>&2 2>&1 1>&3 | grep -v -e "^\$" | tee \"$3\"; } 2>&1"
            { { taskset -c ${CORES} bash -c "cd \"$DIRECTORY\"; for i in \$(seq 1 ${RUNS}); do time ${LAUNCH} \"$CLASS\"; done"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
        else
            echo "{ { taskset -c ${CORES} bash -c \"for i in \\\$(seq 1 ${RUNS}); do time ${LAUNCH} \\\"$1\\\"\"; done } 3>&2 2>&1 1>&3 | grep -v -e "^\$" | tee \"$3\"; } 2>&1"
            { { taskset -c ${CORES} bash -c "for i in \$(seq 1 ${RUNS}); do time ${LAUNCH} \"$1\"; done"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" | tee "$3"; } 2>&1 || exit 1
        fi
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
if [ $(type dotnet 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/Dotnet/Debug/Dotnet.dll" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_csharp_dotnet_debug.txt" || exit 1
    run_benchmark "$BUILD/Dotnet/Release/Dotnet.dll" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_csharp_dotnet_release.txt" || exit 1
fi
if [ $(type javac 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/JavaDebug/Dijkstra.class" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_java_openjdk_debug.txt" || exit 1
    run_benchmark "$BUILD/JavaRelease/Dijkstra.class" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_java_openjdk_release.txt" || exit 1
fi
if [ $(type gfortran 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_fortran_gfortran_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gfortran_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_fortran_gfortran_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_fortran_gfortran_release.txt" || exit 1
fi
if [ $(type fpc 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_pascal_fpc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_pascal_fpc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_pascal_fpc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_pascal_fpc_release.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_delphi_fpc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_delphi_fpc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_delphi_fpc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_delphi_fpc_release.txt" || exit 1
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
    run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_python_cpython.txt" || exit 1
fi
if [ $(type pypy3 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_python_pypy.txt" "" pypy3 || exit 1
fi
if [ $(type node 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$SOURCE/dijkstra_js.js" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_js_node.txt" || exit 1
fi
if [ $(type lua 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$SOURCE/dijkstra_lua.lua" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_lua_lua.txt" || exit 1
fi
if [ $(type ghc 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark "$BUILD/dijkstra_haskell_ghc_debug" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_debug.txt" || exit 1
    run_benchmark "$BUILD/dijkstra_haskell_ghc_release" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_haskell_ghc_release.txt" || exit 1
fi
if [ $(type matlab 2>/dev/null | wc -l) -gt 0 ]; then
    run_benchmark_matlab "$BUILD/dijkstra_matlab.m" "$BUILD/dijkstra.txt" "$BUILD/dijkstra_matlab_matlab.txt" || exit 1
fi
