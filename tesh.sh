#!/bin/sh
SOURCE="$(dirname $(readlink -f $0))"
BUILD="$SOURCE/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

#compile_debug(source, destination)
compile_debug()
{
    if [ "$1" -nt "$2" ]; then
        echo g++ -std=c++11 "$1" -o "$2"
        g++ -std=c++11 "$1" -o "$2" || exit 1
    fi
}

#compile_release(source, destination)
compile_release()
{
    if [ "$1" -nt "$2" ]; then
        echo g++ -std=c++11 -O3 -DNDEBUG -fno-rtti "$1" -o "$2"
        g++ -std=c++11 -O3 -DNDEBUG -fno-rtti "$1" -o "$2" || exit 1
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
        echo "{ { time $1; } 3>&2 2>&1 1>&3 | grep -v -e "^$" > $2; } 2>&1"
        { { time "$1"; } 3>&2 2>&1 1>&3 | grep -v -e "^$" > "$2"; } 2>&1 || exit 1
    fi
}

compile_release "$SOURCE/create_dijkstra.cpp" "$BUILD/create_dijkstra" || exit 1
create_benchmark "$BUILD/create_dijkstra" "$BUILD/dijkstra.txt" || exit 1

compile_debug "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_debug" || exit 1
compile_release "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_release" || exit 1

run_benchmark "$BUILD/dijkstra_cpp_debug" "$BUILD/dijkstra_cpp_debug.txt" || exit 1
run_benchmark "$BUILD/dijkstra_cpp_release" "$BUILD/dijkstra_cpp_release.txt" || exit 1
run_benchmark "$SOURCE/dijkstra_python.py" "$BUILD/dijkstra_python.txt" || exit 1