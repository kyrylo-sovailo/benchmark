#!/bin/sh
SOURCE="$(dirname $(readlink -f $0))"
BUILD="$SOURCE/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

# COMPILE
if [ "$SOURCE/create_dijkstra.cpp" -nt "$BUILD/create_dijkstra" ]; then
    echo 'g++ "$SOURCE/create_dijkstra.cpp" -o "$BUILD/create_dijkstra"'
    g++ "$SOURCE/create_dijkstra.cpp" -o "$BUILD/create_dijkstra" || exit 1
fi
if [ "$SOURCE/create_dijkstra.cpp" -nt "$BUILD/dijkstra.txt" ]; then
    echo '"$BUILD/create_dijkstra"'
    "$BUILD/create_dijkstra" || exit 1
fi
if [ "$SOURCE/dijkstra_cpp.cpp" -nt "$BUILD/dijkstra_cpp_debug" ]; then
    echo 'g++ -std=c++11 "$SOURCE/dijkstra_cpp.cpp" -o "$BUILD/dijkstra_cpp_debug"'
    g++ "$SOURCE/dijkstra_cpp.cpp" -std=c++11 -o "$BUILD/dijkstra_cpp_debug" || exit 1
fi
if [ "$SOURCE/dijkstra_cpp.cpp" -nt "$BUILD/dijkstra_cpp_release" ]; then
    echo 'g++ -std=c++11 -O3 -DNDEBUG -fno-rtti "$SOURCE/dijkstra_cpp.cpp" -o "$BUILD/dijkstra_cpp_release"'
    g++ -std=c++11 -O3 -DNDEBUG -fno-rtti "$SOURCE/dijkstra_cpp.cpp" -o "$BUILD/dijkstra_cpp_release" || exit 1
fi

# TEST
if [ "$BUILD/dijkstra_cpp_debug" -nt "$BUILD/dijkstra_cpp_debug.txt" ]; then
    echo '{ time "$BUILD/dijkstra_cpp_debug"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_cpp_debug.txt"'
    { time "$BUILD/dijkstra_cpp_debug"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_cpp_debug.txt"
fi
if [ "$BUILD/dijkstra_cpp_release" -nt "$BUILD/dijkstra_cpp_release.txt" ]; then
    echo '{ time "$BUILD/dijkstra_cpp_release"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_cpp_release.txt"'
    { time "$BUILD/dijkstra_cpp_release"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_cpp_release.txt"
fi
if [ "$SOURCE/dijkstra_python.py" -nt "$BUILD/dijkstra_python.txt" ]; then
    echo '{ time "$SOURCE/dijkstra_python.py"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_python.txt"'
    { time "$SOURCE/dijkstra_python.py"; } 2>&1 | grep -v -e "^$" > "$BUILD/dijkstra_python.txt"
fi