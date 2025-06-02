#!/bin/bash
SOURCE="$(dirname $(dirname $(readlink -f $0)))/source"
BUILD="$(dirname $(dirname $(readlink -f $0)))/build"
if [ ! -d "$BUILD" ]; then mkdir "$BUILD"; fi
cd "$BUILD"

#compile_debug(compiler, source, destination, options)
compile_debug()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            FLAGS="-std=c++11 -Wall -Wextra -Wconversion -Wsign-conversion -pedantic -g"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            FLAGS="-std=c11 -Wall -Wextra -Wconversion -Wsign-conversion -pedantic -g"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 -a "$1" == "dotnet" ]; then #C#
            #Actual destination is ignored, assuming destination has form $DIRECTORY/$NAME/Debug/$NAME.dll
            DIRECTORY=$(dirname $(dirname $(dirname "$3")))
            if [ ! -d "$DIRECTORY" ]; then
                echo mkdir \"$DIRECTORY\"
                mkdir "$DIRECTORY" || exit 1
            fi
            CWD=$(pwd)
            NAME=$(basename $(dirname $(dirname "$3")))
            if [ ! -f "$DIRECTORY/$NAME/$NAME.csproj" ]; then
                echo cd \"$DIRECTORY\" \&\& dotnet new console -n \"$NAME\" \&\& cd \"$CWD\"
                cd "$DIRECTORY" && dotnet new console -n "$NAME" && cd "$CWD" || exit 1
            fi
            FLAGS="-c Debug"
            echo cd \"$DIRECTORY/$NAME\" \&\& cp \"$2\" \"./Program.cs\" \&\& $1 build $FLAGS -o \"./Debug\" \&\& cd \"$CWD\"
            cd "$DIRECTORY/$NAME" && cp "$2" "./Program.cs" && $1 build $FLAGS -o "./Debug" && cd "$CWD" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 ]; then #C#
            FLAGS="-debug -platform:x64"
            echo $1 $FLAGS "$2" -out:"$3"
            $1 $FLAGS "$2" -out:"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.java$' | wc -l) -gt 0 ]; then #Java
            #Actual destination basename is ignored, assuming destination basename is equal to source basename
            DIRECTORY=$(dirname "$3")
            FLAGS="-g"
            echo $1 $FLAGS \"$2\" -d \"$DIRECTORY\"
            $1 $FLAGS "$2" -d "$DIRECTORY" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            FLAGS="-std=f95 -g -fcheck=all"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.pas$' | wc -l) -gt 0 -a "$4" == "delphi" ]; then #Delphi
            FLAGS="-g -Mdelphi"
            echo $1 $FLAGS "$2" -o"$3"
            $1 $FLAGS "$2" -o"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.pas$' | wc -l) -gt 0 ]; then #Pascal
            FLAGS="-g -Mtp"
            echo $1 $FLAGS "$2" -o"$3"
            $1 $FLAGS "$2" -o"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.hs$' | wc -l) -gt 0 ]; then #Haskell
            FLAGS="-rtsopts"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        else
            exit 1
        fi
    fi
}

#compile_release(compiler, source, destination, options)
compile_release()
{
    if [ "$2" -nt "$3" ]; then
        if [ $( echo "$2" | grep -e '.*.cpp$' | wc -l) -gt 0 ]; then #C++
            FLAGS="-std=c++11 -Wall -Wextra -Wconversion -Wsign-conversion -O3 -DNDEBUG -fno-rtti -flto -march=native"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.c$' | wc -l) -gt 0 ]; then #C
            FLAGS="-std=c11 -Wall -Wextra -Wconversion -Wsign-conversion -O3 -DNDEBUG -flto -march=native"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 -a "$1" == "dotnet" ]; then #C#
            #Actual destination is ignored, assuming destination has form $DIRECTORY/$NAME/Debug/$NAME.dll
            DIRECTORY=$(dirname $(dirname $(dirname "$3")))
            if [ ! -d "$DIRECTORY" ]; then
                echo mkdir \"$DIRECTORY\"
                mkdir "$DIRECTORY" || exit 1
            fi
            CWD=$(pwd)
            NAME=$(basename $(dirname $(dirname "$3")))
            if [ ! -f "$DIRECTORY/$NAME/$NAME.csproj" ]; then
                echo cd \"$DIRECTORY\" \&\& dotnet new console -n \"$NAME\" \&\& cd \"$CWD\"
                cd "$DIRECTORY" && dotnet new console -n "$NAME" && cd "$CWD" || exit 1
            fi
            FLAGS="-c Release"
            echo cd \"$DIRECTORY/$NAME\" \&\& cp \"$2\" \"./Program.cs\" \&\& $1 build $FLAGS -o \"./Release\" \&\& cd \"$CWD\"
            cd "$DIRECTORY/$NAME" && cp "$2" "./Program.cs" && $1 build $FLAGS -o "./Release" && cd "$CWD" || exit 1
        elif [ $( echo "$2" | grep -e '.*.cs$' | wc -l) -gt 0 ]; then #C#
            FLAGS="-optimize+ -platform:x64"
            echo $1 $FLAGS "$2" -out:"$3"
            $1 $FLAGS "$2" -out:"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.java$' | wc -l) -gt 0 ]; then #Java
            DIRECTORY=$(dirname "$3")
            FLAGS=""
            echo $1 $FLAGS \"$2\" -d \"$DIRECTORY\"
            $1 $FLAGS "$2" -d "$DIRECTORY" || exit 1
        elif [ $( echo "$2" | grep -e '.*.f90$' | wc -l) -gt 0 ]; then #Fortran
            FLAGS="-std=f95 -O3 -flto -march=native"
            echo $1 $FLAGS "$2" -o "$3"
            $1 $FLAGS "$2" -o "$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.pas$' | wc -l) -gt 0 -a "$4" == "delphi" ]; then #Delphi
            FLAGS="-O4 -Mdelphi"
            echo $1 $FLAGS "$2" -o"$3"
            $1 $FLAGS "$2" -o"$3" || exit 1
        elif [ $( echo "$2" | grep -e '.*.pas$' | wc -l) -gt 0 ]; then #Pascal
            FLAGS="-O4 -Mtp"
            echo $1 $FLAGS "$2" -o"$3"
            $1 $FLAGS "$2" -o"$3" || exit 1
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
if [ $(type g++ 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_debug" || exit 1
    compile_release g++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_gcc_release" || exit 1
fi
if [ $(type clang++ 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_debug" || exit 1
    compile_release clang++ "$SOURCE/dijkstra_cpp.cpp" "$BUILD/dijkstra_cpp_clang_release" || exit 1
fi

# C
if [ $(type gcc 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_debug" || exit 1
    compile_release gcc "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_gcc_release" || exit 1
fi
if [ $(type clang 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_debug" || exit 1
    compile_release clang "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c_clang_release" || exit 1
fi

# Extras
if [ $(type gcc 2>/dev/null | wc -l) -gt 0 ]; then
    compile_release "gcc -nostdlib -ffreestanding -Wno-unused-parameter -Wno-implicit-function-declaration" "$SOURCE/dijkstra_c_freestanding.c" "$BUILD/dijkstra_c_gcc_release_freestanding" || exit 1
    compile_release "gcc -DUSE_MAPPING -nostdlib -ffreestanding -Wno-unused-parameter -Wno-implicit-function-declaration" "$SOURCE/dijkstra_c_freestanding.c" "$BUILD/dijkstra_c_gcc_release_freestanding_mapping" || exit 1
fi
if [ $(type g++ 2>/dev/null | wc -l) -gt 0 ]; then
    copy "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c.cpp" || exit 1
    compile_release "g++ -Drestrict=" "$BUILD/dijkstra_c.cpp" "$BUILD/dijkstra_c_gcc_release_cpp" || exit 1 #C as C++
fi
if [ $(type clang++ 2>/dev/null | wc -l) -gt 0 ]; then
    copy "$SOURCE/dijkstra_c.c" "$BUILD/dijkstra_c.cpp" || exit 1
    compile_release "clang++ -Drestrict=" "$BUILD/dijkstra_c.cpp" "$BUILD/dijkstra_c_clang_release_cpp" || exit 1 #C as C++
fi

# C#
if [ $(type mcs 2>/dev/null | wc -l) -gt 0 ]; then
    copy "$SOURCE/dijkstra_csharp.cs" "$BUILD/dijkstra_csharp_debug.cs" || exit 1
    compile_debug mcs "$BUILD/dijkstra_csharp_debug.cs" "$BUILD/dijkstra_csharp_mcs_debug.exe" || exit 1
    copy "$SOURCE/dijkstra_csharp.cs" "$BUILD/dijkstra_csharp_release.cs" || exit 1
    compile_release mcs "$BUILD/dijkstra_csharp_release.cs" "$BUILD/dijkstra_csharp_mcs_release.exe" || exit 1
fi
if [ $(type dotnet 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug dotnet "$SOURCE/dijkstra_csharp.cs" "$BUILD/Dotnet/Debug/Dotnet.dll" dotnet || exit 1
    compile_release dotnet "$SOURCE/dijkstra_csharp.cs" "$BUILD/Dotnet/Release/Dotnet.dll" dotnet || exit 1
fi

# Java
if [ $(type javac 2>/dev/null | wc -l) -gt 0 ]; then
    if [ ! -d "$BUILD/JavaDebug" ]; then
        echo mkdir \"$BUILD/JavaDebug\"
        mkdir "$BUILD/JavaDebug" || exit 1
    fi
    if [ ! -d "$BUILD/JavaRelease" ]; then
        echo mkdir \"$BUILD/JavaRelease\"
        mkdir "$BUILD/JavaRelease" || exit 1
    fi
    copy "$SOURCE/dijkstra_java.java" "$BUILD/JavaDebug/Dijkstra.java" || exit 1
    compile_debug javac "$BUILD/JavaDebug/Dijkstra.java" "$BUILD/JavaDebug/Dijkstra.class" || exit 1
    copy "$SOURCE/dijkstra_java.java" "$BUILD/JavaRelease/Dijkstra.java" || exit 1
    compile_release javac "$BUILD/JavaRelease/Dijkstra.java" "$BUILD/JavaRelease/Dijkstra.class" || exit 1
fi

# Fortran
if [ $(type gfortran 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gfortran_debug" || exit 1
    compile_release gfortran "$SOURCE/dijkstra_fortran.f90" "$BUILD/dijkstra_fortran_gfortran_release" || exit 1
fi

# Pascal/Delphi
if [ $(type fpc 2>/dev/null | wc -l) -gt 0 ]; then
    compile_debug fpc "$SOURCE/dijkstra_pascal.pas" "$BUILD/dijkstra_pascal_fpc_debug" || exit 1
    compile_release fpc "$SOURCE/dijkstra_pascal.pas" "$BUILD/dijkstra_pascal_fpc_release" || exit 1
    compile_debug fpc "$SOURCE/dijkstra_delphi.pas" "$BUILD/dijkstra_delphi_fpc_debug" delphi || exit 1
    compile_release fpc "$SOURCE/dijkstra_delphi.pas" "$BUILD/dijkstra_delphi_fpc_release" delphi || exit 1
fi

# Haskell
if [ $(type ghc 2>/dev/null | wc -l) -gt 0 ]; then
    copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_debug.hs" || exit 1
    compile_debug ghc "$BUILD/dijkstra_haskell_debug.hs" "$BUILD/dijkstra_haskell_ghc_debug" || exit 1
    copy "$SOURCE/dijkstra_haskell.hs" "$BUILD/dijkstra_haskell_release.hs" || exit 1
    compile_release ghc "$BUILD/dijkstra_haskell_release.hs" "$BUILD/dijkstra_haskell_ghc_release" || exit 1
fi

# Matlab
if [ $(type matlab 2>/dev/null | wc -l) -gt 0 ]; then
    copy "$SOURCE/dijkstra_matlab.m" "$BUILD/dijkstra_matlab.m" || exit 1
fi

# Testing
if [ $(type g++ 2>/dev/null | wc -l) -gt 0 ]; then
    compile_release g++ "$SOURCE/create_dijkstra.cpp" "$BUILD/create_dijkstra" || exit 1
elif [ $(type clang++ 2>/dev/null | wc -l) -gt 0 ]; then
    compile_release clang++ "$SOURCE/create_dijkstra.cpp" "$BUILD/create_dijkstra" || exit 1
else
    echo "Either g++ or clang++ needs to be present" && exit 1
fi
create_benchmark "$BUILD/create_dijkstra" "$BUILD/dijkstra.txt" || exit 1
