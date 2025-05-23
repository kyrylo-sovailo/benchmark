### Proving that C++ is not slow, it's just a skill issue

This repository contains implementations of Dijkstra algorithm in different programming languages. Quick peek at the results:

![Bar chart with executions times, Matlab is the slowest, C++ is the fastest](data/benchmark.png "Execution times")
All data is included, you are welcome to play with the configuration of the visualizer. This graph was produced with: `cd data && ../script/create_graphics.py --reverse --inverse --relative-slowest --no-debug --no-extras`.

### Goal
The benchmark is a combined benchmark that strives to capture a performance of a **reasonably-written** and **balanced** program. Check out the [daughter project](https://github.com/kyrylo-sovailo/benchmark_masterrace) to see what happens with these constraints dropped.

A "reasonable" code is a code that:
 - Stays readable
 - In particular, accesses structure members by names rather than by index
 - Stays native, does not delegate the task to other programming languages
 - Does not use assembler optimizations
 - Does not use hardware acceleration
 - Uses standard I/O
 - Does not use multithreading
 - Does not assume any prior knowledge about the program input

A "balanced" program is a program that does all types of operations that you'd expect a non-computation-heavy utility to do, in close-to-real-life proportions. It includes:
 - Logics
 - Floating-point operations
 - Memory management
 - Routines from the standard library
 - Input and output
 - Initialization and finalization
 - etc.

The benchmark does not focus on only one of these aspects. Finding out which exact part is faster in which languages is outside of the scope of this repository. Some further research on `C++` and `C` is coming soon.

### Notes
The names of the bars on the chart are pretty self-explanatory. Except for:
 - `C, g++` is a C program compiled with g++ as valid C++.
 - `C, clang++` is same but for clang++.

My knowledge of programming languages is not on the same level, and not all implementations are created equally. Here are my comments on the probability of improvement:
 - `C++`: reference implementation, well-researched and well-optimized.
 - `C`, `Fortran`: direct translation, the optimization is powerful, no improvement is expected.
 - `Pascal`, `C#`, `Java`: direct translation, the optimization is not great, some improvement may be achieved by micromanagement.
 - `Python`, `JS`, `Lua`: not well-researched, some improvement may be achieved by micromanagement and different data structures.
 - `Matlab`: I highly doubt that something can be done to the algorithm, though improvement through micromanagement is very likely.
 - `Delphi`: since Pascal is (roughly) a subset of Delphi, the improvement can be achieved by simply downgrading to Pascal.
 - `Haskell`: further improvements are probable. Better Haskell knowledge is required.

### Hardware
The measurements were performed on Intel Pentium 4415U. To reduce noise, the kernel was booted with `isolcpus=1,3`, where 1 and 3 are logical cores that correspond to physical core 1.

### Software
```
GCC                  14.2.1
Clang                19.1.7
GFortran             14.2.1
Free Pascal Compiler 3.2.2
MCS                  6.12.0.199, Target 4.5
.NET                 9.0.101
Java                 21.0.6
Python               3.12.9
PyPy                 7.3.17, Target 3.10.14
GHC                  9.8.4
Node                 22.13.1
Lua                  5.4.6
Matlab               24.2.0.2833386 (R2024b) Update 4
Kernel               6.12.10-zen1-x86_64
```

### Conclusions
 - C++ is faster than C (with "reasonable" code, see [Goal](#goal). Further research is coming soon)
 - Fortran is not that fast
