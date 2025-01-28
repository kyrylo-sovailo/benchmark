Proving that C++ is not slow, it's just a skill issue

![Bar chart with executions times](benchmark.png "Execution times")

### Overview
This repository contains implementations of Dijkstra algorithm in different programming languages.

### Goal
The benchmark is a combined benchmark that strives to capture a performance of a balanced program. The execution time is affected by:
 - Raw speed
 - Efficiency of memory management
 - Efficiency of standard library
 - Input/output speed
 - Initialization time
 - etc.
This is also the reason why the programs don't reserve the memory in advance -- because frequent memory allocation is what applications do in real life. The programs (with one exception) are written with standard cross-platform I/O, no third-party libraries and no multithreading.

### Bar chart
The names of the bars on the chart are pretty self-explanatory. Except for:
 - `C, clang++, release` is a C program compiled with clang++ as valid C++
 - `C, clang, I/O + threads` is a C program written with multi-threading support and memory mapping enabled. It is written as a demonstration, comparing its performance to other programs is not fair.

### Conclusions
 - C++ is faster than C (no comments)
 - C is faster than Fortran
 - Clang is faster than GCC
 - Haskell is terrible for everything algorithm-related

##### But where's the language that all 3.5 members of my cult use???
Fork. I may accept the pull request if I'm in a good mood.