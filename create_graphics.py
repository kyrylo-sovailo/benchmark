#!/usr/bin/python
import matplotlib.pyplot as plt

def read_time(filename):
    with open(filename, 'r') as f:
        for line in f:
            if not "real" in line: continue
            time = line.split()[1]
            split_time = time.replace('h',' ').replace('m',' ').replace('s',' ').split()
            if len(split_time) == 1: return float(split_time[0])
            elif len(split_time) == 2: return 60*int(split_time[0]) + float(split_time[1])
            elif len(split_time) == 3: return 3600*int(split_time[0]) + 60*int(split_time[1]) + float(split_time[2])
            else: raise Exception(f"Invalid format in {filename}")
    raise Exception(f"No time found in {filename}")

def get_color(label):
    if "C++" in label: return "red"
    elif "C#" in label: return "blue"
    elif "C" in label: return "orange"
    elif "Haskell" in label: return "purple"
    elif "Fortran" in label: return "green"
    else: return "black"
    
def main():
    label_to_filename = {
        "C++, g++, debug" : "dijkstra_cpp_gcc_debug.txt",
        "C++, g++, release" : "dijkstra_cpp_gcc_release.txt",
        "C++, clang++, debug" : "dijkstra_cpp_clang_debug.txt",
        "C++, clang++, release" : "dijkstra_cpp_clang_release.txt",
        "C, gcc, debug" : "dijkstra_c_gcc_debug.txt",
        "C, gcc, release" : "dijkstra_c_gcc_release.txt",
        "C, clang, debug" : "dijkstra_c_clang_debug.txt",
        "C, clang, release" : "dijkstra_c_clang_release.txt",
        "C#, mcs 4.5, debug" : "dijkstra_csharp_mcs_debug.txt",
        "C#, mcs 4.5, release" : "dijkstra_csharp_mcs_release.txt",
        "Fortran, gfortran, debug" : "dijkstra_fortran_gfortran_debug.txt",
        "Fortran, gfortran, release" : "dijkstra_fortran_gfortran_release.txt",
        "Haskell, ghc, debug" : "dijkstra_haskell_ghc_debug.txt",
        "Haskell, ghc, release" : "dijkstra_haskell_ghc_release.txt",
        "Python 3.12" : "dijkstra_python.txt",

        "C, clang, I/O + threads" : "dijkstra_c_clang_release_opt.txt",
    }
    label_to_time = []
    for label, filename in label_to_filename.items(): label_to_time.append((label, read_time(filename)))
    label_to_time = sorted(label_to_time, key=lambda pair: pair[1], reverse=False)
    
    labels = [ pair[0] for pair in label_to_time ]
    times = [ pair[1] for pair in label_to_time ]
    colors = [ get_color(label) for label in labels ]

    plt.bar(labels, times, color=colors)
    for i, time in enumerate(times): plt.text(i, time + max(times) / 100, f"{time:0.3f}", ha="center")
    plt.xticks(rotation=15)
    plt.title("Execution times")
    plt.xlabel("Language/compiler")
    plt.ylabel("Time")
    plt.show()

if __name__ == "__main__":
    main()