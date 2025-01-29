#!/usr/bin/python
import matplotlib.pyplot as plt
import math

class Measurement:
    def __init__(self, label, filename):
        times = []
        with open(filename, 'r') as f:
            for line in f:
                if not "real" in line: continue
                string_time = line.split()[1]
                split_time = string_time.replace('h',' ').replace('m',' ').replace('s',' ').split()
                if len(split_time) == 1: time = float(split_time[0])
                elif len(split_time) == 2: time = 60*int(split_time[0]) + float(split_time[1])
                elif len(split_time) == 3: time = 3600*int(split_time[0]) + 60*int(split_time[1]) + float(split_time[2])
                else: raise Exception(f"Invalid format in {filename}")
                times.append(time)
        if len(times) == 0: raise Exception(f"No time found in {filename}")

        self.label = label
        self.mean = sum(times) / len(times)
        self.variance = sum([ (time - self.mean) ** 2 for time in times ]) / len(times)
        self.deviation = math.sqrt(self.variance)

        if "++" in label: self.color = "red"
        elif "#" in label: self.color = "blue"
        elif "C" in label: self.color = "orange"
        elif "Haskell" in label: self.color = "purple"
        elif "Fortran" in label: self.color = "green"
        else: self.color = "pink"
    
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
        "C#, mcs, debug" : "dijkstra_csharp_mcs_debug.txt",
        "C#, mcs, release" : "dijkstra_csharp_mcs_release.txt",
        "Fortran, gfortran, debug" : "dijkstra_fortran_gfortran_debug.txt",
        "Fortran, gfortran, release" : "dijkstra_fortran_gfortran_release.txt",
        "Haskell, ghc, debug" : "dijkstra_haskell_ghc_debug.txt",
        "Haskell, ghc, release" : "dijkstra_haskell_ghc_release.txt",
        "Python" : "dijkstra_python.txt",

        "C, clang, map+threads" : "dijkstra_c_clang_release_opt.txt",
        "C, clang++, release" : "dijkstra_c_clang_release_cpp.txt",
    }
    measurements = []
    for label, filename in label_to_filename.items(): measurements.append(Measurement(label, filename))
    measurements.sort(key=lambda m: m.mean)
    
    labels = [ m.label for m in measurements ]
    deviations = [ m.deviation for m in measurements ]
    times = [ m.mean for m in measurements ]
    colors = [ m.color for m in measurements ]

    plt.bar(labels, times, yerr=deviations, color=colors)
    for i, m in enumerate(measurements): plt.text(i, m.mean + m.deviation + max(times) / 100, f"{m.mean:0.3f}", ha="center")
    plt.xticks(rotation=15)
    plt.title("Execution times")
    plt.xlabel("Language/compiler")
    plt.ylabel("Time")
    plt.show()

if __name__ == "__main__":
    main()