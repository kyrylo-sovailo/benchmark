#!/usr/bin/python
import matplotlib.pyplot as plt
import sys, math

class Measurement:
    def __init__(self, label, filename):
        times = []
        with open(filename, 'r') as f:
            for line in f:
                if not "real" in line: continue
                string_time = line.split()[1]
                split_time = string_time.replace('h',' ').replace('m',' ').replace('s',' ').replace(',', '.').split()
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

        inv_times = [ 1 / time for time in times ]
        self.inv_mean = sum(inv_times) / len(inv_times)
        self.inv_variance = sum([ (inv_time - self.inv_mean) ** 2 for inv_time in inv_times ]) / len(inv_times)
        self.inv_deviation = math.sqrt(self.inv_variance)

        labels = label.split(", ")
        if "C" in labels: self.color = "gold"
        elif "C++" in labels: self.color = "orange"
        elif "C#" in labels: self.color = "red"
        elif "Fortran" in labels: self.color = "maroon"
        elif "Haskell" in labels: self.color = "magenta"
        elif "Python" in labels: self.color = "cyan"
        elif "Matlab" in labels: self.color = "blue"
        else: self.color = "black"
    
def main(inverse):
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
        "Matlab" : "dijkstra_matlab_matlab.txt",

        "C, clang, map+threads" : "dijkstra_c_clang_release_opt.txt",
        "C, clang++, release" : "dijkstra_c_clang_release_cpp.txt",
    }
    measurements = []
    for label, filename in label_to_filename.items(): measurements.append(Measurement(label, filename))
    measurements.sort(key=lambda m: m.mean)
    
    labels = [ m.label for m in measurements ]
    values = [ m.mean for m in measurements ] if not inverse else [ m.inv_mean for m in measurements ]
    deviations = [ m.deviation for m in measurements ] if not inverse else [ m.inv_deviation for m in measurements ]
    colors = [ m.color for m in measurements ]

    title = "Execution time diagram" if not inverse else "Execution speed diagram"
    figure, axes = plt.subplots(1, 1, num=title)
    axes.bar(labels, values, yerr=deviations, color=colors)
    for i, m in enumerate(measurements):
        position = ((m.mean + m.deviation) if not inverse else (m.inv_mean + m.inv_deviation)) + max(values) / 100
        text = f"{m.mean:0.2f}s" if not inverse else f"{m.inv_mean:0.3f}$s^{{-1}}$"
        axes.text(i, position, text, ha="center")
    axes.set_xticks(range(len(labels)))
    axes.set_xticklabels(labels, rotation=15)
    axes.set_title(title)
    axes.set_xlabel("Language/compiler")
    axes.set_ylabel("Time, s" if not inverse else "Speed, $s^{-1}$")
    plt.show()

if __name__ == "__main__":
    if len(sys.argv) >= 2 and sys.argv[1] == "--inverse": main(True)
    elif len(sys.argv) == 1: main(False)
    else: raise Exception("Invalid usage")