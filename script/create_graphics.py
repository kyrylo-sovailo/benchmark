#!/usr/bin/python
import matplotlib.pyplot as plt
import math, shutil, sys

def find_closest_color(desired_color):
    import matplotlib.colors as mcolors
    map = mcolors.get_named_colors_mapping()
    desired_rgb = mcolors.to_rgb(desired_color)
    closest_name = None
    closest_distance = math.inf
    for name, value in map.items():
        rgb = mcolors.to_rgb(value)
        distance = abs(desired_rgb[0] - rgb[0]) + abs(desired_rgb[1] - rgb[1]) + abs(desired_rgb[2] - rgb[2])
        if distance < closest_distance:
            closest_distance = distance
            closest_name = name
    return closest_name

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
        find_closest = True
        if "C" in labels: self.color = "xkcd:gunmetal" if find_closest else "#555555"
        elif "C++" in labels: self.color = "xkcd:warm pink" if find_closest else "#f34b7d"
        elif "C#" in labels: self.color = "green" if find_closest else "#178600"
        elif "Java" in labels: self.color = "xkcd:caramel" if find_closest else "#b07219"
        elif "Fortran" in labels: self.color = "xkcd:blue with a hint of purple" if find_closest else "#4d41b1"
        elif "Pascal" in labels or "Delphi" in labels: self.color = "xkcd:sandy yellow" if find_closest else "#e3f171"
        elif "Haskell" in labels: self.color = "xkcd:twilight" if find_closest else "#5e5086"
        elif "Python" in labels: self.color = "xkcd:flat blue" if find_closest else "#3572a5"
        elif "Javascript" in labels: self.color = "xkcd:dull yellow" if find_closest else "#f1e05a"
        elif "Lua" in labels: self.color = "navy" if find_closest else "#000080"
        elif "Matlab" in labels: self.color = "xkcd:terra cotta" if find_closest else "#e16737"
        else: self.color = "black" if find_closest else "#000000"

class Graphics:
    def __init__(self):
        self.inverse = False
        self.reverse = False
        self.log = False
        self.no_extras = False
        self.no_debug = False
        self.relative_fastest = False
        self.relative_slowest = False
        self.faster_threshold = math.inf
        self.slower_threshold = -math.inf

        i = 1
        while i < len(sys.argv):
            arg = sys.argv[i]
            if arg == "--help":
                print("Usage: ./create_graphics.py <options>")
                print("Options:")
                print("  --help")
                print("  --inverse")
                print("  --reverse")
                print("  --log")
                print("  --no-extras")
                print("  --no-debug")
                print("  --relative-fastest | --relative-slowest")
                print("  --slower TIME")
                print("  --faster TIME")
                sys.exit(0)
            elif arg == "--inverse": self.inverse = True
            elif arg == "--reverse": self.reverse = True
            elif arg == "--log": self.log = True
            elif arg == "--no-extras": self.no_extras = True
            elif arg == "--no-debug": self.no_debug = True
            elif arg == "--faster":
                i += 1
                if i >= len(sys.argv): raise Exception("Expected time after --faster")
                self.faster_threshold = float(sys.argv[i])
            elif arg == "--slower":
                i += 1
                if i >= len(sys.argv): raise Exception("Expected time after --slower")
                self.slower_threshold = float(sys.argv[i])
            elif arg == "--relative-fastest": self.relative_fastest = True
            elif arg == "--relative-slowest": self.relative_slowest = True
            else: raise Exception("Invalid argument")
            i += 1
                
        if self.relative_slowest and self.relative_fastest: raise Exception("--relative-fastest incompatible with --relative-fastest")
        if self.faster_threshold < self.slower_threshold: raise Exception("Maximum threshold is lower than minium threshold")

    def run(self):
        label_to_filename = dict()
        gpp_present = bool(shutil.which("g++"))
        label_to_filename.update({
            "C++, g++, debug" : ("dijkstra_cpp_gcc_debug.txt", gpp_present),
            "C++, g++, release" : ("dijkstra_cpp_gcc_release.txt", gpp_present)
        })
        clangpp_present = bool(shutil.which("clang++"))
        label_to_filename.update({
            "C++, clang++, debug" : ("dijkstra_cpp_clang_debug.txt", clangpp_present),
            "C++, clang++, release" : ("dijkstra_cpp_clang_release.txt", clangpp_present),
        })
        gcc_present = bool(shutil.which("gcc"))
        label_to_filename.update({
            "C, gcc, debug" : ("dijkstra_c_gcc_debug.txt", gcc_present),
            "C, gcc, release" : ("dijkstra_c_gcc_release.txt", gcc_present),
        })
        clang_present = bool(shutil.which("clang"))
        label_to_filename.update({
            "C, clang, debug" : ("dijkstra_c_clang_debug.txt", clang_present),
            "C, clang, release" : ("dijkstra_c_clang_release.txt", clang_present),
        })
        mcs_present = bool(shutil.which("mcs"))
        label_to_filename.update({
            "C#, mcs, debug" : ("dijkstra_csharp_mcs_debug.txt", mcs_present),
            "C#, mcs, release" : ("dijkstra_csharp_mcs_release.txt", mcs_present),
        })
        dotnet_present = bool(shutil.which("dotnet"))
        label_to_filename.update({
            "C#, .NET, debug" : ("dijkstra_csharp_dotnet_debug.txt", dotnet_present),
            "C#, .NET, release" : ("dijkstra_csharp_dotnet_release.txt", dotnet_present),
        })
        javac_present = bool(shutil.which("mcs"))
        label_to_filename.update({
            "Java, OpenJDK, debug" : ("dijkstra_java_openjdk_debug.txt", javac_present),
            "Java, OpenJDK, release" : ("dijkstra_java_openjdk_release.txt", javac_present),
        })
        gfortran_present = bool(shutil.which("gfortran"))
        label_to_filename.update({
            "Fortran, gfortran, debug" : ("dijkstra_fortran_gfortran_debug.txt", gfortran_present),
            "Fortran, gfortran, release" : ("dijkstra_fortran_gfortran_release.txt", gfortran_present),
        })
        fpc_present = bool(shutil.which("fpc"))
        label_to_filename.update({
            "Pascal, FPC, debug" : ("dijkstra_pascal_fpc_debug.txt", fpc_present),
            "Pascal, FPC, release" : ("dijkstra_pascal_fpc_release.txt", fpc_present),
            "Delphi, FPC, debug" : ("dijkstra_delphi_fpc_debug.txt", fpc_present),
            "Delphi, FPC, release" : ("dijkstra_delphi_fpc_release.txt", fpc_present),
        })
        ghc_present = bool(shutil.which("ghc"))
        if shutil.which("ghc"): label_to_filename.update({
            "Haskell, ghc, debug" : ("dijkstra_haskell_ghc_debug.txt", ghc_present),
            "Haskell, ghc, release" : ("dijkstra_haskell_ghc_release.txt", ghc_present),
        })
        label_to_filename.update({
            "Javascript, Node" : ("dijkstra_js_node.txt", bool(shutil.which("node")))
        })
        label_to_filename.update({
            "Lua, Lua" : ("dijkstra_lua_lua.txt", bool(shutil.which("lua")))
        })
        label_to_filename.update({
            "Lua, LuaJIT" : ("dijkstra_lua_luajit.txt", bool(shutil.which("luajit")))
        })
        label_to_filename.update({
            "Python, CPython" : ("dijkstra_python_cpython.txt", bool(shutil.which("python")))
        })
        label_to_filename.update({
            "Python, PyPy" : ("dijkstra_python_pypy.txt", bool(shutil.which("pypy3")))
        })
        label_to_filename.update({
            "Matlab" : ("dijkstra_matlab_matlab.txt", bool(shutil.which("matlab")))
        })
        if not self.no_extras: label_to_filename.update({
            "C, g++, release" : ("dijkstra_c_gcc_release_cpp.txt", gpp_present)
        })
        if not self.no_extras: label_to_filename.update({
            "C, clang++, release" : ("dijkstra_c_clang_release_cpp.txt", clangpp_present)
        })
        if self.no_debug:
            label_to_filename = { key: value for key, value in label_to_filename.items() if not "debug" in key }

        measurements = []
        for label, (filename, present) in label_to_filename.items():
            if present:
                m = Measurement(label, filename)
            else:
                try: m = Measurement(label, filename)
                except: continue
            if m.mean <= self.faster_threshold and m.mean >= self.slower_threshold: measurements.append(m)
        if not self.reverse: measurements.sort(key=lambda m: m.mean)
        else: measurements.sort(key=lambda m: -m.mean)
        
        labels = [ m.label for m in measurements ]
        values = [ m.mean for m in measurements ] if not self.inverse else [ m.inv_mean for m in measurements ]
        deviations = [ m.deviation for m in measurements ] if not self.inverse else [ m.inv_deviation for m in measurements ]
        colors = [ m.color for m in measurements ]

        if self.relative_fastest:
            fastest_i = -1 if self.reverse else 0
            fastest = values[fastest_i]
            for i in range(len(values)):
                values[i] /= fastest
                deviations[i] /= fastest
            values[fastest_i] = 1
        elif self.relative_slowest:
            slowest_i = 0 if self.reverse else -1
            slowest = values[slowest_i]
            for i in range(len(values)):
                values[i] /= slowest
                deviations[i] /= slowest
            values[slowest_i] = 1

        title = "Execution time diagram" if not self.inverse else "Execution speed diagram"
        figure, axes = plt.subplots(1, 1, num=title)
        axes.bar(labels, values, yerr=deviations, color=colors)
        axes.set_title(title)
        for i, (value, deviation) in enumerate(zip(values, deviations)):
            position = value + deviation
            if self.log: position *= math.exp(math.log(max(values)) / 100)
            else: position += max(values) / 100
            if self.relative_fastest or self.relative_slowest: text = f"{value:0.2f}x" if self.relative_slowest == self.inverse else f"{1/value:0.2f}x"
            else: text = f"{value:0.2f}s" if not self.inverse else f"{value:0.3f}$s^{{-1}}$"
            axes.text(i, position, text, ha="center")
        axes.set_xlabel("Language/compiler")
        axes.set_xticks(range(len(labels)))
        axes.set_xticklabels(labels, rotation=15)
        axes.set_ylabel("Time, s" if not self.inverse else "Speed, $s^{-1}$")
        if self.log: axes.set_yscale('log')
        plt.show()

def main():
    graphics = Graphics()
    graphics.run()

if __name__ == "__main__":
    main()