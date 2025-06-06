#!/usr/bin/env python3
import matplotlib.pyplot as plt
import math, platform, shutil, sys

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
        elif "NASM" in labels: self.color = "xkcd:mud" if find_closest else "#6E4C13"
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
        self.inverse_diagram = False
        self.inverse_labels = False
        self.alphabetic = False
        self.reverse = False
        self.log = False
        self.no_extras = False
        self.no_debug = False
        self.relative_fastest = False
        self.relative_slowest = False
        self.faster_threshold = math.inf
        self.slower_threshold = -math.inf

        i = 1
        demonstration = False
        options_set = False
        while i < len(sys.argv):
            arg = sys.argv[i]
            if arg == "--help":
                print("Usage: ./create_graphics.py [ <options> | --demo ]")
                print("Options:")
                print("  --help")
                print("  --inverse-diagram")
                print("  --inverse-labels")
                print("  --alphabetic")
                print("  --reverse")
                print("  --log")
                print("  --no-extras")
                print("  --no-debug")
                print("  --relative-fastest | --relative-slowest")
                print("  --slower TIME")
                print("  --faster TIME")
                sys.exit(0)
            elif arg == "--demo": demonstration = True
            elif arg == "--inverse-diagram": self.inverse_diagram = options_set = True
            elif arg == "--inverse-labels": self.inverse_labels = options_set = True
            elif arg == "--alphabetic": self.alphabetic = options_set = True
            elif arg == "--reverse": self.reverse = options_set = True
            elif arg == "--log": self.log = options_set = True
            elif arg == "--no-extras": self.no_extras = options_set = True
            elif arg == "--no-debug": self.no_debug = options_set = True
            elif arg == "--faster":
                i += 1
                if i >= len(sys.argv): raise Exception("Expected time after --faster")
                self.faster_threshold = float(sys.argv[i])
                options_set = True
            elif arg == "--slower":
                i += 1
                if i >= len(sys.argv): raise Exception("Expected time after --slower")
                self.slower_threshold = float(sys.argv[i])
                options_set = True
            elif arg == "--relative-fastest": self.relative_fastest = options_set = True
            elif arg == "--relative-slowest": self.relative_slowest = options_set = True
            else: raise Exception("Invalid argument")
            i += 1
        
        if demonstration and options_set: raise Exception("--demo is incompatible with any other options")
        if self.relative_slowest and self.relative_fastest: raise Exception("--relative-slowest incompatible with --relative-fastest")
        if self.faster_threshold < self.slower_threshold: raise Exception("Maximum threshold is lower than minium threshold")
        if demonstration:
            self.inverse_diagram = True
            self.reverse = True
            self.no_extras = True
            self.no_debug = True
            self.relative_slowest = True
            print("--demo is alias for --reverse --relative-slowest --inverse-diagram --no-debug --no-extras")

    def run(self):
        x86_64 = (platform.architecture == "x86_64" and platform.system == "Linux")
        gpp_present = bool(shutil.which("g++"))
        llvmpp_present = bool(shutil.which("clang++"))
        gcc_present = bool(shutil.which("gcc"))
        clang_present = bool(shutil.which("clang"))
        nasm_present = bool(shutil.which("nasm")) and bool(shutil.which("ld"))
        mcs_present = bool(shutil.which("mcs"))
        dotnet_present = bool(shutil.which("dotnet"))
        javac_present = bool(shutil.which("javac"))
        gfortran_present = bool(shutil.which("gfortran"))
        fpc_present = bool(shutil.which("fpc"))
        ghc_present = bool(shutil.which("ghc"))

        label_to_filename = dict()
        label_to_filename.update({
            "C++, g++, debug" : ("dijkstra_cpp_gcc_debug.txt", gpp_present),
            "C++, g++, release" : ("dijkstra_cpp_gcc_release.txt", gpp_present)
        })
        label_to_filename.update({
            "C++, clang++, debug" : ("dijkstra_cpp_clang_debug.txt", llvmpp_present),
            "C++, clang++, release" : ("dijkstra_cpp_clang_release.txt", llvmpp_present),
        })
        label_to_filename.update({
            "C, gcc, debug" : ("dijkstra_c_gcc_debug.txt", gcc_present),
            "C, gcc, release" : ("dijkstra_c_gcc_release.txt", gcc_present),
        })
        label_to_filename.update({
            "C, clang, debug" : ("dijkstra_c_clang_debug.txt", clang_present),
            "C, clang, release" : ("dijkstra_c_clang_release.txt", clang_present),
        })
        label_to_filename.update({
            "NASM, debug" : ("dijkstra_asm_nasm_debug.txt", nasm_present and x86_64),
            "NASM, release" : ("dijkstra_asm_nasm_release.txt", nasm_present and x86_64),
        })
        label_to_filename.update({ #Important, doesn't count as extra
            "C, gcc, release, freestanding" : ("dijkstra_c_gcc_release_freestanding.txt", gpp_present)
        })
        if not self.no_extras: label_to_filename.update({
            "C, gcc, release, freestanding/asm" : ("dijkstra_c_gcc_release_freestanding_asm.txt", gpp_present and x86_64),
            "C, gcc, release, freestanding/mapping" : ("dijkstra_c_gcc_release_freestanding_mapping.txt", gpp_present and x86_64)
        })
        if not self.no_extras: label_to_filename.update({
            "C, g++, release" : ("dijkstra_c_gcc_release_cpp.txt", gpp_present),
            "C, g++, release, restrict" : ("dijkstra_c_gcc_release_cpp_restrict.txt", gpp_present)
        })
        if not self.no_extras: label_to_filename.update({
            "C, clang++, release" : ("dijkstra_c_clang_release_cpp.txt", llvmpp_present),
            "C, clang++, release, restrict" : ("dijkstra_c_clang_release_cpp_restrict.txt", llvmpp_present)
        })
        label_to_filename.update({
            "C#, mcs, debug" : ("dijkstra_csharp_mcs_debug.txt", mcs_present),
            "C#, mcs, release" : ("dijkstra_csharp_mcs_release.txt", mcs_present),
        })
        label_to_filename.update({
            "C#, .NET, debug" : ("dijkstra_csharp_dotnet_debug.txt", dotnet_present),
            "C#, .NET, release" : ("dijkstra_csharp_dotnet_release.txt", dotnet_present),
        })
        label_to_filename.update({
            "Java, OpenJDK, debug" : ("dijkstra_java_openjdk_debug.txt", javac_present),
            "Java, OpenJDK, release" : ("dijkstra_java_openjdk_release.txt", javac_present),
        })
        label_to_filename.update({
            "Fortran, gfortran, debug" : ("dijkstra_fortran_gfortran_debug.txt", gfortran_present),
            "Fortran, gfortran, release" : ("dijkstra_fortran_gfortran_release.txt", gfortran_present),
        })
        label_to_filename.update({
            "Pascal, FPC, debug" : ("dijkstra_pascal_fpc_debug.txt", fpc_present),
            "Pascal, FPC, release" : ("dijkstra_pascal_fpc_release.txt", fpc_present),
            "Delphi, FPC, debug" : ("dijkstra_delphi_fpc_debug.txt", fpc_present),
            "Delphi, FPC, release" : ("dijkstra_delphi_fpc_release.txt", fpc_present),
        })
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
        if self.alphabetic: measurements.sort(key=lambda m: m.label, reverse=True)
        else: measurements.sort(key=lambda m: m.mean)
        if self.reverse: measurements = reversed(measurements)
        
        labels = [ m.label for m in measurements ]
        text_values = [ m.inv_mean for m in measurements ] if self.inverse_labels else [ m.mean for m in measurements ]
        diagram_values = [ m.inv_mean for m in measurements ] if self.inverse_diagram else [ m.mean for m in measurements ]
        diagram_deviations = [ m.inv_deviation for m in measurements ] if self.inverse_diagram else [ m.deviation for m in measurements ]
        colors = [ m.color for m in measurements ]

        if self.relative_fastest:
            fastest_i = -1 if self.reverse else 0
            fastest = diagram_values[fastest_i]
            for i in range(len(diagram_values)):
                diagram_values[i] /= fastest
                diagram_deviations[i] /= fastest
            diagram_values[fastest_i] = 1
        elif self.relative_slowest:
            slowest_i = 0 if self.reverse else -1
            slowest = diagram_values[slowest_i]
            for i in range(len(diagram_values)):
                diagram_values[i] /= slowest
                diagram_deviations[i] /= slowest
            diagram_values[slowest_i] = 1

        if self.inverse_diagram: title = "Execution speed diagram"
        else: title = "Execution time diagram"
        figure, axes = plt.subplots(1, 1, num=title)
        axes.bar(labels, diagram_values, yerr=diagram_deviations, color=colors)
        axes.set_title(title)
        for i, (text_value, diagram_value, diagram_deviation) in enumerate(zip(text_values, diagram_values, diagram_deviations)):
            position = diagram_value + diagram_deviation
            if self.log: position *= math.exp(math.log(max(diagram_values)) / 100)
            else: position += max(diagram_values) / 100
            if self.relative_fastest or self.relative_slowest:
                text = f"{diagram_value:0.2f}x"
            else:
                text = f"{text_value:0.3f}$s^{{-1}}$" if self.inverse_labels else f"{text_value:0.2f}s"
            axes.text(i, position, text, ha="center")
        axes.set_xlabel("Language/compiler")
        axes.set_xticks(range(len(labels)))
        if self.no_debug:
            axes.set_xticklabels(labels, rotation=20)
        else:
            axes.set_xticklabels(labels, rotation=90)
            figure.subplots_adjust(bottom=0.25)
        if self.relative_fastest or self.relative_slowest:
            axes.set_ylabel("Speed" if self.inverse_diagram else "Time")
        else:
            axes.set_ylabel("Speed, $s^{-1}$" if self.inverse_diagram else "Time, s")
        if self.log: axes.set_yscale('log')
        plt.show()

def main():
    graphics = Graphics()
    graphics.run()

if __name__ == "__main__":
    main()
