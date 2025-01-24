#!/usr/bin/python3
import heapq
import sys
from typing import List, Tuple

class Candidate:
    def __init__(self, id, int_distance, distance):
        self.id = id
        self.int_distance = int_distance
        self.distance = distance

    def __lt__(self, other):
        return self.distance < other.distance

def parse_ver1():
    with open("dijkstra.txt", 'r') as file:
        graph = dict()
        benchmarks = []
        read_benchmarks = False
        for line in file:
            if line == "GRAPH\n":
                read_benchmarks = False
                continue
            if line == "BENCHMARK\n":
                read_benchmarks = True
                continue
            split = line.split()
            if read_benchmarks:
                source = int(split[0])
                destination = int(split[1])
                benchmarks.append((source, destination))
            else:
                source = int(split[0])
                destination = int(split[1])
                distance = float(split[2])

                if source in graph: source_dict = graph[source]
                else: source_dict = graph[source] = dict()
                if destination in graph: destination_dict = graph[destination]
                else: destination_dict = graph[destination] = dict()

                source_dict[destination] = distance
                destination_dict[source] = distance
        return graph, benchmarks

def parse_ver2():
    with open("dijkstra.txt", 'r') as file:
        graph = []
        benchmarks = []
        read_benchmarks = False
        for line in file:
            if line == "GRAPH\n":
                read_benchmarks = False
                continue
            if line == "BENCHMARK\n":
                read_benchmarks = True
                continue
            split = line.split()
            if read_benchmarks:
                source = int(split[0])
                destination = int(split[1])
                benchmarks.append((source, destination))
            else:
                source = int(split[0])
                destination = int(split[1])
                distance = float(split[2])

                extension = max(source, destination) - len(graph) + 1
                if extension > 0: graph += [ dict() for _ in range(extension) ]

                graph[source][destination] = distance
                graph[destination][source] = distance
        return graph, benchmarks

def parse_ver4():
    with open("dijkstra.txt", 'r') as file:
        graph = []
        benchmarks = []
        read_benchmarks = False
        for line in file:
            if line == "GRAPH\n":
                read_benchmarks = False
                continue
            if line == "BENCHMARK\n":
                read_benchmarks = True
                continue
            split = line.split()
            if read_benchmarks:
                source = int(split[0])
                destination = int(split[1])
                benchmarks.append((source, destination))
            else:
                source = int(split[0])
                destination = int(split[1])
                distance = float(split[2])

                extension = max(source, destination) - len(graph) + 1
                if extension > 0: graph += [ [] for _ in range(extension) ]

                graph[source].append((destination, distance))
                graph[destination].append((source, distance))
        return graph, benchmarks

def solve_ver1(graph, benchmarks):
    for source, destination in benchmarks:
        candidates = []
        explored = set()
        heapq.heappush(candidates, Candidate(source, 0, 0.0))
        int_distance = 0
        distance = float('inf')

        while candidates:
            candidate = heapq.heappop(candidates)
            if candidate.id == destination:
                int_distance = candidate.int_distance
                distance = candidate.distance
                break
            if candidate.id in explored: continue
            explored.add(candidate.id)
            for neighbor, neighbor_distance in graph[candidate.id].items():
                if destination in explored: continue
                heapq.heappush(candidates, Candidate(neighbor, candidate.int_distance + 1, candidate.distance + neighbor_distance))

        print(source, destination, int_distance, distance)

def solve_ver2(graph, benchmarks):
    for source, destination in benchmarks:
        candidates = []
        explored = [ False for _ in range(len(graph)) ]
        heapq.heappush(candidates, Candidate(source, 0, 0.0))
        int_distance = 0
        distance = float('inf')

        while candidates:
            candidate = heapq.heappop(candidates)
            if candidate.id == destination:
                int_distance = candidate.int_distance
                distance = candidate.distance
                break
            if explored[candidate.id]: continue
            explored[candidate.id] = True
            for neighbor, neighbor_distance in graph[candidate.id].items():
                if explored[destination]: continue
                heapq.heappush(candidates, Candidate(neighbor, candidate.int_distance + 1, candidate.distance + neighbor_distance))

        print(source, destination, int_distance, distance)

def solve_ver4(graph, benchmarks):
    for source, destination in benchmarks:
        candidates = []
        explored = [ False for _ in range(len(graph)) ]
        heapq.heappush(candidates, Candidate(source, 0, 0.0))
        int_distance = 0
        distance = float('inf')

        while candidates:
            candidate = heapq.heappop(candidates)
            if candidate.id == destination:
                int_distance = candidate.int_distance
                distance = candidate.distance
                break
            if explored[candidate.id]: continue
            explored[candidate.id] = True
            for neighbor, neighbor_distance in graph[candidate.id]:
                if explored[destination]: continue
                heapq.heappush(candidates, Candidate(neighbor, candidate.int_distance + 1, candidate.distance + neighbor_distance))

        print(source, destination, int_distance, distance)

def main_ver1():
    graph, benchmarks = parse_ver1()
    solve_ver1(graph, benchmarks)

def main_ver2():
    graph, benchmarks = parse_ver2()
    solve_ver2(graph, benchmarks)

def main_ver4():
    graph, benchmarks = parse_ver4()
    solve_ver4(graph, benchmarks)

if __name__ == "__main__":
    try:
        main_ver2()
    except Exception as e:
        print(str(e), file=sys.stderr)
        sys.exit(1)