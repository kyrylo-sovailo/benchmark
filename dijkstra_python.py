#!/usr/bin/python3
import heapq
import sys
from typing import List, Tuple

def parse():
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
                weight = float(split[2])

                if source in graph: source_dict = graph[source]
                else: source_dict = graph[source] = dict()
                if destination in graph: destination_dict = graph[destination]
                else: destination_dict = graph[destination] = dict()

                source_dict[destination] = weight
                destination_dict[source] = weight
        return graph, benchmarks

def solve(graph, benchmarks):
    class Candidate:
        def __init__(self, node, int_distance, distance):
            self.node = node
            self.int_distance = int_distance
            self.distance = distance

        def __lt__(self, other):
            return self.distance < other.distance

    for source, destination in benchmarks:
        candidates = []
        explored = set()
        heapq.heappush(candidates, Candidate(source, 0, 0.0))
        int_distance = 0
        distance = float('inf')

        while candidates and distance == float('inf'):
            candidate = heapq.heappop(candidates)
            if candidate.node == destination:
                int_distance = candidate.int_distance
                distance = candidate.distance
                break
            if candidate.node in explored: continue
            explored.add(candidate.node)
            for neighbor, weight in graph[candidate.node].items():
                if destination in explored: continue
                heapq.heappush(candidates, Candidate(neighbor, candidate.int_distance + 1, candidate.distance + weight))

        print(source, destination, int_distance, distance)

def main():
    graph, benchmarks = parse()
    solve(graph, benchmarks)

if __name__ == "__main__":
    try:
        main()
    except Exception as e:
        print(str(e), file=sys.stderr)
        sys.exit(1)