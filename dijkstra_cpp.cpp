#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <set>
#include <stdexcept>
#include <string>
#include <vector>

void parse(std::map<unsigned int, std::map<unsigned int, float>> *graph, std::vector<std::pair<unsigned int, unsigned int>> *benchmarks)
{
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    std::string string;

    file >> string;
    while (true)
    {
        unsigned int source, destination;
        float weight;
        file >> source >> destination >> weight;
        if (!file.good()) { file.clear(); break; }
        
        auto source_connections = graph->find(source);
        if (source_connections == graph->cend()) source_connections = graph->insert({ source, std::map<unsigned int, float>() }).first;
        source_connections->second.insert({ destination, weight });
        auto destination_connections = graph->find(destination);
        if (destination_connections == graph->cend()) destination_connections = graph->insert({ destination, std::map<unsigned int, float>() }).first;
        destination_connections->second.insert({ source, weight });
    }    

    file >> string;
    while (true)
    {
        unsigned int source, destination;
        file >> source >> destination;
        if (!file.good()) break;

        benchmarks->push_back({ source, destination });
    }
}

void solve(const std::map<unsigned int, std::map<unsigned int, float>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    struct Candidate
    {
        unsigned int node;
        unsigned int int_distance;
        float distance;
        bool operator<(const Candidate &other) const { return this->distance < other.distance; }
        bool operator>(const Candidate &other) const { return this->distance > other.distance; }
        Candidate(unsigned int node, unsigned int int_distance, float distance) : node(node), int_distance(int_distance), distance(distance) {}
    };

    for (const auto &benchmark : benchmarks)
    {
        unsigned int source = benchmark.first;
        unsigned int destination = benchmark.second;
        std::priority_queue<Candidate, std::vector<Candidate>, std::greater<Candidate>> candidates;
        std::set<unsigned int> explored;
        candidates.push({ source, 0, 0.0 });
        unsigned int int_distance = 0;
        float distance = std::numeric_limits<float>::infinity();
        while (!candidates.empty() && distance == std::numeric_limits<float>::infinity())
        {
            Candidate candidate = candidates.top();
            if (candidate.node == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            candidates.pop();
            if (explored.count(candidate.node) > 0) continue;
            explored.insert(candidate.node);
            auto candidate_neighbors = graph.find(candidate.node);

            for (const auto &candidate_neighbor : candidate_neighbors->second)
            {
                if (explored.count(destination) > 0) continue;
                candidates.push({ candidate_neighbor.first, candidate.int_distance + 1, candidate.distance + candidate_neighbor.second });
            }
        }
        std::cout << source << ' ' << destination << ' ' << int_distance << ' ' << distance << '\n';
    }
}

int _main()
{
    std::map<unsigned int, std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    parse(&graph, &benchmarks);
    solve(graph, benchmarks);
    return 0;
}

int main()
{
    try
    {
        return _main();
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
}