#include <exception>
#include <fstream>
#include <iostream>
#include <map>
#include <random>
#include <stdexcept>
#include <string>
#include <vector>

void write(std::ofstream *file, const std::string &string)
{
    if (!file->write(string.c_str(), string.size())) throw std::runtime_error("std::ofstream::write() failed");
}

int _main()
{
    const unsigned int node_count = 100000;
    const unsigned int min_node_connections = 5;
    const unsigned int max_node_connections = 10;
    const float min_weight = 1;
    const float max_weight = 100;
    const unsigned int benchmark_count = 4;

    std::default_random_engine engine(1234);
    std::uniform_int_distribution<unsigned int> node_distribution(0, node_count-1);
    std::uniform_int_distribution<unsigned int> neighbor_distribution(min_node_connections, max_node_connections-1);
    std::uniform_real_distribution<float> weight_distribution(min_weight, max_weight);
    std::map<unsigned int, std::map<unsigned int, float>> graph;
    for (unsigned int source = 0; source < node_count; source++)
    {
        unsigned int neighbors_count = neighbor_distribution(engine);
        for (unsigned int i = 0; i < neighbors_count; i++)
        {
            unsigned int destination = source;
            while (destination == source) destination = node_distribution(engine);
            float weight = weight_distribution(engine);
            auto source_connections = graph.find(source);
            if (source_connections == graph.cend()) source_connections = graph.insert({ source, std::map<unsigned int, float>() }).first;
            source_connections->second.insert({ destination, weight });
            auto destination_connections = graph.find(destination);
            if (destination_connections == graph.cend()) destination_connections = graph.insert({ destination, std::map<unsigned int, float>() }).first;
            destination_connections->second.insert({ source, weight });
        }
    }

    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    benchmarks.reserve(benchmark_count);
    for (unsigned int benchmark = 0; benchmark < benchmark_count; benchmark++)
    {
        unsigned int source = node_distribution(engine);
        unsigned int destination = source;
        while (destination == source) destination = node_distribution(engine);
        benchmarks.push_back({ source, destination });
    }

    std::ofstream file("dijkstra.txt");
    write(&file, "GRAPH\n");
    for (const auto &node1 : graph)
    {
        for (const auto &node2 : node1.second)
        {
            if (node1.first > node2.first) continue;
            write(&file, std::to_string(node1.first));
            write(&file, " ");
            write(&file, std::to_string(node2.first));
            write(&file, " ");
            write(&file, std::to_string(node2.second));
            write(&file, "\n");
        }
    }
    write(&file, "BENCHMARK\n");
    for (const auto &benchmark : benchmarks)
    {
        write(&file, std::to_string(benchmark.first));
        write(&file, " ");
        write(&file, std::to_string(benchmark.second));
        write(&file, "\n");
    }

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