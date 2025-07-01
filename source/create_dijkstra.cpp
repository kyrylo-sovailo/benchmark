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
    if (!file->write(string.c_str(), static_cast<std::streamsize>(string.size()))) throw std::runtime_error("std::ofstream::write() failed");
}

void write_separator(std::ofstream *file, std::uniform_int_distribution<unsigned int> *distribution, std::default_random_engine *engine, bool required)
{
    if (required) write(file, " ");
    while ((*distribution)(*engine) == 0)
    {
        write(file, ((*distribution)(*engine) == 0) ? "\t" : " ");
    }
}

void write_newline(std::ofstream *file, std::uniform_int_distribution<unsigned int> *distribution, std::default_random_engine *engine, bool required)
{
    if (required) write(file, "\n");
    while ((*distribution)(*engine) == 0)
    {
        write(file, "\n");
    }
}

int _main()
{
    const unsigned int node_count = 100000;
    const unsigned int min_node_connections = 5;
    const unsigned int max_node_connections = 10;
    const float min_weight = 1;
    const float max_weight = 100;
    const unsigned int benchmark_count = 20;

    std::default_random_engine engine(1234);
    std::uniform_int_distribution<unsigned int> node_distribution(0, node_count-1);
    std::uniform_int_distribution<unsigned int> neighbor_distribution(min_node_connections, max_node_connections-1);
    std::uniform_real_distribution<float> weight_distribution(min_weight, max_weight);
    std::uniform_int_distribution<unsigned int> chaos_distribution(0, 10);
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
    write_separator(&file, &chaos_distribution, &engine, true);
    write(&file, "GRAPH");
    write_separator(&file, &chaos_distribution, &engine, true);
    write_newline(&file, &chaos_distribution, &engine, true);
    for (const auto &node1 : graph)
    {
        for (const auto &node2 : node1.second)
        {
            if (node1.first > node2.first) continue;
            write_separator(&file, &chaos_distribution, &engine, false);
            write(&file, std::to_string(node1.first));
            write_separator(&file, &chaos_distribution, &engine, true);
            write(&file, std::to_string(node2.first));
            write_separator(&file, &chaos_distribution, &engine, true);
            write(&file, std::to_string(node2.second));
            write_separator(&file, &chaos_distribution, &engine, false);
            write_newline(&file, &chaos_distribution, &engine, true);
        }
    }
    write_separator(&file, &chaos_distribution, &engine, true);
    write(&file, "BENCHMARK");
    write_separator(&file, &chaos_distribution, &engine, true);
    write_newline(&file, &chaos_distribution, &engine, true);
    for (const auto &benchmark : benchmarks)
    {
        write_separator(&file, &chaos_distribution, &engine, false);
        write(&file, std::to_string(benchmark.first));
        write_separator(&file, &chaos_distribution, &engine, true);
        write(&file, std::to_string(benchmark.second));
        write_separator(&file, &chaos_distribution, &engine, false);
        write_newline(&file, &chaos_distribution, &engine, true);
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
