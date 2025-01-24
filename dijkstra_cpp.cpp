#include <algorithm>
#include <fstream>
#include <iostream>
#include <limits>
#include <map>
#include <queue>
#include <set>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

#define VERSION 4

struct Candidate
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
    constexpr Candidate(unsigned int id, unsigned int int_distance, float distance) : id(id), int_distance(int_distance), distance(distance) {}
    inline bool constexpr operator<(const Candidate &other) const { return this->distance < other.distance; }
    inline bool constexpr operator>(const Candidate &other) const { return this->distance > other.distance; }
};

#if VERSION == 3 || VERSION == 4
template<typename T> struct indexed_priority_queue
{
    std::vector<T> data;
    std::vector<unsigned int> indices;

    inline indexed_priority_queue(size_t size)
    {
        data.reserve(size);
        indices.resize(size);
    }

    inline void reset()
    {
        data.clear();
        for (auto &index : indices) index = static_cast<unsigned int>(-1);
    }

    inline constexpr const T &top() const
    {
        return data.front();
    }

    inline void pop()
    {
        indices[data.front().id] = static_cast<unsigned int>(-2);
        indices[data.back().id] = 0;
        data.front() = data.back();
        data.pop_back();

        unsigned int i = 0;
        while (true)
        {
            const unsigned int left_i = 2 * i + 1;
            const unsigned int right_i = 2 * i + 2;
            const bool left_exists = left_i < data.size();
            const bool right_exists = right_i < data.size();
            if (/*left_exists &&*/ right_exists)
            {
                if (data[left_i] < data[right_i])
                {
                    if (data[left_i] < data[i]) { std::swap(indices[data[i].id], indices[data[left_i].id]); std::swap(data[i], data[left_i]); i = left_i; }
                    else break;
                }
                else
                {
                    if (data[right_i] < data[i]) { std::swap(indices[data[i].id], indices[data[right_i].id]); std::swap(data[i], data[right_i]); i = right_i; }
                    else break;
                }
            }
            else if (left_exists /*&& !right_exists*/)
            {
                if (data[left_i] < data[i]) { std::swap(indices[data[i].id], indices[data[left_i].id]); std::swap(data[i], data[left_i]); i = left_i; }
                else break;
            }
            else
            {
                break;
            }
        }
    }

    inline void push(T &&element)
    {
        unsigned int i = indices[element.id];
        if (i == static_cast<unsigned int>(-1))
        {
            i = data.size();
            indices[element.id] = i;
            data.push_back(element);
        }
        else if (i == static_cast<unsigned int>(-2))
        {
            return;
        }
        else
        {
            if (element < data[i]) data[i] = element;
            else return;
        }
        while (i > 0)
        {
            const unsigned int parent_i = (i - 1) / 2;
            if (data[i] < data[parent_i]) { std::swap(indices[data[i].id], indices[data[parent_i].id]); std::swap(data[i], data[parent_i]); i = parent_i; }
            else break;
        }
    }

    inline constexpr bool empty() const
    {
        return data.empty();
    };
};
#endif

#if VERSION == 1
void parse_ver1(std::map<unsigned int, std::map<unsigned int, float>> *graph, std::vector<std::pair<unsigned int, unsigned int>> *benchmarks)
{
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    std::string string;

    file >> string;
    while (true)
    {
        unsigned int source, destination;
        float distance;
        file >> source >> destination >> distance;
        if (!file.good()) { file.clear(); break; }
        
        auto source_connections = graph->find(source);
        if (source_connections == graph->cend()) source_connections = graph->insert({ source, std::map<unsigned int, float>() }).first;
        source_connections->second.insert({ destination, distance });
        auto destination_connections = graph->find(destination);
        if (destination_connections == graph->cend()) destination_connections = graph->insert({ destination, std::map<unsigned int, float>() }).first;
        destination_connections->second.insert({ source, distance });
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
#endif

#if VERSION == 2 || VERSION == 3
void parse_ver2(std::vector<std::map<unsigned int, float>> *graph, std::vector<std::pair<unsigned int, unsigned int>> *benchmarks)
{
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    std::string string;

    file >> string;
    while (true)
    {
        unsigned int source, destination;
        float distance;
        file >> source >> destination >> distance;
        if (!file.good()) { file.clear(); break; }
        if (std::max(source, destination) >= graph->size()) graph->resize(std::max(source, destination) + 1);
        
        (*graph)[source].insert({ destination, distance });
        (*graph)[destination].insert({ source, distance });
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
#endif

#if VERSION == 4
void parse_ver4(std::vector<std::vector<std::pair<unsigned int, float>>> *graph, std::vector<std::pair<unsigned int, unsigned int>> *benchmarks)
{
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    std::string string;

    file >> string;
    while (true)
    {
        unsigned int source, destination;
        float distance;
        file >> source >> destination >> distance;
        if (!file.good()) { file.clear(); break; }
        if (std::max(source, destination) >= graph->size()) graph->resize(std::max(source, destination) + 1);
        
        (*graph)[source].push_back({ destination, distance });
        (*graph)[destination].push_back({ source, distance });
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
#endif

#if VERSION == 1
void solve_ver1(const std::map<unsigned int, std::map<unsigned int, float>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        std::priority_queue<Candidate, std::vector<Candidate>, std::greater<Candidate>> candidates;
        std::set<unsigned int> explored;
        candidates.push({ source, 0, 0.0 });
        unsigned int int_distance = 0;
        float distance = std::numeric_limits<float>::infinity();
        while (!candidates.empty())
        {
            Candidate candidate = candidates.top();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            candidates.pop();
            if (explored.count(candidate.id) > 0) continue;
            explored.insert(candidate.id);
            const auto connections = graph.find(candidate.id);

            for (const auto &connection : connections->second)
            {
                if (explored.count(destination) > 0) continue;
                candidates.push({ connection.first, candidate.int_distance + 1, candidate.distance + connection.second });
            }
        }
        std::cout << source << ' ' << destination << ' ' << int_distance << ' ' << distance << '\n';
    }
}
#endif

#if VERSION == 2
void solve_ver2(const std::vector<std::map<unsigned int, float>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        std::priority_queue<Candidate, std::vector<Candidate>, std::greater<Candidate>> candidates;
        std::vector<bool> explored(graph.size());
        candidates.push({ source, 0, 0.0 });
        unsigned int int_distance = 0;
        float distance = std::numeric_limits<float>::infinity();
        while (!candidates.empty())
        {
            Candidate candidate = candidates.top();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            candidates.pop();
            if (explored[candidate.id]) continue;
            explored[candidate.id] = true;
            const auto &connections = graph[candidate.id];

            for (const auto &connection : connections)
            {
                if (explored[destination]) continue;
                candidates.push({ connection.first, candidate.int_distance + 1, candidate.distance + connection.second });
            }
        }
        std::cout << source << ' ' << destination << ' ' << int_distance << ' ' << distance << '\n';
    }
}
#endif

#if VERSION == 3
void solve_ver3(const std::vector<std::map<unsigned int, float>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    indexed_priority_queue<Candidate> candidates(graph.size());

    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        candidates.reset();
        candidates.push({ source, 0, 0.0 });
        unsigned int int_distance = 0;
        float distance = std::numeric_limits<float>::infinity();
        while (!candidates.empty())
        {
            Candidate candidate = candidates.top();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            candidates.pop();
            const auto &connections = graph[candidate.id];

            for (const auto &connection : connections)
            {
                candidates.push({ connection.first, candidate.int_distance + 1, candidate.distance + connection.second });
            }
        }
        std::cout << source << ' ' << destination << ' ' << int_distance << ' ' << distance << '\n';
    }
}
#endif

#if VERSION == 4
void solve_ver4(const std::vector<std::vector<std::pair<unsigned int, float>>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    indexed_priority_queue<Candidate> candidates(graph.size());

    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        candidates.reset();
        candidates.push({ source, 0, 0.0 });
        unsigned int int_distance = 0;
        float distance = std::numeric_limits<float>::infinity();
        while (!candidates.empty())
        {
            Candidate candidate = candidates.top();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            candidates.pop();
            const auto &connections = graph[candidate.id];

            for (const auto &connection : connections)
            {
                candidates.push({ connection.first, candidate.int_distance + 1, candidate.distance + connection.second });
            }
        }
        std::cout << source << ' ' << destination << ' ' << int_distance << ' ' << distance << '\n';
    }
}
#endif

#if VERSION == 1
int main_ver1()
{
    std::map<unsigned int, std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    parse_ver1(&graph, &benchmarks);
    solve_ver1(graph, benchmarks);
    return 0;
}
#endif

#if VERSION == 2
int main_ver2()
{
    std::vector<std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    parse_ver2(&graph, &benchmarks);
    solve_ver2(graph, benchmarks);
    return 0;
}
#endif

#if VERSION == 3
int main_ver3()
{
    std::vector<std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    parse_ver2(&graph, &benchmarks);
    solve_ver3(graph, benchmarks);
    return 0;
}
#endif

#if VERSION == 4
int main_ver4()
{
    std::vector<std::vector<std::pair<unsigned int, float>>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    parse_ver4(&graph, &benchmarks);
    solve_ver4(graph, benchmarks);
    return 0;
}
#endif

int main()
{
    try
    {
        #if VERSION == 1
            return main_ver1();
        #elif VERSION == 2
            return main_ver2();
        #elif VERSION == 3
            return main_ver3();
        #elif VERSION == 4
            return main_ver4();
        #endif
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
}