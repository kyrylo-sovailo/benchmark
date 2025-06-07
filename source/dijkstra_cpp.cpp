#include <algorithm>
#include <array>
#include <fstream>
#include <iostream>
#include <limits>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <utility>
#include <vector>

/*
VERSION 1 - naive implementation, graph represented as double map
VERSION 2 - naive implementation, graph represented as vector of maps
VERSION 3 - naive implementation, graph represented as vector of vectors
VERSION 4 - indexed optimization, graph represented as vector of maps
VERSION 5 - indexed optimization, graph represented as vector of vectors
VERSION 6 - indexed optimization, graph represented as vector of lists
VERSION 7 - indexed optimization, graph represented as vector of custom forward lists
*/
#define VERSION 5

struct Candidate
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
    constexpr Candidate(unsigned int id, unsigned int int_distance, float distance) noexcept : id(id), int_distance(int_distance), distance(distance) {}
    inline bool constexpr operator<(const Candidate &other) const noexcept { return this->distance < other.distance; }
    inline bool constexpr operator>(const Candidate &other) const noexcept { return this->distance > other.distance; }
};

#if VERSION == 4 || VERSION == 5 || VERSION == 6 || VERSION == 7
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
        if (data.size() == 1) { data.pop_back(); return; }
        unsigned int index = 0;

        for (;;)
        {
            const unsigned int left_index = 2 * index + 1;
            const unsigned int right_index = 2 * index + 2;
            const bool left_exists = left_index < data.size();
            const bool right_exists = right_index < data.size();

            bool index_moved = false;
            if (left_exists || right_exists)
            {
                unsigned int next_index;
                if (left_exists && right_exists)
                {
                    if (data[left_index].distance < data[right_index].distance)
                    {
                        next_index = left_index;
                    }
                    else
                    {
                        next_index = right_index;
                    }
                }
                else
                {
                    next_index = left_index;
                }

                if (data[next_index].distance < data.back().distance)
                {
                    data[index] = data[next_index];
                    indices[data[index].id] = index;
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data[index] = data.back();
                indices[data.back().id] = index;
                data.pop_back();
                break;
            }
        }
    }

    inline void push(T &&element)
    {
        unsigned int index = indices[element.id];
        if (index == static_cast<unsigned int>(-1))
        {
            index = static_cast<unsigned int>(data.size());
            data.push_back(element); //allocating space, element does not go to the back
        }
        else if (index == static_cast<unsigned int>(-2))
        {
            return;
        }
        else
        {
            if (element.distance >= data[index].distance) return;
        }
        
        while (true)
        {
            const bool parent_exists = index > 0;
            bool index_moved = false;
            if (parent_exists)
            {
                const unsigned int parent_i = (index - 1) / 2;
                if (element < data[parent_i])
                {
                    data[index] = data[parent_i];
                    indices[data[index].id] = index;
                    index = parent_i;
                    index_moved = true;
                }
            }
            if (!index_moved)
            {
                data[index] = element;
                indices[element.id] = index;
                break;
            }
        }
    }

    inline constexpr bool empty() const
    {
        return data.empty();
    };
};
#endif

#if VERSION == 7
template<class T> class reverse_forward_list
{
public:
    struct Link
    {
        T t;
        Link *next;
    };

    struct iterator
    {
        Link *link;
        inline const iterator operator++() { link = link->next; return *this; };
        inline const iterator operator++(int) { iterator copy = *this; link = link->next; return copy; };
        inline const T *operator->() { return &link->t; };
        inline const T &operator*() { return link->t; };
        inline bool operator==(const iterator &other) const { return link == other.link; };
        inline bool operator!=(const iterator &other) const { return link != other.link; };
    };

private:
    Link *_first = nullptr;
    Link *_last = nullptr;

public:
    inline void push_back(const T &element)
    {
        Link *new_link = new Link{element, nullptr};
	if (_first == nullptr) { _first = new_link; _last = new_link; }
        else { _last->next = new_link; _last = new_link; }
    };
    inline iterator begin() const { return { _first }; }
    inline iterator end() const { return { nullptr }; }
    inline const iterator cbegin() const { return { _first }; }
    inline const iterator cend() const { return { nullptr }; }
};
#endif

#if VERSION == 1
std::pair<std::map<unsigned int, std::map<unsigned int, float>>, std::vector<std::pair<unsigned int, unsigned int>>> parse_ver1()
{
    std::map<unsigned int, std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    
    bool read_benchmarks = false;
    std::string string;
    while (std::getline(file, string))
    {
        if (string.find("GRAPH") != std::string::npos) { read_benchmarks = false; continue; }
        if (string.find("BENCHMARK") != std::string::npos) { read_benchmarks = true; continue; }
        std::istringstream stream(string);
        if (read_benchmarks)
        {
            unsigned int source, destination;
            stream >> source >> destination;
            if (stream.bad()) break;
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            stream >> source >> destination >> distance;
            if (stream.bad()) break;
            auto source_connections = graph.find(source);
            if (source_connections == graph.cend()) source_connections = graph.insert({ source, std::map<unsigned int, float>() }).first;
            source_connections->second.insert({ destination, distance });
            auto destination_connections = graph.find(destination);
            if (destination_connections == graph.cend()) destination_connections = graph.insert({ destination, std::map<unsigned int, float>() }).first;
            destination_connections->second.insert({ source, distance });
        }
    }
    return { graph, benchmarks };
}
#endif

#if VERSION == 2 || VERSION == 4
std::pair<std::vector<std::map<unsigned int, float>>, std::vector<std::pair<unsigned int, unsigned int>>> parse_ver2()
{
    std::vector<std::map<unsigned int, float>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");
    
    bool read_benchmarks = false;
    std::string string;
    while (std::getline(file, string))
    {
        if (string.find("GRAPH") != std::string::npos) { read_benchmarks = false; continue; }
        if (string.find("BENCHMARK") != std::string::npos) { read_benchmarks = true; continue; }
        std::istringstream stream(string);
        if (read_benchmarks)
        {
            unsigned int source, destination;
            stream >> source >> destination;
            if (stream.bad()) break;
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            stream >> source >> destination >> distance;
            if (stream.bad()) break;
            if (std::max(source, destination) >= graph.size()) graph.resize(std::max(source, destination) + 1);
            graph[source].insert({ destination, distance });
            graph[destination].insert({ source, distance });
        }
    }
    return { graph, benchmarks };
}
#endif

#if VERSION == 3 || VERSION == 5
std::pair<std::vector<std::vector<std::pair<unsigned int, float>>>, std::vector<std::pair<unsigned int, unsigned int>>> parse_ver3()
{
    std::vector<std::vector<std::pair<unsigned int, float>>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
#elif VERSION == 6
std::pair<std::vector<std::list<std::pair<unsigned int, float>>>, std::vector<std::pair<unsigned int, unsigned int>>> parse_ver3()
{
    std::vector<std::list<std::pair<unsigned int, float>>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
#elif VERSION == 7
std::pair<std::vector<reverse_forward_list<std::pair<unsigned int, float>>>, std::vector<std::pair<unsigned int, unsigned int>>> parse_ver3()
{
    std::vector<reverse_forward_list<std::pair<unsigned int, float>>> graph;
    std::vector<std::pair<unsigned int, unsigned int>> benchmarks;
#endif
#if VERSION == 3 || VERSION == 5 || VERSION == 6 || VERSION == 7
    std::ifstream file("dijkstra.txt");
    if (!file.is_open()) throw std::runtime_error("std::ifstream::ifstream() failed");

    bool read_benchmarks = false;
    std::string string;
    while (std::getline(file, string))
    {
        if (string.find("GRAPH") != std::string::npos) { read_benchmarks = false; continue; }
        if (string.find("BENCHMARK") != std::string::npos) { read_benchmarks = true; continue; }
        std::istringstream stream(string);
        if (read_benchmarks)
        {
            unsigned int source, destination;
            stream >> source >> destination;
            if (stream.bad()) break;
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            stream >> source >> destination >> distance;
            if (stream.bad()) break;
            if (std::max(source, destination) >= graph.size()) graph.resize(std::max(source, destination) + 1);
            graph[source].push_back({ destination, distance });
            graph[destination].push_back({ source, distance });
        }
    }
    return { graph, benchmarks };
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
        float distance = std::numeric_limits<float>::infinity();
        unsigned int int_distance = 0;
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
        std::cout << source << " -> " << destination << ": " << distance << " (" << int_distance << ")\n";
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
        float distance = std::numeric_limits<float>::infinity();
        unsigned int int_distance = 0;
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
        std::cout << source << " -> " << destination << ": " << distance << " (" << int_distance << ")\n";
    }
}
#endif

#if VERSION == 3
void solve_ver3(const std::vector<std::vector<std::pair<unsigned int, float>>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
{
    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        std::priority_queue<Candidate, std::vector<Candidate>, std::greater<Candidate>> candidates;
        std::vector<bool> explored(graph.size());
        candidates.push({ source, 0, 0.0 });
        float distance = std::numeric_limits<float>::infinity();
        unsigned int int_distance = 0;
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
        std::cout << source << " -> " << destination << ": " << distance << " (" << int_distance << ")\n";
    }
}
#endif

#if VERSION == 4
void solve_ver4(const std::vector<std::map<unsigned int, float>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
#elif VERSION == 5
void solve_ver5(const std::vector<std::vector<std::pair<unsigned int, float>>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
#elif VERSION == 6
void solve_ver6(const std::vector<std::list<std::pair<unsigned int, float>>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
#elif VERSION == 7
void solve_ver7(const std::vector<reverse_forward_list<std::pair<unsigned int, float>>> &graph, const std::vector<std::pair<unsigned int, unsigned int>> &benchmarks)
#endif
#if VERSION == 4 || VERSION == 5 || VERSION == 6 || VERSION == 7
{
    indexed_priority_queue<Candidate> candidates(graph.size());

    for (const auto &benchmark : benchmarks)
    {
        const unsigned int source = benchmark.first;
        const unsigned int destination = benchmark.second;
        candidates.reset();
        candidates.push({ source, 0, 0.0 });
        float distance = std::numeric_limits<float>::infinity();
        unsigned int int_distance = 0;
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
        std::cout << source << " -> " << destination << ": " << distance << " (" << int_distance << ")\n";
    }
}
#endif

#if VERSION == 1
int main_ver1()
{
    auto graph_and_benchmarks = parse_ver1();
    solve_ver1(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 2
int main_ver2()
{
    auto graph_and_benchmarks = parse_ver2();
    solve_ver2(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 3
int main_ver3()
{
    auto graph_and_benchmarks = parse_ver3();
    solve_ver3(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 4
int main_ver4()
{
    auto graph_and_benchmarks = parse_ver2();
    solve_ver4(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 5
int main_ver5()
{
    auto graph_and_benchmarks = parse_ver3();
    solve_ver5(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 6
int main_ver6()
{
    auto graph_and_benchmarks = parse_ver3();
    solve_ver6(graph_and_benchmarks.first, graph_and_benchmarks.second);
    return 0;
}
#endif

#if VERSION == 7
int main_ver7()
{
    auto graph_and_benchmarks = parse_ver3();
    solve_ver7(graph_and_benchmarks.first, graph_and_benchmarks.second);
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
        #elif VERSION == 5
            return main_ver5();
        #elif VERSION == 6
            return main_ver6();
        #elif VERSION == 7
            return main_ver7();
        #endif
    }
    catch (std::exception &e)
    {
        std::cerr << e.what() << std::endl;
        return 1;
    }
}
