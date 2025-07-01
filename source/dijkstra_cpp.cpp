#include <algorithm>
#include <array>
#include <cctype>
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
#include <type_traits>
#include <utility>
#include <vector>
#include <charconv>

/*
VERSION 1 - naive implementation, graph represented as double map
VERSION 2 - naive implementation, graph represented as vector of maps
VERSION 3 - naive implementation, graph represented as vector of vectors
VERSION 4 - indexed optimization, graph represented as vector of maps
VERSION 5 - indexed optimization, graph represented as vector of vectors
VERSION 6 - indexed optimization, graph represented as vector of lists
VERSION 7 - indexed optimization, graph represented as vector of custom forward lists

PARSING_METHOD 0 - scanf
PARSING_METHOD 1 - strtoul/strtof
PARSING_METHOD 2 - stringstream
PARSING_METHOD 3 - from_chars

STRUCTURE_TRAITS 0 - copy constructible, but not default constructible
STRUCTURE_TRAITS 1 - not copy constructible
STRUCTURE_TRAITS 2 - default constructible, but not copy constructible
*/
#define VERSION 5
#define PARSING_METHOD 3
#define STRUCTURE_TRAITS 0

struct Candidate
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
    constexpr Candidate(unsigned int id, unsigned int int_distance, float distance) noexcept : id(id), int_distance(int_distance), distance(distance) {}
    inline bool constexpr operator<(const Candidate &other) const noexcept { return this->distance < other.distance; }
    inline bool constexpr operator>(const Candidate &other) const noexcept { return this->distance > other.distance; }
    inline bool constexpr operator<=(const Candidate &other) const noexcept { return this->distance <= other.distance; }
    inline bool constexpr operator>=(const Candidate &other) const noexcept { return this->distance >= other.distance; }

    #if STRUCTURE_TRAITS == 1
        Candidate(Candidate&&) = default;
        Candidate& operator=(Candidate&&) = default;
        Candidate(const Candidate&) = delete;
        Candidate& operator=(const Candidate&) = delete;
    #elif STRUCTURE_TRAITS == 2
        Candidate(Candidate&&) = default;
        Candidate& operator=(Candidate&&) = default;
        Candidate() = default;
        Candidate(const Candidate&) = delete;
        Candidate& operator=(const Candidate&) = delete;
    #endif
};

#if VERSION == 4 || VERSION == 5 || VERSION == 6 || VERSION == 7
template<class T, int S> struct indexed_priority_queue_helper //helper for general case
{
    inline static void push(std::vector<T> &, const T &) { }
    inline static void push_or_assign(std::vector<T> &data, size_t index, T &&element)
    {
        if (index == data.size()) data.push_back(std::move(element));
        else data[index] = std::move(element);
    }
};
template<class T> struct indexed_priority_queue_helper<T, 1> //helper copy constructible T
{
    inline static void push(std::vector<T> &data, const T &element)
    {
        data.push_back(element);
    }
    inline static void push_or_assign(std::vector<T> &data, size_t index, T &&element)
    {
        data[index] = std::move(element);
    }
};
template<class T> struct indexed_priority_queue_helper<T, 2> //helper default constructible T
{
    inline static void push(std::vector<T> &data, const T &)
    {
        data.push_back(T());
    }
    inline static void push_or_assign(std::vector<T> &data, size_t index, T &&element)
    {
        data[index] = std::move(element);
    }
};

template<typename T> class indexed_priority_queue
{
private:
    std::vector<T> data;
    std::vector<unsigned int> indices;

    typedef indexed_priority_queue_helper<T, std::is_default_constructible<T>::value ? 2 : (std::is_copy_constructible<T>::value ? 1 : 0)> default_helper;

public:
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

    inline T &top()
    {
        return data.front();
    }

    inline void pop()
    {
        indices[data.front().id] = static_cast<unsigned int>(-2);
        T back = std::move(data.back());
        data.pop_back();
        if (data.empty()) return; //If the front is the back, the algorithm no longer works
        
        unsigned int index = 0;
        while (true)
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
                    if (data[left_index] < data[right_index])
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

                if (data[next_index] < back)
                {
                    data[index] = std::move(data[next_index]);
                    indices[data[index].id] = index;
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data[index] = std::move(back);
                indices[back.id] = index;
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
            default_helper::push(data, element); //allocating space if T is default or copy constructible
        }
        else if (index == static_cast<unsigned int>(-2))
        {
            return;
        }
        else
        {
            if (element >= data[index]) return;
        }
        
        while (true)
        {
            const bool parent_exists = index > 0;
            bool index_moved = false;
            if (parent_exists)
            {
                const unsigned int parent_index = (index - 1) / 2;
                if (element < data[parent_index])
                {
                    default_helper::push_or_assign(data, index, std::move(data[parent_index]));
                    indices[data[index].id] = index;
                    index = parent_index;
                    index_moved = true;
                }
            }
            if (!index_moved)
            {
                default_helper::push_or_assign(data, index, std::move(element));
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

#if PARSING_METHOD == 1
inline constexpr char* skip_spaces(char *p)
{
    for (; *p != '\0' && std::isspace(*p); p++) {}
    return p;
}
#else
inline constexpr const char* skip_spaces(const char *p, const char *end)
{
    for (; p < end && std::isspace(*p); p++) {}
    return p;
}
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
    string.reserve(128);
    while (std::getline(file, string))
    {
        if (string.find("GRAPH") != std::string::npos) { read_benchmarks = false; continue; }
        if (string.find("BENCHMARK") != std::string::npos) { read_benchmarks = true; continue; }
        if (read_benchmarks)
        {
            unsigned int source, destination;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%1s", &source, &destination, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 2) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%f%1s", &source, &destination, &distance, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 3) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                distance = strtof(p, &p);
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                stream >> distance;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, distance);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
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
        if (read_benchmarks)
        {
            unsigned int source, destination;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%1s", &source, &destination, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 2) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%f%1s", &source, &destination, &distance, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 3) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                distance = strtof(p, &p);
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                stream >> distance;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, distance);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
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
        if (read_benchmarks)
        {
            unsigned int source, destination;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%1s", &source, &destination, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 2) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
            benchmarks.push_back({ source, destination });
        }
        else
        {
            unsigned int source, destination;
            float distance;
            #if PARSING_METHOD == 0
                char endline[2];
                const int result = sscanf(string.c_str(), "%u%u%f%1s", &source, &destination, &distance, endline);
                if (result == EOF) continue; //Whitespace
                if (result != 3) break; //Error
            #elif PARSING_METHOD == 1
                char *p = skip_spaces(&string[0]);
                if (*p == '\0') continue; //Whitespace
                source = static_cast<unsigned int>(strtoul(p, &p, 10));
                destination = static_cast<unsigned int>(strtoul(p, &p, 10));
                distance = strtof(p, &p);
                if (*p != '\0' && !std::isspace(*p)) break; //Error
            #elif PARSING_METHOD == 2
                std::istringstream stream(string);
                stream >> source;
                if (!stream) { if (stream.eof()) continue; else break; } //Continue if whitespace, break if error
                stream >> destination;
                stream >> distance;
                if (!stream) break; //Error
                char endline;
                stream >> std::skipws >> endline;
                if (stream) break; //Error
            #else
                const char *end = string.c_str() + string.size();
                std::from_chars_result result;
                result.ptr = skip_spaces(string.c_str(), end);
                if (result.ptr == end) continue; //Whitespace
                result = std::from_chars(result.ptr, end, source);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, destination);
                if (result.ec != std::errc()) break; //Error
                result = std::from_chars(skip_spaces(result.ptr, end), end, distance);
                if (result.ec != std::errc()) break; //Error
                result.ptr = skip_spaces(result.ptr, end);
                if (result.ptr != end) break; //Error
            #endif
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
            Candidate candidate = std::move(candidates.top());
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
