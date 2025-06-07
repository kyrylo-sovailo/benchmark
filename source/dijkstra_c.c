#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
VERSION 5 - indexed optimization, graph represented as vector of vectors
VERSION 8 - indexed optimization, allocator optimization, graph represented as vector of pointers
VERSION 9 - indexed optimization, copyless allocator optimization, graph represented as vector of pointers
*/
#define VERSION 5

#define MIN(A, B) (((A) < (B)) ? (A) : (B))
#define MAX(A, B) (((A) > (B)) ? (A) : (B))

#define DECLARE_VECTOR(T) \
typedef struct \
{ \
    T *begin; \
    unsigned int length; \
    unsigned int capacity; \
} T ## Vector;

#define DECLARE_VECTOR_RESERVE(T) \
static void reserve_ ## T ## Vector(T ## Vector *vector, unsigned int capacity) \
{ \
    if (capacity <= vector->capacity) return; \
    vector->capacity = capacity; \
    vector->begin = (T*)realloc(vector->begin, vector->capacity * sizeof(T)); \
    if (vector->begin == NULL) { printf("realloc() failed"); exit(1); } \
}

#define DECLARE_VECTOR_GROW(T) \
static void grow_ ## T ## Vector(T ## Vector *vector, unsigned int length) \
{ \
    if (length <= vector->length) return; \
    if (length > vector->capacity) \
    { \
        if (vector->capacity == 0) vector->capacity = 1; \
        while (vector->capacity < length) vector->capacity = (vector->capacity << 1); \
        vector->begin = (T*)realloc(vector->begin, vector->capacity * sizeof(T)); \
        if (vector->begin == NULL) { printf("realloc() failed"); exit(1); } \
    } \
    memset(vector->begin + vector->length, 0, (length - vector->length) * sizeof(T)); \
    vector->length = length; \
}

#define DECLARE_VECTOR_PUSH(T) \
static void push_ ## T ## Vector(T ## Vector *vector, T item) \
{ \
    vector->length++; \
    if (vector->length > vector->capacity) \
    { \
        if (vector->capacity == 0) vector->capacity = 1; \
        else vector->capacity = vector->capacity << 1; \
        vector->begin = (T*)realloc(vector->begin, vector->capacity * sizeof(T)); \
        if (vector->begin == NULL) { printf("realloc() failed"); exit(2); } \
    } \
    vector->begin[vector->length - 1] = item; \
}

#define DECLARE_VECTOR_UNSAFE_PUSH(T) \
static void unsafe_push_ ## T ## Vector(T ## Vector *vector, T item) \
{ \
    vector->begin[vector->length] = item; \
    vector->length++; \
}

typedef struct
{
    unsigned int source;
    unsigned int destination;
} Benchmark;
DECLARE_VECTOR(Benchmark)
DECLARE_VECTOR_PUSH(Benchmark)

#if VERSION == 5
typedef struct
{
    unsigned int destination;
    float distance;
} Connection;
DECLARE_VECTOR(Connection)
DECLARE_VECTOR_PUSH(Connection)
DECLARE_VECTOR(ConnectionVector)
DECLARE_VECTOR_GROW(ConnectionVector)
#endif

#if VERSION == 8
typedef struct
{
    unsigned int source;
    unsigned int destination;
    float distance;
} UnassignedConnection;
DECLARE_VECTOR(UnassignedConnection)
DECLARE_VECTOR_PUSH(UnassignedConnection)
typedef struct
{
    unsigned int destination;
    float distance;
} Connection;
typedef struct
{
    Connection *begin;
    unsigned int length;
} ConnectionVector;
typedef struct
{
    ConnectionVector *begin;
    unsigned int length;
} ConnectionVectorVector;
#endif

#if VERSION == 9
typedef struct
{
    unsigned int source;
    unsigned int destination;
    float distance;
} UnassignedConnection;
DECLARE_VECTOR(UnassignedConnection)
DECLARE_VECTOR_PUSH(UnassignedConnection)
typedef struct
{
    UnassignedConnection *begin;
    unsigned int length;
} ConnectionVector;
typedef struct
{
    ConnectionVector *begin;
    unsigned int length;
} ConnectionVectorVector;
#endif

typedef struct
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
} Candidate;

static void push_indexed_heap(Candidate *restrict data, unsigned int *restrict length, unsigned int *restrict indices, Candidate element)
{
    unsigned int index = indices[element.id];
    if (index == (unsigned int)-1)
    {
        index = (*length);
        (*length)++;
    }
    else if (index == (unsigned int)-2)
    {
        return;
    }
    else
    {
        if (element.distance >= data[index].distance) return;
    }
    
    for (;;)
    {
        const bool parent_exists = index != 0;
        bool index_moved = 0;
        if (parent_exists)
        {
            const unsigned int parent_index = (index - 1) / 2;
            if (element.distance < data[parent_index].distance)
            {
                data[index] = data[parent_index];
                indices[data[index].id] = index;
                index = parent_index;
                index_moved = 1;
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

static Candidate pop_indexed_heap(Candidate *restrict data, unsigned int *restrict length, unsigned int *restrict indices)
{
    (*length)--;
    const Candidate top = data[0];
    indices[top.id] = (unsigned int)-2;
    if (*length == 0) return top;
    
    const Candidate buffer = data[*length];
    unsigned int index = 0;

    for (;;)
    {
        const unsigned int left_index = 2 * index + 1;
        const unsigned int right_index = 2 * index + 2;
        const bool left_exists = left_index <= *length;
        const bool right_exists = right_index <= *length;

        bool index_moved = 0;
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

            if (data[next_index].distance < buffer.distance)
            {
                data[index] = data[next_index];
                indices[data[index].id] = index;
                index = next_index;
                index_moved = 1;
            }
        }

        if (!index_moved)
        {
            data[index] = buffer;
            indices[buffer.id] = index;
            break;
        }
    }
    return top;
}

#if VERSION == 5
static void parse_ver5(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    FILE *file = fopen("dijkstra.txt", "r");
    if (file == NULL) { printf("fopen() failed"); exit(2); }

    bool read_benchmarks = false;
    unsigned int line_capacity = 128;
    char *line = (char*)malloc(line_capacity);
    while (true)
    {
        unsigned int line_size = 0;
        while (true)
        {
            char c;
            if (fread(&c, 1, 1, file) == 0 || c == '\n' || c == '\r') break;
            if (line_size + 2 > line_capacity) { line_capacity <<= 1; line = (char*)realloc(line, line_capacity); }
            line[line_size] = c;
            line_size++;
        }
        line[line_size] = '\0';
        if (strstr(line, "GRAPH") != NULL) { read_benchmarks = false; continue; }
        if (strstr(line, "BENCHMARK") != NULL) { read_benchmarks = true; continue; }
        if (read_benchmarks)
        {
            unsigned int source, destination;
            if (sscanf(line, "%u %u", &source, &destination) != 2) break;

            Benchmark benchmark = { .source = source, .destination = destination };
            push_BenchmarkVector(benchmarks, benchmark);
        }
        else
        {
            unsigned int source, destination;
            float distance;
            if (sscanf(line, "%u %u %f", &source, &destination, &distance) != 3) break;

            grow_ConnectionVectorVector(graph, MAX(source, destination) + 1);
            Connection forward = { .destination = destination, .distance = distance };
            push_ConnectionVector(&graph->begin[source], forward);
            Connection backward = { .destination = source, .distance = distance };
            push_ConnectionVector(&graph->begin[destination], backward);
        }
    }
    fclose(file);
}
#endif

#if VERSION == 8
static int parse_ver8_sort(const void *a, const void *b)
{
    return ((const UnassignedConnection*)a)->source - ((const UnassignedConnection*)b)->source;
}

static void parse_ver8(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    UnassignedConnectionVector connections; memset(&connections, 0, sizeof(connections));
    unsigned int max_node_id = 0;
    FILE *file = fopen("dijkstra.txt", "r");
    if (file == NULL) { printf("fopen() failed"); exit(2); }

    //Stage 1
    bool read_benchmarks = false;
    unsigned int line_capacity = 128;
    char *line = (char*)malloc(line_capacity);
    while (true)
    {
        unsigned int line_size = 0;
        while (true)
        {
            char c;
            if (fread(&c, 1, 1, file) == 0 || c == '\n' || c == '\r') break;
            if (line_size + 2 > line_capacity) { line_capacity <<= 1; line = (char*)realloc(line, line_capacity); }
            line[line_size] = c;
            line_size++;
        }
        line[line_size] = '\0';
        if (strstr(line, "GRAPH") != NULL) { read_benchmarks = false; continue; }
        if (strstr(line, "BENCHMARK") != NULL) { read_benchmarks = true; continue; }
        if (read_benchmarks)
        {
            unsigned int source, destination;
            if (sscanf(line, "%u %u", &source, &destination) != 2) break;

            Benchmark benchmark = { .source = source, .destination = destination };
            push_BenchmarkVector(benchmarks, benchmark);
        }
        else
        {
            unsigned int source, destination;
            float distance;
            if (sscanf(line, "%u %u %f", &source, &destination, &distance) != 3) break;
            
            UnassignedConnection forward = { .source = source, .destination = destination, .distance = distance };
            push_UnassignedConnectionVector(&connections, forward);
            UnassignedConnection backward = { .source = destination, .destination = source, .distance = distance };
            push_UnassignedConnectionVector(&connections, backward);
            if (source > max_node_id) max_node_id = source;
            if (destination > max_node_id) max_node_id = destination;
        }
    }

    //Stage 2
    qsort(connections.begin, connections.length, sizeof(*connections.begin), parse_ver8_sort);

    //Stage 3
    graph->length = max_node_id + 1;
    graph->begin = (ConnectionVector*)malloc(graph->length * sizeof(*graph->begin));
    for (unsigned int i = 0; i < connections.length;)
    {
        const unsigned int current_id = connections.begin[i].source;
        unsigned int length;
        for (length = 1; i + length < connections.length && connections.begin[i + length].source == current_id; length++) {}
        Connection *begin = (Connection*)malloc(length * sizeof(*begin));
        for (unsigned int length_repeat = 0; length_repeat < length; length_repeat++)
        {
            begin[length_repeat].destination = connections.begin[i + length_repeat].destination;
            begin[length_repeat].distance = connections.begin[i + length_repeat].distance;
        }
        graph->begin[current_id].length = length;
        graph->begin[current_id].begin = begin;
        i += length;
    }

    if (connections.begin != NULL) free(connections.begin);
    fclose(file);
}
#endif

#if VERSION == 9
static int parse_ver9_sort(const void *a, const void *b)
{
    return ((const UnassignedConnection*)a)->source - ((const UnassignedConnection*)b)->source;
}

static void parse_ver9(UnassignedConnectionVector *connections, ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    unsigned int max_node_id = 0;
    FILE *file = fopen("dijkstra.txt", "r");
    if (file == NULL) { printf("fopen() failed"); exit(2); }

    //Stage 1
    bool read_benchmarks = false;
    unsigned int line_capacity = 128;
    char *line = (char*)malloc(line_capacity);
    while (true)
    {
        unsigned int line_size = 0;
        while (true)
        {
            char c;
            if (fread(&c, 1, 1, file) == 0 || c == '\n' || c == '\r') break;
            if (line_size + 2 > line_capacity) { line_capacity <<= 1; line = (char*)realloc(line, line_capacity); }
            line[line_size] = c;
            line_size++;
        }
        line[line_size] = '\0';
        if (strstr(line, "GRAPH") != NULL) { read_benchmarks = false; continue; }
        if (strstr(line, "BENCHMARK") != NULL) { read_benchmarks = true; continue; }
        if (read_benchmarks)
        {
            unsigned int source, destination;
            if (sscanf(line, "%u %u", &source, &destination) != 2) break;

            Benchmark benchmark = { .source = source, .destination = destination };
            push_BenchmarkVector(benchmarks, benchmark);
        }
        else
        {
            unsigned int source, destination;
            float distance;
            if (sscanf(line, "%u %u %f", &source, &destination, &distance) != 3) break;
            
            UnassignedConnection forward = { .source = source, .destination = destination, .distance = distance };
            push_UnassignedConnectionVector(connections, forward);
            UnassignedConnection backward = { .source = destination, .destination = source, .distance = distance };
            push_UnassignedConnectionVector(connections, backward);
            if (source > max_node_id) max_node_id = source;
            if (destination > max_node_id) max_node_id = destination;
        }
    }

    //Stage 2
    qsort(connections->begin, connections->length, sizeof(*connections->begin), parse_ver9_sort);

    //Stage 3
    graph->length = max_node_id + 1;
    graph->begin = (ConnectionVector*)malloc(graph->length * sizeof(*graph->begin));
    for (unsigned int i = 0; i < connections->length;)
    {
        const unsigned int current_id = connections->begin[i].source;
        unsigned int length;
        for (length = 1; i + length < connections->length && connections->begin[i + length].source == current_id; length++) {}
        graph->begin[current_id].length = length;
        graph->begin[current_id].begin = &connections->begin[i];
        i += length;
    }

    fclose(file);
}
#endif

static void solve_ver5(const ConnectionVectorVector *graph, const BenchmarkVector *benchmarks)
{
    Candidate *candidates = (Candidate*)malloc(graph->length * sizeof(Candidate));
    unsigned int candidates_length = 0;
    unsigned int *candidate_indices = (unsigned int*)malloc(graph->length * sizeof(unsigned int));

    for (const Benchmark *benchmark = benchmarks->begin;
        benchmark < &benchmarks->begin[benchmarks->length];
        benchmark++)
    {
        const unsigned int source = benchmark->source;
        const unsigned int destination = benchmark->destination;
        candidates_length = 0;
        memset(candidate_indices, 0xFF, graph->length * sizeof(unsigned int));
        Candidate candidate = { .id = source, .int_distance = 0, .distance = 0.0 };
        push_indexed_heap(candidates, &candidates_length, candidate_indices, candidate);
        while (true)
        {
            if (candidates_length == 0)
            {
                candidate.distance = INFINITY;
                candidate.int_distance = 0;
                break;
            }

            candidate = pop_indexed_heap(candidates, &candidates_length, candidate_indices);
            if (candidate.id == destination) break;
            ConnectionVector *connections = &graph->begin[candidate.id];

            for (const
                #if VERSION == 9
                UnassignedConnection
                #else
                Connection
                #endif
                *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++)
            {
                Candidate new_candidate;
                new_candidate.id = connection->destination;
                new_candidate.distance = candidate.distance + connection->distance;
                new_candidate.int_distance = candidate.int_distance + 1;
                push_indexed_heap(candidates, &candidates_length, candidate_indices, new_candidate);
            }
        }
        printf("%u -> %u: %f (%u) \n", source, destination, candidate.distance, candidate.int_distance);
    }

    free(candidates);
    free(candidate_indices);
}

static int main_ver5(void)
{
    ConnectionVectorVector graph; memset(&graph, 0, sizeof(graph));
    BenchmarkVector benchmarks; memset(&benchmarks, 0, sizeof(benchmarks));
    #if VERSION == 9
    UnassignedConnectionVector connections; memset(&connections, 0, sizeof(connections));
    #endif

    #if VERSION == 5
    parse_ver5(&graph, &benchmarks);
    #endif
    #if VERSION == 8
    parse_ver8(&graph, &benchmarks);
    #endif
    #if VERSION == 9
    parse_ver9(&connections, &graph, &benchmarks);
    #endif
    solve_ver5(&graph, &benchmarks);

    #if VERSION == 5 || VERSION == 8
    for (const ConnectionVector *connections = graph.begin; connections < &graph.begin[graph.length]; connections++)
    {
        if (connections->begin != NULL) free(connections->begin);
    }
    #elif VERSION == 9
    if (connections.begin != NULL) free(connections.begin);
    #endif
    if (graph.begin != NULL) free(graph.begin);
    if (benchmarks.begin != NULL) free(benchmarks.begin);
    return 0;
}

int main(void)
{
    return main_ver5();
}
