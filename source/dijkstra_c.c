#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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
void reserve_ ## T ## Vector(T ## Vector *vector, unsigned int capacity) \
{ \
    if (capacity <= vector->capacity) return; \
    vector->capacity = capacity; \
    vector->begin = (T*)realloc(vector->begin, vector->capacity * sizeof(T)); \
    if (vector->begin == NULL) { printf("realloc() failed"); exit(1); } \
}

#define DECLARE_VECTOR_GROW(T) \
void grow_ ## T ## Vector(T ## Vector *vector, unsigned int length) \
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
void push_ ## T ## Vector(T ## Vector *vector, T item) \
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
void unsafe_push_ ## T ## Vector(T ## Vector *vector, T item) \
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

typedef struct
{
    unsigned int destination;
    float distance;
} Connection;
DECLARE_VECTOR(Connection)
DECLARE_VECTOR_PUSH(Connection)
DECLARE_VECTOR(ConnectionVector)
DECLARE_VECTOR_GROW(ConnectionVector)

typedef struct
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
} Candidate;

void push_indexed_heap(Candidate *restrict data, unsigned int *restrict length, unsigned int *restrict indices, Candidate element)
{
    unsigned int i = indices[element.id];
    if (i == (unsigned int)-1)
    {
        i = *length;
        indices[element.id] = i;
        data[i] = element;
        (*length)++;
    }
    else if (i == (unsigned int)-2)
    {
        return;
    }
    else
    {
        if (element.distance < data[i].distance) data[i] = element;
        else return;
    }
    while (i > 0)
    {
        const unsigned int parent_i = (i - 1) / 2;
        if (data[i].distance < data[parent_i].distance)
        {
            const unsigned int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
            const Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
            i = parent_i;
        }
        else break;
    }
}

Candidate pop_indexed_heap(Candidate *restrict data, unsigned int *restrict length, unsigned int *restrict indices)
{
    Candidate top = data[0];
    indices[data[0].id] = (unsigned int)-2;
    indices[data[*length - 1].id] = 0;
    data[0] = data[*length - 1];
    (*length)--;

    unsigned int i = 0;
    while (true)
    {
        const unsigned int left_i = 2 * i + 1;
        const unsigned int right_i = 2 * i + 2;
        const bool left_exists = left_i < *length;
        const bool right_exists = right_i < *length;
        if (/*left_exists &&*/ right_exists)
        {
            if (data[left_i].distance < data[right_i].distance)
            {
                if (data[left_i].distance < data[i].distance)
                {
                    const unsigned int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[left_i].id]; indices[data[left_i].id] = b1;
                    const Candidate b2 = data[i]; data[i] = data[left_i]; data[left_i] = b2;
                    i = left_i;
                }
                else break;
            }
            else
            {
                if (data[right_i].distance < data[i].distance)
                {
                    const unsigned int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[right_i].id]; indices[data[right_i].id] = b1;
                    const Candidate b2 = data[i]; data[i] = data[right_i]; data[right_i] = b2;
                    i = right_i;
                }
                else break;
            }
        }
        else if (left_exists /*&& !right_exists*/)
        {
            if (data[left_i].distance < data[i].distance)
            {
                const unsigned int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[left_i].id]; indices[data[left_i].id] = b1;
                const Candidate b2 = data[i]; data[i] = data[left_i]; data[left_i] = b2;
                i = left_i;
            }
            else break;
        }
        else
        {
            break;
        }
    }
    return top;
}

void parse_ver5(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
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

void solve_ver5(const ConnectionVectorVector *graph, const BenchmarkVector *benchmarks)
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
        float distance = INFINITY;
        unsigned int int_distance = 0;
        while (candidates_length != 0)
        {
            candidate = pop_indexed_heap(candidates, &candidates_length, candidate_indices);
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            ConnectionVector *connections = &graph->begin[candidate.id];

            for (const Connection *connection = connections->begin;
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
        printf("%u -> %u: %f (%u) \n", source, destination, distance, int_distance);
    }

    free(candidates);
    free(candidate_indices);
}

int main_ver5(void)
{
    ConnectionVectorVector graph; memset(&graph, 0, sizeof(graph));
    BenchmarkVector benchmarks; memset(&benchmarks, 0, sizeof(benchmarks));

    parse_ver5(&graph, &benchmarks);
    solve_ver5(&graph, &benchmarks);
    
    for (const ConnectionVector *connections = graph.begin; connections < &graph.begin[graph.length]; connections++) free(connections->begin);
    free(graph.begin);
    free(benchmarks.begin);
    return 0;
}

int main(void)
{
    return main_ver5();
}
