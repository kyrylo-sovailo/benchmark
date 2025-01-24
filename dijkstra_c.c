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

#define DECLARE_VECTOR_GROW(T) \
void grow_ ## T ## Vector(T ## Vector *vector, unsigned int length) \
{ \
    if (length <= vector->length) return; \
    if (length > vector->capacity) \
    { \
        if (vector->capacity == 0) vector->capacity = 1; \
        while (vector->capacity < length) vector->capacity = (vector->capacity << 1); \
        vector->begin = realloc(vector->begin, vector->capacity * sizeof(*vector->begin)); \
        if (vector->begin == NULL) { printf("realloc() failed"); exit(1); } \
    } \
    memset(vector->begin + vector->length, 0, (length - vector->length) * sizeof(*vector->begin)); \
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
        vector->begin = realloc(vector->begin, vector->capacity * sizeof(*vector->begin)); \
        if (vector->begin == NULL) { printf("realloc() failed"); exit(2); } \
    } \
    vector->begin[vector->length - 1] = item; \
}

typedef struct
{
    unsigned int id;
    unsigned int int_distance;
    float distance;
} Candidate;
DECLARE_VECTOR(Candidate);
DECLARE_VECTOR_PUSH(Candidate);

typedef struct
{
    unsigned int source;
    unsigned int destination;
} Benchmark;
DECLARE_VECTOR(Benchmark);
DECLARE_VECTOR_PUSH(Benchmark);

typedef struct
{
    unsigned int destination;
    float distance;
} Connection;
DECLARE_VECTOR(Connection);
DECLARE_VECTOR_PUSH(Connection);
DECLARE_VECTOR(ConnectionVector);
DECLARE_VECTOR_GROW(ConnectionVector);

void push_indexed_heap(CandidateVector *data, unsigned int *indices, Candidate element)
{
    unsigned int i = indices[element.id];
    if (i == (unsigned int)-1)
    {
        i = data->length;
        indices[element.id] = i;
        push_CandidateVector(data, element);
    }
    else if (i == (unsigned int)-2)
    {
        return;
    }
    else
    {
        if (element.distance < data->begin[i].distance) data->begin[i] = element;
        else return;
    }
    while (i > 0)
    {
        const unsigned int parent_i = (i - 1) / 2;
        if (data->begin[i].distance < data->begin[parent_i].distance)
        {
            const unsigned int b1 = indices[data->begin[i].id]; indices[data->begin[i].id] = indices[data->begin[parent_i].id]; indices[data->begin[parent_i].id] = b1;
            const Candidate b2 = data->begin[i]; data->begin[i] = data->begin[parent_i]; data->begin[parent_i] = b2;
            i = parent_i;
        }
        else break;
    }
}

Candidate pop_indexed_heap(CandidateVector *data, unsigned int *indices)
{
    Candidate top = data->begin[0];
    indices[data->begin[0].id] = (unsigned int)-2;
    indices[data->begin[data->length - 1].id] = 0;
    data->begin[0] = data->begin[data->length - 1];
    data->length--;

    unsigned int i = 0;
    while (true)
    {
        const unsigned int left_i = 2 * i + 1;
        const unsigned int right_i = 2 * i + 2;
        const bool left_exists = left_i < data->length;
        const bool right_exists = right_i < data->length;
        if (/*left_exists &&*/ right_exists)
        {
            if (data->begin[left_i].distance < data->begin[right_i].distance)
            {
                if (data->begin[left_i].distance < data->begin[i].distance)
                {
                    const unsigned int b1 = indices[data->begin[i].id]; indices[data->begin[i].id] = indices[data->begin[left_i].id]; indices[data->begin[left_i].id] = b1;
                    const Candidate b2 = data->begin[i]; data->begin[i] = data->begin[left_i]; data->begin[left_i] = b2;
                    i = left_i;
                }
                else break;
            }
            else
            {
                if (data->begin[right_i].distance < data->begin[i].distance)
                {
                    const unsigned int b1 = indices[data->begin[i].id]; indices[data->begin[i].id] = indices[data->begin[right_i].id]; indices[data->begin[right_i].id] = b1;
                    const Candidate b2 = data->begin[i]; data->begin[i] = data->begin[right_i]; data->begin[right_i] = b2;
                    i = right_i;
                }
                else break;
            }
        }
        else if (left_exists /*&& !right_exists*/)
        {
            if (data->begin[left_i].distance < data->begin[i].distance)
            {
                const unsigned int b1 = indices[data->begin[i].id]; indices[data->begin[i].id] = indices[data->begin[left_i].id]; indices[data->begin[left_i].id] = b1;
                const Candidate b2 = data->begin[i]; data->begin[i] = data->begin[left_i]; data->begin[left_i] = b2;
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

void parse_ver4(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    FILE *file = fopen("dijkstra.txt", "r");
    if (file == NULL) { printf("fopen() failed"); exit(2); }

    char string[128];
    if (fscanf(file, "%127s", string) != 1) { printf("fscanf() failed"); exit(3); }
    while (true)
    {
        unsigned int source, destination;
        float distance;
        if (fscanf(file, "%u %u %f", &source, &destination, &distance) != 3) { clearerr(file); break; }
        grow_ConnectionVectorVector(graph, MAX(source, destination) + 1);
        
        Connection forward = { .destination = destination, .distance = distance };
        push_ConnectionVector(&graph->begin[source], forward);
        Connection backward = { .destination = source, .distance = distance };
        push_ConnectionVector(&graph->begin[destination], backward);
    }

    if (fscanf(file, "%127s", string) != 1) { printf("fscanf() failed"); exit(4); }
    while (true)
    {
        unsigned int source, destination;
        if (fscanf(file, "%u %u", &source, &destination) != 2) break;

        Benchmark benchmark = { .source = source, .destination = destination };
        push_BenchmarkVector(benchmarks, benchmark);
    }
    fclose(file);
}

void solve_ver4(const ConnectionVectorVector *graph, const BenchmarkVector *benchmarks)
{
    CandidateVector candidates; memset(&candidates, 0, sizeof(candidates));
    unsigned int *candidate_indices = malloc(graph->length * sizeof(unsigned int));

    for (const Benchmark *benchmark = benchmarks->begin;
        benchmark < &benchmarks->begin[benchmarks->length];
        benchmark++)
    {
        const unsigned int source = benchmark->source;
        const unsigned int destination = benchmark->destination;
        candidates.length = 0;
        memset(candidate_indices, 0xFF, graph->length * sizeof(unsigned int));
        Candidate candidate = { .id = source, .int_distance = 0, .distance = 0.0 };
        push_indexed_heap(&candidates, candidate_indices, candidate);
        unsigned int int_distance = 0;
        float distance = INFINITY;
        while (candidates.length != 0)
        {
            candidate = pop_indexed_heap(&candidates, candidate_indices);
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            ConnectionVector *connections = &graph->begin[candidate.id];

            for (const Connection *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++)
            {
                Candidate new_candidate = { .id = connection->destination, .int_distance = candidate.int_distance + 1, .distance = candidate.distance + connection->distance };
                push_indexed_heap(&candidates, candidate_indices, new_candidate);
            }
        }
        printf("%u %u %f\n", destination, int_distance, distance);
    }

    free(candidates.begin);
    free(candidate_indices);
}

int main_ver4()
{
    ConnectionVectorVector graph; memset(&graph, 0, sizeof(graph));
    BenchmarkVector benchmarks; memset(&benchmarks, 0, sizeof(benchmarks));

    parse_ver4(&graph, &benchmarks);
    solve_ver4(&graph, &benchmarks);
    
    for (const ConnectionVector *connections = graph.begin; connections < &graph.begin[graph.length]; connections++) free(connections->begin);
    free(graph.begin);
    free(benchmarks.begin);
    return 0;
}

int main()
{
    return main_ver4();
}