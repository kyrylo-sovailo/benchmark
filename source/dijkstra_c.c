#include <math.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef ENABLE_MAPPING
    #include <fcntl.h>
    #include <unistd.h>
    #include <sys/mman.h>
    #include <sys/stat.h>
#endif
#ifdef ENABLE_MULTITHREADING
    #include <threads.h>
#endif

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

#ifdef ENABLE_MAPPING
void parse_space(const char **restrict p, const char *restrict endp)
{
    while (true)
    {
        if (*p >= endp) return;
        const char c = **p;
        if (c == ' ' || c == '\t') (*p)++;
        else break;
    }
}

unsigned int parse_uint(const char **restrict p, const char *restrict endp, bool *restrict success)
{
    bool local_success = false;
    unsigned int number = 0;
    while (true)
    {
        if (*p >= endp) break;
        const char c = **p;
        if (c >= '0' && c <= '9') { number = 10 * number + (c - '0'); local_success = true; (*p)++; }
        else break;
    }
    if (!local_success) *success = false;
    return number;
}

float parse_float(const char **restrict p, const char *restrict endp, bool *restrict success)
{
    bool local_success = false;
    unsigned int number = 0;
    while (true)
    {
        if (*p >= endp) { if (!local_success) *success = false; return number; }
        const char c = **p;
        if (c >= '0' && c <= '9') { number = 10 * number + (c - '0'); local_success = true; (*p)++; }
        else break;
    }

    float fnumber = number;
    if (*p < endp && **p == '.')
    {
        (*p)++;
        unsigned int divider = 10;
        while (true)
        {
            if (*p >= endp) { if (!local_success) *success = false; return fnumber; }
            char c = **p;
            if (c >= '0' && c <= '9') { fnumber += ((float)(c - '0'))/((float)divider); local_success = true; divider *= 10; (*p)++; }
            else break;
        }
        if (!local_success) p--; //Read dot, but no numbers
    }

    if (!local_success) *success = false;
    return fnumber;
}

void parse_ver5(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    int file = open("dijkstra.txt", O_RDONLY);
    if (file < 0) exit(2);
    struct stat status;
    if (fstat(file, &status) < 0) exit(3);
    char *map = mmap(NULL, status.st_size, PROT_READ, MAP_PRIVATE, file, 0);
    if (map == NULL) { close(file); exit(4); }

    const size_t graph_len = strlen("GRAPH");
    const size_t benchmark_len = strlen("BENCHMARK");
    const char *p = map;
    const char *endp = map + status.st_size;
    
    bool read_benchmarks = false;
    while (true)
    {
        parse_space(&p, endp); //Skip space, arrive at decision point

        if (p >= endp) break; //End of file
        else if (*p == '\n' || *p == '\r') p++; //End of line
        else if (p + graph_len <= endp && memcmp(p, "GRAPH", graph_len) == 0) //GRAPH keyword
        {
            read_benchmarks = false;
            p += graph_len;
        }
        else if (p + benchmark_len <= endp && memcmp(p, "BENCHMARK", benchmark_len) == 0) //BENCHMARK keyword
        {
            read_benchmarks = true;
            p += benchmark_len;
        }
        else if (read_benchmarks) //Read benchmark
        {
            bool success = true;
            unsigned int source = parse_uint(&p, endp, &success);
            parse_space(&p, endp);
            unsigned int destination = parse_uint(&p, endp, &success);
            if (!success) break;

            Benchmark benchmark = { .source = source, .destination = destination };
            push_BenchmarkVector(benchmarks, benchmark);
        }
        else //Read connection
        {
            bool success = true;
            unsigned int source = parse_uint(&p, endp, &success);
            parse_space(&p, endp);
            unsigned int destination = parse_uint(&p, endp, &success);
            parse_space(&p, endp);
            float distance = parse_float(&p, endp, &success);
            if (!success) break;

            grow_ConnectionVectorVector(graph, MAX(source, destination) + 1);
            Connection forward = { .destination = destination, .distance = distance };
            push_ConnectionVector(&graph->begin[source], forward);
            Connection backward = { .destination = source, .distance = distance };
            push_ConnectionVector(&graph->begin[destination], backward);
        }
    }
    
    munmap(map, status.st_size);
    close(file);
}
#else
void parse_ver5(ConnectionVectorVector *graph, BenchmarkVector *benchmarks)
{
    FILE *file = fopen("dijkstra.txt", "r");
    if (file == NULL) { printf("fopen() failed"); exit(2); }

    bool read_benchmarks = false;
    char line[256];
    while (true)
    {
        for (unsigned int i = 0;; i++)
        {
            char c;
            if (fread(&c, 1, 1, file) == 0) { line[MIN(i, sizeof(line) - 1)] = '\0'; break; }
            if (i < sizeof(line) - 1) line[i] = c;
            if (c == '\n' || c == '\r') { line[MIN(i, sizeof(line) - 1)] = '\0'; break; }
        }
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

#ifdef ENABLE_MULTITHREADING
typedef struct
{
    const ConnectionVectorVector *graph;
    const BenchmarkVector *benchmarks;
    mtx_t mutex;
    const Benchmark *current_benchmark;
} ThreadArguments;
int solve_target_ver5(void *void_arguments)
{
    const ConnectionVectorVector *graph = ((ThreadArguments*)void_arguments)->graph;
    const BenchmarkVector *benchmarks = ((ThreadArguments*)void_arguments)->benchmarks;
    mtx_t *mutex = &((ThreadArguments*)void_arguments)->mutex;
    const Benchmark **current_benchmark = &((ThreadArguments*)void_arguments)->current_benchmark;

    Candidate *candidates = (Candidate*)malloc(graph->length * sizeof(Candidate));
    unsigned int candidates_length = 0;
    unsigned int *candidate_indices = malloc(graph->length * sizeof(unsigned int));

    mtx_lock(mutex);
    const Benchmark *benchmark = *current_benchmark;
    (*current_benchmark)++;
    mtx_unlock(mutex);
    while (benchmark < &benchmarks->begin[benchmarks->length])
    {
        const unsigned int source = benchmark->source;
        const unsigned int destination = benchmark->destination;
        candidates_length = 0;
        memset(candidate_indices, 0xFF, graph->length * sizeof(unsigned int));
        Candidate candidate = { .id = source, .int_distance = 0, .distance = 0.0 };
        push_indexed_heap(candidates, &candidates_length, candidate_indices, candidate);
        unsigned int int_distance = 0;
        float distance = INFINITY;
        while (candidates_length != 0)
        {
            candidate = pop_indexed_heap(candidates, &candidates_length, candidate_indices);
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            ConnectionVector *connections = &graph->begin[candidate.id];

            for (const Connection *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++)
            {
                Candidate new_candidate = { .id = connection->destination, .int_distance = candidate.int_distance + 1, .distance = candidate.distance + connection->distance };
                push_indexed_heap(candidates, &candidates_length, candidate_indices, new_candidate);
            }
        }
        mtx_lock(mutex);
        printf("%u %u %f\n", destination, int_distance, distance);
        benchmark = *current_benchmark;
        (*current_benchmark)++;
        mtx_unlock(mutex);
    }

    free(candidates);
    free(candidate_indices);
    return 0;
}

void solve_ver5(const ConnectionVectorVector *graph, const BenchmarkVector *benchmarks)
{
    ThreadArguments arguments;
    arguments.graph = graph;
    arguments.benchmarks = benchmarks;
    arguments.current_benchmark = benchmarks->begin;
    if (mtx_init(&arguments.mutex, mtx_plain) != thrd_success) exit(5);
    const unsigned int nthreads = MIN(4, benchmarks->length);
    thrd_t threads[4];
    for (unsigned int i = 0; i < nthreads; i++) thrd_create(threads + i, solve_target_ver5, &arguments);
    for (unsigned int i = 0; i < nthreads; i++) thrd_join(threads[i], NULL);
    mtx_destroy(&arguments.mutex);
}
#else
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
        unsigned int int_distance = 0;
        float distance = INFINITY;
        while (candidates_length != 0)
        {
            candidate = pop_indexed_heap(candidates, &candidates_length, candidate_indices);
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            ConnectionVector *connections = &graph->begin[candidate.id];

            for (const Connection *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++)
            {
                Candidate new_candidate = { .id = connection->destination, .int_distance = candidate.int_distance + 1, .distance = candidate.distance + connection->distance };
                push_indexed_heap(candidates, &candidates_length, candidate_indices, new_candidate);
            }
        }
        printf("%u %u %f\n", destination, int_distance, distance);
    }

    free(candidates);
    free(candidate_indices);
}
#endif

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
