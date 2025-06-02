#define CHUNK_SIZE (16 * 4096)
#define IO_BUFFER_SIZE (16 * 4096)
//#define USE_MAPPING
//#define USE_SYSCALL
#define ASM __asm__ __volatile__

#ifdef USE_SYSCALL
    extern long int syscall(long int sysno, ...);
    typedef void* (pointer_syscall_t)(long int sysno, ...);
    #define pointer_syscall ((pointer_syscall_t*)(&syscall))
#endif

typedef signed char int8_t;
typedef unsigned char uint8_t;
typedef signed short int16_t;
typedef unsigned short uint16_t;
typedef signed int int32_t;
typedef unsigned int uint32_t;
typedef signed long int int64_t;
typedef unsigned long int uint64_t;
typedef uint8_t bool;

#ifndef USE_MAPPING
static int64_t __read(int64_t fd, void *buf, uint64_t count)
{
    #ifdef USE_SYSCALL
    return syscall(0, fd, buf, count);
    #else
    int64_t result;
    ASM (
        "mov $0, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "mov %3, %%rdx\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (fd), "r" (buf), "r" (count)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}
#endif

static int64_t __write(int64_t fd, const void *buf, uint64_t count)
{
    #ifdef USE_SYSCALL
    return syscall(1, fd, buf, count);
    #else
    int64_t result;
    ASM (
        "mov $1, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "mov %3, %%rdx\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (fd), "r" (buf), "r" (count)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}

static int64_t __open(const char *pathname, int64_t flags)
{
    #ifdef USE_SYSCALL
    return syscall(2, pathname, flags);
    #else
    int64_t result;
    ASM (
        "mov $2, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (pathname), "r" (flags)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}

static int64_t __close(int64_t fd)
{
    #ifdef USE_SYSCALL
    return syscall(3, fd);
    #else
    int64_t result;
    ASM (
        "mov $3, %%rax\n"
        "mov %1, %%rdi\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (fd)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}

#ifdef USE_MAPPING
static int64_t __stat(const char *restrict pathname, char *restrict statbuf)
{
    //const uint64_t i = sizeof(struct stat); //144
    //const uint64_t j = ((void*)&(((struct stat*)0)->st_size) - (void*)0); //48
    //const uint64_t k = sizeof(((struct stat*)0)->st_size); //8
    
    #ifdef USE_SYSCALL
    return syscall(4, pathname, statbuf);
    #else
    int64_t result;
    ASM (
        "mov $4, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (pathname), "r" (statbuf)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}

static void *__mmap(void *addr, uint64_t length, int64_t prot, int64_t flags, int64_t fd, int64_t offset)
{
    #ifdef USE_SYSCALL
    return pointer_syscall(9, addr, length, prot, flags, fd, offset);
    #else
    void *result;
    ASM (
        "mov $9, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "mov %3, %%rdx\n"
        "mov %4, %%r10\n"
        "mov %5, %%r8\n"
        "mov %6, %%r9\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (addr), "r" (length), "r" (prot), "r" (flags), "r" (fd), "r" (offset)
        //: "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory" //not enough registers
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "memory"
    );
    return result;
    #endif
}

static int64_t __munmap(void *addr, uint64_t length)
{
    #ifdef USE_SYSCALL
    return syscall(11, addr, length);
    #else
    int64_t result;
    ASM (
        "mov $11, %%rax\n"
        "mov %1, %%rdi\n"
        "mov %2, %%rsi\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (addr), "r" (length)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}
#endif

static void *__brk(void *addr)
{
    #ifdef USE_SYSCALL
    return pointer_syscall(12, addr);
    #else
    void *result;
    ASM (
        "mov $12, %%rax\n"
        "mov %1, %%rdi\n"
        "syscall\n"
        "mov %%rax, %0\n"
        : "=r" (result)
        : "r" (addr)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    return result;
    #endif
}

static void __exit(int64_t code)
{
    #ifdef USE_SYSCALL
    syscall(60, code);
    #else
    ASM (
        "mov $60, %%rax\n"
        "mov %0, %%rdi\n"
        "syscall\n"
        :
        : "r" (code)
        : "%rax", "%rcx", "%rdx", "%rsi", "%rdi", "%r8", "%r9", "%r10", "%r11", "memory"
    );
    #endif
}

static float __floor(float a)
{
    float result;
    ASM (
        "movss %1, %%xmm0\n"
        "roundps $1, %%xmm0, %%xmm0\n"
        "movss %%xmm0, %0"
        : "=m" (result)
        : "m" (a)
        : "%xmm0"
    );
    return result;
}

static char *print_integer(char *p, uint64_t number)
{
    if (number == 0)
    {
        *p = '0';
        p++;
        return p;
    }

    uint64_t order = 1;
    while (order <= number)
    {
        order *= 10;
    }
    while (order > 1)
    {
        order /= 10;
        uint64_t digit = number / order;
        *p = '0' + (char)digit;
        p++;
        number -= digit * order;
    }
    return p;
}

static char *print_float(char *p, float number)
{
    float integral = __floor(number);
    p = print_integer(p, (uint64_t)integral);
    *p = '.';
    p++;
    number -= integral;
    for (uint64_t i = 0; i < 4; i++)
    {
        number *= 10;
        integral = __floor(number);
        *p = '0' + (char)integral;
        number -= integral;
        p++;
    }
    return p;
}

struct Benchmark
{
    uint32_t source;
    uint32_t destination;
};

struct BenchmarkVector
{
    struct Benchmark *begin;
    uint32_t length;
    uint32_t capacity;
};

struct Connection
{
    uint32_t destination;
    float distance;
};

struct ConnectionVector
{
    struct Connection *begin;
    uint32_t length;
    uint32_t capacity;
};

struct ConnectionVectorVector
{
    struct ConnectionVector *begin;
    uint32_t length;
    uint32_t capacity;
};

struct Candidate
{
    uint64_t id;
    uint32_t int_distance;
    float distance;
};

struct Chunk
{
    void *previous_chunk;
    uint64_t size;
};

#ifndef NDEBUG
bool check[(
    sizeof(int8_t) == 1 && sizeof(uint8_t) == 1 &&
    sizeof(int16_t) == 2 && sizeof(uint16_t) == 2 &&
    sizeof(int32_t) == 4 && sizeof(uint32_t) == 4 &&
    sizeof(int64_t) == 8 && sizeof(uint64_t) == 8 &&
    sizeof(float) == 4 && sizeof(double) == 8 &&
    sizeof(void*) == 8 &&
    sizeof(struct Benchmark) == 8 &&
    sizeof(struct BenchmarkVector) == 16 &&
    sizeof(struct Connection) == 8 &&
    sizeof(struct ConnectionVector) == 16 &&
    sizeof(struct ConnectionVectorVector) == 16 &&
    sizeof(struct Candidate) == 16
) ? 64 : -1];
#endif
void *malloc_memory_begin = (void*)0;
void *malloc_memory_end = (void*)0;
void *malloc_memory_capacity_end = (void*)0;

static void *__malloc(uint64_t size)
{
    if (malloc_memory_end == (void*)0)
    {
        void *old_break = __brk((void*)-1);
        if (old_break == (void*)-1) { __write(1, "brk error 1\n", 12); __exit(1); }
        malloc_memory_begin = old_break;
        malloc_memory_end = old_break;
        malloc_memory_capacity_end = old_break;
    }
    const uint64_t available_memory = (uint64_t)((char*)malloc_memory_capacity_end - (char*)malloc_memory_end);
    if (size > available_memory)
    {
        const uint64_t bytes_to_request = size - available_memory;
        const uint64_t bytes_to_request_up = ((bytes_to_request + CHUNK_SIZE - 1) / CHUNK_SIZE) * CHUNK_SIZE;
        void *requested_new_break = (char*)malloc_memory_capacity_end + bytes_to_request_up;
        void *new_break = __brk(requested_new_break);
        if (new_break != requested_new_break) { __write(1, "brk error 2\n", 12); __exit(1); }
        malloc_memory_capacity_end = new_break;
    }
    void *memory = malloc_memory_end;
    malloc_memory_end = (char*)malloc_memory_end + size;
    //for (uint64_t i = 0; i < size; i++) ((unsigned char*)memory)[i] = 0xCC;
    return memory;
}

static void __free(void)
{
    if (malloc_memory_begin != (void*)0)
    {
        void *new_break = __brk(malloc_memory_begin);
        if (new_break != malloc_memory_begin) { __write(1, "brk error 3\n", 12); __exit(2); }
    }
}

static void push_benchmark(struct BenchmarkVector *benchmarks, uint64_t source, uint64_t destination)
{
    if (benchmarks->length == benchmarks->capacity)
    {
        if (benchmarks->capacity == 0) benchmarks->capacity = 1;
        else benchmarks->capacity <<= 1;
        struct Benchmark *new_begin = __malloc(benchmarks->capacity * sizeof(*new_begin));
        for (uint64_t i = 0; i < benchmarks->length; i++) new_begin[i] = benchmarks->begin[i];
        benchmarks->begin = new_begin;
    }
    const struct Benchmark benchmark = { .source = (uint32_t)source, .destination = (uint32_t)destination };
    benchmarks->begin[benchmarks->length] = benchmark;
    benchmarks->length++;
}

static void push_connection(struct ConnectionVector *connections, uint64_t destination, float distance)
{
    if (connections->length == connections->capacity)
    {
        if (connections->capacity == 0) connections->capacity = 1;
        else connections->capacity <<= 1;
        struct Connection *new_begin = __malloc(connections->capacity * sizeof(*new_begin));
        for (uint64_t i = 0; i < connections->length; i++) new_begin[i] = connections->begin[i];
        connections->begin = new_begin;
    }
    const struct Connection connection = { .destination = (uint32_t)destination, .distance = distance };
    connections->begin[connections->length] = connection;
    connections->length++;
}

static void push_undirected_connection(struct ConnectionVectorVector *connections, uint64_t source, uint64_t destination, float distance)
{
    const uint64_t new_length = ((source > destination) ? source : destination) + 1;
    if (new_length > connections->length)
    {
        if (new_length > connections->capacity)
        {
            if (connections->capacity == 0) connections->capacity = 1;
            while (connections->capacity < new_length) connections->capacity <<= 1;
            struct ConnectionVector *new_begin = __malloc(connections->capacity * sizeof(*new_begin));
            for (uint64_t i = 0; i < connections->length; i++) new_begin[i] = connections->begin[i];
            connections->begin = new_begin;
        }
        for (uint64_t i = connections->length; i < new_length; i++)
        {
            const struct ConnectionVector zero = { 0, 0, 0 };
            connections->begin[i] = zero;
        }
        connections->length = (uint32_t)new_length;
    }
    push_connection(&connections->begin[source], destination, distance);
    push_connection(&connections->begin[destination], source, distance);
}

static uint64_t push_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices, struct Candidate element) //returns new length
{
    uint64_t i = indices[element.id];
    if (i == (uint32_t)-1)
    {
        i = length;
        indices[element.id] = (uint32_t)length;
        data[length] = element;
        length++;
    }
    else if (i == (uint32_t)-2)
    {
        return length;
    }
    else
    {
        if (element.distance < data[i].distance) data[i] = element;
        else return length;
    }
    while (i > 0)
    {
        const uint64_t parent_i = (i - 1) / 2;
        if (data[i].distance < data[parent_i].distance)
        {
            const uint32_t b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
            const struct Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
            i = parent_i;
        }
        else break;
    }
    return length;
}

static struct Candidate pop_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices) //length should be reduced by 1
{
    const uint64_t length_minus_1 = length - 1;
    struct Candidate top = data[0];
    indices[data[0].id] = (uint32_t)-2;
    indices[data[length_minus_1].id] = 0;
    data[0] = data[length_minus_1];

    uint64_t i = 0;
    for (;;)
    {
        const uint64_t left_i = 2 * i + 1;
        const uint64_t right_i = 2 * i + 2;
        const bool left_exists = left_i <= length_minus_1;
        const bool right_exists = right_i <= length_minus_1;
        if (/*left_exists &&*/ right_exists)
        {
            if (data[left_i].distance < data[right_i].distance)
            {
                if (data[left_i].distance < data[i].distance)
                {
                    const uint32_t b1 = indices[data[i].id]; indices[data[i].id] = indices[data[left_i].id]; indices[data[left_i].id] = b1;
                    const struct Candidate b2 = data[i]; data[i] = data[left_i]; data[left_i] = b2;
                    i = left_i;
                }
                else break;
            }
            else
            {
                if (data[right_i].distance < data[i].distance)
                {
                    const uint32_t b1 = indices[data[i].id]; indices[data[i].id] = indices[data[right_i].id]; indices[data[right_i].id] = b1;
                    const struct Candidate b2 = data[i]; data[i] = data[right_i]; data[right_i] = b2;
                    i = right_i;
                }
                else break;
            }
        }
        else if (left_exists /*&& !right_exists*/)
        {
            if (data[left_i].distance < data[i].distance)
            {
                const uint32_t b1 = indices[data[i].id]; indices[data[i].id] = indices[data[left_i].id]; indices[data[left_i].id] = b1;
                const struct Candidate b2 = data[i]; data[i] = data[left_i]; data[left_i] = b2;
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

enum state
{
    st_search_source_begin,
    st_search_source_end,
    st_search_destination_begin,
    st_search_destination_end,
    st_search_distance_begin,
    st_search_distance_dot,
    st_search_distance_end,
    st_search_graph_end,
    st_search_benchmark_end,
    st_search_endline,
    st_endfile
};

static void parse_ver5(struct ConnectionVectorVector *graph, struct BenchmarkVector *benchmarks)
{
    //I/O
    int64_t code;
    int64_t file = __open("dijkstra.txt", 0); //O_RDONLY
    if (file < 0) { __write(1, "open failed\n", 12); __exit(3); }
    #ifdef USE_MAPPING
    uint64_t buffer_size;
    char *buffer;
    {
        char statbuffer[144] = { 0 };
        code = __stat("dijkstra.txt", statbuffer);
        if (code < 0) { __write(1, "stat failed\n", 12); __exit(4); }
        buffer_size = *((uint64_t*)&statbuffer[48]);
        buffer = __mmap((void*)0, buffer_size, 0x01, 0x02, (int)file, 0); //PROT_READ, MAP_PRIVATE
        if (buffer == (void*)-1) { __write(1, "mmap failed\n", 12); __exit(5); }
    }
    #else
    char buffer[IO_BUFFER_SIZE];
    int64_t buffer_size = 0;
    #endif
    char *p = buffer;
    bool eof = 0;

    //State
    enum state state = st_search_source_begin;
    bool read_benchmarks = 1;
    uint64_t source;
    uint64_t destination;
    float distance;
    float order;
    uint8_t keyword_check;

    for (;;)
    {
        #ifdef USE_MAPPING
        eof = (p == buffer + buffer_size);
        #else
        if (p == buffer + buffer_size) { p = buffer; buffer_size = __read(file, buffer, sizeof(buffer)); eof = (buffer_size == 0); }
        #endif
        const char c = eof ? '\0' : *p;
        p++;

        switch (state)
        {
        case st_search_source_begin:
            if (eof) state = st_endfile; //EOF -> end
            else if (c == ' ' || c == '\t') {} //space -> repeat
            else if (c == '\n' || c == '\r') {} //endline -> repeat
            else if (c >= '0' && c <= '9') { source = (unsigned char)((unsigned char)(c - '0')); state = st_search_source_end; } //number -> set source, search source end
            else if (c == 'G') { keyword_check = 1; state = st_search_graph_end; } //G -> keyword check
            else if (c == 'B') { keyword_check = 1; state = st_search_benchmark_end; } //B -> keyword check
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_source_end:
            if (eof) state = st_endfile; //EOF -> format error, end
            else if (c == ' ' || c == '\t') state = st_search_destination_begin; //space -> search destination begin
            else if (c == '\n' || c == '\r') state = st_endfile; //endline -> format error, end
            else if (c >= '0' && c <= '9') source = 10 * source + (unsigned char)(c - '0'); //number -> modify source, repeat
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_destination_begin:
            if (eof) state = st_endfile; //EOF -> syntax error, end
            else if (c == ' ' || c == '\t') {} //space -> repeat
            else if (c == '\n' || c == '\r') state = st_endfile; //endline -> format error, end
            else if (c >= '0' && c <= '9') { destination = (unsigned char)(c - '0'); state = st_search_destination_end; } //number -> set destination, search destination end
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_destination_end:
            if (eof)
            {
                if (read_benchmarks) { push_benchmark(benchmarks, source, destination); state = st_endfile; } //EOF -> push benchmark, end
                else state = st_endfile; //EOF -> syntax error, end
            }
            else if (c == ' ' || c == '\t')
            {
                if (read_benchmarks) { push_benchmark(benchmarks, source, destination); state = st_search_endline; } //space -> push benchmark, search for endline
                else state = st_search_distance_begin; //space -> search distance begin
            }
            else if (c == '\n' || c == '\r')
            {
                if (read_benchmarks) { push_benchmark(benchmarks, source, destination); state = st_search_source_begin; } //endline -> push benchmark, start over
                else state = st_endfile; //endline -> syntax error, end
            }
            else if (c >= '0' && c <= '9') destination = 10 * destination + (unsigned char)(c - '0'); //number -> modify destination, repeat
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_distance_begin:
            if (eof) state = st_endfile; //EOF -> syntax error, end
            else if (c == ' ' || c == '\t') {} //space -> repeat
            else if (c == '\n' || c == '\r') state = st_endfile; //endline -> format error, end
            else if (c >= '0' && c <= '9') { distance = (unsigned char)(c - '0'); state = st_search_distance_dot; } //number -> set distance, search distance dot
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_distance_dot:
            if (eof) { push_undirected_connection(graph, source, destination, distance); state = st_endfile; } //EOF -> push connection, end
            else if (c == ' ' || c == '\t') { push_undirected_connection(graph, source, destination, distance); state = st_search_endline; } //space -> push connection, search endline
            else if (c == '\n' || c == '\r') { push_undirected_connection(graph, source, destination, distance); state = st_search_source_begin; } //endline -> push connection, start over
            else if (c >= '0' && c <= '9') distance = 10 * distance + (unsigned char)(c - '0'); //number -> modify distance, repeat
            else if (c == '.') { order = 1; state = st_search_distance_end; } //dot -> set order, search distance end
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_distance_end:
            if (eof) { push_undirected_connection(graph, source, destination, distance); state = st_endfile; } //EOF -> push connection, end
            else if (c == ' ' || c == '\t') { push_undirected_connection(graph, source, destination, distance); state = st_search_endline; } //space -> push connection, search endline
            else if (c == '\n' || c == '\r') { push_undirected_connection(graph, source, destination, distance); state = st_search_source_begin; } //endline -> push connection, start over
            else if (c >= '0' && c <= '9') { order /= 10; distance += order * (unsigned char)(c - '0'); } //number -> modify distance, repeat
            else state = st_endfile; //else -> format error, end
            break;
        
        case st_search_graph_end:
            if (eof) state = st_endfile; //EOF -> format error, end
            else if (c == "GRAPH"[keyword_check])
            {
                keyword_check++; //match -> increase counter
                if (keyword_check == 5) { read_benchmarks = 0; state = st_search_endline; }
            }
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_benchmark_end:
            if (eof) state = st_endfile; //EOF -> format error, end
            else if (c == "BENCHMARK"[keyword_check])
            {
                keyword_check++; //match -> increase counter
                if (keyword_check == 9) { read_benchmarks = 1; state = st_search_endline; }
            }
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_endline:
            if (eof) state = st_endfile; //EOF -> enf
            else if (c == ' ' || c == '\t') {} //space -> repeat
            else if (c == '\n' || c == '\r') state = st_search_source_begin; //endline -> start over
            else state = st_endfile; //else -> format error, end
            break;
        
        default:
            #ifdef USE_MAPPING
            code = __munmap(buffer, buffer_size);
            if (code < 0) { __write(1, "munmap failed\n", 14); __exit(6); }
            #endif
            code = __close(file);
            if (code < 0) { __write(1, "close failed\n", 13); __exit(7); }
            return;
        }
    }
}

static void solve_ver5(const struct ConnectionVectorVector *graph, const struct BenchmarkVector *benchmarks)
{
    struct Candidate *candidates = __malloc(graph->length * sizeof(struct Candidate));
    uint64_t candidates_length = 0;
    uint32_t *candidate_indices = __malloc(graph->length * sizeof(uint32_t));

    for (const struct Benchmark *benchmark = benchmarks->begin;
        benchmark < &benchmarks->begin[benchmarks->length];
        benchmark++)
    {
        const uint32_t source = benchmark->source;
        const uint32_t destination = benchmark->destination;
        candidates_length = 0;
        for (uint64_t i = 0; i < graph->length; i++) candidate_indices[i] = (uint32_t)-1;
        struct Candidate candidate = { .id = source, .int_distance = 0, .distance = 0.0 };
        candidates_length = push_indexed_heap(candidates, candidates_length, candidate_indices, candidate);
        candidate.distance = 1.0f / 0.0f;
        candidate.int_distance = 0;
        while (candidates_length != 0)
        {
            candidate = pop_indexed_heap(candidates, candidates_length, candidate_indices);
            candidates_length--;
            if (candidate.id == destination) break;
            struct ConnectionVector *connections = &graph->begin[candidate.id];

            for (const struct Connection *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++)
            {
                struct Candidate new_candidate = {
                    .id = connection->destination,
                    .distance = candidate.distance + connection->distance,
                    .int_distance = candidate.int_distance + 1
                };
                candidates_length = push_indexed_heap(candidates, candidates_length, candidate_indices, new_candidate);
            }
        }
        char buffer[256];
        char *p = buffer;
        p = print_integer(p, source);
        *p = ' '; p++;
        *p = '-'; p++;
        *p = '>'; p++;
        *p = ' '; p++;
        p = print_integer(p, destination);
        *p = ':'; p++;
        *p = ' '; p++;
        p = print_float(p, candidate.distance);
        *p = ' '; p++;
        *p = '('; p++;
        p = print_integer(p, candidate.int_distance);
        *p = ')'; p++;
        *p = '\n'; p++;
        __write(1, buffer, (uint64_t)(p - buffer));
    }
}

static void main_ver5(void)
{
    struct ConnectionVectorVector graph = { 0, 0, 0 };
    struct BenchmarkVector benchmarks = { 0, 0, 0 };

    parse_ver5(&graph, &benchmarks);
    solve_ver5(&graph, &benchmarks);

    __free();
}

void _start(void)
{
    ASM(
        "and $-16, %rsp\n"
        "mov %rsp, %rbp\n"
    );
    main_ver5();
    __exit(0);
}
