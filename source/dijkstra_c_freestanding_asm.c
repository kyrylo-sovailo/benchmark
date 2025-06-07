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
    //p - RDI
    //number - RSI

    if (number == 0)
    {
        *p = '0';
        p++;
        return p;
    }

    uint64_t order = 1; //RAX
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
    //p - RDI
    //number - XMM0
    float integral = __floor(number); //XMM1
    number -= integral;
    p = print_integer(p, (uint64_t)integral);
    *p = '.';
    p++;
    
    for (uint64_t i = 0; i < 4; i++) //RCX
    {
        number *= 10;
        integral = __floor(number);
        number -= integral;
        *p = '0' + (char)integral;
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
    float distance;
    uint32_t int_distance;
    uint64_t id;
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
void *__malloc_memory_begin = (void*)0;
void *__malloc_memory_end = (void*)0;
void *__malloc_memory_capacity_end = (void*)0;

static void *__malloc(uint64_t size)
{
    //subfunctions are called rarely, prioritize scratch registers

    //size - RDI
    void *memory_end; //RCX
    void *memory_end_copy; //RAX
    void *memory_capacity_end; //RDX
    void *memory_capacity_end_copy; //RAX
    uint64_t available_memory; //RAX
    uint64_t bytes_to_request; //RSI

    size = ((size + 7) / 8) * 8;
    
    memory_end = __malloc_memory_end; //borrow __malloc_memory_end
    
    if (memory_end == (void*)0)
    {
        memory_end = __brk((void*)-1);
        if (memory_end == (void*)-1) { __write(1, "brk error 1\n", 12); __exit(1); }
        __malloc_memory_begin = memory_end;
        memory_capacity_end = memory_end; //borrow __malloc_memory_capacity_end
    }
    else
    {
        memory_capacity_end = __malloc_memory_capacity_end; //borrow __malloc_memory_capacity_end
    }

    available_memory = (uint64_t)((char*)memory_capacity_end - (char*)memory_end);
    if (size > available_memory)
    {
        bytes_to_request = size - available_memory;
        bytes_to_request = (bytes_to_request + CHUNK_SIZE - 1) & (uint64_t)~(CHUNK_SIZE - 1);
        memory_capacity_end = (char*)memory_capacity_end + bytes_to_request;
        memory_capacity_end_copy = __brk(memory_capacity_end);
        if (memory_capacity_end_copy != memory_capacity_end) { __write(1, "brk error 2\n", 12); __exit(1); }
    }
    __malloc_memory_capacity_end = memory_capacity_end; //store __malloc_memory_capacity_end
    memory_end_copy = memory_end;
    memory_end = (char*)memory_end + size;
    __malloc_memory_end = memory_end; //store __malloc_memory_end
    return memory_end_copy;
}

static void __free(void)
{
    //subfunctions are called rarely, prioritize scratch registers

    void *memory_begin; //RDI
    void *memory_begin_copy; //RAX

    memory_begin = __malloc_memory_begin;
    if (memory_begin != (void*)0)
    {
        memory_begin_copy = __brk(memory_begin);
        if (memory_begin_copy != memory_begin) { __write(1, "brk error 3\n", 12); __exit(2); }
    }
}

static void push_benchmark(struct BenchmarkVector *benchmarks, uint64_t source, uint64_t destination)
{
    //subfunctions are rarely called, prioritize scratch registers

    //benchmarks - RAX
    //source - RDX
    //destination - RDX, upper bits
    struct Benchmark *benchmarks_begin; //R8
    uint32_t benchmarks_length; //R9D
    uint32_t benchmarks_capacity; //EDI
    
    struct Benchmark *src; //RSI
    struct Benchmark *dst; //RDI
    //i - RCX

    benchmarks_begin = benchmarks->begin; //borrow benchmarks->begin
    benchmarks_length = benchmarks->length; //borrow benchmarks->length
    if (benchmarks_length == benchmarks->capacity)
    {
        benchmarks_capacity = benchmarks->capacity; //borrow benchmarks->capacity (Reading the memory twice?)
        if (benchmarks_capacity == 0) benchmarks_capacity = 1;
        else benchmarks_capacity <<= 1;
        benchmarks->capacity = benchmarks_capacity; //store benchmarks->capacity
        src = benchmarks_begin;
        benchmarks_begin = __malloc(benchmarks_capacity * sizeof(*dst));
        benchmarks->begin = benchmarks_begin; //store benchmarks->begin
        dst = benchmarks_begin;
        for (uint64_t i = 0; i < benchmarks_length; i++, src++, dst++) *dst = *src;
    }
    
    const struct Benchmark benchmark = { .source = (uint32_t)source, .destination = (uint32_t)destination };
    benchmarks_begin[benchmarks_length] = benchmark;
    benchmarks_length++;
    benchmarks->length = benchmarks_length; //store benchmarks->length
}

static float push_connection(struct ConnectionVector *connections, uint64_t destination, float distance)
{
    //subfunctions are rarely called, prioritize scratch registers

    //connections - RAX
    //destination - RDX
    //distance - RDX, upper bits
    struct Connection *connections_begin; //R8
    uint32_t connections_length; //R9D
    uint32_t connections_capacity; //EDI
    
    struct Connection *src; //RSI
    struct Connection *dst; //RDI
    //i - RCX

    connections_begin = connections->begin; //borrow connections->begin
    connections_length = connections->length; //borrow connections->length
    if (connections_length == connections->capacity)
    {
        connections_capacity = connections->capacity; //borrow connections->capacity (Reading the memory twice?)
        if (connections_capacity == 0) connections_capacity = 1;
        else connections_capacity <<= 1;
        connections->capacity = connections_capacity; //store connections->capacity
        src = connections_begin;
        connections_begin = __malloc(connections_capacity * sizeof(*dst));
        connections->begin = connections_begin; //store connections->begin
        dst = connections_begin;
        for (uint64_t i = 0; i < connections_length; i++, src++, dst++) *dst = *src;
    }
    const struct Connection connection = { .destination = (uint32_t)destination, .distance = distance };
    connections_begin[connections_length] = connection;
    connections_length++;
    connections->length = connections_length; //store connections->length
    return distance;
}

static float push_bi_connection(struct ConnectionVectorVector *connections, uint64_t source, uint64_t destination, float distance)
{
    //subfunctions are relatively rarely called, prioritize scratch registers

    //connections - RAX
    //source - ESI, stack
    //destination - EDX, stack
    //distance - XMM0
    uint32_t source_destination_max; //EDX
    uint32_t new_connections_length; //EDX
    struct ConnectionVector *connections_begin; //R8
    uint32_t connections_length; //R9D
    uint32_t connections_capacity; //EDI

    struct ConnectionVector *src; //RSI
    struct ConnectionVector *dst; //RDI
    //i - RCX

    connections_begin = connections->begin; //borrow connections->begin
    source_destination_max = (uint32_t)((source > destination) ? source : destination);
    if (source_destination_max >= connections->length)
    {
        new_connections_length = source_destination_max + 1;
        connections_length = connections->length; //borrow connections->length
        connections->length = (uint32_t)new_connections_length; //store connections->length
        if (new_connections_length > connections->capacity)
        {
            connections_capacity = connections->capacity; //borrow connections->capacity
            if (connections_capacity == 0) connections_capacity = 1;
            while (connections_capacity < new_connections_length) connections_capacity <<= 1;
            connections->capacity = connections_capacity; //store connections->capacity
            src = connections_begin;
            connections_begin = __malloc(connections_capacity * sizeof(*dst));
            connections->begin = connections_begin; //store connections->begin
            dst = connections_begin;
            for (uint64_t i = 0; i < connections_length; i++) *dst = *src;
        }
        dst = connections_begin + connections_length;
        const struct ConnectionVector zero = { 0, 0, 0 };
        for (uint64_t i = connections_length; i < new_connections_length; i++) *dst = zero;
    }
    distance = push_connection(&connections_begin[source], destination, distance);
    distance = push_connection(&connections_begin[destination], source, distance);
    return distance;
}

static uint64_t push_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices, struct Candidate element) //returns new length
{
    //subfunctions are never called, prioritize scratch registers
    //data - RDI
    //length - RSI
    //indices - RDX
    //element - RCX/R8, RCX part moved to XMM0

    uint64_t index = indices[element.id]; //RCX
    struct Candidate *index_pointer; //RAX
    if (index == (uint32_t)-1)
    {
        index = length;
        length++;
        index_pointer = &data[index];
    }
    else if (index == (uint32_t)-2)
    {
        return length;
    }
    else
    {
        index_pointer = &data[index];
        if (element.distance >= index_pointer->distance) return length;
    }
    
    for (;;)
    {
        if (index == 0)
        {
            *index_pointer = element;
            indices[element.id] = (uint32_t)index;
            break;
        }

        const uint64_t parent_i = (index - 1) / 2; //R9
        struct Candidate *c = &data[parent_i]; //R10, float in XMM1
        if (element.distance < c->distance)
        {
            *index_pointer = *c;
            indices[index_pointer->id] = (uint32_t)index;
            index = parent_i;
            index_pointer = c;
        }
        else
        {
            *index_pointer = element;
            indices[element.id] = (uint32_t)index;
            break;
        }
    }
    return length;
}

static struct Candidate pop_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices) //length should be reduced by 1
{
    //subfunctions are never called, prioritize scratch registers
    //data - RDI
    //length - RSI
    //indices - RDX

    const uint64_t length_minus_1 = length - 1; //RSI
    const struct Candidate top = data[0]; //XMM0, XMM1/RAX temporarily
    indices[top.id] = (uint32_t)-2;
    if (length_minus_1 == 0) return top;
    
    struct Candidate buffer = data[length_minus_1]; //XMM1
    uint64_t index = 0; //RCX
    struct Candidate *index_pointer = &data[0]; //RAX

    for (;;)
    {
        const uint64_t left_index = 2 * index + 1; //R8
        const uint64_t right_index = 2 * index + 2;
        //const bool left_exists = left_index <= length_minus_1;
        //const bool right_exists = right_index <= length_minus_1; //implies left_exists

        if (left_index > length_minus_1) // if (*!left_exists && !right_exists)
        {
            *index_pointer = buffer;
            indices[buffer.id] = (uint32_t)index;
            break;
        }
        
        struct Candidate *c = &data[left_index]; //R9, XMM2 for float cache
        uint64_t next_index;
        
        if (left_index < length_minus_1) // if (left_exists && right_exists)
        {
            if (c->distance < (c + 1)->distance) //XMM2 and XMM3
            {
                next_index = left_index;
            }
            else
            {
                c++; //c = c + 1, XMM2 = XMM3
                next_index = right_index;
            }
        }
        else // if (left_exists && !right_exists)
        {
            next_index = left_index;
        }

        if (c->distance < buffer.distance)
        {
            *index_pointer = *c;
            indices[index_pointer->id] = (uint32_t)index;
            index = next_index;
            index_pointer = c;
        }
        else
        {
            *index_pointer = buffer;
            indices[buffer.id] = (uint32_t)index;
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
    //graph - stack (R14?)
    //benchmarks - stack (R15?)

    //I/O
    int64_t code; //RAX
    int64_t file; //RAX, stack [rsp]
    char c; //AL
    char *p; //R12
    bool eof; //R13b, 0x01 mask
    #ifdef USE_MAPPING
    char *buffer;
    #else
    char buffer[IO_BUFFER_SIZE]; //stack [rbp - 0x10000]==[rsp + 0x30] to [rbp]==[rsp + 0x10030]
    #endif
    char *buffer_end; //RBP

    //State
    enum state state; //RBX
    bool read_benchmarks; //R13, 0x02 mask
    uint64_t source; //RSI, stack [rsp + 8]
    uint64_t destination; //RDX, stack [rsp + 16]
    float distance; //XMM0, [rsp + 24], integer part in RCX, [rsp+32]
    float order;    //XMM1, [rsp + 28]
    uint8_t keyword_check; //RSI
    
    //I/O
    file = __open("dijkstra.txt", 0); //O_RDONLY
    if (file < 0) { __write(1, "open error\n", 11); __exit(3); }
    #ifdef USE_MAPPING
    {
        char statbuffer[144] = { 0 };
        code = __stat("dijkstra.txt", statbuffer);
        if (code < 0) { __write(1, "stat error\n", 11); __exit(4); }
        uint64_t buffer_size = *((uint64_t*)&statbuffer[48]);
        buffer = __mmap((void*)0, buffer_size, 0x01, 0x02, (int)file, 0); //PROT_READ, MAP_PRIVATE
        if (buffer == (void*)-1) { __write(1, "mmap error\n", 11); __exit(5); }
        buffer_end = buffer + buffer_size;
        p = buffer;
    }
    #else
    buffer_end = buffer + sizeof(buffer);
    p = buffer_end;
    #endif
    eof = 0;

    //State
    state = st_search_source_begin;
    read_benchmarks = 0;

    for (;;)
    {
        #ifdef USE_MAPPING
        eof = (p >= buffer_end);
        #else
        if (p >= buffer_end)
        {
            int64_t buffer_size = __read(file, buffer, sizeof(buffer));
            p = buffer;
            buffer_end = buffer + buffer_size;
            eof = (buffer_size == 0);
        }
        #endif
        if (!eof) c = *p;
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
            if (eof) { push_bi_connection(graph, source, destination, distance); state = st_endfile; } //EOF -> push connection, end
            else if (c == ' ' || c == '\t') { push_bi_connection(graph, source, destination, distance); state = st_search_endline; } //space -> push connection, search endline
            else if (c == '\n' || c == '\r') { push_bi_connection(graph, source, destination, distance); state = st_search_source_begin; } //endline -> push connection, start over
            else if (c >= '0' && c <= '9') distance = 10 * distance + (unsigned char)(c - '0'); //number -> modify distance, repeat
            else if (c == '.') { order = 1; state = st_search_distance_end; } //dot -> set order, search distance end
            else state = st_endfile; //else -> format error, end
            break;

        case st_search_distance_end:
            if (eof) { push_bi_connection(graph, source, destination, distance); state = st_endfile; } //EOF -> push connection, end
            else if (c == ' ' || c == '\t') { push_bi_connection(graph, source, destination, distance); state = st_search_endline; } //space -> push connection, search endline
            else if (c == '\n' || c == '\r') { push_bi_connection(graph, source, destination, distance); state = st_search_source_begin; } //endline -> push connection, start over
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
            code = __munmap(buffer, (uint64_t)(p - buffer));
            if (code < 0) { __write(1, "munmap error\n", 13); __exit(6); }
            #endif
            code = __close(file);
            if (code < 0) { __write(1, "close error\n", 12); __exit(7); }
            return;
        }
    }
}

static void solve_ver5(const struct ConnectionVectorVector *graph, const struct BenchmarkVector *benchmarks)
{
    //graph - RDI, begin to [rsp], length at [rsp+8], end to [rsp+16]
    //benchmark - RSI, begin at [rsp+24], end at [rsp+32]
    struct Candidate *candidates = __malloc(graph->length * sizeof(struct Candidate)); //RBX
    uint64_t candidates_length = 0; //RAX
    uint32_t *candidate_indices = __malloc(graph->length * sizeof(uint32_t)); //R12

    for (const struct Benchmark *benchmark = benchmarks->begin;
        benchmark < &benchmarks->begin[benchmarks->length];
        benchmark++)
    {
        const uint32_t source = benchmark->source; //stack [rsp+40]
        const uint32_t destination = benchmark->destination; //stack [rsp+48]
        candidates_length = 0;
        for (uint64_t i = 0; i < graph->length; i++) candidate_indices[i] = (uint32_t)-1;
        struct Candidate candidate = { .distance = 0.0, .int_distance = 0, .id = source }; //distance and int_distance in R13
        candidates_length = push_indexed_heap(candidates, candidates_length, candidate_indices, candidate);
        while (1)
        {
            if (candidates_length == 0)
            {
                candidate.distance = 1.0f / 0.0f;
                candidate.int_distance = 0;
                break;
            }
            candidate = pop_indexed_heap(candidates, candidates_length, candidate_indices);
            candidates_length--;
            if (candidate.id == destination) break;
            struct ConnectionVector *connections = &graph->begin[candidate.id];

            for (const struct Connection *connection = connections->begin;
                connection < &connections->begin[connections->length];
                connection++) //R14 and R15 for iteration
            {
                struct Candidate new_candidate = {
                    .distance = candidate.distance + connection->distance,
                    .int_distance = candidate.int_distance + 1,
                    .id = connection->destination
                };
                candidates_length = push_indexed_heap(candidates, candidates_length, candidate_indices, new_candidate);
            }
        }
        char buffer[256]; //stack [rsp+72] to [rsp+328]
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
