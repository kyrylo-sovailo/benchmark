extern __malloc_memory_begin
extern __malloc_memory_end
extern __malloc_memory_capacity_end

global print_integer
global print_float
global __malloc
global __free
global push_benchmark
global push_connection
global push_bi_connection
global pop_indexed_heap
global push_indexed_heap
global parse_ver5
global solve_ver5
global _start

section .rodata
string_file_name: db "dijkstra.txt", 0
string_brk_error_1: db "brk error 1", 10
string_brk_error_2: db "brk error 2", 10
string_brk_error_3: db "brk error 3", 10
string_open_error: db "open error", 10
string_close_error: db "close error", 10
string_graph: db "GRAPH"
string_benchmark: db "BENCHMARK"

section .data
__malloc_memory_begin:     dq 0
__malloc_memory_end:       dq 0
__malloc_memory_capacity_end: dq 0

section .text

; char *print_integer(char *p, uint64_t number)
print_integer:
    cld
    test rsi, rsi
    jnz print_integer_skip_early_exit           ; if (number == 0)
    mov al, '0'
    stosb                                       ; *p = '0'; p++;
    mov rax, rdi
    ret                                         ; return p;
    print_integer_skip_early_exit:

    xor rdx, rdx
    mov r8, 10

    mov rax, 1                                  ; uint64_t order = 1;
    print_integer_loop1:
    cmp rax, rsi
    ja print_integer_loop1_end                  ; while (order <= number)
        mul r8                                  ; order *= 10;
        jmp print_integer_loop1
    print_integer_loop1_end:

    print_integer_loop2:
        cmp rax, 1
        jbe print_integer_loop2_end             ; while (order > 1)
        div r8                                  ; order /= 10;
        mov rcx, rax                            ; order to rcx
        mov rax, rsi                            ; number to rax
        div rcx                                 ; uint64_t digit = number / order;
        add rax, '0'
        stosb                                   ; *p = '0' + (char)digit; p++;
        sub rax, '0'
        mul rcx                                 ; digit * order;
        sub rsi, rax                            ; number -= digit * order;
        mov rax, rcx                            ; restore order
        jmp print_integer_loop2
    print_integer_loop2_end:
    
    mov rax, rdi
    ret

; char *print_float(char *p, float number)
print_float:
    cld
    cvttss2si rsi, xmm0
    cvtsi2ss xmm1, rsi                          ; float integral = __floor(number);
    subss xmm0, xmm1                            ; number -= integral;
    sub rsp, 8
    call print_integer                          ; p = print_integer(p, (uint64_t)integral);
    mov rdi, rax
    add rsp, 8
    mov al, '.'
    stosb                                       ; *p = '.'; p++;

    mov rax, 10
    cvtsi2ss xmm2, rax
    mov rcx, 4
    print_float_loop:
        mulss xmm0, xmm2                        ; number *= 10;
        cvttss2si rax, xmm0
        cvtsi2ss xmm1, rax                      ; integral = __floor(number);
        subss xmm0, xmm1                        ; number -= integral;
        add rax, '0'
        stosb                                   ; *p = '0' + (char)integral; p++
        loop print_float_loop                   ; for (uint64_t i = 0; i < 4; i++)

    mov rax, rdi
    ret

; void *__malloc(uint64_t size)
__malloc:
    add rdi, 7
    and dil, 0xF8                               ; size = ((size + 7) / 8) * 8

    mov rcx, [__malloc_memory_end]              ; memory_end = __malloc_memory_end;
    
    test rcx, rcx
    jnz __malloc_dont_init                      ; if (memory_end == (void*)0)
        push rdi    ; size
        mov rax, 12
        mov rdi, 0xFFFFFFFFFFFFFFFF
        syscall                                 ; memory_end = __brk((void*)-1);
        mov rcx, rax
        pop rdi     ; size
        cmp rcx, 0xFFFFFFFFFFFFFFFF
        jne __malloc_dont_fail1                 ; if (memory_end == (void*)-1)
            mov rax, 1
            mov rdi, 1
            lea rsi, [string_brk_error_1]
            mov rdx, 12
            syscall                             ; __write(1, "brk error 1\n", 12);
            mov rax, 60
            mov rdi, 1
            syscall                             ; __exit(1);
        __malloc_dont_fail1:
        mov [__malloc_memory_begin], rcx        ; __malloc_memory_begin = memory_end;
        mov rdx, rcx                            ; memory_capacity_end = memory_end;
        jmp __malloc_do_init
    __malloc_dont_init:                         ; else
        mov rdx, [__malloc_memory_capacity_end] ; memory_capacity_end = __malloc_memory_capacity_end;
    __malloc_do_init:                           ; end else

    mov rax, rdx
    sub rax, rcx                                ; available_memory = (uint64_t)((char*)memory_capacity_end - (char*)memory_end);
    cmp rdi, rax
    jbe __malloc_dont_allocation                ; if (size > available_memory)
        mov rsi, rdi
        sub rsi, rax                            ; bytes_to_request = size - available_memory;
        add rsi, 0xFFFF
        and rsi, 0xFFFFFFFFFFFF0000             ; bytes_to_request = (bytes_to_request + CHUNK_SIZE - 1) & ~(CHUNK_SIZE - 1);
        add rdx, rsi                            ; memory_capacity_end = (char*)memory_capacity_end + bytes_to_request;
        sub rsp, 24         ; 24 = 32 - 8
        mov [rsp], rdi      ; size
        mov [rsp+8], rcx    ; memory_end
        mov [rsp+16], rdx   ; memory_capacity_end
        mov rax, 12
        mov rdi, rdx
        syscall                                 ; memory_capacity_end_copy = __brk(memory_capacity_end);
        mov rdx, [rsp+16]   ; memory_capacity_end
        mov rcx, [rsp+8]    ; memory_end
        mov rdi, [rsp]      ; size
        add rsp, 24
        cmp rax, rdx
        je __malloc_dont_fail2                  ; if (memory_capacity_end_copy != memory_capacity_end)
            mov rax, 1
            mov rdi, 1
            lea rsi, [string_brk_error_2]
            mov rdx, 12
            syscall                             ; __write(1, "brk error 2\n", 12);
            mov rax, 60
            mov rdi, 1
            syscall                             ; __exit(1);
        __malloc_dont_fail2:
    __malloc_dont_allocation:
    mov [__malloc_memory_capacity_end], rdx     ; __malloc_memory_capacity_end = memory_capacity_end;
    mov rax, rcx                                ; memory_end_copy = memory_end;
    add rcx, rdi                                ; memory_end = (char*)memory_end + size;
    mov [__malloc_memory_end], rcx              ; __malloc_memory_end = memory_end;
    ret                                         ; return memory_end_copy;

; static void __free(void)
__free:
    mov rdi, [__malloc_memory_begin]            ; memory_begin = __malloc_memory_begin;
    test rdi, rdi
    jz __free_dont_deallocate                   ; if (memory_begin != (void*)0)
        push rdi
        mov rax, 12
        syscall                                 ; memory_begin_copy = __brk(memory_begin);
        pop rdi
        cmp rdi, rax
        je __free_dont_fail                     ; if (memory_begin_copy != memory_begin)
            mov rax, 1
            mov rdi, 1
            lea rsi, [string_brk_error_3]
            mov rdx, 12
            syscall                             ; __write(1, "brk error 3\n", 12);
            mov rax, 60
            mov rdi, 1
            syscall                             ; __exit(1);
        __free_dont_fail:
    __free_dont_deallocate:
    ret

; void push_connection(struct ConnectionVector *connections, uint64_t destination, float distance)
push_connection:
    movd edx, xmm0                              ; copy xmm0 (float argument) to rdx (integer argument) and proceed with push_benchmark

; void push_benchmark(struct BenchmarkVector *benchmarks, uint64_t source, uint64_t destination)
push_benchmark:
    shl rdx, 32                                 ; struct Benchmark benchmark = { (uint32_t)source, (uint32_t)destination };
    or rdx, rsi
    mov rax, rdi                                ; benchmarks

    mov r8, [rax]                               ; benchmarks_begin = benchmarks->begin;
    mov r9d, [rax + 8]                          ; benchmarks_length = benchmarks->length;
    cmp r9d, [rax + 12]
    jne push_benchmark_dont_allocate            ; if (benchmarks_length == benchmarks->capacity)
        mov edi, [rax + 12]                     ; benchmarks_capacity = benchmarks->capacity;
        shl edi, 1                              ; benchmarks_capacity <<= 1;
        jne push_benchmark_dont_set_1           ; if (benchmarks_capacity == 0)
            mov edi, 1                          ; benchmarks_capacity = 1;
        push_benchmark_dont_set_1:
        mov [rax + 12], edi                     ; benchmarks->capacity = benchmarks_capacity;
        shl rdi, 3
        sub rsp, 40         ; 40 = 32 + 8
        mov [rsp], rax      ; benchmarks
        mov [rsp+8], rdx    ; source/destination
        mov [rsp+16], r9d   ; benchmarks_length
        mov [rsp+24], r8    ; benchmarks_begin
        call __malloc                           ; __malloc(benchmarks_capacity * sizeof(*dst));
        mov r8, rax
        mov rsi, [rsp+24]   ; benchmarks_begin  ; src = benchmarks_begin;
        mov r9d, [rsp+16]   ; benchmarks_length
        mov rdx, [rsp+8]    ; source/destination
        mov rax, [rsp]      ; benchmarks
        add rsp, 40
        mov [rax], r8                           ; benchmarks->begin = benchmarks_begin;
        test r9d, r9d
        jz push_benchmark_dont_loop             ; for (i = 0; i < benchmarks_length; i++)
            mov ecx, r9d
            mov rdi, r8                         ; dst = benchmarks_begin;
            cld
            push_benchmark_loop:
                movsq                           ; *dst = *src;
                loop push_benchmark_loop        ; for (i = 0; i < benchmarks_length; i++)
        push_benchmark_dont_loop:
    
    push_benchmark_dont_allocate:
    mov [r8 + r9 * 8], rdx                      ; benchmarks_begin[benchmarks_length] = benchmark;
    inc r9d                                     ; benchmarks_length++;
    mov [rax + 8], r9d                          ; benchmarks->length = benchmarks_length;
    ret

;  void push_bi_connection(struct ConnectionVectorVector *connections, uint64_t source, uint64_t destination, float distance)
push_bi_connection:
    sub rsp, 40 ; 40 = 32 + 8
    mov rax, rdi                                ; connections
    mov [rsp], rax
    mov r8, [rax]                               ; connections_begin = connections->begin;
    mov [rsp+8], esi
    mov [rsp+12], edx
    cmp esi, edx
    cmovg edx, esi                              ; source_destination_max = ((source > destination) ? source : destination);
    
    cmp edx, [rax + 8]
    jb push_bi_connection_dont_grow             ; if (source_destination_max >= connections->length)
        inc edx                                 ; new_connections_length = source_destination_max + 1;
        mov r9d, [rax + 8]                      ; connections_length = connections->length;
        mov [rax + 8], edx                      ; connections->length = (uint32_t)new_connections_length;
        cmp edx, [rax + 12]
        jbe push_bi_connection_dont_alloc       ; if (new_connections_length > connections->capacity)
            mov edi, edx
            dec edi                             ; rdi = max(pow(2, ceil(log2(edx))), 1)
            lzcnt ecx, edi
            mov edi, 0xFFFFFFFF
            shr rdi, cl
            inc edi
            shl rdi, 4
            mov [rsp+16], r8    ; connections_begin
            mov [rsp+24], edx   ; new_connections_length
            mov [rsp+28], r9d   ; connections_length
            call __malloc                                   ; __malloc(connections_capacity * sizeof(*dst));
            mov r8, rax
            mov r9d, [rsp+28]   ; connections_length
            mov edx, [rsp+24]   ; new_connections_length
            mov rsi, [rsp+16]   ; connections_begin         ; src = connections_begin;
            mov rax, [rsp]      ; connections
            mov [rax], r8                                   ; connections->begin = connections_begin;
            test r9d, r9d
            jz push_bi_connection_dont_loop                 ; for (uint64_t i = 0; i < connections_length; i++)
                mov ecx, r9d
                shl rcx, 1
                mov rdi, r8                                 ; dst = connections_begin;
                cld
                push_bi_connection_loop:
                    movsq                                   ; *dst = *src;
                    loop push_bi_connection_loop            ; for (uint64_t i = 0; i < connections_length; i++)
            push_bi_connection_dont_loop:
        push_bi_connection_dont_alloc:
        mov ecx, edx
        sub ecx, r9d
        jz push_bi_connection_dont_loop2                    ; for (uint64_t i = connections_length; i <= source_destination_max; i++)
            shl rcx, 1
            mov rsi, rax    ; push rax
            xor rax, rax                                    ; const struct ConnectionVector zero = { 0, 0, 0 };
            mov edi, r9d
            shl rdi, 4
            add rdi, r8                                     ; dst = connections_begin + connections_length;
            cld
            push_bi_connection_loop2:
                stosq                                       ; *dst = zero;
                loop push_bi_connection_loop2               ; for (uint64_t i = connections_length; i <= source_destination_max; i++)
            mov rax, rsi    ; pop rax
        push_bi_connection_dont_loop2:
    push_bi_connection_dont_grow:

    mov [rsp+16], r8    ; connections_begin
    
    mov edi, [rsp+8]    ; source
    shl rdi, 4
    add rdi, r8
    mov esi, [rsp+12]   ; destination
    call push_connection
    
    mov edi, [rsp+12]   ; destination
    shl rdi, 4
    add rdi, [rsp+16]
    mov esi, [rsp+8]    ; source
    call push_connection
    add rsp, 40         ; 40 = 32 + 8
    ret

; uint64_t push_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices, struct Candidate element)

; uint64_t push_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices, struct Candidate element)
push_indexed_heap:
    mov eax, [rdx + r8 * 4]                     ; uint64_t index = indices[element.id];
    cmp eax, 0xFFFFFFFE
    je push_indexed_heap_early_exit             ; if (index == (uint32_t)-2)
    movq xmm0, rcx
    jb push_indexed_heap_normal                 ; if (index == (uint32_t)-1)
        mov rcx, rsi                            ; index = length;
        inc rsi                                 ; length++;
        mov rax, rcx
        shl rax, 4
        add rax, rdi                            ; index_pointer = &data[index];
        jmp push_indexed_heap_loop
    push_indexed_heap_normal:
        mov rcx, rax
        shl rax, 4
        add rax, rdi                            ; index_pointer = &data[index];
        movss xmm1, [rax]
        ucomiss xmm0, xmm1
        jae push_indexed_heap_early_exit        ; if (element.distance >= index_pointer->distance) return length;

    push_indexed_heap_loop:
        jrcxz push_indexed_heap_exit            ; if (index == 0)
        mov r9, rcx
        dec r9
        shr r9, 1                               ; uint64_t parent_i = (index - 1) / 2;
        mov r10, r9
        shl r10, 4
        add r10, rdi                            ; struct Candidate *c = &data[parent_i];
        movss xmm1, [r10]
        ucomiss xmm0, xmm1
        jae push_indexed_heap_exit              ; if (element.distance >= c->distance)
        mov r11, [r10]
        mov [rax], r11
        mov r11, [r10 + 8]
        mov [rax + 8], r11                      ; *index_pointer = *c;
        mov [rdx + r11 * 4], ecx                ; indices[index_pointer->id] = index;
        mov rcx, r9                             ; index = parent_i;
        mov rax, r10                            ; index_pointer = c;
        jmp push_indexed_heap_loop

    push_indexed_heap_exit:
    movq [rax], xmm0
    mov [rax + 8], r8
    mov [rdx + r8 * 4], ecx

    push_indexed_heap_early_exit:
    mov rax, rsi
    ret

; struct Candidate pop_indexed_heap(struct Candidate *data, uint64_t length, uint32_t *indices)
pop_indexed_heap:
    movups xmm0, [rdi]                          ; struct Candidate top = data[0];
    movups xmm1, xmm0
    punpckhqdq xmm1, xmm1                       ; stackoverflow.com/questions/41222574
    movq rcx, xmm1
    mov eax, 0xFFFFFFFE
    mov [rdx + rcx * 4], eax                    ; indices[top.id] = (uint32_t)-2;
    dec rsi                                     ; uint64_t length_minus_1 = length - 1;
    jz pop_indexed_heap_early_exit              ; if (length_minus_1 == 0)

    mov rcx, rsi
    shl rcx, 4
    add rcx, rdi
    movups xmm1, [rcx]                          ; struct Candidate buffer = data[length_minus_1];
    xor rcx, rcx                                ; uint64_t index = 0;
    mov rax, rdi                                ; struct Candidate *index_pointer = &data[0];

    pop_indexed_heap_loop:
        lea r8, [rcx * 2 + 1]                   ; uint64_t left_i = 2 * i + 1;
        mov r9, r8
        shl r9, 4
        add r9, rdi                             ; struct Candidate *c = &data[left_index];
        cmp r8, rsi
        ja pop_indexed_heap_exit                ; if (left_i > length_minus_1) ...
        movss xmm2, [r9]
        je pop_indexed_heap_compare_exit        ; if (left_i == length_minus_1) //no right leaf
            movss xmm3, [r9 + 16]
            ucomiss xmm2, xmm3
            jb pop_indexed_heap_compare_exit    ; if (c->distance < (c + 1)->distance)
                inc r8
                add r9, 16                      ; c++
                movss xmm2, xmm3
        
        pop_indexed_heap_compare_exit:
        ucomiss xmm2, xmm1
        jae pop_indexed_heap_exit               ; if (c->distance < buffer.distance) ...
        mov r10, [r9]
        mov [rax], r10
        mov r10, [r9 + 8]
        mov [rax + 8], r10                      ; *index_pointer = *c;
        mov [rdx + r10 * 4], ecx                ; indices[index_pointer->id] = index;
        mov rcx, r8                             ; index = left_index|right_index
        mov rax, r9                             ; index_pointer = c;
        jmp pop_indexed_heap_loop

    pop_indexed_heap_exit:
    movups [rax], xmm1                          ; *index_pointer = buffer;
    punpckhqdq xmm1, xmm1
    movq rax, xmm1
    mov [rdx + rax * 4], ecx                    ; indices[buffer.id] = index;
    
    pop_indexed_heap_early_exit:
    movq rax, xmm0
    punpckhqdq xmm0, xmm0
    movq rdx, xmm0
    ret                                         ; return top;

; void parse_ver5(struct ConnectionVectorVector *graph, struct BenchmarkVector *benchmarks)
parse_ver5:
    mov r14, rdi
    mov r15, rsi
    push rbp
    mov rbp, rsp                                ; buffer_end = buffer + sizeof(buffer);
    sub rsp, 0x10030

    mov rax, 2
    mov rdi, string_file_name
    xor rsi, rsi
    syscall                                     ; file = __open("dijkstra.txt", /*O_RDONLY*/0);
    jge parse_ver5_open_success
        mov rax, 1
        mov rdi, string_open_error
        mov rsi, 12
        syscall                                 ; __write(1, "open error\n", 11);
        mov rax, 60
        mov rdi, 3
        syscall                                 ; __exit(3);
    parse_ver5_open_success:
    mov [rsp], rax
    mov r12, rbp                                ; p = buffer_end;
    xor r13b, r13b                              ; eof = 0; read_benchmarks = 0;

    parse_ver5_switch_search_source_begin:
    lea rbx, [parse_ver5_search_source_begin]
    jmp parse_ver5_loop
    parse_ver5_switch_search_source_end:
    lea rbx, [parse_ver5_search_source_end]
    jmp parse_ver5_loop
    parse_ver5_switch_search_destination_begin:
    lea rbx, [parse_ver5_search_destination_begin]
    jmp parse_ver5_loop
    parse_ver5_switch_search_destination_end:
    lea rbx, [parse_ver5_search_destination_end]
    jmp parse_ver5_loop
    parse_ver5_switch_search_distance_begin:
    lea rbx, [parse_ver5_search_distance_begin]
    jmp parse_ver5_loop
    parse_ver5_switch_search_distance_dot:
    lea rbx, [parse_ver5_search_distance_dot]
    jmp parse_ver5_loop
    parse_ver5_switch_search_distance_end:
    lea rbx, [parse_ver5_search_distance_end]
    jmp parse_ver5_loop
    parse_ver5_switch_search_graph_end:
    lea rbx, [parse_ver5_search_graph_end]
    jmp parse_ver5_loop
    parse_ver5_switch_search_benchmark_end:
    lea rbx, [parse_ver5_search_benchmark_end]
    jmp parse_ver5_loop
    parse_ver5_switch_search_endline:
    lea rbx, [parse_ver5_search_endline]
    ;jmp parse_ver5_loop

    parse_ver5_loop:
        cmp r12, rbp
        jb parse_ver5_buffer_not_empty              ; if (p >= buffer_end)
            mov [rsp+8], rsi
            mov [rsp+16], rdx
            movss [rsp+24], xmm0
            movss [rsp+28], xmm1
            mov [rsp+32], rcx
            xor rax, rax
            mov rdi, [rsp]
            lea rsi, [rbp - 0x10000]
            mov rdx, 0x10000
            syscall
            mov r12, rbp
            sub r12, rax                            ; p = buffer
            test rax, rax
            jnz parse_ver5_file_not_empty
                or r13b, 0x01                       ; eof = (buffer_size == 0);
                jmp parse_ver5_restore_read
            parse_ver5_file_not_empty:
            cmp rax, 0x10000
            je parse_ver5_restore_read
            cld                                     ; Shifting buffer if it wasn't full
            mov rcx, rax
            lea rsi, [rbp - 0x10000]
            mov rdi, r12
            parse_ver5_buffer_shift:
                movsb
                loop parse_ver5_buffer_shift
            parse_ver5_restore_read:
            mov rcx, 10
            cvtsi2ss xmm2, rcx                      ; Constant 10 in xmm2
            mov rsi, [rsp+8]
            mov rdx, [rsp+16]
            movss xmm0, [rsp+24]
            movss xmm1, [rsp+28]
            mov rcx, [rsp+32]
        parse_ver5_buffer_not_empty:
        movzx rax, BYTE [r12]                       ; c = *p;
        inc r12                                     ; p++;
        jmp rbx                                     ; switch (state)

        parse_ver5_search_source_begin:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; if (eof) state = st_endfile;
            cmp al, '0'
            jb parse_ver5_search_source_begin_below
            cmp al, '9'
            ja parse_ver5_search_source_begin_above
            sub al, '0'
            movzx rsi, al                           ; source = (unsigned char)((unsigned char)(c - '0'));
            jmp parse_ver5_switch_search_source_end ; state = st_search_source_end;
            
            parse_ver5_search_source_begin_below:
            cmp al, ' '
            je parse_ver5_loop                      ; {}
            cmp al, 9
            je parse_ver5_loop                      ; {}
            cmp al, 10
            je parse_ver5_loop                      ; {}
            cmp al, 13
            je parse_ver5_loop                      ; {}
            jmp parse_ver5_loop_end                 ; state = st_endfile;

            parse_ver5_search_source_begin_above:
            cmp al, 'G'
            je parse_ver5_search_source_begin_g     ; if (c == 'G')
            cmp al, 'B'
            je parse_ver5_search_source_begin_b     ; if (c == 'B')
            jmp parse_ver5_loop_end                 ; state = st_endfile;
            parse_ver5_search_source_begin_g:
                mov rsi, 1                          ; keyword_check = 1;
                jmp parse_ver5_switch_search_graph_end
            parse_ver5_search_source_begin_b:
                mov rsi, 1                          ; keyword_check = 1;
                jmp parse_ver5_switch_search_benchmark_end
        
        parse_ver5_search_source_end:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; if (eof) state = st_endfile;
            cmp al, '0'
            jb parse_ver5_search_source_end_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_source_end_above
            sub al, '0'
            mov rcx, rsi
            shl rsi, 3
            lea rsi, [rsi + 2 * rcx]
            add rsi, rax                            ; source = 10 * source + (unsigned char)(c - '0');
            jmp parse_ver5_loop

            parse_ver5_search_source_end_below:
            cmp al, ' '
            je parse_ver5_switch_search_destination_begin
            cmp al, 9
            je parse_ver5_switch_search_destination_begin
            jmp parse_ver5_loop_end                 ; state = st_endfile;

        parse_ver5_search_destination_begin:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; if (eof) state = st_endfile;
            cmp al, '0'
            jb parse_ver5_search_destination_begin_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_destination_begin_above
            sub al, '0'
            movzx rdx, al                           ; source = (unsigned char)((unsigned char)(c - '0'));
            jmp parse_ver5_switch_search_destination_end

            parse_ver5_search_destination_begin_below:
            cmp al, ' '
            je parse_ver5_loop
            cmp al, 9
            je parse_ver5_loop
            jmp parse_ver5_loop_end                 ; state = st_endfile;

        parse_ver5_search_destination_end:
            cmp r13b, 0x01
            je parse_ver5_loop_end                  ; state = st_endfile;
            cmp r13b, 0x03
            jne parse_ver5_search_destination_end_skip_call
            mov rdi, r15
            call push_benchmark                     ; push_benchmark(benchmarks, source, destination);
            jmp parse_ver5_loop_end                 ; state = st_endfile;
            parse_ver5_search_destination_end_skip_call:
            cmp al, '0'
            jb parse_ver5_search_destination_end_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_destination_end_above
            sub al, '0'
            mov rcx, rdx
            shl rdx, 3
            lea rdx, [rdx + 2 * rcx]
            add rdx, rax                            ; destination = 10 * destination + (unsigned char)(c - '0');
            jmp parse_ver5_loop

            parse_ver5_search_destination_end_below:
            cmp al, ' '
            je parse_ver5_search_destination_end_space
            cmp al, 9
            je parse_ver5_search_destination_end_space
            cmp al, 10
            je parse_ver5_search_destination_end_line
            cmp al, 13
            je parse_ver5_search_destination_end_line
            jmp parse_ver5_loop_end
            
            parse_ver5_search_destination_end_space:
            test r13b, 0x02
            jz parse_ver5_switch_search_distance_begin
            mov rdi, r15
            call push_benchmark                     ; push_benchmark(benchmarks, source, destination);
            jmp parse_ver5_switch_search_endline    ; state = st_search_endline;
            parse_ver5_search_destination_end_line:
            test r13b, 0x02
            jz parse_ver5_switch_search_endline     ; state = st_search_endline;
            mov rdi, r15
            call push_benchmark                     ; push_benchmark(benchmarks, source, destination);
            jmp parse_ver5_switch_search_source_begin; state = st_search_source_begin;

        parse_ver5_search_distance_begin:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; if (eof) state = st_endfile;
            cmp al, '0'
            jb parse_ver5_search_distance_begin_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_distance_begin_above
            sub al, '0'
            movzx rcx, al                           ; distance = (unsigned char)(c - '0');
            jmp parse_ver5_switch_search_distance_dot
            
            parse_ver5_search_distance_begin_below:
            cmp al, ' '
            je parse_ver5_loop
            cmp al, 9
            je parse_ver5_loop
            jmp parse_ver5_loop_end

        parse_ver5_search_distance_dot:
            test r13b, 0x01
            jnz parse_ver5_search_distance_dot_call1
            cmp al, '0'
            jb parse_ver5_search_distance_dot_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_distance_dot_above
            sub al, '0'
            mov r8, rcx
            shl rcx, 3
            lea rcx, [rcx + 2 * r8]
            add rcx, rax                            ; distance = 10 * distance + (unsigned char)(c - '0');
            jmp parse_ver5_loop

            parse_ver5_search_distance_dot_below:
            cmp al, '.'
            jne parse_ver5_search_distance_dot_below2
            cvtsi2ss xmm0, rcx                      ; Distance is float now
            mov rcx, 1
            cvtsi2ss xmm1, rcx                      ; order = 1;
            jmp parse_ver5_switch_search_distance_end

            parse_ver5_search_distance_dot_below2:
            cmp al, ' '
            je parse_ver5_search_distance_dot_call2
            cmp al, 9
            je parse_ver5_search_distance_dot_call2
            cmp al, 10
            je parse_ver5_search_distance_dot_call3
            cmp al, 13
            je parse_ver5_search_distance_dot_call3
            jmp parse_ver5_loop_end

            parse_ver5_search_distance_dot_call1:
            mov rdi, r14
            cvtsi2ss xmm0, rcx
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_loop_end
            parse_ver5_search_distance_dot_call2:
            mov rdi, r14
            cvtsi2ss xmm0, rcx
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_switch_search_endline
            parse_ver5_search_distance_dot_call3:
            mov rdi, r14
            cvtsi2ss xmm0, rcx
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_switch_search_source_begin

        parse_ver5_search_distance_end:
            test r13b, 0x01
            jnz parse_ver5_search_distance_end_call1
            cmp al, '0'
            jb parse_ver5_search_distance_end_below
            cmp al, '9'
            ja parse_ver5_loop_end ;parse_ver5_search_distance_end_above
            mov r8, 10
            divss xmm1, xmm2                        ; order /= 10;
            sub al, '0'
            cvtsi2ss xmm3, rax
            mulss xmm3, xmm1
            addss xmm0, xmm3                        ; distance += order * (unsigned char)(c - '0');
            jmp parse_ver5_loop

            parse_ver5_search_distance_end_below:
            cmp al, ' '
            je parse_ver5_search_distance_end_call2
            cmp al, 9
            je parse_ver5_search_distance_end_call2
            cmp al, 10
            je parse_ver5_search_distance_end_call3
            cmp al, 13
            je parse_ver5_search_distance_end_call3
            jmp parse_ver5_loop_end

            parse_ver5_search_distance_end_call1:
            mov rdi, r14
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_loop_end
            parse_ver5_search_distance_end_call2:
            mov rdi, r14
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_switch_search_endline
            parse_ver5_search_distance_end_call3:
            mov rdi, r14
            call push_bi_connection                 ; push_bi_connection(graph, source, destination, distance);
            jmp parse_ver5_switch_search_source_begin

        parse_ver5_search_graph_end:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; state = st_endfile;
            cmp al, [ string_graph + rsi ]
            jne parse_ver5_loop_end                 ; if (c == "GRAPH"[keyword_check])
            inc rsi                                 ; keyword_check++;
            cmp rsi, 5
            jne parse_ver5_loop                     ; if (keyword_check == 5)
            and r13b, 0xFD                          ; read_benchmarks = 0;
            jmp parse_ver5_switch_search_endline    ; state = st_search_endline;

        parse_ver5_search_benchmark_end:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; state = st_endfile;
            cmp al, [ string_benchmark + rsi ]
            jne parse_ver5_loop_end                 ; if (c == "BENCHMARK"[keyword_check])
            inc rsi                                 ; keyword_check++;
            cmp rsi, 9
            jne parse_ver5_loop                     ; if (keyword_check == 9)
            or r13b, 0x02                           ; read_benchmarks = 1;
            jmp parse_ver5_switch_search_endline    ; state = st_search_endline;

        parse_ver5_search_endline:
            test r13b, 0x01
            jnz parse_ver5_loop_end                 ; state = st_endfile;
            cmp al, ' '
            je parse_ver5_loop                      ; {}
            cmp al, 9
            je parse_ver5_loop                      ; {}
            cmp al, 10
            je parse_ver5_switch_search_source_begin; state = st_search_source_begin;
            cmp al, 13
            je parse_ver5_switch_search_source_begin; state = st_search_source_begin;
            jmp parse_ver5_loop_end                 ; state = st_endfile;
    
    parse_ver5_loop_end:
    mov rax, 3
    mov rdi, [rsp]
    syscall                                     ; code = __close(file);
    jge parse_ver5_close_success
        mov rax, 1
        mov rdi, string_close_error
        mov rsi, 12
        syscall                                 ; __write(1, "open error\n", 12);
        mov rax, 60
        mov rdi, 7
        syscall                                 ; __exit(7);
    parse_ver5_close_success:

    add rsp, 0x10030
    pop rbp
    ret

; void solve_ver5(const struct ConnectionVectorVector *graph, const struct BenchmarkVector *benchmarks)
solve_ver5:
    push rbp
    mov rbp, rsp
    sub rsp, 328

    mov rax, [rsi]                                  ; Storing benchmarks at stack
    mov [rsp+24], rax
    mov ecx, [rsi + 8]
    shl ecx, 3
    add rax, rcx
    mov [rsp+32], rax

    mov rax, [rdi]                                  ; Storing graph at stack
    mov [rsp], rax
    mov ecx, [rdi + 8]
    mov [rsp+8], rcx
    shl rcx, 4
    add rax, rcx
    mov [rsp+16], rax

    mov rdi, [rsp+8]
    shl rdi, 4
    call __malloc                                   ; struct Candidate *candidates = __malloc(graph->length * sizeof(struct Candidate));
    mov rbx, rax
    mov rdi, [rsp+8]
    shl rdi, 2
    call __malloc                                   ; uint32_t *candidate_indices = __malloc(graph->length * sizeof(uint32_t));
    mov r12, rax

    solve_ver5_loop1:
        mov rax, [rsp+24]
        cmp rax, [rsp+32]
        jae solve_ver5_loop1_exit                   ; for (const struct Benchmark *benchmark = benchmarks->begin; ...)
        mov ecx, [rax]
        mov [rsp+40], rcx                           ; uint32_t source = benchmark->source;
        mov ecx, [rax + 4]
        mov [rsp+48], rcx                           ; uint32_t destination = benchmark->destination;
        
        mov rcx, [rsp+8]                            ; for (uint64_t i = 0; i < graph->length; i++) candidate_indices[i] = (uint32_t)-1;
        test rcx, rcx
        jz solve_ver5_memset_end
            shr rcx, 1
            mov rax, 0xFFFFFFFFFFFFFFFF
            mov rdi, r12
            solve_ver5_memset:
                stosq
                loop solve_ver5_memset
            mov rcx, [rsp+8]
            test rcx, 0x1
            jz solve_ver5_memset_end
            mov [rdi], ecx
        solve_ver5_memset_end:

        mov rdi, rbx
        xor rsi, rsi
        mov rdx, r12
        xor rcx, rcx
        mov r8, [rsp+40]
        call push_indexed_heap                      ; push_indexed_heap(candidates, candidates_length, candidate_indices, candidate);
        
        xor r13, r13                                ; .distance = 0.0, .int_distance = 0
        
        test rax, rax
        jz solve_ver5_loop2_fail                    ; while (candidates_length != 0)
        solve_ver5_loop2:
            mov rdi, rbx
            mov rsi, rax
            mov rdx, r12
            xchg rax, r13                           ; Save candidates_length
            call pop_indexed_heap                   ; candidate = pop_indexed_heap(candidates, candidates_length, candidate_indices);
            cmp rdx, [rsp+48]
            je solve_ver5_loop2_success             ; if (candidate.id == destination) break;
            xchg rax, r13                           ; Restore candidates_length
            dec rax                                 ; candidates_length--;
            shl rdx, 4
            add rdx, [rsp]
            mov r14, [rdx]                          ; struct Connection *connection = connections->begin;
            mov r15d, [rdx + 8]
            shl r15, 3
            add r15, r14

            cmp r14, r15
            jae solve_ver5_loop3_exit               ; for (connection < &connections->begin[connections->length])
            solve_ver5_loop3:
                movd xmm0, r13d
                addss xmm0, [r14 + 4]
                movd ecx, xmm0                      ; .distance = candidate.distance + connection->distance,
                mov rdx, r13
                shr rdx, 32
                inc rdx                             ; .int_distance = candidate.int_distance + 1,
                shl rdx, 32
                or rcx, rdx
                mov r8d, DWORD [r14]                ; .id = connection->destination

                mov rdi, rbx
                mov rsi, rax
                mov rdx, r12
                call push_indexed_heap              ; candidates_length = push_indexed_heap(candidates, candidates_length, candidate_indices, new_candidate);

                add r14, 8                          ; connection++
                cmp r14, r15
                jb solve_ver5_loop3                 ; for (const struct Connection *connection ...)
            solve_ver5_loop3_exit:
            test rax, rax
            jnz solve_ver5_loop2                    ; while (candidates_length != 0)
        solve_ver5_loop2_fail:
        mov r13d, 0x7f800000                        ; .distance = 0.0/0.0, .int_distance = 0
        jmp solve_ver5_loop2_exit
        solve_ver5_loop2_success:
        mov r13, rax
        solve_ver5_loop2_exit:

        lea rdi, [rsp+72]
        mov rsi, [rsp+40]
        call print_integer                          ; p = print_integer(p, source);
        mov rdi, rax
        cld
        mov al, ' '
        stosb
        mov al, '-'
        stosb
        mov al, '>'
        stosb
        mov al, ' '
        stosb
        mov rsi, [rsp+48]
        call print_integer                          ; p = print_integer(p, destination);
        mov rdi, rax
        cld
        mov al, ':'
        stosb
        mov al, ' '
        stosb
        movd xmm0, r13d
        call print_float                            ; p = print_float(p, candidate.distance);
        mov rdi, rax
        cld
        mov al, ' '
        stosb
        mov al, '('
        stosb
        mov rsi, r13
        shr rsi, 32
        call print_integer                          ; p = print_integer(p, candidate.int_distance);
        mov rdi, rax
        cld
        mov al, ')'
        stosb
        mov al, 10
        stosb
        mov rdx, rdi
        sub rdx, rsp
        sub rdx, 72
        mov rax, 1
        mov rdi, 1
        lea rsi, [rsp+72]
        syscall                                     ; __write(1, buffer, (uint64_t)(p - buffer));

        mov rax, [rsp+24]
        add rax, 8                                  ; benchmark++
        mov [rsp+24], rax
        jmp solve_ver5_loop1
        
    solve_ver5_loop1_exit:
    
    add rsp, 328
    pop rbp
    ret

; void _start(void)
_start:
    and spl, 0xF0           ; Setup pointers
    mov rbp, rsp

    sub rsp, 32             ; Zero stack
    xor rax, rax
    mov rdi, rsp
    stosq
    stosq
    stosq
    stosq

    lea rdi, [rsp]
    lea rsi, [rsp + 16]
    call parse_ver5         ; parse_ver5(&graph, &benchmarks);

    lea rdi, [rsp]
    lea rsi, [rsp + 16]
    call solve_ver5         ; solve_ver5(&graph, &benchmarks);

    call __free             ; __free();

    mov rax, 60
    xor rdi, rdi
    syscall                 ; __exit(0);

section .note.GNU-stack
    dd 0
