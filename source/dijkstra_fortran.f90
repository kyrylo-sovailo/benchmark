 

program dijkstra

    implicit none

    type :: Candidate
        integer :: id
        integer :: int_distance
        real :: distance
    end type Candidate

    type :: Benchmark
        integer :: source
        integer :: destination
    end type Benchmark

    type :: BenchmarkVector
        type(Benchmark), dimension(:), pointer :: array => null()
        integer :: length = 0
    end type BenchmarkVector

    type :: Connection
        integer :: destination
        real :: distance
    end type Connection

    type :: ConnectionVector
        type(Connection), dimension(:), pointer :: array => null()
        integer :: length = 0
    end type ConnectionVector

    type :: ConnectionVectorVector
        type(ConnectionVector), dimension(:), pointer :: array => null()
        integer :: length = 0
    end type ConnectionVectorVector

    call main_ver5()

contains

    subroutine push_BenchmarkVector(vector, element)
        type(BenchmarkVector), intent(inout) :: vector
        type(Benchmark), intent(in) :: element
        type(Benchmark), dimension(:), pointer :: new_array
        integer :: old_capacity, new_capacity
        vector%length = vector%length + 1
        if (associated(vector%array)) then
            old_capacity = size(vector%array)
        else
            old_capacity = 0
        end if
        if (vector%length > old_capacity) then
            if (old_capacity == 0) then
                new_capacity = 1
            else
                new_capacity = 2 * old_capacity
            end if
            allocate(new_array(new_capacity))
            if (old_capacity /= 0) new_array(1:old_capacity) = vector%array
            vector%array => new_array
        end if
        vector%array(vector%length) = element
    end subroutine push_BenchmarkVector

    subroutine push_ConnectionVector(vector, element)
        type(ConnectionVector), intent(inout) :: vector
        type(Connection), intent(in) :: element
        type(Connection), dimension(:), pointer :: new_array
        integer :: old_capacity, new_capacity
        vector%length = vector%length + 1
        if (associated(vector%array)) then
            old_capacity = size(vector%array)
        else
            old_capacity = 0
        end if
        if (vector%length > old_capacity) then
            if (old_capacity == 0) then
                new_capacity = 1
            else
                new_capacity = 2 * old_capacity
            end if
            allocate(new_array(new_capacity))
            if (old_capacity /= 0) new_array(1:old_capacity) = vector%array
            vector%array => new_array
        end if
        vector%array(vector%length) = element
    end subroutine push_ConnectionVector

    subroutine grow_ConnectionVectorVector(vector, length)
        type(ConnectionVectorVector), intent(inout) :: vector
        integer, intent(in) :: length
        type(ConnectionVector), dimension(:), pointer :: new_array
        integer :: old_capacity, new_capacity
        if (length <= vector%length) return
        if (associated(vector%array)) then
            old_capacity = size(vector%array)
        else
            old_capacity = 0
        end if
        if (length > old_capacity) then
            if (old_capacity == 0) then
                new_capacity = 1
            else
                new_capacity = old_capacity
            end if
            do while (new_capacity < length)
                new_capacity = 2 * new_capacity
            end do
            allocate(new_array(new_capacity))
            if (old_capacity /= 0) new_array(1:old_capacity) = vector%array
            vector%array => new_array
        end if
        vector%length = length
    end subroutine grow_ConnectionVectorVector

    subroutine push_indexed_heap(data, data_length, indices, element)
        type(Candidate), intent(inout), dimension(:) :: data
        integer, intent(inout) :: data_length
        integer, intent(inout), dimension(:) :: indices
        type(Candidate), intent(in) :: element
        integer :: index, parent_index
        logical :: parent_exists, index_moved

        index = indices(element%id)
        if (index == -1) then
            index = data_length + 1
            data_length = data_length + 1
        else if (index == -2) then
            return
        else
            if (element%distance >= data(index)%distance) then
                return
            end if
        end if

        do while (.true.)
            parent_exists = index > 1
            index_moved = .false.
            if (parent_exists) then
                parent_index = index / 2
                if (element%distance < data(parent_index)%distance) then
                    data(index) = data(parent_index)
                    indices(data(index)%id) = index
                    index = parent_index
                    index_moved = .true.
                end if
            end if

            if (.not. index_moved) then
                data(index) = element
                indices(element%id) = index
                exit
            end if
        end do
    end subroutine push_indexed_heap

    function pop_indexed_heap(data, data_length, indices) result(top)
        type(Candidate), intent(inout), dimension(:) :: data
        integer, intent(inout) :: data_length
        integer, intent(inout), dimension(:) :: indices
        type(Candidate) :: top, back
        integer :: index, left_index, right_index, next_index
        logical :: left_exists, right_exists, index_moved

        top = data(1)
        indices(top%id) = -2
        back = data(data_length)
        data_length = data_length - 1
        if (data_length == 0) then
            return !If the front is the back, the algorithm no longer works
        end if

        index = 1
        do while (.true.)
            left_index = 2 * index
            right_index = 2 * index + 1
            left_exists = (left_index <= data_length)
            right_exists = (right_index <= data_length)

            index_moved = .false.
            if (left_exists .or. right_exists) then
                index_moved = .false.
                if (left_exists .and. right_exists) then
                    if (data(left_index)%distance < data(right_index)%distance) then
                        next_index = left_index
                    else
                        next_index = right_index
                    end if
                else
                    next_index = left_index
                end if

                if (data(next_index)%distance < back%distance) then
                    data(index) = data(next_index)
                    indices(data(index)%id) = index
                    index = next_index
                    index_moved = .true.
                end if
            end if

            if (.not. index_moved) then
                data(index) = back
                indices(back%id) = index
                exit
            end if
        end do
    end function pop_indexed_heap

    subroutine parse_ver5(graph, benchmarks)
        type(ConnectionVectorVector), intent(out) :: graph
        type(BenchmarkVector), intent(out) :: benchmarks

        integer :: source, destination
        real :: distance
        character(256) :: line
        integer :: line_length, line_spaces
        integer :: status
        logical :: read_benchmarks, is_whitespace

        open(unit=10, iostat=status, file="dijkstra.txt", status="old", action="read")
        if (status /= 0) then
            print *, "open() failed"
            stop
        end if

        ! Fortran implementation does not verify whether line contains any unused arguments
        ! and therefore is inferior to all other implementations
        read_benchmarks = .false.
        do while (.true.)
            read(10, '(A)', iostat=status) line
            if (status /= 0) exit
            line_length = len_trim(line)
            line_spaces = 0
            do while ((line_spaces < line_length) .and. &
                (line(line_spaces+1:line_spaces+1) == ' ' .or. line(line_spaces+1:line_spaces+1) == '\t'))
                line_spaces = line_spaces + 1
            end do
            if (line_spaces /= 0) then
                line = line(line_spaces+1:)
            end if

            if (line_spaces == line_length) then
                ! Continue
            else if (line(1:line_length-line_spaces) == "GRAPH") then
                read_benchmarks = .false.
            else if (line(1:line_length-line_spaces) == "BENCHMARK") then
                read_benchmarks = .true.
            else if (read_benchmarks) then
                read(line, *, iostat=status) source, destination
                if (status /= 0) then
                    exit
                end if
                source = source + 1
                destination = destination + 1
                call push_BenchmarkVector(benchmarks, Benchmark(source, destination))
            else
                read(line, *, iostat=status) source, destination, distance
                if (status /= 0) then
                    exit
                end if
                !write (*,'(I0.1, A, I0.1, A, F0.4)') &
                !    source, ' -> ', destination, ': ', distance
                source = source + 1
                destination = destination + 1
                call grow_ConnectionVectorVector(graph, max(source, destination))
                call push_ConnectionVector(graph%array(source), Connection(destination, distance))
                call push_ConnectionVector(graph%array(destination), Connection(source, distance))
            end if
        end do

        close(10)
    end subroutine parse_ver5

    subroutine solve_ver5(graph, benchmarks)
        type(ConnectionVectorVector), intent(in) :: graph
        type(BenchmarkVector), intent(in) :: benchmarks

        type(Candidate), dimension(:), pointer :: candidates
        integer :: candidates_length
        integer, dimension(:), pointer :: candidate_indices
        integer :: benchmark_i, connection_i
        type(Candidate) :: current_candidate, new_candidate
        type(ConnectionVector), pointer :: connections
        integer :: source, destination
        integer :: int_distance
        real :: distance

        allocate(candidate_indices(graph%length))
        allocate(candidates(graph%length))

        do benchmark_i = 1, benchmarks%length
            source = benchmarks%array(benchmark_i)%source
            destination = benchmarks%array(benchmark_i)%destination
            candidates_length = 0
            candidate_indices = -1
            call push_indexed_heap(candidates, candidates_length, candidate_indices, Candidate(source, 0, 0.0))
            int_distance = 0
            distance = huge(1.0)

            do while (candidates_length /= 0)
                current_candidate = pop_indexed_heap(candidates, candidates_length, candidate_indices)
                if (current_candidate%id == destination) then
                    int_distance = current_candidate%int_distance
                    distance = current_candidate%distance
                    exit
                end if

                connections => graph%array(current_candidate%id)
                do connection_i = 1, connections%length
                    new_candidate = Candidate(connections%array(connection_i)%destination, current_candidate%int_distance + 1, &
                        current_candidate%distance + connections%array(connection_i)%distance)
                    call push_indexed_heap(candidates, candidates_length, candidate_indices, new_candidate)
                end do
            end do

            if (distance /= huge(1.0)) then
                write (*,'(I0.1, A, I0.1, A, F0.4, A, I0.1, A)') &
                    source-1, ' -> ', destination-1, ': ', distance, ' (', int_distance, ')'
            else
                write (*,'(I0.1, A, I0.1, A)') &
                    source-1, ' -> ', destination-1, ': inf (0)'
            end if
        end do
        
        deallocate(candidate_indices)
        deallocate(candidates)
    end subroutine solve_ver5

    subroutine main_ver5()
        type(ConnectionVectorVector) :: graph
        type(BenchmarkVector) :: benchmarks
        integer :: node_i

        call parse_ver5(graph, benchmarks)
        call solve_ver5(graph, benchmarks)

        do node_i = 1, graph%length
            deallocate(graph%array(node_i)%array)
        end do
        deallocate(graph%array)
        deallocate(benchmarks%array)
    end subroutine main_ver5

end program dijkstra
