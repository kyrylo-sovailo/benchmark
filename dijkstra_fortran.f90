 

program dijkstra

    implicit none

    type :: Candidate
        integer :: id
        integer :: int_distance
        real :: distance
    end type Candidate

    type :: CandidateVector
        type(Candidate), dimension(:), allocatable :: array
        integer :: length = 0
    end type CandidateVector

    type :: Benchmark
        integer :: source
        integer :: destination
    end type Benchmark

    type :: BenchmarkVector
        type(Benchmark), dimension(:), allocatable :: array
        integer :: length = 0
    end type BenchmarkVector

    type :: Connection
        integer :: destination
        real :: distance
    end type Connection

    type :: ConnectionVector
        type(Connection), dimension(:), allocatable :: array
        integer :: length = 0
    end type ConnectionVector

    type :: ConnectionVectorVector
        type(ConnectionVector), dimension(:), allocatable :: array
        integer :: length = 0
    end type ConnectionVectorVector

    call main_ver4()

contains

    subroutine push_CandidateVector(vector, element)
        type(CandidateVector), intent(inout) :: vector
        type(Candidate), intent(in) :: element
        type(Candidate), dimension(:), allocatable :: new_array
        integer :: old_capacity, new_capacity
        vector%length = vector%length + 1
        if (allocated(vector%array)) then
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
            call move_alloc(new_array, vector%array)
        end if
        vector%array(vector%length) = element
    end subroutine push_CandidateVector

    subroutine push_BenchmarkVector(vector, element)
        type(BenchmarkVector), intent(inout) :: vector
        type(Benchmark), intent(in) :: element
        type(Benchmark), dimension(:), allocatable :: new_array
        integer :: old_capacity, new_capacity
        vector%length = vector%length + 1
        if (allocated(vector%array)) then
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
            call move_alloc(new_array, vector%array)
        end if
        vector%array(vector%length) = element
    end subroutine push_BenchmarkVector

    subroutine push_ConnectionVector(vector, element)
        type(ConnectionVector), intent(inout) :: vector
        type(Connection), intent(in) :: element
        type(Connection), dimension(:), allocatable :: new_array
        integer :: old_capacity, new_capacity
        vector%length = vector%length + 1
        if (allocated(vector%array)) then
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
            call move_alloc(new_array, vector%array)
        end if
        vector%array(vector%length) = element
    end subroutine push_ConnectionVector

    subroutine grow_ConnectionVectorVector(vector, length)
        type(ConnectionVectorVector), intent(inout) :: vector
        integer, intent(in) :: length
        type(ConnectionVector), dimension(:), allocatable :: new_array
        integer :: old_capacity, new_capacity
        if (length <= vector%length) return
        if (allocated(vector%array)) then
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
            call move_alloc(new_array, vector%array)
        end if
        vector%length = length
    end subroutine grow_ConnectionVectorVector

    subroutine swap_candidates(index1, index2, candidate1, candidate2)
        integer, intent(inout) :: index1, index2
        type(Candidate), intent(inout) :: candidate1, candidate2
        
        integer :: b1
        type(Candidate) :: b2

        b1 = index1
        index1 = index2
        index2 = b1
        b2 = candidate1
        candidate1 = candidate2
        candidate2 = b2
    end subroutine swap_candidates

    subroutine push_indexed_heap(data, indices, element)
        type(CandidateVector), intent(inout) :: data
        integer, dimension(:), allocatable :: indices
        type(Candidate), intent(in) :: element
        integer :: i, parent_i

        i = indices(element%id)
        if (i == -1) then
            i = data%length + 1
            indices(element%id) = i
            call push_CandidateVector(data, element)
        else if (i == -2) then
            return
        else
            if (element%distance < data%array(i)%distance) then
                data%array(i) = element
            else
                return
            end if
        end if

        do while (i > 1)
            parent_i = i / 2
            if (data%array(i)%distance < data%array(parent_i)%distance) then
                call swap_candidates(indices(data%array(i)%id), indices(data%array(parent_i)%id), &
                    data%array(i), data%array(parent_i))
                i = parent_i
            else
                exit
            end if
        end do
    end subroutine push_indexed_heap

    function pop_indexed_heap(data, indices) result(top)
        type(CandidateVector), intent(inout) :: data
        integer, dimension(:), allocatable :: indices
        type(Candidate) :: top
        integer :: i, left_i, right_i
        logical :: left_exists, right_exists

        top = data%array(1)
        indices(data%array(1)%id) = -2
        indices(data%array(data%length)%id) = 1
        data%array(1) = data%array(data%length)
        data%length = data%length - 1

        i = 1
        do while (.true.)
            left_i = 2 * i
            right_i = 2 * i + 1
            left_exists = (left_i <= data%length)
            right_exists = (right_i <= data%length)

            if (right_exists) then
                if (data%array(left_i)%distance < data%array(right_i)%distance) then
                    if (data%array(left_i)%distance < data%array(i)%distance) then
                        call swap_candidates(indices(data%array(i)%id), indices(data%array(left_i)%id), &
                            data%array(i), data%array(left_i))
                        i = left_i
                    else
                        exit
                    end if
                else
                    if (data%array(right_i)%distance < data%array(i)%distance) then
                        call swap_candidates(indices(data%array(i)%id), indices(data%array(right_i)%id), &
                            data%array(i), data%array(right_i))
                        i = right_i
                    else
                        exit
                    end if
                end if
            else if (left_exists) then
                if (data%array(left_i)%distance < data%array(i)%distance) then
                    call swap_candidates(indices(data%array(i)%id), indices(data%array(left_i)%id), &
                        data%array(i), data%array(left_i))
                    i = left_i
                else
                    exit
                end if
            else
                exit
            end if
        end do
    end function pop_indexed_heap

    subroutine parse_ver4(graph, benchmarks)
        type(ConnectionVectorVector), intent(out) :: graph
        type(BenchmarkVector), intent(out) :: benchmarks

        integer :: source, destination
        real :: distance
        character(128) :: string
        integer :: status

        open(unit=10, iostat=status, file="dijkstra.txt", status="old", action="read")
        if (status /= 0) then
            print *, "open() failed"
            stop
        end if

        read(10, *) string
        do
            read(10, *, iostat=status) source, destination, distance
            source = source + 1
            destination = destination + 1
            if (status /= 0) exit

            call grow_ConnectionVectorVector(graph, max(source, destination))
            call push_ConnectionVector(graph%array(source), Connection(destination, distance))
            call push_ConnectionVector(graph%array(destination), Connection(source, distance))
        end do

        !read(10, *) string
        do
            read(10, *, iostat=status) source, destination
            source = source + 1
            destination = destination + 1
            if (status /= 0) exit

            call push_BenchmarkVector(benchmarks, Benchmark(source, destination))
        end do

        close(10)
    end subroutine parse_ver4

    subroutine solve_ver4(graph, benchmarks)
        type(ConnectionVectorVector), intent(in) :: graph
        type(BenchmarkVector), intent(in) :: benchmarks

        type(CandidateVector) :: candidates
        integer, dimension(:), allocatable :: candidate_indices
        integer :: benchmark_i, neighbor_i
        type(Candidate) :: current_candidate, new_candidate
        type(ConnectionVector) :: current_neighbors
        integer :: source, destination
        integer :: int_distance
        real :: distance

        allocate(candidate_indices(graph%length))

        do benchmark_i = 1, benchmarks%length
            source = benchmarks%array(benchmark_i)%source
            destination = benchmarks%array(benchmark_i)%destination
            candidates%length = 0
            candidate_indices = -1
            call push_indexed_heap(candidates, candidate_indices, Candidate(source, 0, 0.0))
            int_distance = 0
            distance = huge(1.0)

            do while (candidates%length /= 0)
                current_candidate = pop_indexed_heap(candidates, candidate_indices)
                if (current_candidate%id == destination) then
                    int_distance = current_candidate%int_distance
                    distance = current_candidate%distance
                    exit
                end if

                current_neighbors = graph%array(current_candidate%id)
                do neighbor_i = 1, current_neighbors%length
                    new_candidate = Candidate(current_neighbors%array(neighbor_i)%destination, current_candidate%int_distance + 1, &
                        current_candidate%distance + current_neighbors%array(neighbor_i)%distance)
                    call push_indexed_heap(candidates, candidate_indices, new_candidate)
                end do
            end do

            if (distance /= huge(1.0)) then
                write (*,'(I0.1, A, I0.1, A, F0.4)') destination-1, ' ', int_distance, ' ', distance
            else
                write (*,'(I0.1, A, I0.1, A, A)') destination-1, ' ', int_distance, ' ', 'inf'
            end if
        end do
    end subroutine solve_ver4

    subroutine main_ver4()
        type(ConnectionVectorVector) :: graph
        type(BenchmarkVector) :: benchmarks

        call parse_ver4(graph, benchmarks)
        call solve_ver4(graph, benchmarks)
    end subroutine main_ver4

end program dijkstra