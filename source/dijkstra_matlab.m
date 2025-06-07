classdef dijkstra_matlab < handle
    properties
        dummy_connection
        dummy_graph
        dummy_benchmark
        dummy_candidate
        graph
        benchmarks
        queue
        queue_length
        indices
        label_new
        label_deleted
    end
    methods
        function obj = dijkstra_matlab()
            obj.dummy_connection = struct('destination', uint32(0), 'distance', 0.0);
            obj.dummy_graph = struct('connections', obj.dummy_connection(1:0));
            obj.dummy_benchmark = struct('source', uint32(0), 'destination', uint32(0));
            obj.dummy_candidate = struct('destination', uint32(0), 'int_distance', uint32(0), 'distance', 0.0);

            obj.graph = obj.dummy_graph(1:0);
            obj.benchmarks = obj.dummy_benchmark(1:0);

            obj.label_new = intmax('uint32');
            obj.label_deleted = intmax('uint32') - 1;
        end

        function parse_ver5(obj)
            graph_length = 0;
            graph_lengths = zeros(1, 0, 'uint32');
            benchmarks_length = 0;

            file = fopen("dijkstra.txt");
            read_benchmarks = false;
            while true
                line = fgetl(file);
                if ~ischar(line)
                    break;
                end
                if ~isempty(strfind(line, "GRAPH"))
                    read_benchmarks = false;
                elseif ~isempty(strfind(line, "BENCHMARK"))
                    read_benchmarks = true;
                elseif read_benchmarks
                    [scan, n] = sscanf(line, "%u %u");
                    if n ~= 2
                        break
                    end
                    source = uint32(scan(1)) + 1;
                    destination = uint32(scan(2)) + 1;

                    benchmarks_length = benchmarks_length + 1;
                    if benchmarks_length > size(obj.benchmarks, 2)
                        index = 2^ceil(log2(double(benchmarks_length)));
                        obj.benchmarks(index) = obj.dummy_benchmark;
                    end
                    obj.benchmarks(benchmarks_length) = struct('source', source, 'destination', destination);
                else
                    [scan, n] = sscanf(line, "%u %u %f");
                    if n ~= 3
                        break
                    end
                    source = uint32(scan(1)) + 1;
                    destination = uint32(scan(2)) + 1;
                    distance = scan(3);
                    
                    graph_length = max([graph_length, source, destination]);
                    if graph_length > size(obj.graph, 2)
                        index = 2^ceil(log2(double(graph_length)));
                        obj.graph(index) = obj.dummy_graph;
                        graph_lengths(index) = 0;
                    end

                    source_length = graph_lengths(source) + 1; graph_lengths(source) = source_length;
                    if source_length > size(obj.graph(source).connections, 2)
                        index = 2^ceil(log2(double(source_length)));
                        obj.graph(source).connections(index) = obj.dummy_connection;
                    end
                    obj.graph(source).connections(source_length) = struct('destination', destination, 'distance', distance);

                    destination_length = graph_lengths(destination) + 1; graph_lengths(destination) = destination_length;
                    if destination_length > size(obj.graph(destination).connections, 2)
                        index = 2^ceil(log2(double(destination_length)));
                        obj.graph(destination).connections(index) = obj.dummy_connection;
                    end
                    obj.graph(destination).connections(destination_length) = struct('destination', source, 'distance', distance);
                end
            end
            fclose(file);

            for i = 1 : graph_length
                obj.graph(i).connections = obj.graph(i).connections(1:graph_lengths(i));
            end
            obj.graph = obj.graph(1:graph_length);
            obj.benchmarks = obj.benchmarks(1:benchmarks_length);
        end

        function push_queue(obj, candidate)
            index = obj.indices(candidate.destination);
            if index == obj.label_new
                index = obj.queue_length + 1;
                obj.queue_length = obj.queue_length + 1;
            elseif index == obj.label_deleted
                return;
            else
                if candidate.distance >= obj.queue(index).distance
                    return;
                end
            end

            while true
                parent_exists = index > 1;
                index_moved = false;
                if parent_exists
                    parent_index = (index - 1) / 2;
                    if candidate.distance < obj.queue(parent_index).distance
                        obj.queue(index) = obj.queue(parent_index);
                        obj.indices(obj.queue(index).destination) = index;
                        index = parent_index;
                        index_moved = true;
                    end
                end

                if ~index_moved
                    obj.queue(index) = candidate;
                    obj.indices(candidate.destination) = index;
                    break;
                end
            end
        end

        function top = pop_queue(obj)
            obj.queue_length = obj.queue_length - 1;
            top = obj.queue(1);
            obj.indices(top.destination) = obj.label_deleted;
            if obj.queue_length == 0
                return;
            end

            buffer = obj.queue(obj.queue_length + 1);
            index = uint32(1);
            while true
                left_index = 2 * index;
                right_index = 2 * index + 1;
                left_exists = left_index <= obj.queue_length;
                right_exists = right_index <= obj.queue_length;
                
                index_moved = false;
                if left_exists || right_exists
                    if left_exists && right_exists
                        if obj.queue(left_index).distance < obj.queue(right_index).distance
                            next_index = left_index;
                        else
                            next_index = right_index;
                        end
                    else
                        next_index = left_index;
                    end
                    if obj.queue(next_index).distance < buffer.distance
                        obj.queue(index) = obj.queue(next_index);
                        obj.indices(obj.queue(index).destination) = index;
                        index = next_index;
                        index_moved = true;
                    end
                end

                if ~index_moved
                    obj.queue(index) = buffer;
                    obj.indices(obj.queue(index).destination) = index;
                    break;
                end
            end
        end

        function solve_ver5(obj)
            obj.queue = obj.dummy_candidate;
            obj.queue(size(obj.graph, 2)) = obj.dummy_candidate;

            for benchmark_i = 1 : size(obj.benchmarks, 2)
                source = obj.benchmarks(benchmark_i).source;
                destination = obj.benchmarks(benchmark_i).destination;
                obj.indices = obj.label_new * ones(size(obj.graph), 'uint32');
                obj.queue_length = uint32(0);
                obj.push_queue(struct('destination', source, 'int_distance', uint32(0), 'distance', 0.0));
                int_distance = uint32(0);
                distance = inf;

                while obj.queue_length ~= 0
                    current_candidate = obj.pop_queue();
                    if current_candidate.destination == destination
                        int_distance = current_candidate.int_distance;
                        distance = current_candidate.distance;
                        break;
                    end

                    connections = obj.graph(current_candidate.destination).connections;
                    for connection_i = 1 : size(connections, 2)
                        obj.push_queue(struct('destination', connections(connection_i).destination, ...
                            'int_distance', current_candidate.int_distance + 1, ...
                            'distance', current_candidate.distance + connections(connection_i).distance));
                    end
                end

                disp([ num2str(source-1) ' -> ' num2str(destination-1) ': ' num2str(distance) ' (' num2str(int_distance) ')' ]);
            end
        end

        function main(obj)
            obj.parse_ver5();
            obj.solve_ver5();
        end
    end
end
