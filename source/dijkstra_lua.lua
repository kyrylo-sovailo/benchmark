#!/usr/bin/env lua

local function indexed_heap_push(data, indices, element)
    local index = indices[element.id]
    if index == nil then
        index = #data + 1
        table.insert(data, element) --value used only when #data == 0, otherwise only for allocation
    elseif index == 0 then
        return
    else
        if element.distance >= data[index].distance then
            return
        end
    end

    while true do
        local parent_exists = index > 1
        local index_moved = false
        if parent_exists then
            local parent_index = math.floor(index / 2)
            if element.distance < data[parent_index].distance then
                data[index] = data[parent_index]
                indices[data[index].id] = index
                index = parent_index
                index_moved = true
            end
        end
        if not index_moved then
            data[index] = element
            indices[element.id] = index
            break
        end
    end
end

local function indexed_heap_pop(data, indices)
    local top = data[1]
    indices[top.id] = 0
    local back = data[#data]
    table.remove(data)
    if #data == 0 then
        return top --If the front is the back, the algorithm no longer works
    end
    
    local index = 1
    while true do
        local left_index = 2 * index
        local right_index = 2 * index + 1
        local left_exists = left_index <= #data
        local right_exists = right_index <= #data

        local index_moved = false
        if left_exists or right_exists then
            local next_index = 0
            if left_exists and right_exists then
                if data[left_index].distance < data[right_index].distance then
                    next_index = left_index
                else
                    next_index = right_index
                end
            else
                next_index = left_index
            end

            if data[next_index].distance < back.distance then
                data[index] = data[next_index]
                indices[data[index].id] = index
                index = next_index
                index_moved = true
            end
        end

        if not index_moved then
            data[index] = back
            indices[data[index].id] = index
            break
        end
    end

    return top
end

local function parse_ver2()
    local graph = {}
    local benchmarks = {}
    local read_benchmarks = false

    local file = io.open("dijkstra.txt", "r")
    if not file then
        error("Cannot open file 'dijkstra.txt'")
    end

    for line in file:lines() do
        local line_iterator = line:gmatch("%S+")
        local source_or_keyword_str = line_iterator()
        if source_or_keyword_str == nil then
            -- Whitespace
        elseif source_or_keyword_str == "GRAPH" then
            read_benchmarks = false
        elseif source_or_keyword_str == "BENCHMARK" then
            read_benchmarks = true
        elseif read_benchmarks then
            local source = tonumber(source_or_keyword_str) + 1
            local destination = tonumber(line_iterator()) + 1
            if line_iterator() ~= nil then break end --Error
            table.insert(benchmarks, { source=source, destination=destination })
        else
            local source = tonumber(source_or_keyword_str) + 1
            local destination = tonumber(line_iterator()) + 1
            local distance = tonumber(line_iterator())
            if line_iterator() ~= nil then break end --Error
            local max_index = math.max(source, destination)
            for _ = #graph, max_index do
                table.insert(graph, {})
            end
            graph[source][destination] = distance
            graph[destination][source] = distance
        end
    end
    file:close()
    return graph, benchmarks
end

local function solve_ver4(graph, benchmarks)
    for _, benchmark in ipairs(benchmarks) do
        local source = benchmark.source
        local destination = benchmark.destination
        local candidates = {}
        local indices = {}
        indexed_heap_push(candidates, indices, { id=source, int_distance=0, distance=0.0 })
        local distance = math.huge
        local int_distance = 0

        while #candidates > 0 do
            local candidate = indexed_heap_pop(candidates, indices)
            if candidate.id == destination then
                int_distance = candidate.int_distance
                distance = candidate.distance
                break
            end
            for connection_destination, connection_distance in pairs(graph[candidate.id]) do
                indexed_heap_push(candidates, indices, { id=connection_destination, int_distance=candidate.int_distance+1, distance=candidate.distance+connection_distance })
            end
        end
        print(string.format("%d -> %d: %f (%d)", source - 1, destination - 1, distance, int_distance))
    end
end

local function main_ver4()
    local graph, benchmarks = parse_ver2()
    solve_ver4(graph, benchmarks)
end

main_ver4()
