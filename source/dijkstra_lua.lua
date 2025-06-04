#!/usr/bin/lua

local function indexed_heap_push(data, indices, element)
    local i = indices[element.id]
    if i == nil then
        i = #data + 1
        indices[element.id] = i
        table.insert(data, element)
    elseif i == 0 then
        return
    else
        if element.distance < data[i].distance then
            data[i] = element
        else
            return
        end
    end

    while i > 1 do
        local parent_i = math.floor(i / 2)
        if data[i].distance < data[parent_i].distance then
            indices[data[i].id], indices[data[parent_i].id] = indices[data[parent_i].id], indices[data[i].id]
            data[i], data[parent_i] = data[parent_i], data[i]
            i = parent_i
        else
            return
        end
    end
end

local function indexed_heap_pop(data, indices)
    local top = data[1]
    indices[data[#data].id] = 1
    indices[data[1].id] = 0
    data[1] = data[#data]
    table.remove(data)

    local i = 1
    while true do
        local left_i = 2 * i
        local right_i = 2 * i + 1
        local left_exists = left_i <= #data
        local right_exists = right_i <= #data

        if right_exists then
            if data[left_i].distance < data[right_i].distance then
                if data[left_i].distance < data[i].distance then
                    indices[data[i].id], indices[data[left_i].id] = indices[data[left_i].id], indices[data[i].id]
                    data[i], data[left_i] = data[left_i], data[i]
                    i = left_i
                else
                    return top
                end
            else
                if data[right_i].distance < data[i].distance then
                    indices[data[i].id], indices[data[right_i].id] = indices[data[right_i].id], indices[data[i].id]
                    data[i], data[right_i] = data[right_i], data[i]
                    i = right_i
                else
                    return top
                end
            end
        elseif left_exists then
            if data[left_i].distance < data[i].distance then
                indices[data[i].id], indices[data[left_i].id] = indices[data[left_i].id], indices[data[i].id]
                data[i], data[left_i] = data[left_i], data[i]
                i = left_i
            else
                return top
            end
        else
            return top
        end
    end
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
        if line:find("GRAPH") then
            read_benchmarks = false
        elseif line:find("BENCHMARK") then
            read_benchmarks = true
        else
            local split = {}
            for token in line:gmatch("%S+") do
                table.insert(split, token)
            end

            if read_benchmarks then
                local source = tonumber(split[1]) + 1
                local destination = tonumber(split[2]) + 1
                table.insert(benchmarks, { source=source, destination=destination })
            else
                local source = tonumber(split[1]) + 1
                local destination = tonumber(split[2]) + 1
                local distance = tonumber(split[3])
                local max_index = math.max(source, destination)
                for _ = #graph, max_index do
                    table.insert(graph, {})
                end
                graph[source][destination] = distance
                graph[destination][source] = distance
            end
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
