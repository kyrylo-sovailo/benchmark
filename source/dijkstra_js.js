#!/usr/bin/env node
const readline = require('readline');
const fs = require('fs');

class IndexedPriorityQueue
{
    constructor(size)
    {
        this.size = size;
    }

    clear()
    {
        this.data = new Array();
        this.indices = new Array(this.size).fill(-1);
    }

    pop()
    {
        let top = this.data[0];
        this.indices[top.id] = -2;
        let back = this.data[this.data.length - 1];
        this.data.pop();
        if (this.data.length == 0) return top; //If the front is the back, the algorithm no longer works

        let index = 0;
        while (true)
        {
            let left_index = 2 * index + 1;
            let right_index = 2 * index + 2;
            let left_exists = left_index < this.data.length;
            let right_exists = right_index < this.data.length;

            let index_moved = false;
            if (left_exists || right_exists)
            {
                let next_index;
                if (left_exists && right_exists)
                {
                    if (this.data[left_index].distance < this.data[right_index].distance)
                    {
                        next_index = left_index;
                    }
                    else
                    {
                        next_index = right_index;
                    }
                }
                else
                {
                    next_index = left_index;
                }

                if (this.data[next_index].distance < back.distance)
                {
                    this.data[index] = this.data[next_index];
                    this.indices[this.data[index].id] = index;
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                this.data[index] = back;
                this.indices[back.id] = index;
                break;
            }
        }

        return top;
    }

    push(candidate)
    {
        let index = this.indices[candidate.id];
        if (index == -1)
        {
            index = this.data.length;
            this.data.push(candidate); //value used only when length == 0, otherwise only for allocation
        }
        else if (index == -2)
        {
            return;
        }
        else
        {
            if (candidate.distance >= this.data[index].distance) return;
        }
        
        while (true)
        {
            let parent_exists = index > 0;
            let index_moved = false;
            if (parent_exists)
            {
                let parent_index = (index - 1) >> 1;
                if (candidate.distance < this.data[parent_index].distance)
                {
                    this.data[index] = this.data[parent_index];
                    this.indices[this.data[index].id] = index;
                    index = parent_index;
                    index_moved = true;
                }
            }
            if (!index_moved)
            {
                this.data[index] = candidate;
                this.indices[candidate.id] = index;
                break;
            }
        }
    }

    length()
    {
        return this.data.length;
    }
};

async function parse_ver4()
{
    let graph = new Array();
    let benchmarks = new Array();
    let read_benchmarks = false;

    let reader = readline.createInterface({ input: fs.createReadStream("dijkstra.txt"), terminal: false });
    for await (line of reader)
    {
        if (line.includes("GRAPH"))
        {
            read_benchmarks = false;
        }
        else if (line.includes("BENCHMARK"))
        {
            read_benchmarks = true;
        }
        else if (read_benchmarks)
        {
            let split = line.split(' ');
            let source = parseInt(split[0]);
            let destination = parseInt(split[1]);
            if (isNaN(source) || isNaN(destination)) return;
            benchmarks.push({ source: source, destination: destination });
        }
        else
        {
            let split = line.split(' ');
            let source = parseInt(split[0]);
            let destination = parseInt(split[1]);
            let distance = parseFloat(split[2]);
            if (isNaN(source) || isNaN(destination) || isNaN(distance)) return;
            let extension = Math.max(source, destination) - graph.length + 1;
            if (extension > 0) graph = graph.concat(new Array(extension).fill().map(() => new Map()));
            graph[source].set(destination, distance);
            graph[destination].set(source, distance);
        }
    }

    return [ graph, benchmarks ];
}

async function parse_ver5()
{
    let graph = new Array();
    let benchmarks = new Array();
    let read_benchmarks = false;

    let reader = readline.createInterface({ input: fs.createReadStream("dijkstra.txt"), terminal: false });
    for await (line of reader)
    {
        split = line.split(/[ \t]/).filter(Boolean)
        if (split.length == 0) {} //Whitespace
        else if (split.length == 1 && split[0] == "GRAPH")
        {
            read_benchmarks = false;
        }
        else if (split.length == 1 && split[0] == "BENCHMARK")
        {
            read_benchmarks = true;
        }
        else if (read_benchmarks)
        {
            if (split.length != 2) break; //Error
            let source = parseInt(split[0]);
            let destination = parseInt(split[1]);
            if (isNaN(source) || isNaN(destination)) break;
            benchmarks.push({ source: source, destination: destination });
        }
        else
        {
            if (split.length != 3) break; //Error
            let source = parseInt(split[0]);
            let destination = parseInt(split[1]);
            let distance = parseFloat(split[2]);
            if (isNaN(source) || isNaN(destination) || isNaN(distance)) break;
            let extension = Math.max(source, destination) - graph.length + 1;
            if (extension > 0) graph = graph.concat(new Array(extension).fill().map(() => []));
            graph[source].push({ destination: destination, distance: distance });
            graph[destination].push({ destination: source, distance: distance });
        }
    }

    return [ graph, benchmarks ];
}

function solve_ver4(graph, benchmarks)
{
    let candidates = new IndexedPriorityQueue(graph.length);

    benchmarks.forEach((benchmark, benchmark_i) =>
    {
        let source = benchmark.source;
        let destination = benchmark.destination;
        candidates.clear();
        candidates.push({ id: source, int_distance: 0, distance: 0 });
        let distance = Infinity;
        let int_distance = 0;
        while (candidates.length() != 0)
        {
            candidate = candidates.pop();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            graph[candidate.id].forEach((connection_distance, connection_destination) =>
            {
                candidates.push({ id: connection_destination, int_distance: candidate.int_distance + 1, distance: candidate.distance + connection_distance });
            });
        }
        console.log("%d -> %d: %f (%d)", source, destination, distance, int_distance);
    });
}

function solve_ver5(graph, benchmarks)
{
    let candidates = new IndexedPriorityQueue(graph.length);

    benchmarks.forEach((benchmark, benchmark_i) =>
    {
        let source = benchmark.source;
        let destination = benchmark.destination;
        candidates.clear();
        candidates.push({ id: source, int_distance: 0, distance: 0 });
        let distance = Infinity;
        let int_distance = 0;
        while (candidates.length() != 0)
        {
            candidate = candidates.pop();
            if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
            graph[candidate.id].forEach((connection, connection_i) =>
            {
                candidates.push({ id: connection.destination, int_distance: candidate.int_distance + 1, distance: candidate.distance + connection.distance });
            });
        }
        console.log("%d -> %d: %f (%d)", source, destination, distance, int_distance);
    });
}

async function main()
{
    [ graph, benchmarks ] = await parse_ver5();
    solve_ver5(graph, benchmarks);
}

main();
