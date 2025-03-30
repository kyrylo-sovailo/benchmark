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
        this.indices[this.data[0].id] = -2;
        this.indices[this.data[this.data.length - 1].id] = 0;
        this.data[0] = this.data[this.data.length - 1];
        this.data.pop();

        let i = 0;
        while (true)
        {
            let parent_i = 2 * i + 1;
            let right_i = 2 * i + 2;
            let left_exists = parent_i < this.data.length;
            let right_exists = right_i < this.data.length;
            if (/*left_exists &&*/ right_exists)
            {
                if (this.data[parent_i].distance < this.data[right_i].distance)
                {
                    if (this.data[parent_i].distance < this.data[i].distance)
                    {
                        let b1 = this.indices[this.data[i].id]; this.indices[this.data[i].id] = this.indices[this.data[parent_i].id]; this.indices[this.data[parent_i].id] = b1;
                        let b2 = this.data[i]; this.data[i] = this.data[parent_i]; this.data[parent_i] = b2;
                        i = parent_i;
                    }
                    else break;
                }
                else
                {
                    if (this.data[right_i].distance < this.data[i].distance)
                    {
                        let b1 = this.indices[this.data[i].id]; this.indices[this.data[i].id] = this.indices[this.data[right_i].id]; this.indices[this.data[right_i].id] = b1;
                        let b2 = this.data[i]; this.data[i] = this.data[right_i]; this.data[right_i] = b2;
                        i = right_i;
                    }
                    else break;
                }
            }
            else if (left_exists /*&& !right_exists*/)
            {
                if (this.data[parent_i].distance < this.data[i].distance)
                {
                    let b1 = this.indices[this.data[i].id]; this.indices[this.data[i].id] = this.indices[this.data[parent_i].id]; this.indices[this.data[parent_i].id] = b1;
                    let b2 = this.data[i]; this.data[i] = this.data[parent_i]; this.data[parent_i] = b2;
                    i = parent_i;
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

    push(candidate)
    {
        let i = this.indices[candidate.id];
        if (i == -1)
        {
            i = this.data.length;
            this.indices[candidate.id] = i;
            this.data.push(candidate);
        }
        else if (i == -2)
        {
            return;
        }
        else
        {
            if (candidate.distance < this.data[i].distance) this.data[i] = candidate;
            else return;
        }
        while (i > 0)
        {
            let parent_i = (i - 1) >> 2;
            if (this.data[i].distance < this.data[parent_i].distance)
            {
                let b1 = this.indices[this.data[i].id]; this.indices[this.data[i].id] = this.indices[this.data[parent_i].id]; this.indices[this.data[parent_i].id] = b1;
                let b2 = this.data[i]; this.data[i] = this.data[parent_i]; this.data[parent_i] = b2;
                i = parent_i;
            }
            else break;
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
