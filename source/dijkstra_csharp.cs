#define VERSION5

using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;

class Connection
{
    public int destination;
    public float distance;
    public Connection(int destination, float distance)
    {
        this.destination = destination;
        this.distance = distance;
    }
};

class Benchmark
{
    public int source;
    public int destination;
    public Benchmark(int source, int destination)
    {
        this.source = source;
        this.destination = destination;
    }
};

interface IIndexed
{
    int Id { get; }
};

interface IPrioritized
{
    float Priority { get; }
};

class Candidate : IIndexed, IPrioritized
{
    public int id;
    public int int_distance;
    public float distance;

    public int Id { get { return id; } }
    public float Priority { get { return distance; } }

    public Candidate(int id, int int_distance, float distance)
    {
        this.id = id;
        this.int_distance = int_distance;
        this.distance = distance;
    }
};

#if VERSION3
class PriorityQueue<T> where T: IPrioritized
{
    List<T> data;

    public PriorityQueue(int size)
    {
        data = new List<T>(size);
    }

    public void Clear()
    {
        data.Clear();
    }

    public T Dequeue()
    {
        T top = data[0];
        T back = data[data.Count - 1];
        data.RemoveAt(data.Count - 1);
        if (data.Count == 0) return top; //If the front is the back, the algorithm no longer works
        
        int index = 0;
        while (true)
        {
            int left_index = 2 * index + 1;
            int right_index = 2 * index + 2;
            bool left_exists = left_index < data.Count;
            bool right_exists = right_index < data.Count;

            bool index_moved = false;
            if (left_exists || right_exists)
            {
                int next_index;
                if (left_exists && right_exists)
                {
                    if (data[left_index].Priority < data[right_index].Priority)
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

                if (data[next_index].Priority < back.Priority)
                {
                    data[index] = data[next_index];
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data[index] = back;
                break;
            }
        }

        return top;
    }

    public void Enqueue(T item)
    {
        int index = data.Count;
        data.Add(item); //Value not important, we just can't pass null
        while (true)
        {
            bool parent_exists = index > 0;
            bool index_moved = false;
            if (parent_exists)
            {
                int parent_index = (index - 1) / 2;
                if (item.Priority < data[parent_index].Priority)
                {
                    data[index] = data[parent_index];
                    index = parent_index;
                    index_moved = true;
                }
            }
            if (!index_moved)
            {
                data[index] = item;
                break;
            }
        }
    }

    public int Count { get { return data.Count; } }
};
#endif

#if VERSION5
class IndexedPriorityQueue<T> where T: IPrioritized, IIndexed
{
    List<T> data;
    int[] indices;

    public IndexedPriorityQueue(int size)
    {
        data = new List<T>(size);
        indices = new int[size];
    }

    public void Clear()
    {
        data.Clear();
        for (int i = 0; i < indices.Length; i++) indices[i] = -1;
    }

    public T Dequeue()
    {
        T top = data[0];
        indices[top.Id] = -2;
        T back = data[data.Count - 1];
        data.RemoveAt(data.Count - 1);
        if (data.Count == 0) return top; //If the front is the back, the algorithm no longer works

        int index = 0;
        while (true)
        {
            int left_index = 2 * index + 1;
            int right_index = 2 * index + 2;
            bool left_exists = left_index < data.Count;
            bool right_exists = right_index < data.Count;

            bool index_moved = false;
            if (left_exists || right_exists)
            {
                int next_index;
                if (left_exists && right_exists)
                {
                    if (data[left_index].Priority < data[right_index].Priority)
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

                if (data[next_index].Priority < back.Priority)
                {
                    data[index] = data[next_index];
                    indices[data[index].Id] = index;
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data[index] = back;
                indices[back.Id] = index;
                break;
            }
        }

        return top;
    }

    public void Enqueue(T item)
    {
        int index = indices[item.Id];
        if (index == -1)
        {
            index = data.Count;
            data.Add(item); //Value not important, we just can't pass null
        }
        else if (index == -2)
        {
            return;
        }
        else
        {
            if (item.Priority >= data[index].Priority) return;
        }
        
        while (true)
        {
            bool parent_exists = index > 0;
            bool index_moved = false;
            if (parent_exists)
            {
                int parent_index = (index - 1) / 2;
                if (item.Priority < data[parent_index].Priority)
                {
                    data[index] = data[parent_index];
                    indices[data[index].Id] = index;
                    index = parent_index;
                    index_moved = true;
                }
            }
            if (!index_moved)
            {
                data[index] = item;
                indices[item.Id] = index;
                break;
            }
        }
    }

    public int Count { get { return data.Count; } }
};
#endif

class Program
{
    static void ParseVer3(out List<List<Connection>> graph, out List<Benchmark> benchmarks)
    {
        graph = new List<List<Connection>>();
        benchmarks = new List<Benchmark>();

        NumberStyles integral = NumberStyles.None;
        NumberStyles real = NumberStyles.AllowDecimalPoint;
        IFormatProvider format = CultureInfo.InvariantCulture;
        StreamReader file = new StreamReader("dijkstra.txt");
        if (file == null) throw new IOException("StreamReader() failed");
        bool read_benchmarks = false;
        while (true)
        {
            #if NO_NULLABLE_REFERENCES
            string line = file.ReadLine();
            #else
            string? line = file.ReadLine();
            #endif
            if (line == null) break;
            #if NO_NULLABLE_REFERENCES
            string[] split = line.Split((char[])null,  StringSplitOptions.RemoveEmptyEntries);
            #else
            string[] split = line.Split((char[]?)null,  StringSplitOptions.RemoveEmptyEntries);
            #endif
            if (split.Length == 0) {} //Whitespace
            else if (split.Length == 1 && split[0] == "GRAPH")
            {
                read_benchmarks = false;
            }
            else if (split.Length == 1 && split[0] == "BENCHMARK")
            {
                read_benchmarks = true;
            }
            else if (read_benchmarks)
            {
                if (split.Length != 2) break; //Error
                if (!int.TryParse(split[0], integral, format, out int source)) break;
                if (!int.TryParse(split[1], integral, format, out int destination)) break;
                benchmarks.Add(new Benchmark(source, destination));
            }
            else
            {
                if (split.Length != 3) break; //Error
                if (!int.TryParse(split[0], integral, format, out int source)) break;
                if (!int.TryParse(split[1], integral, format, out int destination)) break;
                if (!float.TryParse(split[2], real, format, out float distance)) break;
                int extension = Math.Max(source, destination) - graph.Count + 1;
                if (extension > 0) { for (int i = 0; i < extension; i++) graph.Add(new List<Connection>()); }
                graph[source].Add(new Connection(destination, distance));
                graph[destination].Add(new Connection(source, distance));
            }
        }
    }

    #if VERSION3
    static void SolveVer3(List<List<Connection>> graph, List<Benchmark> benchmarks)
    {
        PriorityQueue<Candidate> candidates = new PriorityQueue<Candidate>(graph.Count);
        bool[] explored = new bool[graph.Count];

        foreach (Benchmark benchmark in benchmarks)
        {
            int source = benchmark.source;
            int destination = benchmark.destination;
            candidates.Clear();
            for (int i = 0; i < graph.Count; i++) explored[i] = false;
            candidates.Enqueue(new Candidate(source, 0, 0f));
            float distance = float.PositiveInfinity;
            int int_distance = 0;
            while (candidates.Count != 0)
            {
                Candidate candidate = candidates.Dequeue();
                if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
                if (explored[candidate.id]) continue;
                explored[candidate.id] = true;
                foreach (Connection connection in graph[candidate.id])
                {
                    if (explored[destination]) continue;
                    candidates.Enqueue(new Candidate(connection.destination, candidate.int_distance + 1, candidate.distance + connection.distance));
                }
            }
            Console.WriteLine("{0} -> {1}: {2} ({3})", source, destination, distance.ToString(CultureInfo.InvariantCulture), int_distance);
        }
    }
    #endif

    #if VERSION5
    static void SolveVer5(List<List<Connection>> graph, List<Benchmark> benchmarks)
    {
        IndexedPriorityQueue<Candidate> candidates = new IndexedPriorityQueue<Candidate>(graph.Count);

        foreach (Benchmark benchmark in benchmarks)
        {
            int source = benchmark.source;
            int destination = benchmark.destination;
            candidates.Clear();
            candidates.Enqueue(new Candidate(source, 0, 0f));
            float distance = float.PositiveInfinity;
            int int_distance = 0;
            while (candidates.Count != 0)
            {
                Candidate candidate = candidates.Dequeue();
                if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
                foreach (Connection connection in graph[candidate.id])
                {
                    candidates.Enqueue(new Candidate(connection.destination, candidate.int_distance + 1, candidate.distance + connection.distance));
                }
            }
            Console.WriteLine("{0} -> {1}: {2} ({3})", source, destination, distance.ToString(CultureInfo.InvariantCulture), int_distance);
        }
    }
    #endif

    static void Main()
    {
        #if VERSION3
        ParseVer3(out List<List<Connection>> graph, out List<Benchmark> benchmarks);
        SolveVer3(graph, benchmarks);
        #elif VERSION5
        ParseVer3(out List<List<Connection>> graph, out List<Benchmark> benchmarks);
        SolveVer5(graph, benchmarks);
        #endif
    }
}