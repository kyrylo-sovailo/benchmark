using System.Collections.Generic;
using System.IO;
using System;

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

class Candidate
{
    public int id;
    public int int_distance;
    public float distance;
    public Candidate(int id, int int_distance, float distance)
    {
        this.id = id;
        this.int_distance = int_distance;
        this.distance = distance;
    }
};

class IndexedPriorityQueue
{
    List<Candidate> data;
    int[] indices;

    public IndexedPriorityQueue(int size)
    {
        data = new List<Candidate>(size);
        indices = new int[size];
    }

    public void Reset()
    {
        data.Clear();
        for (int i = 0; i < indices.Length; i++) indices[i] = -1;
    }

    public Candidate Top()
    {
        return data[0];
    }

    public void Pop()
    {
        indices[data[0].id] = -2;
        indices[data[data.Count - 1].id] = 0;
        data[0] = data[data.Count - 1];
        data.RemoveAt(data.Count - 1);

        int i = 0;
        while (true)
        {
            int parent_i = 2 * i + 1;
            int right_i = 2 * i + 2;
            bool left_exists = parent_i < data.Count;
            bool right_exists = right_i < data.Count;
            if (/*left_exists &&*/ right_exists)
            {
                if (data[parent_i].distance < data[right_i].distance)
                {
                    if (data[parent_i].distance < data[i].distance)
                    {
                        int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
                        Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
                        i = parent_i;
                    }
                    else break;
                }
                else
                {
                    if (data[right_i].distance < data[i].distance)
                    {
                        int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[right_i].id]; indices[data[right_i].id] = b1;
                        Candidate b2 = data[i]; data[i] = data[right_i]; data[right_i] = b2;
                        i = right_i;
                    }
                    else break;
                }
            }
            else if (left_exists /*&& !right_exists*/)
            {
                if (data[parent_i].distance < data[i].distance)
                {
                    int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
                    Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
                    i = parent_i;
                }
                else break;
            }
            else
            {
                break;
            }
        }
    }

    public void Add(Candidate candidate)
    {
        int i = indices[candidate.id];
        if (i == -1)
        {
            i = data.Count;
            indices[candidate.id] = i;
            data.Add(candidate);
        }
        else if (i == -2)
        {
            return;
        }
        else
        {
            if (candidate.distance < data[i].distance) data[i] = candidate;
            else return;
        }
        while (i > 0)
        {
            int parent_i = (i - 1) / 2;
            if (data[i].distance < data[parent_i].distance)
            {
                int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
                Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
                i = parent_i;
            }
            else break;
        }
    }

    public bool Empty()
    {
        return data.Count == 0;
    }
};

class Program
{
    static void ParseVer4(List<List<Connection>> graph, List<Benchmark> benchmarks)
    {
        StreamReader file = new StreamReader("dijkstra.txt");
        if (file == null) throw new IOException("StreamReader() failed");

        bool read_benchmarks = false;
        while (true)
        {
            string line = file.ReadLine();
            if (line == null) break;
            if (line.Contains("GRAPH")) { read_benchmarks = false; continue; }
            if (line.Contains("BENCHMARK")) { read_benchmarks = true; continue; }
            string[] split = line.Split(' ');
            if (read_benchmarks)
            {
                if (!int.TryParse(split[0], out int source)) break;
                if (!int.TryParse(split[1], out int destination)) break;
                benchmarks.Add(new Benchmark(source, destination));
            }
            else
            {
                if (!int.TryParse(split[0], out int source)) break;
                if (!int.TryParse(split[1], out int destination)) break;
                if (!float.TryParse(split[2], out float distance)) break;
                int extension = Math.Max(source, destination) - graph.Count + 1;
                if (extension > 0) { for (int i = 0; i < extension; i++) graph.Add(new List<Connection>()); }
                graph[source].Add(new Connection(destination, distance));
                graph[destination].Add(new Connection(source, distance));
            }
        }
    }

    static void SolveVer4(List<List<Connection>> graph, List<Benchmark> benchmarks)
    {
        IndexedPriorityQueue candidates = new IndexedPriorityQueue(graph.Count);

        foreach (Benchmark benchmark in benchmarks)
        {
            int source = benchmark.source;
            int destination = benchmark.destination;
            candidates.Reset();
            candidates.Add(new Candidate(source, 0, 0f));
            int int_distance = 0;
            float distance = float.PositiveInfinity;
            while (!candidates.Empty())
            {
                Candidate candidate = candidates.Top();
                if (candidate.id == destination) { int_distance = candidate.int_distance; distance = candidate.distance; break; }
                candidates.Pop();
                foreach (Connection connection in graph[candidate.id])
                {
                    candidates.Add(new Candidate(connection.destination, candidate.int_distance + 1, candidate.distance + connection.distance));
                }
            }
            Console.WriteLine("{0} {1} {2} {3}", source, destination, int_distance, distance);
        }
    }

    static void Main()
    {
        List<List<Connection>> graph = new List<List<Connection>>();
        List<Benchmark> benchmarks = new List<Benchmark>();
        ParseVer4(graph, benchmarks);
        SolveVer4(graph, benchmarks);
    }
}