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

class Candidate
{
    public int id;
    public int int_distance;
    public float distance;

    public float Priority { get { return distance; } }

    public Candidate(int id, int int_distance, float distance)
    {
        this.id = id;
        this.int_distance = int_distance;
        this.distance = distance;
    }
};

#if VERSION3
class PriorityQueue
{
    List<Candidate> data;

    public PriorityQueue(int size)
    {
        data = new List<Candidate>(size);
    }

    public void Clear()
    {
        data.Clear();
    }

    public Candidate Dequeue()
    {
        Candidate top = data[0];
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
                if (data[parent_i].Priority < data[right_i].Priority)
                {
                    if (data[parent_i].Priority < data[i].Priority)
                    {
                        Candidate b1 = data[i]; data[i] = data[parent_i]; data[parent_i] = b1;
                        i = parent_i;
                    }
                    else break;
                }
                else
                {
                    if (data[right_i].Priority < data[i].Priority)
                    {
                        Candidate b1 = data[i]; data[i] = data[right_i]; data[right_i] = b1;
                        i = right_i;
                    }
                    else break;
                }
            }
            else if (left_exists /*&& !right_exists*/)
            {
                if (data[parent_i].Priority < data[i].Priority)
                {
                    Candidate b1 = data[i]; data[i] = data[parent_i]; data[parent_i] = b1;
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

    public void Enqueue(Candidate candidate)
    {
        int i = data.Count;
        data.Add(candidate);
        while (i > 0)
        {
            int parent_i = (i - 1) / 2;
            if (data[i].Priority < data[parent_i].Priority)
            {
                Candidate b1 = data[i]; data[i] = data[parent_i]; data[parent_i] = b1;
                i = parent_i;
            }
            else break;
        }
    }

    public int Count { get { return data.Count; } }
};
#endif

#if VERSION5
class IndexedPriorityQueue
{
    List<Candidate> data;
    int[] indices;

    public IndexedPriorityQueue(int size)
    {
        data = new List<Candidate>(size);
        indices = new int[size];
    }

    public void Clear()
    {
        data.Clear();
        for (int i = 0; i < indices.Length; i++) indices[i] = -1;
    }

    public Candidate Dequeue()
    {
        Candidate top = data[0];
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
                if (data[parent_i].Priority < data[right_i].Priority)
                {
                    if (data[parent_i].Priority < data[i].Priority)
                    {
                        int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
                        Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
                        i = parent_i;
                    }
                    else break;
                }
                else
                {
                    if (data[right_i].Priority < data[i].Priority)
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
                if (data[parent_i].Priority < data[i].Priority)
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

        return top;
    }

    public void Enqueue(Candidate candidate)
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
            if (candidate.Priority < data[i].Priority) data[i] = candidate;
            else return;
        }
        while (i > 0)
        {
            int parent_i = (i - 1) / 2;
            if (data[i].Priority < data[parent_i].Priority)
            {
                int b1 = indices[data[i].id]; indices[data[i].id] = indices[data[parent_i].id]; indices[data[parent_i].id] = b1;
                Candidate b2 = data[i]; data[i] = data[parent_i]; data[parent_i] = b2;
                i = parent_i;
            }
            else break;
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
            string line = file.ReadLine();
            if (line == null) break;
            if (line.Contains("GRAPH")) { read_benchmarks = false; continue; }
            if (line.Contains("BENCHMARK")) { read_benchmarks = true; continue; }
            string[] split = line.Split(' ');
            if (read_benchmarks)
            {
                if (!int.TryParse(split[0], integral, format, out int source)) break;
                if (!int.TryParse(split[1], integral, format, out int destination)) break;
                benchmarks.Add(new Benchmark(source, destination));
            }
            else
            {
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
        PriorityQueue candidates = new PriorityQueue(graph.Count);
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
        IndexedPriorityQueue candidates = new IndexedPriorityQueue(graph.Count);

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