import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
//import java.text.NumberFormat;
//import java.text.ParseException;
import java.util.ArrayList;
//import java.util.List;
//import java.util.Locale;

class Connection
{
    public int destination;
    public float distance;

    public Connection(int destination, float distance)
    {
        this.destination = destination;
        this.distance = distance;
    }
}

class Benchmark
{
    public int source;
    public int destination;

    public Benchmark(int source, int destination)
    {
        this.source = source;
        this.destination = destination;
    }
}

class Candidate
{
    public int id;
    public int int_distance;
    public float distance;

    public float get_priority()
    {
        return distance;
    }

    public Candidate(int id, int int_distance, float distance)
    {
        this.id = id;
        this.int_distance = int_distance;
        this.distance = distance;
    }
}

class IndexedPriorityQueue
{
    private ArrayList<Candidate> data;
    private int[] indices;

    public IndexedPriorityQueue(int size)
    {
        data = new ArrayList<Candidate>(size);
        indices = new int[size];
    }

    public void clear()
    {
        data.clear();
        for (int i = 0; i < indices.length; i++) indices[i] = -1;
    }

    public Candidate dequeue()
    {
        Candidate top = data.get(0);
        indices[data.get(0).id] = -2;
        indices[data.get(data.size() - 1).id] = 0;
        data.set(0, data.get(data.size() - 1));
        data.remove(data.size() - 1);

        int i = 0;
        while (true)
        {
            int left_i = 2 * i + 1;
            int right_i = 2 * i + 2;
            boolean left_exists = left_i < data.size();
            boolean right_exists = right_i < data.size();

            if (/*left_exists &&*/ right_exists)
            {
                if (data.get(left_i).get_priority() < data.get(right_i).get_priority())
                {
                    if (data.get(left_i).get_priority() < data.get(i).get_priority())
                    {
                        int b1 = indices[data.get(i).id]; indices[data.get(i).id] = indices[data.get(left_i).id]; indices[data.get(left_i).id] = b1;
                        Candidate b2 = data.get(i); data.set(i, data.get(left_i)); data.set(left_i, b2);
                        i = left_i;
                    }
                    else break;
                }
                else
                {
                    if (data.get(right_i).get_priority() < data.get(i).get_priority())
                    {
                        int b1 = indices[data.get(i).id]; indices[data.get(i).id] = indices[data.get(right_i).id]; indices[data.get(right_i).id] = b1;
                        Candidate b2 = data.get(i); data.set(i, data.get(right_i)); data.set(right_i, b2);
                        i = right_i;
                    }
                    else break;
                }
            }
            else if (left_exists /*&& !right_exists*/)
            {
                if (data.get(left_i).get_priority() < data.get(i).get_priority())
                {
                    int b1 = indices[data.get(i).id]; indices[data.get(i).id] = indices[data.get(left_i).id]; indices[data.get(left_i).id] = b1;
                    Candidate b2 = data.get(i); data.set(i, data.get(left_i)); data.set(left_i, b2);
                    i = left_i;
                }
                else break;
            }
            else break;
        }

        return top;
    }

    public void enqueue(Candidate candidate)
    {
        int i = indices[candidate.id];
        if (i == -1)
        {
            i = data.size();
            indices[candidate.id] = i;
            data.add(candidate);
        }
        else if (i == -2)
        {
            return;
        }
        else
        {
            if (candidate.get_priority() < data.get(i).get_priority()) data.set(i, candidate);
            else return;
        }
        while (i > 0)
        {
            int parent_i = (i - 1) / 2;
            if (data.get(i).get_priority() < data.get(parent_i).get_priority())
            {
                int b1 = indices[data.get(i).id]; indices[data.get(i).id] = indices[data.get(parent_i).id]; indices[data.get(parent_i).id] = b1;
                Candidate b2 = data.get(i); data.set(i, data.get(parent_i)); data.set(parent_i, b2);
                i = parent_i;
            }
            else break;
        }
    }

    public int size()
    {
        return data.size();
    }
}

public class Dijkstra
{
    public static void parse_ver3(ArrayList<ArrayList<Connection>> graph, ArrayList<Benchmark> benchmarks) throws IOException
    {
        BufferedReader reader = new BufferedReader(new FileReader("../dijkstra.txt"));
        boolean read_benchmarks = false;
        while (true)
        {
            String line = reader.readLine();
            if (line == null) break;
            if (line.contains("GRAPH")) { read_benchmarks = false; continue; }
            if (line.contains("BENCHMARK")) { read_benchmarks = true; continue; }
            String[] split = line.split(" ");
            if (read_benchmarks)
            {
                int source = Integer.parseInt(split[0]);
                int destination = Integer.parseInt(split[1]);
                benchmarks.add(new Benchmark(source, destination));
            }
            else
            {
                int source = Integer.parseInt(split[0]);
                int destination = Integer.parseInt(split[1]);
                float distance = Float.parseFloat(split[2]);
                int extension = Math.max(source, destination) - graph.size() + 1;
                if (extension > 0) { for (int i = 0; i < extension; i++) graph.add(new ArrayList<Connection>()); }
                graph.get(source).add(new Connection(destination, distance));
                graph.get(destination).add(new Connection(source, distance));
            }
        }
    }

    public static void solve_ver5(ArrayList<ArrayList<Connection>> graph, ArrayList<Benchmark> benchmarks)
    {
        IndexedPriorityQueue candidates = new IndexedPriorityQueue(graph.size());

        for (Benchmark benchmark : benchmarks)
        {
            int source = benchmark.source;
            int destination = benchmark.destination;
            candidates.clear();
            candidates.enqueue(new Candidate(source, 0, 0f));
            float distance = Float.POSITIVE_INFINITY;
            int int_distance = 0;
            while (candidates.size() != 0)
            {
                Candidate candidate = candidates.dequeue();
                if (candidate.id == destination)
                {
                    int_distance = candidate.int_distance;
                    distance = candidate.distance;
                    break;
                }
                for (Connection connection : graph.get(candidate.id))
                {
                    candidates.enqueue(new Candidate(connection.destination, candidate.int_distance + 1, candidate.distance + connection.distance));
                }
            }
            System.out.printf("%d -> %d: %.6f (%d)\n", source, destination, distance, int_distance);
        }
    }

    public static void main(String[] args) throws IOException
    {
        ArrayList<ArrayList<Connection>> graph = new ArrayList<ArrayList<Connection>>();
        ArrayList<Benchmark> benchmarks = new ArrayList<Benchmark>();
        parse_ver3(graph, benchmarks);
        solve_ver5(graph, benchmarks);
    }
}
