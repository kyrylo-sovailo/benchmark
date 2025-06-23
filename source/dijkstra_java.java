import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;

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

interface Indexed
{
    int get_id();
}

interface Prioritized
{
    float get_priority();
}

class Candidate implements Indexed, Prioritized
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

    @Override
    public int get_id()
    {
        return this.id;
    }

    @Override
    public float get_priority()
    {
        return this.distance;
    }
}

class IndexedPriorityQueue<T extends Indexed & Prioritized>
{
    private ArrayList<T> data;
    private int[] indices;

    public IndexedPriorityQueue(int size)
    {
        data = new ArrayList<T>(size);
        indices = new int[size];
    }

    public void clear()
    {
        data.clear();
        for (int i = 0; i < indices.length; i++) indices[i] = -1;
    }

    public T dequeue()
    {
        T top = data.get(0);
        indices[top.get_id()] = -2;

        if (data.size() == 1)
        {
            data.remove(0);
            return top;
        }

        T back = data.get(data.size() - 1);
        int index = 0;

        while (true)
        {
            int left_index = 2 * index + 1;
            int right_index = 2 * index + 2;
            boolean left_exists = left_index < data.size();
            boolean right_exists = right_index < data.size();

            boolean index_moved = false;

            if (left_exists || right_exists)
            {
                int next_index;
                if (left_exists && right_exists)
                {
                    if (data.get(left_index).get_priority() < data.get(right_index).get_priority())
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

                if (data.get(next_index).get_priority() < back.get_priority())
                {
                    data.set(index, data.get(next_index));
                    indices[data.get(index).get_id()] = index;
                    index = next_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data.set(index, back);
                indices[back.get_id()] = index;
                data.remove(data.size() - 1);
                break;
            }
        }

        return top;
    }

    public void enqueue(T item)
    {
        int index = indices[item.get_id()];

        if (index == -1)
        {
            index = data.size();
            data.add(item); //value used only when size == 0, otherwise only for allocation
        }
        else if (index == -2)
        {
            return;
        }
        else
        {
            if (item.get_priority() >= data.get(index).get_priority()) return;
        }

        while (true)
        {
            boolean parent_exists = index > 0;
            boolean index_moved = false;

            if (parent_exists)
            {
                int parent_index = (index - 1) / 2;
                if (item.get_priority() < data.get(parent_index).get_priority())
                {
                    data.set(index, data.get(parent_index));
                    indices[data.get(index).get_id()] = index;
                    index = parent_index;
                    index_moved = true;
                }
            }

            if (!index_moved)
            {
                data.set(index, item);
                indices[item.get_id()] = index;
                break;
            }
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
        IndexedPriorityQueue<Candidate> candidates = new IndexedPriorityQueue<Candidate>(graph.size());

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
