<?php

class Connection
{
    public $destination;
    public $distance;

    public function __construct($destination, $distance)
    {
        $this->destination = $destination;
        $this->distance = $distance;
    }
}

class Benchmark
{
    public $source;
    public $destination;

    public function __construct($source, $destination)
    {
        $this->source = $source;
        $this->destination = $destination;
    }
}

class Candidate
{
    public $id;
    public $int_distance;
    public $distance;

    public function __construct($id, $int_distance, $distance)
    {
        $this->id = $id;
        $this->int_distance = $int_distance;
        $this->distance = $distance;
    }
}

class IndexedPriorityQueue
{
    private $size;
    private $data = [];
    private $indices = [];

    public function __construct($size)
    {
        $this->size = $size;
        $this->clear();
    }

    public function clear()
    {
        $this->data = [];
        $this->indices = array_fill(0, $this->size, -1);
    }

    public function pop()
    {
        if (empty($this->data))
        {
            return null;
        }

        $top = $this->data[0];
        $this->indices[$top->id] = -2;

        if (count($this->data) === 1)
        {
            array_pop($this->data);
            return $top;
        }

        $back = array_pop($this->data);
        $index = 0;
        $len = count($this->data);

        while (true)
        {
            $left_index = 2 * $index + 1;
            $right_index = 2 * $index + 2;
            $left_exists = $left_index < $len;
            $right_exists = $right_index < $len;

            $index_moved = false;

            if ($left_exists || $right_exists)
            {
                if ($left_exists && $right_exists)
                {
                    if ($this->data[$left_index]->distance < $this->data[$right_index]->distance)
                    {
                        $next_index = $left_index;
                    }
                    else
                    {
                        $next_index = $right_index;
                    }
                }
                else
                {
                    $next_index = $left_index;
                }

                if ($this->data[$next_index]->distance < $back->distance)
                {
                    $this->data[$index] = $this->data[$next_index];
                    $this->indices[$this->data[$index]->id] = $index;
                    $index = $next_index;
                    $index_moved = true;
                }
            }

            if (!$index_moved)
            {
                $this->data[$index] = $back;
                $this->indices[$back->id] = $index;
                break;
            }
        }

        return $top;
    }

    public function push($candidate)
    {
        $index = $this->indices[$candidate->id] ?? -1;

        if ($index === -1)
        {
            $index = count($this->data);
            $this->data[] = $candidate;
        }
        elseif ($index === -2)
        {
            return;
        }
        else
        {
            if ($candidate->distance >= $this->data[$index]->distance)
            {
                return;
            }
        }

        while ($index > 0)
        {
            $parent_index = ($index - 1) >> 1;
            if ($candidate->distance < $this->data[$parent_index]->distance)
            {
                $this->data[$index] = $this->data[$parent_index];
                $this->indices[$this->data[$index]->id] = $index;
                $index = $parent_index;
            }
            else
            {
                break;
            }
        }

        $this->data[$index] = $candidate;
        $this->indices[$candidate->id] = $index;
    }

    public function length()
    {
        return count($this->data);
    }
}

function parse_ver5()
{
    $graph = [];
    $benchmarks = [];
    $read_benchmarks = false;

    $file = fopen("dijkstra.txt", "r");
    if (!$file)
    {
        die("Failed to open file");
    }

    while (($line = fgets($file)) !== false)
    {
        $line = trim($line);
        if (strpos($line, "GRAPH") !== false)
        {
            $read_benchmarks = false;
        }
        elseif (strpos($line, "BENCHMARK") !== false)
        {
            $read_benchmarks = true;
        }
        elseif ($read_benchmarks)
        {
            $split = preg_split('/\s+/', $line);
            if (count($split) < 2) continue;
            $source = intval($split[0]);
            $destination = intval($split[1]);
            if (is_nan($source) || is_nan($destination)) continue;
            $benchmarks[] = new Benchmark($source, $destination);
        }
        else
        {
            $split = preg_split('/\s+/', $line);
            if (count($split) < 3) continue;
            $source = intval($split[0]);
            $destination = intval($split[1]);
            $distance = floatval($split[2]);
            if (is_nan($source) || is_nan($destination) || is_nan($distance)) continue;

            $max_index = max($source, $destination);
            while (count($graph) <= $max_index)
            {
                $graph[] = [];
            }
            $graph[$source][] = new Connection($destination, $distance);
            $graph[$destination][] = new Connection($source, $distance);
        }
    }

    fclose($file);
    return [$graph, $benchmarks];
}

function solve_ver5($graph, $benchmarks)
{
    $n = count($graph);
    $candidates = new IndexedPriorityQueue($n);

    foreach ($benchmarks as $benchmark)
    {
        $source = $benchmark->source;
        $destination = $benchmark->destination;

        $candidates->clear();
        $start_node = new Candidate($source, 0, 0);
        $candidates->push($start_node);

        $distance = INF;
        $int_distance = 0;

        while ($candidates->length() != 0)
        {
            $candidate = $candidates->pop();
            if ($candidate->id == $destination)
            {
                $int_distance = $candidate->int_distance;
                $distance = $candidate->distance;
                break;
            }
            foreach ($graph[$candidate->id] as $connection)
            {
                $new_candidate = new Candidate(
                    $connection->destination,
                    $candidate->int_distance + 1,
                    $candidate->distance + $connection->distance
                );
                $candidates->push($new_candidate);
            }
        }
        printf("%d -> %d: %f (%d)\n", $source, $destination, $distance, $int_distance);
    }
}

function main_ver5()
{
    list($graph, $benchmarks) = parse_ver5();
    solve_ver5($graph, $benchmarks);
}

ini_set('memory_limit', '8192M');
main_ver5()

?>