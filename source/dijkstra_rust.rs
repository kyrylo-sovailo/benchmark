use std::error::Error;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::f32::INFINITY;

trait Indexed
{
    fn get_id(&self) -> u32;
}

trait Prioritized
{
    fn get_priority(&self) -> f32;
}

#[derive(Debug)]
struct Benchmark
{
    source: u32,
    destination: u32
}

#[derive(Debug, Clone)]
struct Connection
{
    destination: u32,
    distance: f32
}

#[derive(Debug, Clone)]
struct Candidate
{
    id: u32,
    int_distance: u32,
    distance: f32
}

struct IndexedPriorityQueue<T: PartialOrd + Indexed + Prioritized>
{
    data: Vec<T>,
    indices: Vec<u32>
}

impl PartialEq for Candidate
{
    fn eq(&self, other: &Self) -> bool { self.distance == other.distance }
}

impl Eq for Candidate
{
}

impl PartialOrd for Candidate
{
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { self.distance.partial_cmp(&other.distance) }
}

impl Indexed for Candidate
{
    fn get_id(&self) -> u32 { self.id }
}

impl Prioritized for Candidate
{
    fn get_priority(&self) -> f32 { self.distance }
}

const LABEL_NEW: u32 = u32::MAX;
const LABEL_DELETED: u32 = u32::MAX - 1;

impl<T: Clone + PartialOrd + Indexed + Prioritized> IndexedPriorityQueue<T>
{
    fn new(size: usize) -> Self
    {
        let mut n = Self{data: Vec::<T>::new(), indices: vec![0; size]};
        n.reset();
        n
    }

    fn reset(&mut self) -> ()
    {
        self.data.clear();
        self.indices.fill(LABEL_NEW);
    }

    fn push(&mut self, item: T) -> ()
    {
        let mut index: u32 = self.indices[item.get_id() as usize];
        match index
        {
            LABEL_NEW =>
            {
                index = self.data.len() as u32;
                self.data.push(item.clone());
            },
            LABEL_DELETED =>
            {
                return;
            }
            _ =>
            {
                if item.get_priority() >= self.data[index as usize].get_priority() { return; }
            }
        }

        loop
        {
            let index_moved =
            {
                let parent_exists = index > 0;
                if parent_exists
                {
                    let parent_index = (index - 1) / 2;
                    if &item < &self.data[parent_index as usize]
                    {
                        self.data[index as usize] = self.data[parent_index as usize].clone();
                        self.indices[self.data[index as usize].get_id() as usize] = index;
                        index = parent_index;
                        true
                    }
                    else
                    {
                        false
                    }
                }
                else
                {
                    false
                }
            };
            if !index_moved
            {
                self.data[index as usize] = item.clone();
                self.indices[item.get_id() as usize] = index;
                return;
            }
        }
    }

    fn pop(&mut self) -> Option<T>
    {
        if self.data.len() == 0 { return None; }
        let top: T = self.data[0].clone();
        self.indices[top.get_id() as usize] = LABEL_DELETED;
        if self.data.len() == 1 { self.data.pop(); return Some(top); }

        let back: T = self.data.last().unwrap().clone();
        let mut index: u32 = 0;
        loop
        {
            let left_index = 2 * index + 1;
            let right_index = 2 * index + 2;
            let left_exists = (left_index as usize) < self.data.len();
            let right_exists = (right_index as usize) < self.data.len();

            let index_moved =
            {
                if left_exists || right_exists
                {
                    let next_index =
                    {
                        if left_exists && right_exists
                        {
                            if self.data[left_index as usize].get_priority() < self.data[right_index as usize].get_priority()
                            {
                                left_index
                            }
                            else
                            {
                                right_index
                            }
                        }
                        else
                        {
                            left_index
                        }
                    };

                    if self.data[next_index as usize].get_priority() < back.get_priority()
                    {
                        self.data[index as usize] = self.data[next_index as usize].clone();
                        self.indices[self.data[index as usize].get_id() as usize] = index;
                        index = next_index;
                        true
                    }
                    else
                    {
                        false
                    }
                }
                else
                {
                    false
                }
            };

            if !index_moved
            {
                self.data[index as usize] = back.clone();
                self.indices[back.get_id() as usize] = index;
                self.data.pop();
                return Some(top);
            }
        }
    }
}

fn parse_ver3() -> Result<(Vec<Vec<Connection>>, Vec<Benchmark>), Box<dyn Error>>
{
    let mut graph: Vec<Vec<Connection>> = Vec::new();
    let mut benchmarks: Vec<Benchmark> = Vec::new();
    let file = File::open("dijkstra.txt")?;
    let reader = BufReader::new(file);
    
    let mut read_benchmarks = false;
    for option_line in reader.lines()
    {
        if let Ok(line) = option_line
        {
            if line.contains("GRAPH")
            {
                read_benchmarks = false;
            }
            else if line.contains("BENCHMARK")
            {
                read_benchmarks = true;
            }
            else if read_benchmarks
            {
                let mut parts = line.trim().split_whitespace();
                if let (Some(source_str), Some(destination_str)) = (parts.next(), parts.next())
                {
                    if let (Ok(source), Ok(destination)) = (source_str.parse::<u32>(), destination_str.parse::<u32>())
                    {
                        benchmarks.push(Benchmark{source:source, destination:destination});
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
            else
            {
                let mut parts = line.trim().split_whitespace();
                if let (Some(source_str), Some(destination_str), Some(distance_str)) = (parts.next(), parts.next(), parts.next())
                {
                    if let (Ok(source), Ok(destination), Ok(distance)) = (source_str.parse::<u32>(), destination_str.parse::<u32>(), distance_str.parse::<f32>())
                    {
                        let source_destination_max: usize = std::cmp::max(source as usize, destination as usize);
                        if graph.len() <= source_destination_max
                        {
                            graph.resize(source_destination_max + 1, Vec::new());
                        }
                        graph[source as usize].push(Connection{destination:destination, distance:distance});
                        graph[destination as usize].push(Connection{destination:source, distance:distance});
                    }
                    else
                    {
                        break;
                    }
                }
                else
                {
                    break;
                }
            }
        }
        else
        {
            break;
        }
    };
    Ok((graph, benchmarks))
}

fn solve_ver5(graph: &Vec<Vec<Connection>>, benchmarks: &Vec<Benchmark>)
{
    let mut candidates = IndexedPriorityQueue::new(graph.len());

    for benchmark in benchmarks
    {
        let Benchmark{ source, destination } = *benchmark;    
        candidates.reset();
        candidates.push(Candidate { id: source, int_distance: 0, distance: 0.0 });
        let mut distance = INFINITY;
        let mut int_distance = 0;

        while let Some(candidate) = candidates.pop()
        {
            if candidate.id == destination
            {
                int_distance = candidate.int_distance;
                distance = candidate.distance;
                break;
            }
            for connection in &graph[candidate.id as usize]
            {
                let new_candidate = Candidate {
                    id: connection.destination,
                    int_distance: candidate.int_distance + 1,
                    distance: candidate.distance + connection.distance,
                };
                candidates.push(new_candidate);
            }
        }
        println!("{source} -> {destination}: {distance} ({int_distance})");
    }
}

fn main() -> Result<(), Box<dyn Error>>
{
    let (graph, benchmarks) = parse_ver3()?;
    solve_ver5(&graph, &benchmarks);
    Ok(())
}