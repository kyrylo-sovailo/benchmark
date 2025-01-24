import qualified Control.Exception as E (try)
import qualified Data.Char as C (isDigit, digitToInt)
import qualified Data.List as L (null, isPrefixOf, foldl')
import qualified Data.Map.Strict as M (empty, insertWith, lookup)
import qualified Data.Set as S (empty, singleton, member, insert, deleteFindMin)
import qualified System.IO as I (hSetBuffering, stdin, stdout)

import Control.Exception (SomeException)
import Data.Map (Map)
import Data.Set (Set)
import Data.Word (Word32)
import System.IO (BufferMode(NoBuffering))

-- Parsing --
-------------
data Benchmark = Benchmark Word32 Word32 deriving (Show)
data Connection = Connection Word32 Float deriving (Show)
parse :: String -> ((Map Word32 [Connection]), [Benchmark])
parse input = parse_graph (M.empty, []) input

parse_graph :: ((Map Word32 [Connection]), [Benchmark]) -> String -> ((Map Word32 [Connection]), [Benchmark])
parse_graph (graph, benchmarks) input = let postspace = parse_space input in
    if L.null postspace then
        (graph, benchmarks)
    else if L.isPrefixOf "GRAPH" postspace then
        parse_graph (graph, benchmarks) (drop 5 postspace)
    else if L.isPrefixOf "BENCHMARK" postspace then
        parse_benchmarks (graph, benchmarks) (drop 10 postspace)
    else if C.isDigit $ head postspace then let
        (source, postsource) = parse_integer (0, postspace)
        postspace2 = parse_space postsource
        (destination, postdestination) = parse_integer (0, postspace2)
        postspace3 = parse_space postdestination
        (distance, postdistance) = parse_real (0.0, postspace3)
        postspace4 = parse_space postdistance
        graph' = M.insertWith (++) source ([Connection destination distance]) graph
        graph'' = M.insertWith (++) destination ([Connection source distance]) graph'
        in graph'' `seq` parse_graph (graph'', benchmarks) postspace4
    else error "Invalid symbol"

parse_benchmarks :: ((Map Word32 [Connection]), [Benchmark]) -> String -> ((Map Word32 [Connection]), [Benchmark])
parse_benchmarks (graph, benchmarks) input = let postspace = parse_space input in
    if L.null postspace then
        (graph, benchmarks)
    else if L.isPrefixOf "GRAPH" postspace then
        parse_graph (graph, benchmarks) (drop 5 postspace)
    else if L.isPrefixOf "BENCHMARK" postspace then
        parse_benchmarks (graph, benchmarks) (drop 10 postspace)
    else if C.isDigit $ head postspace then let
        (source, postsource) = parse_integer (0, postspace)
        postspace2 = parse_space postsource
        (destination, postdestination) = parse_integer (0, postspace2)
        postspace3 = parse_space postdestination
        benchmarks' = (Benchmark source destination):benchmarks
        in benchmarks' `seq` parse_benchmarks (graph, benchmarks') postspace3
    else error "Invalid symbol"

--parse_integer (number, input) -> (number, input)
parse_integer :: (Word32, String) -> (Word32, String)
parse_integer (number, []) = (number, [])
parse_integer (number, (c:rest))
    | C.isDigit c = parse_integer (10 * number + (fromIntegral $ C.digitToInt c), rest)
    | otherwise = (number, (c:rest))

--parse_real (number, input) -> (number, input)
parse_real :: (Float, String) -> (Float, String)
parse_real (number, []) = (number, [])
parse_real (number, (c:rest))
    | C.isDigit c = parse_real (10 * number + (fromIntegral $ C.digitToInt c), rest)
    | c == '.' = parse_real_fraction (number, rest) 10
    | otherwise = (number, (c:rest))

--parse_real_fraction (number, input) -> divider -> (number, input)
parse_real_fraction :: (Float, String) -> Word32 -> (Float, String)
parse_real_fraction (number, []) _ = (number, [])
parse_real_fraction (number, (c:rest)) divider
    | C.isDigit c = parse_real_fraction (number + (fromIntegral $ C.digitToInt c) / (fromIntegral divider), rest) (10 * divider)
    | otherwise = (number, (c:rest))

--parse_space input -> input
parse_space :: String -> String
parse_space [] = []
parse_space (c:rest)
    | c == ' ' || c == '\t' || c == '\n' || c == '\r' = parse_space rest
    | otherwise = (c:rest)

-- Solving --
-------------
data Solution = Solution Word32 Float deriving (Show)
--solve (graph, benchmarks) -> solutions
solve :: (Map Word32 [Connection]) -> [Benchmark] -> [Solution]
solve graph benchmarks = map (solve_one graph) benchmarks

--solve_one (graph, benchmark) -> solution
solve_one :: (Map Word32 [Connection]) -> Benchmark -> Solution
solve_one graph (Benchmark source destination) = solve_recursive graph destination (S.singleton $ Candidate source 0 0.0) S.empty

data Candidate = Candidate Word32 Word32 Float
instance Eq Candidate where
    (Candidate id1 _ distance1) == (Candidate id2 _ distance2) = id1 == id2 && distance1 == distance2
instance Ord Candidate where
    compare (Candidate id1 _ distance1) (Candidate id2 _ distance2)
        | distance1 > distance2 = GT
        | distance1 < distance2 = LT
        | id1 > id2 = GT
        | id1 < id2 = LT
        | otherwise = EQ

--solve_recursive (graph, destination, queue, mask) -> solution
solve_recursive :: (Map Word32 [Connection]) -> Word32 -> (Set Candidate) -> (Set Word32) -> Solution
solve_recursive graph destination queue mask
    | null queue = Solution 0 (1 / 0)
    | otherwise = let
        (candidate, queue') = S.deleteFindMin queue
        (Candidate id int_distance distance) = candidate
        in if id == destination then
            Solution int_distance distance
        else if S.member id mask then
            solve_recursive graph destination queue' mask
        else let
            (Just connections) = M.lookup id graph
            mask' = S.insert id mask
            queue'' = L.foldl' (\q c -> explore_connection q mask candidate c) queue' connections
            in queue'' `seq` solve_recursive graph destination queue'' mask'

--explore_connection (queue, mask, candidate, connection) -> queue
explore_connection :: (Set Candidate) -> (Set Word32) -> Candidate -> Connection -> (Set Candidate)
explore_connection queue mask candidate (Connection neighbor neighbor_distance) =
    if S.member neighbor mask then
        queue
    else let
        (Candidate _ int_distance distance) = candidate
        in S.insert (Candidate neighbor (int_distance + 1) (distance + neighbor_distance)) queue

-- Main --
----------
main :: IO ()
main = do
    I.hSetBuffering I.stdin NoBuffering
    I.hSetBuffering I.stdout NoBuffering
    either_content <- E.try $ readFile "dijkstra.txt" :: IO (Either SomeException String)
    case either_content of
        Left exception ->
            putStrLn "readFile() failed"
        Right content -> let
            format = (\(Benchmark s d, Solution i f) -> show s ++ " " ++ show d ++ " " ++ show i ++ " " ++ show f ++ "\n" )
            (graph, benchmarks) = parse content
            solutions = graph `seq` benchmarks `seq` solve graph benchmarks
            output = solutions `seq` L.foldl' (++) "" (map format (zip benchmarks solutions))
            in putStr output