import qualified Control.Exception as E (try)
import qualified Data.Char as C (isDigit, digitToInt)
import qualified Data.List as L (null, isPrefixOf, foldl')
import qualified Data.Map.Strict as M (empty, insertWith, lookup)
import qualified Data.Set as S (empty, singleton, member, insert, deleteFindMin)
import qualified System.IO as I (hSetBuffering, stdin, stdout)

import Control.Exception (SomeException)
import Data.Bits
import Data.Map (Map)
import Data.Set (Set)
import Data.Word (Word32)
import System.IO (BufferMode(NoBuffering))

-- Random Access Map --
-----------------------
-- 0:                        0
-- 1:            1                       2
-- 2:      3           4          5            6
-- 3:   7     8     9    10    11    12    13    14
-- 4: 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30
internal_is_left :: Word32 -> Bool
internal_is_left key =
    let level = (32 - countLeadingZeros (key + 1) - 1)
        left_threshold = shiftL 3 (level - 1)
        in key + 1 < left_threshold

internal_shift_key :: Word32 -> Word32
internal_shift_key key =
    if key <= 2 then
        0 --math breaks down
    else
        (((((key - 1) `shiftR` 1) - 1) `shiftR` 1) `shiftL` 1) + (2 - (key .&. 1)) -- two steps up, one step down, one step sideways

data RandomMap a = RandomMapEmpty | RandomMapNode (RandomMap a) (Maybe a) (RandomMap a) deriving (Show)

m_empty :: RandomMap a
m_empty = RandomMapEmpty

m_lookup :: Word32 -> RandomMap a -> Maybe a
m_lookup _ RandomMapEmpty = Nothing
m_lookup 0 (RandomMapNode _ old_value _) = old_value
m_lookup key (RandomMapNode left old_value right) =
    let next_key = internal_shift_key key
        in if internal_is_left key then m_lookup next_key left
        else m_lookup next_key right

m_insert :: Word32 -> a -> RandomMap a -> RandomMap a
m_insert 0 new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
m_insert 0 new_value (RandomMapNode left _ right) = RandomMapNode left (Just new_value) right
m_insert key new_value mp =
    let next_key = internal_shift_key key
        partial_insert = m_insert next_key new_value
        in if internal_is_left key then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insert RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insert left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insert RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insert right)

m_insertWith :: (a -> a -> a) -> Word32 -> a -> RandomMap a -> RandomMap a
m_insertWith _ 0 new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
m_insertWith _ 0 new_value (RandomMapNode left Nothing right) = RandomMapNode left (Just new_value) right
m_insertWith f 0 new_value (RandomMapNode left (Just old_value) right) = RandomMapNode left (Just $ f new_value old_value) right
m_insertWith f key new_value mp =
    let next_key = internal_shift_key key
        partial_insertWith = m_insertWith f next_key new_value
        in if internal_is_left key then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insertWith RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insertWith left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insertWith RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insertWith right)

data RandomSet = RandomSetEmpty | RandomSetNode RandomSet Bool RandomSet deriving (Show)

s_empty :: RandomSet
s_empty = RandomSetEmpty

s_member :: Word32 -> RandomSet -> Bool
s_member _ RandomSetEmpty = False
s_member 0 (RandomSetNode _ old_value _) = old_value
s_member key (RandomSetNode left old_value right) =
    let next_key = internal_shift_key key
        in if internal_is_left key then s_member next_key left
        else s_member next_key right

s_insert :: Word32 -> RandomSet -> RandomSet
s_insert 0 RandomSetEmpty = RandomSetNode RandomSetEmpty True RandomSetEmpty
s_insert 0 (RandomSetNode left _ right) = RandomSetNode left True right
s_insert key st =
    let next_key = internal_shift_key key
        partial_insert = s_insert next_key
        in if internal_is_left key then case st of
            RandomSetEmpty -> RandomSetNode (partial_insert RandomSetEmpty) False RandomSetEmpty
            RandomSetNode left old_value right -> RandomSetNode (partial_insert left) old_value right
        else case st of
            RandomSetEmpty -> RandomSetNode RandomSetEmpty False (partial_insert RandomSetEmpty)
            RandomSetNode left old_value right -> RandomSetNode left old_value (partial_insert right)

s_delete :: Word32 -> RandomSet -> RandomSet
s_delete _ RandomSetEmpty = RandomSetEmpty
s_delete 0 (RandomSetNode RandomSetEmpty _ RandomSetEmpty) = RandomSetEmpty
s_delete 0 (RandomSetNode left _ right) = RandomSetNode left False right
s_delete key (RandomSetNode left old_value right) =
    let next_key = internal_shift_key key
        partial_delete = s_delete next_key
        in if internal_is_left key then
            RandomSetNode (partial_delete left) old_value right
        else
            RandomSetNode left old_value (partial_delete right)

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
solve :: (Map Word32 [Connection]) -> [Benchmark] -> IO ()
solve graph benchmarks = case benchmarks of
    [] -> return ()
    (benchmark:benchmarks') -> do
        let (Solution i f) = solve_one graph benchmark
        let (Benchmark s d) = benchmark
        solve graph benchmarks'
        putStrLn (show s ++ " " ++ show d ++ " " ++ show i ++ " " ++ show f)

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
            queue'' = L.foldl' (\q c -> q `seq` c `seq` explore_connection q mask candidate c) queue' connections
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
            (graph, benchmarks) = parse content
            in graph `seq` benchmarks `seq` solve graph benchmarks