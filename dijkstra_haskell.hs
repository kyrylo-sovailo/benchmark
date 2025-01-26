import qualified Control.Exception as E (try)
import qualified Data.Char as C (isDigit, digitToInt)
import qualified Data.List as L (null, isPrefixOf, foldl')
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
internal_bit_key :: Word32 -> (Word32, Word32)
internal_bit_key key = internal_bit_key' key 0 1

internal_bit_key' :: Word32 -> Word32 -> Word32 -> (Word32, Word32)
internal_bit_key' 0 bit_key bit_mask = (bit_key, bit_mask `shiftR` 1)
internal_bit_key' key bit_key bit_mask = let
    key_is_left = (key .&. 1) == 1
    next_key = (key - 1) `shiftR` 1
    next_bit_key = if key_is_left then bit_key else bit_key .|. bit_mask
    next_bit_mask = bit_mask `shiftL` 1
    in internal_bit_key' next_key next_bit_key next_bit_mask

data RandomMap a = RandomMapEmpty | RandomMapNode (RandomMap a) (Maybe a) (RandomMap a) deriving (Show)

m_empty :: RandomMap a
m_empty = RandomMapEmpty

m_lookup :: Word32 -> RandomMap a -> Maybe a
m_lookup key mp = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` mp `seq` m_internal_lookup bit_key bit_mask mp

m_internal_lookup :: Word32 -> Word32 -> RandomMap a -> Maybe a
m_internal_lookup _ _ RandomMapEmpty = Nothing
m_internal_lookup _ 0 (RandomMapNode _ old_value _) = old_value
m_internal_lookup bit_key bit_mask (RandomMapNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0 
        in if go_left then m_internal_lookup bit_key next_bit_mask left
        else m_internal_lookup bit_key next_bit_mask right

m_insert :: Word32 -> a -> RandomMap a -> RandomMap a
m_insert key new_value mp = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` new_value `seq` mp `seq` m_internal_insert bit_key bit_mask new_value mp

m_internal_insert :: Word32 -> Word32 -> a -> RandomMap a -> RandomMap a
m_internal_insert _ 0 new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
m_internal_insert _ 0 new_value (RandomMapNode left _ right) = RandomMapNode left (Just new_value) right
m_internal_insert bit_key bit_mask new_value mp =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = m_internal_insert bit_key next_bit_mask new_value
        in if go_left then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insert RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insert left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insert RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insert right)

m_insertWith :: (a -> a -> a) -> Word32 -> a -> RandomMap a -> RandomMap a
m_insertWith f key new_value mp = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` new_value `seq` mp `seq` m_internal_insertWith f bit_key bit_mask new_value mp

m_internal_insertWith :: (a -> a -> a) -> Word32 -> Word32 -> a -> RandomMap a -> RandomMap a
m_internal_insertWith _ _ 0 new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
m_internal_insertWith _ _ 0 new_value (RandomMapNode left Nothing right) = RandomMapNode left (Just new_value) right
m_internal_insertWith f _ 0 new_value (RandomMapNode left (Just old_value) right) = RandomMapNode left (Just $ f new_value old_value) right
m_internal_insertWith f bit_key bit_mask new_value mp =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = m_internal_insertWith f bit_key next_bit_mask new_value
        in if go_left then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insert RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insert left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insert RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insert right)

data RandomSet = RandomSetEmpty | RandomSetNode RandomSet Bool RandomSet deriving (Show)

s_empty :: RandomSet
s_empty = RandomSetEmpty

s_member :: Word32 -> RandomSet -> Bool
s_member key st = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` st `seq` s_internal_member bit_key bit_mask st

s_internal_member :: Word32 -> Word32 -> RandomSet -> Bool
s_internal_member _ _ RandomSetEmpty = False
s_internal_member _ 0 (RandomSetNode _ old_value _) = old_value
s_internal_member bit_key bit_mask (RandomSetNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0 
        in if go_left then s_internal_member bit_key next_bit_mask left
        else s_internal_member bit_key next_bit_mask right

s_insert :: Word32 -> RandomSet -> RandomSet
s_insert key st = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` st `seq` s_internal_insert bit_key bit_mask st

s_internal_insert :: Word32 -> Word32 -> RandomSet -> RandomSet
s_internal_insert _ 0 RandomSetEmpty = RandomSetNode RandomSetEmpty True RandomSetEmpty
s_internal_insert _ 0 (RandomSetNode left _ right) = RandomSetNode left True right
s_internal_insert bit_key bit_mask st =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = s_internal_insert bit_key next_bit_mask
        in if go_left then case st of
            RandomSetEmpty -> RandomSetNode (partial_insert RandomSetEmpty) False RandomSetEmpty
            RandomSetNode left old_value right -> RandomSetNode (partial_insert left) old_value right
        else case st of
            RandomSetEmpty -> RandomSetNode RandomSetEmpty False (partial_insert RandomSetEmpty)
            RandomSetNode left old_value right -> RandomSetNode left old_value (partial_insert right)

s_delete :: Word32 -> RandomSet -> RandomSet
s_delete key st = let (bit_key, bit_mask) = internal_bit_key key
    in key `seq` st `seq` s_internal_delete bit_key bit_mask st

s_internal_delete :: Word32 -> Word32 -> RandomSet -> RandomSet
s_internal_delete _ _ RandomSetEmpty = RandomSetEmpty
s_internal_delete _ 0 (RandomSetNode RandomSetEmpty _ RandomSetEmpty) = RandomSetEmpty
s_internal_delete _ 0 (RandomSetNode left _ right) = RandomSetNode left False right
s_internal_delete bit_key bit_mask (RandomSetNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_delete = s_internal_delete bit_key next_bit_mask
        in if go_left then
            RandomSetNode (partial_delete left) old_value right
        else
            RandomSetNode left old_value (partial_delete right)

-- Parsing --
-------------
data Benchmark = Benchmark Word32 Word32 deriving (Show)
data Connection = Connection Word32 Float deriving (Show)
parse :: String -> ((RandomMap [Connection]), [Benchmark])
parse input = parse_graph (m_empty, []) input

parse_graph :: ((RandomMap [Connection]), [Benchmark]) -> String -> ((RandomMap [Connection]), [Benchmark])
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
        graph' = m_insertWith (++) source ([Connection destination distance]) graph
        graph'' = m_insertWith (++) destination ([Connection source distance]) graph'
        in graph'' `seq` parse_graph (graph'', benchmarks) postspace4
    else error "Invalid symbol"

parse_benchmarks :: ((RandomMap [Connection]), [Benchmark]) -> String -> ((RandomMap [Connection]), [Benchmark])
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
solve :: (RandomMap [Connection]) -> [Benchmark] -> IO ()
solve graph benchmarks = case benchmarks of
    [] -> return ()
    (benchmark:benchmarks') -> do
        let (Solution i f) = solve_one graph benchmark
        let (Benchmark s d) = benchmark
        solve graph benchmarks'
        putStrLn (show s ++ " " ++ show d ++ " " ++ show i ++ " " ++ show f)

--solve_one (graph, benchmark) -> solution
solve_one :: (RandomMap [Connection]) -> Benchmark -> Solution
solve_one graph (Benchmark source destination) = solve_recursive graph destination (S.singleton $ Candidate source 0 0.0) s_empty

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
solve_recursive :: (RandomMap [Connection]) -> Word32 -> (Set Candidate) -> RandomSet -> Solution
solve_recursive graph destination queue mask
    | null queue = Solution 0 (1 / 0)
    | otherwise = let
        (candidate, queue') = S.deleteFindMin queue
        (Candidate id int_distance distance) = candidate
        in if id == destination then
            Solution int_distance distance
        else if s_member id mask then
            solve_recursive graph destination queue' mask
        else let
            (Just connections) = m_lookup id graph
            mask' = s_insert id mask
            queue'' = L.foldl' (\q c -> q `seq` c `seq` explore_connection q mask candidate c) queue' connections
            in queue'' `seq` solve_recursive graph destination queue'' mask'

--explore_connection (queue, mask, candidate, connection) -> queue
explore_connection :: (Set Candidate) -> RandomSet -> Candidate -> Connection -> (Set Candidate)
explore_connection queue mask candidate (Connection neighbor neighbor_distance) =
    if s_member neighbor mask then
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