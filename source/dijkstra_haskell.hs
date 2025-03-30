import qualified Control.Exception as CE (try)
import qualified Control.Monad as CM (foldM)
import qualified Data.Char as DC (isDigit, digitToInt)
import qualified Data.Either as DE (lefts, rights)
import qualified Data.List as DL (foldl, foldl', find, map)
import qualified Data.Map.Strict as DM (empty, lookup, insert, insertWith, fromListWith)
import qualified Data.Set as DS (empty, member, singleton, insert, deleteFindMin)
import qualified Data.IntMap.Strict as DIM (empty, lookup, insert, insertWith, fromListWith)
import qualified Data.IntSet as DIS (empty, member, singleton, insert, deleteFindMin)
import qualified System.IO as SI (hSetBuffering, stdin, stdout)

import Control.Exception (SomeException)
import Data.Bits
import Data.Map (Map)
import Data.Set (Set)
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Word (Word32)
import System.IO (BufferMode(NoBuffering))

----------------------
-- Version selector --
----------------------
-- Option 1 --
--type Vertices = RandomSet
--vertices_empty = random_set_empty
--vertices_member = random_set_member
--vertices_insert = random_set_insert
-- Option 2 --
--type Vertices = Set Word32
--vertices_empty = DS.empty
--vertices_member = DS.member
--vertices_insert = DS.insert
-- Option 3 --
type Vertices = IntSet
vertices_empty = DIS.empty
vertices_member = \key st -> DIS.member (fromIntegral key) st
vertices_insert = \key st -> DIS.insert (fromIntegral key) st

-- Option 1 --
--type Edges = RandomMap [Connection]
--edges_empty = random_map_empty
--edges_lookup = random_map_lookup
--edges_insert_with = random_map_insert_with
--edges_from_list_with = random_map_from_list_with
-- Option 2 --
--type Edges = Map Word32 [Connection]
--edges_empty = DM.empty
--edges_lookup = DM.lookup
--edges_insert_with = DM.insertWith
--edges_from_list_with = DM.fromListWith
-- Option 3 --
type Edges = IntMap [Connection]
edges_empty = DIM.empty
edges_lookup = \key mp -> DIM.lookup (fromIntegral key) mp
edges_insert_with = \f key value mp -> DIM.insertWith f (fromIntegral key) value mp
edges_from_list_with = \f values -> DIM.fromListWith f (DL.map (\(a, b) -> (fromIntegral a, b)) values)

parse :: String -> (Edges, [Benchmark])
parse = parse_ver1
solve :: Edges -> [Benchmark] -> IO ()
solve = solve_ver1

-----------------------
-- Random Access Map --
-----------------------
random_map_bit_key' :: Word32 -> (Word32, Word32)
random_map_bit_key' key = random_map_bit_key'' key (0, 1)

random_map_bit_key'' :: Word32 -> (Word32, Word32) -> (Word32, Word32)
random_map_bit_key'' 0 (bit_key, bit_mask) = (bit_key, bit_mask `shiftR` 1)
random_map_bit_key'' key (bit_key, bit_mask) = let
    key_is_left = (key .&. 1) == 1
    next_key = (key - 1) `shiftR` 1
    next_bit_key = if key_is_left then bit_key else bit_key .|. bit_mask
    next_bit_mask = bit_mask `shiftL` 1
    in random_map_bit_key'' next_key (next_bit_key, next_bit_mask)

data RandomMap a = RandomMapEmpty | RandomMapNode (RandomMap a) (Maybe a) (RandomMap a) deriving (Show)

random_map_empty :: RandomMap a
random_map_empty = RandomMapEmpty

random_map_lookup :: Word32 -> RandomMap a -> Maybe a
random_map_lookup key mp = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` mp `seq` random_map_lookup' (bit_key, bit_mask) mp

random_map_lookup' :: (Word32, Word32) -> RandomMap a -> Maybe a
random_map_lookup' (_, _) RandomMapEmpty = Nothing
random_map_lookup' (_, 0) (RandomMapNode _ old_value _) = old_value
random_map_lookup' (bit_key, bit_mask) (RandomMapNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0 
        in if go_left then random_map_lookup' (bit_key, next_bit_mask) left
        else random_map_lookup' (bit_key, next_bit_mask) right

random_map_insert :: Word32 -> a -> RandomMap a -> RandomMap a
random_map_insert key new_value mp = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` new_value `seq` mp `seq` random_map_insert' (bit_key, bit_mask) new_value mp

random_map_insert' :: (Word32, Word32) -> a -> RandomMap a -> RandomMap a
random_map_insert' (_, 0) new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
random_map_insert' (_, 0) new_value (RandomMapNode left _ right) = RandomMapNode left (Just new_value) right
random_map_insert' (bit_key, bit_mask) new_value mp =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = random_map_insert' (bit_key, next_bit_mask) new_value
        in if go_left then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insert RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insert left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insert RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insert right)

random_map_insert_with :: (a -> a -> a) -> Word32 -> a -> RandomMap a -> RandomMap a
random_map_insert_with f key new_value mp = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` new_value `seq` mp `seq` random_map_insert_with' f (bit_key, bit_mask) new_value mp

random_map_insert_with' :: (a -> a -> a) -> (Word32, Word32) -> a -> RandomMap a -> RandomMap a
random_map_insert_with' _ (_, 0) new_value RandomMapEmpty = RandomMapNode RandomMapEmpty (Just new_value) RandomMapEmpty
random_map_insert_with' _ (_, 0) new_value (RandomMapNode left Nothing right) = RandomMapNode left (Just new_value) right
random_map_insert_with' f (_, 0) new_value (RandomMapNode left (Just old_value) right) = RandomMapNode left (Just $ f new_value old_value) right
random_map_insert_with' f (bit_key, bit_mask) new_value mp =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = random_map_insert_with' f (bit_key, next_bit_mask) new_value
        in if go_left then case mp of
            RandomMapEmpty -> RandomMapNode (partial_insert RandomMapEmpty) Nothing RandomMapEmpty
            RandomMapNode left old_value right -> RandomMapNode (partial_insert left) old_value right
        else case mp of
            RandomMapEmpty -> RandomMapNode RandomMapEmpty Nothing (partial_insert RandomMapEmpty)
            RandomMapNode left old_value right -> RandomMapNode left old_value (partial_insert right)

random_map_from_list_with :: (a -> a -> a) -> [(Word32, a)] -> RandomMap a
random_map_from_list_with f values = DL.foldl' (\mp (key, value) -> random_map_insert_with f key value mp) random_map_empty values

data RandomSet = RandomSetEmpty | RandomSetNode RandomSet Bool RandomSet deriving (Show)

random_set_empty :: RandomSet
random_set_empty = RandomSetEmpty

random_set_member :: Word32 -> RandomSet -> Bool
random_set_member key st = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` st `seq` random_set_member' bit_key bit_mask st

random_set_member' :: Word32 -> Word32 -> RandomSet -> Bool
random_set_member' _ _ RandomSetEmpty = False
random_set_member' _ 0 (RandomSetNode _ old_value _) = old_value
random_set_member' bit_key bit_mask (RandomSetNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0 
        in if go_left then random_set_member' bit_key next_bit_mask left
        else random_set_member' bit_key next_bit_mask right

random_set_insert :: Word32 -> RandomSet -> RandomSet
random_set_insert key st = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` st `seq` random_set_insert' bit_key bit_mask st

random_set_insert' :: Word32 -> Word32 -> RandomSet -> RandomSet
random_set_insert' _ 0 RandomSetEmpty = RandomSetNode RandomSetEmpty True RandomSetEmpty
random_set_insert' _ 0 (RandomSetNode left _ right) = RandomSetNode left True right
random_set_insert' bit_key bit_mask st =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_insert = random_set_insert' bit_key next_bit_mask
        in if go_left then case st of
            RandomSetEmpty -> RandomSetNode (partial_insert RandomSetEmpty) False RandomSetEmpty
            RandomSetNode left old_value right -> RandomSetNode (partial_insert left) old_value right
        else case st of
            RandomSetEmpty -> RandomSetNode RandomSetEmpty False (partial_insert RandomSetEmpty)
            RandomSetNode left old_value right -> RandomSetNode left old_value (partial_insert right)

random_set_delete :: Word32 -> RandomSet -> RandomSet
random_set_delete key st = let (bit_key, bit_mask) = random_map_bit_key' key
    in key `seq` st `seq` random_set_delete' bit_key bit_mask st

random_set_delete' :: Word32 -> Word32 -> RandomSet -> RandomSet
random_set_delete' _ _ RandomSetEmpty = RandomSetEmpty
random_set_delete' _ 0 (RandomSetNode RandomSetEmpty _ RandomSetEmpty) = RandomSetEmpty
random_set_delete' _ 0 (RandomSetNode left _ right) = RandomSetNode left False right
random_set_delete' bit_key bit_mask (RandomSetNode left old_value right) =
    let next_bit_mask = bit_mask `shiftR` 1
        go_left = bit_key .&. bit_mask == 0
        partial_delete = random_set_delete' bit_key next_bit_mask
        in if go_left then
            RandomSetNode (partial_delete left) old_value right
        else
            RandomSetNode left old_value (partial_delete right)

-------------
-- Parsing --
-------------
data Benchmark = Benchmark Word32 Word32 deriving (Show)
data Connection = Connection Word32 Float deriving (Show)

-- Version 1 --
parse_ver1 :: String -> (Edges, [Benchmark])
parse_ver1 input = parse_graph_ver1 (edges_empty, []) input

parse_graph_ver1 :: (Edges, [Benchmark]) -> String -> (Edges, [Benchmark])
parse_graph_ver1 (edges, benchmarks) input = let postspace = parse_space input in
    case postspace of
        [] ->
            (edges, benchmarks)
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver1 (edges, benchmarks) postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver1 (edges, benchmarks) postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            postspace3 = parse_space postdestination
            (distance, postdistance) = parse_real (0.0, postspace3)
            postspace4 = parse_space postdistance
            edges' = edges_insert_with (++) source ([Connection destination distance]) edges
            edges'' = edges_insert_with (++) destination ([Connection source distance]) edges'
            in edges'' `seq` parse_graph_ver1 (edges'', benchmarks) postspace4
        _ -> error "Invalid symbol"

parse_benchmarks_ver1 :: (Edges, [Benchmark]) -> String -> (Edges, [Benchmark])
parse_benchmarks_ver1 (edges, benchmarks) input = let postspace = parse_space input in
    case postspace of
        [] ->
            (edges, benchmarks)
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver1 (edges, benchmarks) postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver1 (edges, benchmarks) postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            postspace3 = parse_space postdestination
            benchmarks' = (Benchmark source destination):benchmarks
            in benchmarks' `seq` parse_benchmarks_ver1 (edges, benchmarks') postspace3
        _ -> error "Invalid symbol"

-- Version 2 --
parse_ver2 :: String -> (Edges, [Benchmark])
parse_ver2 input = parse_graph_ver2 input

parse_graph_ver2 :: String -> (Edges, [Benchmark])
parse_graph_ver2 input = let postspace = parse_space input in
    case postspace of
        [] ->
            (edges_empty, [])
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver2 postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver2 postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            postspace3 = parse_space postdestination
            (distance, postdistance) = parse_real (0.0, postspace3)
            (edges, benchmarks) = parse_graph_ver2 postdistance
            edges' = edges_insert_with (++) source ([Connection destination distance]) edges
            edges'' = edges_insert_with (++) destination ([Connection source distance]) edges'
            in (edges'', benchmarks)
        _ -> error "Invalid symbol"

parse_benchmarks_ver2 :: String -> (Edges, [Benchmark])
parse_benchmarks_ver2 input = let postspace = parse_space input in
    case postspace of
        [] ->
            (edges_empty, [])
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver2 postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver2 postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            (edges, benchmarks) = parse_benchmarks_ver2 postdestination
            benchmarks' = (Benchmark source destination):benchmarks
            in (edges, benchmarks')
        _ -> error "Invalid symbol"

-- Version 3 --
type EntryVer3 = Either (Word32, Connection) Benchmark
parse_ver3 :: String -> (Edges, [Benchmark])
parse_ver3 input = let
    entries = parse_graph_ver3 input
    connections = DE.lefts entries
    connections' = DL.map (\(key, connection) -> (key, [connection])) connections
    benchmarks = DE.rights entries
    graph = edges_from_list_with (++) connections'
    in (graph, benchmarks)

parse_graph_ver3 :: String -> [EntryVer3]
parse_graph_ver3 input = let postspace = parse_space input in
    case postspace of
        [] ->
            []
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver3 postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver3 postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            postspace3 = parse_space postdestination
            (distance, postdistance) = parse_real (0.0, postspace3)
            entries = parse_graph_ver3 postdistance
            in Left (source, Connection destination distance) : Left (destination, Connection source distance) : entries
        _ -> error "Invalid symbol"

parse_benchmarks_ver3 :: String -> [EntryVer3]
parse_benchmarks_ver3 input = let postspace = parse_space input in
    case postspace of
        [] ->
            []
        'G':'R':'A':'P':'H':postkey ->
            parse_graph_ver3 postkey
        'B':'E':'N':'C':'H':'M':'A':'R':'K':postkey ->
            parse_benchmarks_ver3 postkey
        key:_ | DC.isDigit key -> let
            (source, postsource) = parse_integer (0, postspace)
            postspace2 = parse_space postsource
            (destination, postdestination) = parse_integer (0, postspace2)
            entries = parse_benchmarks_ver3 postdestination
            in (Right $ Benchmark source destination) : entries
        _ -> error "Invalid symbol"

--parse_integer (number, input) -> (number, input)
parse_integer :: (Word32, String) -> (Word32, String)
parse_integer (number, []) = (number, [])
parse_integer (number, (c:rest))
    | DC.isDigit c = parse_integer (10 * number + (fromIntegral $ DC.digitToInt c), rest)
    | otherwise = (number, (c:rest))

--parse_real (number, input) -> (number, input)
parse_real :: (Float, String) -> (Float, String)
parse_real (number, []) = (number, [])
parse_real (number, (c:rest))
    | DC.isDigit c = parse_real (10 * number + (fromIntegral $ DC.digitToInt c), rest)
    | c == '.' = parse_fraction (number, rest) 10
    | otherwise = (number, (c:rest))

--parse_fraction (number, input) -> divider -> (number, input)
parse_fraction :: (Float, String) -> Word32 -> (Float, String)
parse_fraction (number, []) _ = (number, [])
parse_fraction (number, (c:rest)) divider
    | DC.isDigit c = parse_fraction (number + (fromIntegral $ DC.digitToInt c) / (fromIntegral divider), rest) (10 * divider)
    | otherwise = (number, (c:rest))

--parse_space input -> input
parse_space :: String -> String
parse_space [] = []
parse_space (c:rest)
    | c == ' ' || c == '\t' || c == '\n' || c == '\r' = parse_space rest
    | otherwise = (c:rest)

-------------
-- Solving --
-------------
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

-- Version 1 --

--solve_ver1 (edges, benchmarks) -> solutions
solve_ver1 :: Edges -> [Benchmark] -> IO ()
solve_ver1 edges benchmarks = case benchmarks of
    [] -> return ()
    (benchmark:benchmarks') -> do
        let (i, f) = solve_one_ver1 edges benchmark
        let (Benchmark s d) = benchmark
        solve_ver1 edges benchmarks'
        putStrLn (show s ++ " -> " ++ show d ++ ": " ++ show f ++ " (" ++ show i ++ ")")

--solve_one_ver1 (edges, benchmark) -> solution
solve_one_ver1 :: Edges -> Benchmark -> (Word32, Float)
solve_one_ver1 edges (Benchmark source destination) = solve_one_ver1' edges destination (DS.singleton $ Candidate source 0 0.0) vertices_empty

--solve_one_ver1' (edges, destination, queue, mask) -> solution
solve_one_ver1' :: Edges -> Word32 -> (Set Candidate) -> Vertices -> (Word32, Float)
solve_one_ver1' edges destination queue mask
    | null queue = (0, (1 / 0))
    | otherwise = let
        (candidate, queue') = DS.deleteFindMin queue
        (Candidate id int_distance distance) = candidate
        in if id == destination then
            (int_distance, distance)
        else if vertices_member id mask then
            solve_one_ver1' edges destination queue' mask
        else let
            (Just connections) = edges_lookup id edges
            mask' = vertices_insert id mask
            queue'' = DL.foldl' (\q c -> q `seq` c `seq` explore_connection_ver1 q mask candidate c) queue' connections
            in queue'' `seq` solve_one_ver1' edges destination queue'' mask'

--explore_connection_ver1 (queue, mask, candidate, connection) -> queue
explore_connection_ver1 :: (Set Candidate) -> Vertices -> Candidate -> Connection -> (Set Candidate)
explore_connection_ver1 queue mask candidate (Connection neighbor neighbor_distance) =
    if vertices_member neighbor mask then
        queue
    else let
        (Candidate _ int_distance distance) = candidate
        in DS.insert (Candidate neighbor (int_distance + 1) (distance + neighbor_distance)) queue

-- Version 2 --

--solve_ver2 (edges, benchmarks) -> solutions
solve_ver2 :: Edges -> [Benchmark] -> IO ()
solve_ver2 edges benchmarks = CM.foldM (\_ benchmark -> solve_one_ver2 edges benchmark) () benchmarks

--solve_one_ver2 (edges, benchmark) -> IO ()
solve_one_ver2 :: Edges -> Benchmark -> IO ()
solve_one_ver2 edges (Benchmark source destination) = 
    let candidates = enumerate_candidates_ver2 edges source
        solution = DL.find (\(Candidate id _ _) -> id == destination) candidates
        message = show source ++ " -> " ++ show destination ++ ": " ++ case solution of
            Just (Candidate _ int_distance distance) -> show distance ++ "(" ++ show int_distance ++ ")"
            Nothing -> "inf (0)"
        in putStrLn message

--enumerate_candidates_ver2(edges source) -> candidates
enumerate_candidates_ver2 :: Edges -> Word32 -> [Candidate]
enumerate_candidates_ver2 edges source = enumerate_candidates_ver2' edges vertices_empty (DS.singleton $ Candidate source 0 0.0)

--enumerate_candidates_ver2'(edges enumerated_candidates pending_candidates) -> candidates
enumerate_candidates_ver2' :: Edges -> Vertices -> (Set Candidate) -> [Candidate]
enumerate_candidates_ver2' edges enumerated_candidates pending_candidates =
    let (closest_candidate, pending_candidates') = DS.deleteFindMin pending_candidates
        (Candidate closest_candidate_id _ _) = closest_candidate
        neighbors = enumerate_neighbors_ver2 edges closest_candidate enumerated_candidates
        pending_candidates'' = DL.foldl' (\s c -> DS.insert c s) pending_candidates' neighbors
        enumerated_candidates' = vertices_insert closest_candidate_id enumerated_candidates
        in closest_candidate : (enumerated_candidates' `seq` pending_candidates'' `seq` enumerate_candidates_ver2'
            edges enumerated_candidates' pending_candidates'')

--enumerate_neighbors_ver2(edges candidate enumerated_candidates) -> neighbors
enumerate_neighbors_ver2 :: Edges -> Candidate -> Vertices -> [Candidate]
enumerate_neighbors_ver2 edges (Candidate id int_distance distance) enumerated_candidates =
    let (Just connections) = edges_lookup id edges
        filtered_connections = filter (\(Connection n _) -> not $ vertices_member n enumerated_candidates) connections
        new_candidates = DL.map (\(Connection n d) -> Candidate n (int_distance + 1) (d + distance)) filtered_connections
        in new_candidates

----------
-- Main --
----------
main :: IO ()
main = do
    SI.hSetBuffering SI.stdin NoBuffering
    SI.hSetBuffering SI.stdout NoBuffering
    either_content <- CE.try $ readFile "dijkstra.txt" :: IO (Either SomeException String)
    case either_content of
        Left exception ->
            putStrLn "readFile() failed"
        Right content -> let
            (edges, benchmarks) = content `seq` parse content
            in edges `seq` benchmarks `seq` solve edges benchmarks
