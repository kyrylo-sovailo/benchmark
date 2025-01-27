import qualified Control.Exception as E (try)
import qualified Control.Monad as M (foldM)
import qualified Data.Char as C (isDigit, digitToInt)
import qualified Data.List as L (null, isPrefixOf, foldl, foldl', find, map)
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
parse input = parse_graph input

parse_graph :: String -> ((RandomMap [Connection]), [Benchmark])
parse_graph input = let postspace = parse_space input in
    if L.null postspace then
        (m_empty, [])
    else if L.isPrefixOf "GRAPH" postspace then
        parse_graph (drop 5 postspace)
    else if L.isPrefixOf "BENCHMARK" postspace then
        parse_benchmarks (drop 10 postspace)
    else if C.isDigit $ head postspace then let
        (source, postsource) = parse_integer (0, postspace)
        postspace2 = parse_space postsource
        (destination, postdestination) = parse_integer (0, postspace2)
        postspace3 = parse_space postdestination
        (distance, postdistance) = parse_real (0.0, postspace3)
        (graph, benchmarks) = parse_graph postdistance
        graph' = m_insertWith (++) source ([Connection destination distance]) graph
        graph'' = m_insertWith (++) destination ([Connection source distance]) graph'
        in (graph'', benchmarks)
    else error "Invalid symbol"

parse_benchmarks :: String -> ((RandomMap [Connection]), [Benchmark])
parse_benchmarks input = let postspace = parse_space input in
    if L.null postspace then
        (m_empty, [])
    else if L.isPrefixOf "GRAPH" postspace then
        parse_graph (drop 5 postspace)
    else if L.isPrefixOf "BENCHMARK" postspace then
        parse_benchmarks (drop 10 postspace)
    else if C.isDigit $ head postspace then let
        (source, postsource) = parse_integer (0, postspace)
        postspace2 = parse_space postsource
        (destination, postdestination) = parse_integer (0, postspace2)
        (graph, benchmarks) = parse_benchmarks postdestination
        benchmarks' = (Benchmark source destination):benchmarks
        in (graph, benchmarks')
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
--solve (graph, benchmarks) -> solutions
solve :: (RandomMap [Connection]) -> [Benchmark] -> IO ()
solve graph benchmarks = M.foldM (\_ benchmark -> solve_one graph benchmark) () benchmarks

--solve_one (graph, benchmark) -> IO ()
solve_one :: (RandomMap [Connection]) -> Benchmark -> IO ()
solve_one graph (Benchmark source destination) = 
    let candidates = enumerate_candidates graph source
        source_destination = show source ++ " " ++ show destination
        in case L.find (\(Candidate id _ _) -> id == destination) candidates of
            Just (Candidate _ int_distance distance) -> putStrLn (source_destination ++ " " ++ show int_distance ++ " " ++ show distance)
            Nothing -> putStrLn (source_destination ++ " " ++ "0" ++ " " ++ "inf")

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

--enumerate_candidates(graph source) -> candidates
enumerate_candidates :: (RandomMap [Connection]) -> Word32 -> [Candidate]
enumerate_candidates graph source = enumerate_candidates' graph s_empty (S.singleton $ Candidate source 0 0.0)

--enumerate_candidates'(graph enumerated_candidates pending_candidates) -> candidates
enumerate_candidates' :: (RandomMap [Connection]) -> RandomSet -> (Set Candidate) -> [Candidate]
enumerate_candidates' graph enumerated_candidates pending_candidates =
    let (closest_candidate, pending_candidates') = S.deleteFindMin pending_candidates
        (Candidate closest_candidate_id _ _) = closest_candidate
        neighbors = enumerate_neighbors graph closest_candidate enumerated_candidates
        pending_candidates'' = L.foldl (\s c -> S.insert c s) pending_candidates' neighbors
        enumerated_candidates' = s_insert closest_candidate_id enumerated_candidates
        in closest_candidate : (enumerate_candidates' `seq` pending_candidates'' `seq` enumerate_candidates'
            graph enumerated_candidates' pending_candidates'')

--enumerate_neighbors(graph candidate enumerated_candidates) -> neighbors
enumerate_neighbors :: (RandomMap [Connection]) -> Candidate -> RandomSet -> [Candidate]
enumerate_neighbors graph (Candidate id int_distance distance) enumerated_candidates =
    let (Just connections) = m_lookup id graph
        filtered_connections = filter (\(Connection n _) -> not $ s_member n enumerated_candidates) connections
        new_candidates = L.map (\(Connection n d) -> Candidate n (int_distance + 1) (d + distance)) filtered_connections
        in new_candidates

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