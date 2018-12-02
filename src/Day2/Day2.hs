module Day2.Day2
    ( run
    ) where
import Data.List
import Data.Maybe
import qualified Data.Map


histogram :: String -> [(Char, Int)]
histogram s = Data.Map.toList $ Data.Map.fromListWith (+) [(c,1) | c <- s]

count :: Int -> [[(Int)]] -> Int
count number list = 
    length $ filter (\x -> number `elem` (x) ) $ list

sameParts :: String -> String -> String -> String
sameParts (hs1:s1) (hs2:s2) acc =
    if hs1 == hs2
    then sameParts s1 s2 (acc ++ [hs1])
    else sameParts s1 s2 acc
sameParts [] [] acc = acc    

isSolution :: String -> String -> Int -> Bool
isSolution (hs1:s1) (hs2:s2) 0 = 
    if hs1 /= hs2 
    then isSolution s1 s2 1 
    else isSolution s1 s2 0
isSolution (hs1:s1) (hs2:s2) 1 = 
    if hs1 /= hs2 
    then isSolution s1 s2 2 
    else isSolution s1 s2 1
isSolution [] [] 1 = True
isSolution [] [] 0 = False
isSolution [] [] _ = False
isSolution s1 s2 n = False


run :: IO ()
run = do 
    s <- readFile "src/Day2/input"
    let input = lines s
    -- part one
    let histograms = map histogram $ input
    let histNumbers = map (map snd) histograms 
    print $ count 3 histNumbers * count 2 histNumbers
    -- part two
    let pairs = [(x,y) | x:ys <- tails input, y <- ys]
    let result = find (\(s1,s2) -> isSolution s1 s2 0) pairs 
    print result
    print $ sameParts ((fst . fromJust) result) ((snd . fromJust) result) ""