module Day1
    ( run
    ) where
import Data.List
import Data.Maybe

removePluses :: String -> String
removePluses xs = [ x | x <- xs, not (x `elem` "+") ]

folder :: [Int] -> Int -> [Int]
folder acc el  =
    total : acc where total = head acc + el

run :: IO ()
run = do 
    s <- readFile "src/input.txt"
    let stringsToInts = map (read :: String -> Int)
    let numbers = stringsToInts $ map removePluses $ lines s 
    
    -- part one
    print $ sum numbers

    -- part two
    let infiniteNumbers = cycle numbers
    let numbersWithTotals = scanl folder [0] infiniteNumbers
    let result = find (\total -> elem (head total) (tail $ total)) numbersWithTotals
    print $ head $ fromJust result