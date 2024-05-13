module Main where

-- import DataTypes
-- import Chromosome
-- import Population

-- parseLine :: String -> [Int]
-- parseLine = map read . words

-- readFileLines :: FilePath -> IO [[Int]]
-- readFileLines path = do
--     contents <- readFile path
--     return (map parseLine (lines contents))

find_element :: [Int] -> Int -> Int -> Int
find_element (x:xs) e i | e == x = i 
                        | otherwise = find_element xs e (i + 1)

get_cycle :: [Int] -> [Int] -> Int -> Int -> [Int]
get_cycle p1 p2 ini index = 
   
    let ele_p1 = p1 !! index
        ele_p2 = p2 !! index
        
        next_index = find_element p1 ele_p2 0
       
    in if ini == ele_p2 then [index]
       else index:(get_cycle p1 p2 ini next_index)

cycle_crossover' :: [Int] -> [Int] -> [Int] -> Int -> ([Int], [Int])
cycle_crossover' l@(x:xs) (x':xs') (ci:cs) i = 
    if length l == i then ([],[]) 
    else let (f, s) = cycle_crossover' xs xs' cs (i+1)
         in if i == ci then (x:f, x':s)
         else (x':f, x:s)

cycle_crossover :: [Int] -> [Int] -> ([Int], [Int])
cycle_crossover p1 p2 = cycle_crossover' p1 p2 (get_cycle p1 p2 (head p1) 0) 0

main :: IO()
main = do
    let alleles1 = [9,8,2,1,7,4,5,10,6,3]
    let alleles2 = [1,2,3,4,5,6,7,8,9,10]
    print $ cycle_crossover alleles1 alleles2

    -- population <- generate_integer_permuted_population 20 10
    -- print population
    -- genetic_algorithm population 100
