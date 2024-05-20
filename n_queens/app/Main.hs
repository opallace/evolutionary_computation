module Main where

import DataTypes
import Chromosome
import Population

parseLine :: String -> [Int]
parseLine = map read . words

readFileLines :: FilePath -> IO [[Int]]
readFileLines path = do
    contents <- readFile path
    return (map parseLine (lines contents))

main :: IO()
main = do
    print $ evaluate_chromosome'' [(1,1),(2,2),(3,3),(4,4)]
    -- population <- generate_integer_permuted_population 20 10
    -- evaluated_population <- evaluate_population population
    -- print evaluated_population
    -- genetic_algorithm population 100
