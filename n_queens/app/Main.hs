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
    population <- generate_integer_permuted_population 20 10
    print population
    -- genetic_algorithm population 100

   