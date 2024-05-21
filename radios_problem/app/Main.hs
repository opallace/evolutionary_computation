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
    population <- generate_boolean_population 20 10 (False, True)
    let ev = evaluate_population population
    genetic_algorithm ev 10000

   