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
    population <- generate_boolean_population 30 100 (False, True)
    formula    <- readFileLines "entrada.txt"
    let ep = evaluate_population formula population
    
    genetic_algorithm ep 10000 formula 