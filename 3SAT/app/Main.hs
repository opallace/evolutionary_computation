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
    population <- generate_boolean_population 25 100 (False, True)
    formula    <- readFileLines "entrada.txt"
    
    evaluated_population <- evaluate_population formula population
    print $ n_best_fitness_selection 5 evaluated_population
    
    
    -- genetic_algorithm population 10000 formula