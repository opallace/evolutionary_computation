module Main where

import DataTypes
import Chromosome
import Population


main :: IO()
main = do
    population <- generate_integer_permuted_population 30 1024
    let ep = evaluate_population population

    genetic_algorithm ep 10000
