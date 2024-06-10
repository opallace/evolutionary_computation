module Main where

import DataTypes
import Chromosome
import Population

main :: IO()
main = do
    let table        = create_table 8
        valued_table = process_table table

    population <- generate_integer_permuted_population 30 8
    let ep = evaluate_population population valued_table

    genetic_algorithm ep valued_table 100
