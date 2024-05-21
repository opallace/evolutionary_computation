module Population where

import System.IO
import System.Random
import Control.Monad
import Control.Parallel.Strategies (parMap, rpar)

import Data.List
import DataTypes
import Chromosome

------------------------------------- GENERATE POPULATION --------------------------------------------

generate_integer_permuted_population :: Int -> Int -> IO Population
generate_integer_permuted_population n_population n_genes = do
    pop <- replicateM n_population (generate_integer_permuted_chromosome n_genes)
    return pop

------------------------------------- EVALUATE POPULATION --------------------------------------------

-- evaluate_population :: Population -> Population
-- evaluate_population [] = []
-- evaluate_population ((IntegerPermutedChromosome fitness alleles):population) =
--     let new_fitness = evaluate_chromosome (IntegerPermutedChromosome fitness alleles)
--     in (IntegerPermutedChromosome new_fitness alleles) : (evaluate_population population)

evaluate_population :: Population -> Population
evaluate_population [] = []
evaluate_population population = 
    let evaluatedPopulation = parMap rpar evaluateChromosome population
    in evaluatedPopulation
    where
        evaluateChromosome :: Chromosome -> Chromosome
        evaluateChromosome (IntegerPermutedChromosome fitness alleles) =
            let newFitness = evaluate_chromosome (IntegerPermutedChromosome fitness alleles)
            in IntegerPermutedChromosome newFitness alleles

average_fitness_population :: Population -> Int -> Float
average_fitness_population [] _ = 0.0
average_fitness_population ((IntegerPermutedChromosome fitness _):population) n_chromosomes = ((fromIntegral fitness) / (fromIntegral n_chromosomes)) + (average_fitness_population population n_chromosomes)

-- ------------------------------------- SELECT POPULATION --------------------------------------------
best_fitness_selection' :: Population -> Chromosome -> Chromosome
best_fitness_selection' [] x = x
best_fitness_selection' ((IntegerPermutedChromosome a b):xs) (IntegerPermutedChromosome c d)  | a < c = best_fitness_selection' xs (IntegerPermutedChromosome a b)
                                                                            | otherwise = best_fitness_selection' xs (IntegerPermutedChromosome c d)
best_fitness_selection :: Population -> Chromosome
best_fitness_selection ((IntegerPermutedChromosome a b):xs) =  best_fitness_selection' xs (IntegerPermutedChromosome a b)

random_element :: Population -> IO Chromosome
random_element population = do
    index <- randomRIO (0, length population - 1)
    return (population !! index)

random_elements :: Population -> Int -> IO Population
random_elements population n = sequence $ replicate n (random_element population)

tournament_selection :: Population -> IO Chromosome
tournament_selection population = do
    random_chromosomes <- random_elements population 2
    return $ best_fitness_selection random_chromosomes

-- ------------------------------------- GENERATION --------------------------------------------
evolve :: Population -> Int -> IO Population
evolve _ 0 = return []
evolve population n = do
    parent_1 <- tournament_selection population
    parent_2 <- tournament_selection population

    let (children_1, children_2) = cycle_crossover parent_1 parent_2

    children_1' <- swap_mutation children_1
    children_2' <- swap_mutation children_2

    rest <- evolve population (n - 2)

    return $ children_1':children_2':rest

genetic_algorithm :: Population -> Int -> IO ()
genetic_algorithm _ 0 = return ()
genetic_algorithm population max_generations = do

    new_generation <- evolve population (length population)

    let ep   = evaluate_population new_generation
    let av   = average_fitness_population ep (length ep)
    let best = best_fitness_selection ep

    appendFile "saida.txt" $ show (fitness_chromossome best) ++ " " ++ show av ++ "\n"

    if (fitness_chromossome best) /= 0 then
        genetic_algorithm ep (max_generations - 1)
    else print best

    
