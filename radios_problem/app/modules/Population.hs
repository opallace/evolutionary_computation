module Population where

import System.IO
import System.Random
import Control.Monad
import Data.List
import DataTypes
import Chromosome

------------------------------------- GENERATE POPULATION --------------------------------------------

generate_boolean_population :: Int -> Int -> (Bool, Bool) -> IO Population
generate_boolean_population n_population n_genes interval = do
    pop <- replicateM n_population (generate_binary_chromosome n_genes interval)
    return pop

------------------------------------- EVALUATE POPULATION --------------------------------------------

evaluate_population :: Population -> Population
evaluate_population [] = []
evaluate_population ((BinaryChromosome fitness alleles):population) =
    let new_fitness = evaluate_chromosome (BinaryChromosome fitness alleles)
    in (BinaryChromosome new_fitness alleles) : (evaluate_population population)

average_fitness_population :: Population -> Int -> Float
average_fitness_population [] _ = 0.0
average_fitness_population ((BinaryChromosome fitness alleles):population) n_chromosomes = (fitness / (fromIntegral n_chromosomes)) + (average_fitness_population population n_chromosomes)

------------------------------------- SELECT POPULATION --------------------------------------------
best_fitness_selection' :: Population -> Chromosome -> Chromosome
best_fitness_selection' [] x = x
best_fitness_selection' ((BinaryChromosome a b):xs) (BinaryChromosome c d)  | a > c = best_fitness_selection' xs (BinaryChromosome a b)
                                                                            | otherwise = best_fitness_selection' xs (BinaryChromosome c d)
best_fitness_selection :: Population -> Chromosome
best_fitness_selection ((BinaryChromosome a b):xs) =  best_fitness_selection' xs (BinaryChromosome a b)

random_element :: Population -> IO Chromosome
random_element population = do
    index <- randomRIO (0, length population - 1)
    return (population !! index)

random_elements :: Population -> Int -> IO Population
random_elements population n = sequence $ replicate n (random_element population)

tournament_selection :: Population -> IO Chromosome
tournament_selection population = do
    random_chromosomes <- random_elements population 10
    return $ best_fitness_selection random_chromosomes

------------------------------------- GENERATION --------------------------------------------
evolve :: Population -> Int -> IO Population
evolve _ 0 = return []
evolve population n = do
    parent_1 <- tournament_selection population
    parent_2 <- tournament_selection population

    children' <- uniform_crossover_chromosomes parent_1 parent_2
    children <- mutation children'

    rest <- evolve population (n - 1)

    return $ children:rest

genetic_algorithm :: Population -> Int -> IO ()
genetic_algorithm _ 0 = return ()
genetic_algorithm population max_generations = do
    let evaluated_population = evaluate_population population
    new_generation <- evolve evaluated_population (length evaluated_population)

    let evaluated_new_generation = evaluate_population new_generation
    let (BinaryChromosome f a) = best_fitness_selection evaluated_new_generation

    print (BinaryChromosome f a)
    
    genetic_algorithm evaluated_new_generation (max_generations - 1)

    