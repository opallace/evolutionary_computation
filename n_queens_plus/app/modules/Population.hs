module Population where

import System.IO
import System.Random
import Control.Monad

import Data.List
import DataTypes
import Chromosome

instance Eq Chromosome where
    (==) a b = (==) (fitness_chromossome a) (fitness_chromossome b)

instance Ord Chromosome where 
    compare a b = compare (fitness_chromossome a) (fitness_chromossome b)

------------------------------------- GENERATE POPULATION --------------------------------------------

generate_integer_permuted_population :: Int -> Int -> IO Population
generate_integer_permuted_population n_population n_genes = do
    pop <- replicateM n_population (generate_integer_permuted_chromosome n_genes)
    return pop

------------------------------------- EVALUATE POPULATION --------------------------------------------

evaluate_population :: Population -> [[Double]] -> Population
evaluate_population [] _ = []
evaluate_population ((IntegerPermutedChromosome fitness alleles):population) table =
    let new_fitness = evaluate_chromosome (IntegerPermutedChromosome fitness alleles) table
    in (IntegerPermutedChromosome new_fitness alleles) : (evaluate_population population table)

average_fitness_population :: Population -> Int -> Double
average_fitness_population [] _ = 0.0
average_fitness_population ((IntegerPermutedChromosome fitness _):population) n_chromosomes = (fitness / (fromIntegral n_chromosomes)) + (average_fitness_population population n_chromosomes)

-- ------------------------------------- SELECT POPULATION --------------------------------------------
best_fitness_selection' :: Population -> Chromosome -> Chromosome
best_fitness_selection' [] x = x
best_fitness_selection' ((IntegerPermutedChromosome a b):xs) (IntegerPermutedChromosome c d)  | a > c = best_fitness_selection' xs (IntegerPermutedChromosome a b)
                                                                            | otherwise = best_fitness_selection' xs (IntegerPermutedChromosome c d)
best_fitness_selection :: Population -> Chromosome
best_fitness_selection ((IntegerPermutedChromosome a b):xs) =  best_fitness_selection' xs (IntegerPermutedChromosome a b)


n_best_fitness_selection :: Int -> Population -> Population
n_best_fitness_selection n population = take n $ reverse $ sort population

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

genetic_algorithm :: Population -> [[Double]] -> Int -> IO ()
genetic_algorithm population table 0 = do
    let (IntegerPermutedChromosome f alleles) = best_fitness_selection population
    let queens = zip [1..(length alleles)] alleles

    print $ evaluate_chromosome' queens
    print $ f
    print $ evaluate_chromosome'' queens table
    print $ alleles

genetic_algorithm population table max_generations = do
    let elit = n_best_fitness_selection 6 population

    new_generation <- evolve population ((length population) - 6)

    let ep   = evaluate_population (new_generation++elit) table
    let av   = average_fitness_population ep (length ep)
    let best = best_fitness_selection ep

    -- appendFile "saida.txt" $ show (fitness_chromossome best) ++ " " ++ show av ++ "\n"
    genetic_algorithm ep table (max_generations - 1)

    
