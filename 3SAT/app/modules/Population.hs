module Population where
import System.IO
import System.Random
import Control.Monad
import Data.List
import Data.Ord
import DataTypes
import Chromosome

------------------------------------- GENERATE POPULATION --------------------------------------------

generate_boolean_population :: Int -> Int -> (Bool, Bool) -> IO Population
generate_boolean_population n_population n_genes interval = do
    pop <- replicateM n_population (generate_binary_chromosome n_genes interval)
    return pop


generate_integer_population :: Int -> Int -> (Int, Int) -> IO Population
generate_integer_population n_population n_genes interval = do
    pop <- replicateM n_population (generate_integer_chromosome n_genes interval)
    return pop

generate_integer_permuted_population :: Int -> Int -> (Int, Int) -> IO Population
generate_integer_permuted_population _ n_genes _ = do
    return $ map (IntegerPermutedChromosome 0) (permutations [1..n_genes])


------------------------------------- EVALUATE POPULATION --------------------------------------------

evaluate_population :: [[Int]] -> Population -> Population
evaluate_population _ [] = []
evaluate_population formula ((BinaryChromosome fitness alleles):population) =
    let new_fitness = evaluate_chromosome formula (BinaryChromosome fitness alleles)
    in (BinaryChromosome new_fitness alleles) : (evaluate_population formula population)

average_fitness_population :: Population -> Int -> Float
average_fitness_population [] _ = 0.0
average_fitness_population ((BinaryChromosome fitness alleles):population) n_chromosomes = ((fromIntegral fitness) / (fromIntegral n_chromosomes)) + (average_fitness_population population n_chromosomes)

------------------------------------- SELECT POPULATION --------------------------------------------
best_fitness_selection' :: Population -> Chromosome -> Chromosome
best_fitness_selection' [] x = x
best_fitness_selection' ((BinaryChromosome a b):xs) (BinaryChromosome c d)  | a > c = best_fitness_selection' xs (BinaryChromosome a b)
                                                                            | otherwise = best_fitness_selection' xs (BinaryChromosome c d)
best_fitness_selection :: Population -> Chromosome
best_fitness_selection ((BinaryChromosome a b):xs) =  best_fitness_selection' xs (BinaryChromosome a b)

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
    random_chromosomes <- random_elements population 3
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

genetic_algorithm :: Population -> Int -> [[Int]] -> IO ()
genetic_algorithm _ 0 _ = return ()
genetic_algorithm population max_generations content = do
    let elit = n_best_fitness_selection 10 population

    new_generation <- evolve population ((length population) - 10)
    
    let ev   = evaluate_population content (new_generation++elit)
    let av   = average_fitness_population ev (length ev)
    let best = best_fitness_selection ev

    appendFile "saida.txt" $ show (fitness_chromossome best) ++ " " ++ show av ++ "\n"
    -- print $ show (fitness_chromossome best) ++ " " ++ show av ++ "\n"

    if fitness_chromossome best == 430 
        then print best
        else genetic_algorithm ev (max_generations - 1) content
