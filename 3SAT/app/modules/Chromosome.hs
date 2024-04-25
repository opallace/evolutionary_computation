module Chromosome where

import System.IO
import System.Random
import Control.Monad
import Data.List
import DataTypes

------------------------------------- GETS --------------------------------------------
fitness_chromossome :: Chromosome -> Int
fitness_chromossome (BinaryChromosome f _) = f

------------------------------------- GENERATE --------------------------------------------

generate_binary_chromosome :: Int -> (Bool, Bool) -> IO Chromosome
generate_binary_chromosome n_genes interval = do
    binaries <- replicateM n_genes (randomRIO interval)
    return $ BinaryChromosome 0 binaries

generate_integer_chromosome :: Int -> (Int, Int) -> IO Chromosome
generate_integer_chromosome n_genes interval = do
    integers <- replicateM n_genes (randomRIO interval)
    return $ IntegerChromosome 0 integers

------------------------------------- EVALUATE --------------------------------------------

evaluate_chromosome :: [[Int]] -> Chromosome -> Int
evaluate_chromosome [] _ = 0
evaluate_chromosome (clause:clauses) (BinaryChromosome _ alleles) =
    let l0' = clause !! 0
        l1' = clause !! 1
        l2' = clause !! 2

        l0 = if l0' < 0 then not (alleles !! ((abs l0') - 1)) else  alleles !! ((abs l0') - 1)
        l1 = if l1' < 0 then not (alleles !! ((abs l1') - 1)) else  alleles !! ((abs l1') - 1)
        l2 = if l2' < 0 then not (alleles !! ((abs l2') - 1)) else  alleles !! ((abs l2') - 1)

    in if l0 || l1 || l2
        then 1 + evaluate_chromosome clauses (BinaryChromosome 0 alleles)
        else evaluate_chromosome clauses (BinaryChromosome 0 alleles)


------------------------------------- CROSSOVER --------------------------------------------
uniform_crossover_alleles :: [Bool] -> [Bool] -> IO [Bool]
uniform_crossover_alleles [] [] = return []
uniform_crossover_alleles (x:xs) (x':xs') = do
    cross <- randomRIO (False, True)
    
    rest <- uniform_crossover_alleles xs xs'
    
    if cross then return $ x:rest
    else return $ x':rest


uniform_crossover_chromosomes :: Chromosome -> Chromosome -> IO Chromosome
uniform_crossover_chromosomes (BinaryChromosome _ alleles1) (BinaryChromosome _ alleles2) = do
    new_alleles <- (uniform_crossover_alleles alleles1 alleles2)
    return (BinaryChromosome 0 new_alleles)

------------------------------------- MUTATION --------------------------------------------
mutation' :: [Bool] -> IO [Bool]
mutation' [] = return []
mutation' (allele:alleles) = do
    mutate <- randomRIO (1, 100::Int)

    rest <- mutation' alleles

    if mutate <= 2 then return $ ((not allele):rest)
    else return $ (allele:rest)

mutation :: Chromosome -> IO Chromosome
mutation (BinaryChromosome _ alleles) = do
    new_alleles <- mutation' alleles
    return (BinaryChromosome 0 new_alleles)
