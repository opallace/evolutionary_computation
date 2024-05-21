module Chromosome where

import System.IO
import System.Random
import Control.Monad
import Data.List
import DataTypes

instance Eq Chromosome where
    (==) a b = (==) (fitness_chromossome a) (fitness_chromossome b)

instance Ord Chromosome where 
    compare a b = compare (fitness_chromossome a) (fitness_chromossome b)

------------------------------------- GETS --------------------------------------------
fitness_chromossome :: Chromosome -> Float
fitness_chromossome (BinaryChromosome f _) = f

------------------------------------- GENERATE --------------------------------------------

generate_binary_chromosome :: Int -> (Bool, Bool) -> IO Chromosome
generate_binary_chromosome n_genes interval = do
    binaries <- replicateM n_genes (randomRIO interval)
    return $ BinaryChromosome 0.0 binaries

------------------------------------- EVALUATE --------------------------------------------

binary_to_int :: [Bool] -> Int
binary_to_int [] = 0
binary_to_int (x:xs) | x == True = 2 ^ length xs + binary_to_int xs
                     | otherwise = binary_to_int xs

evaluate_chromosome :: Chromosome -> Float
evaluate_chromosome (BinaryChromosome _ alleles) = 
    let (st', lx') = splitAt 5 alleles
        st = 0.75 * fromIntegral (binary_to_int st')
        lx = 0.5 * fromIntegral (binary_to_int lx')

        fo = 30.0 * st + 40.0 * lx
        h  = st + 2.0 * lx

        fo_normalizado = fo / 1360.0
        h_normalizado = max 0.0 ((h - 40.0) / 16.0)

    in if h <= 40 then fo_normalizado - h_normalizado
       else 0

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
    return (BinaryChromosome 0.0 new_alleles)

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
    return (BinaryChromosome 0.0 new_alleles)
