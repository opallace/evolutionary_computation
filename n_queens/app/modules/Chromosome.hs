module Chromosome where

import System.IO
import System.Random
import Control.Monad
import Data.List
import DataTypes

------------------------------------- GETS --------------------------------------------
fitness_chromossome :: Chromosome -> Float
fitness_chromossome (IntegerPermutedChromosome f _) = f

------------------------------------- GENERATE --------------------------------------------

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
  i <- randomRIO (0, length xs - 1)
  let (left, (x:right)) = splitAt i xs
  fmap (x:) (shuffle (left ++ right))


generate_integer_permuted_chromosome :: Int -> IO Chromosome
generate_integer_permuted_chromosome n_genes = do
    alleles <- shuffle [1..n_genes]
    return (IntegerPermutedChromosome 0.0 alleles)

------------------------------------- EVALUATE --------------------------------------------

binary_to_int :: [Bool] -> Int
binary_to_int [] = 0
binary_to_int (x:xs) | x == True = 2 ^ length xs + binary_to_int xs
                     | otherwise = binary_to_int xs

evaluate_chromosome :: Chromosome -> Float
evaluate_chromosome (IntegerPermutedChromosome _ alleles) = 0

------------------------------------- CROSSOVER --------------------------------------------
find_element :: [Int] -> Int -> Int -> Int
find_element (x:xs) e i | e == x = i 
                        | otherwise = find_element xs e (i + 1)

get_cycle :: [Int] -> [Int] -> Int -> Int -> ([Int], [Int])
get_cycle p1 p2 ini next = 
  let index = find_element p1 next 0
      ele   = p2 !! index
  in if ini == ele then ([next], [ele])
     else let retorno = get_cycle p1 p2 ini

cycle_crossover :: Chromosome -> Chromosome -> Chromosome


------------------------------------- MUTATION --------------------------------------------
swap :: Int -> Int -> [a] -> [a]
swap i j xs = let xi = xs !! i
                  xj = xs !! j
                
                  left   = take i xs
                  middle = take (j - i - 1) (drop (i + 1) xs)
                  right  = drop (j + 1) xs
              
              in left ++ [xj] ++ middle ++ [xi] ++ right


swap_mutation :: [Int] -> IO [Int]
swap_mutation alleles = do
    i <- randomRIO (0, (length alleles) - 1)
    j <- randomRIO (0, (length alleles) - 1)
    return $ swap i j alleles
