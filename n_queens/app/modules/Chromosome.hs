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
cycleCrossover :: Chromosome -> Chromosome -> (Chromosome, Chromosome)
cycleCrossover parent1 parent2 = (child1, child2)
  where
    (startIdx, endIdx) = getRandomIndices (length parent1)
    cycles = getCycles parent1 parent2 startIdx endIdx []
    child1 = generateChild parent1 cycles
    child2 = generateChild parent2 cycles

getRandomIndices :: Int -> (Int, Int)
getRandomIndices n = (idx1, idx2)
  where
    [idx1, idx2] = take 2 . randomRs (0, n - 1) $ mkStdGen 42

getCycles :: Chromosome -> Chromosome -> Int -> Int -> [([Int], [Int])] -> [([Int], [Int])]
getCycles parent1 parent2 idx1 idx2 cycles
  | elem idx1 (map fst cycles) = cycles
  | otherwise = getCycles parent1 parent2 nextIdx1 nextIdx2 newCycles
  where
    nextIdx1 = fromMaybe (error "Index not found in parent2") $ elemIndex (parent2 !! idx1) parent1
    nextIdx2 = fromMaybe (error "Index not found in parent1") $ elemIndex (parent1 !! idx2) parent2
    newCycles = (idx1, idx2) : cycles

generateChild :: Chromosome -> [([Int], [Int])] -> Chromosome
generateChild parent cycles = map (\x -> if x `elem` selected then parent2 !! (fromJust (elemIndex x parent2)) else x) parent
  where
    (selected, _) = unzip . concatMap (\(cycle1, cycle2) -> cycle1 ++ cycle2) $ cycles
    parent2 = filter (`notElem` selected) parent

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
