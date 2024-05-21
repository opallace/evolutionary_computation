module Chromosome where

import System.Random
import Control.Monad
import Data.List
import DataTypes
import Control.DeepSeq (NFData, rnf)

instance NFData Chromosome where
    rnf (IntegerPermutedChromosome fitness alleles) = rnf fitness `seq` rnf alleles


------------------------------------- GETS --------------------------------------------
fitness_chromossome :: Chromosome -> Int
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
    return (IntegerPermutedChromosome 0 alleles)

------------------------------------- EVALUATE --------------------------------------------

evaluate_chromosome :: Chromosome -> Int
evaluate_chromosome (IntegerPermutedChromosome _ alleles) = evaluate_chromosome' (zip [1..(length alleles)] alleles)
    where
        evaluate_chromosome' :: [(Int, Int)] -> Int
        evaluate_chromosome' lista =  sum (aplicarOuLista (chunksOf (length lista - 1) [check_queen_conflic x y | x <- lista, y <- lista, x /= y]))

        chunksOf :: Int -> [Int] -> [[Int]]
        chunksOf _ [] = []
        chunksOf n lista = take n lista : chunksOf n (drop n lista)

        aplicarOuLista :: [[Int]] -> [Int]
        aplicarOuLista = map (foldr1 (\x y -> if x /= 0 || y /= 0 then 1 else 0))

        check_queen_conflic :: (Int, Int) -> (Int, Int) -> Int
        check_queen_conflic (ax, ay) (bx, by) = 
            if abs (ax - bx) == abs (ay - by) then 1
            else 0

------------------------------------- CROSSOVER --------------------------------------------
find_element :: [Int] -> Int -> Int -> Maybe Int
find_element [] _ _ = Nothing
find_element (x:xs) e i
    | e == x    = Just i
    | otherwise = find_element xs e (i + 1)

get_cycle :: [Int] -> [Int] -> Int -> Int -> Maybe ([Int],[Int])
get_cycle p1 p2 ini index = do
    let ele_p1 = p1 !! index
    let ele_p2 = p2 !! index
        
    next_index <- find_element p1 ele_p2 0
       
    if ini == ele_p2 then Just ([ele_p1], [ele_p2])
       else do
            rest <- get_cycle p1 p2 ini next_index
            return (ele_p1:(fst rest), ele_p2:(snd rest))

cycle_crossover' :: [Int] -> [Int] -> [Int] -> [Int]
cycle_crossover' [] _ _  = []
cycle_crossover' _ [] _  = []
cycle_crossover' (a:as) (b:bs) cycle = 
    case (find_element cycle a 0) of
        Just _ -> a:(cycle_crossover' as bs cycle)
        Nothing -> b:(cycle_crossover' as bs cycle)
	
cycle_crossover :: Chromosome -> Chromosome -> (Chromosome, Chromosome)
cycle_crossover (IntegerPermutedChromosome _ a1) (IntegerPermutedChromosome _ a2) = 
	case (get_cycle a1 a2 (head a1) 0) of
        Just (c1, c2) -> 
            let a1' = cycle_crossover' a1 a2 c1
                a2' = cycle_crossover' a2 a1 c2
            in ((IntegerPermutedChromosome 0 a1'),(IntegerPermutedChromosome 0 a2'))
        Nothing       -> (IntegerPermutedChromosome 0 [], IntegerPermutedChromosome 0 [])

------------------------------------- MUTATION --------------------------------------------
swap :: Int -> Int -> [Int] -> [Int]
swap i j xs
    | i == j = xs
    | otherwise = 
        let min_index = min i j
            max_index = max i j
            
            min_pos = xs !! min_index
            max_pos = xs !! max_index

            left   = take min_index xs
            middle = take (max_index - min_index - 1) (drop (min_index + 1) xs)
            right  = drop (max_index + 1) xs
        in left ++ [max_pos] ++ middle ++ [min_pos] ++ right

swap_mutation :: Chromosome-> IO Chromosome
swap_mutation (IntegerPermutedChromosome _ alleles) = do
    p <- randomRIO (0, 100::Int)
    if p < 5 then do
        i <- randomRIO (0, (length alleles) - 1)
        j <- randomRIO (0, (length alleles) - 1)
        return (IntegerPermutedChromosome 0 (swap i j alleles))
    else return (IntegerPermutedChromosome 0 alleles)
