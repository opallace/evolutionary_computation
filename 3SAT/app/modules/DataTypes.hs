module DataTypes where

import Chromosome

data TypeChromosome = 
        Binary
    |   Integer
    |   IntegerPermuted
    |   Real

data Chromosome = 
        BinaryChromosome Int [Bool] 
    |   IntegerChromosome Int [Int]
    |   IntegerPermutedChromosome Int [Int]
    deriving (Show)

type Population = [Chromosome]

instance Eq Chromosome where
    (==) a b = (==) (fitness_chromossome a) (fitness_chromossome b)

instance Ord Chromosome where 
    compare a b = compare (fitness_chromossome a) (fitness_chromossome b)