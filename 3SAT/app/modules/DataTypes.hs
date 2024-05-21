module DataTypes where

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