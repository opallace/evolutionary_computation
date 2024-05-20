module DataTypes where

data TypeChromosome = Binary

data Chromosome = IntegerPermutedChromosome Int [Int] deriving (Show)

type Population = [Chromosome]