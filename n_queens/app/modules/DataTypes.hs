module DataTypes where

data TypeChromosome = Binary

data Chromosome = IntegerPermutedChromosome Float [Int] deriving (Show)

type Population = [Chromosome]