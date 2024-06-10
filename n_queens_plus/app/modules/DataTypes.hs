module DataTypes where

data Chromosome = IntegerPermutedChromosome Double [Int] deriving (Show)

type Population = [Chromosome]