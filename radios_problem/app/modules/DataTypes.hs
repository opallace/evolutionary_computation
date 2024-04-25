module DataTypes where

data TypeChromosome = Binary

data Chromosome = BinaryChromosome Float [Bool] deriving (Show)

type Population = [Chromosome]