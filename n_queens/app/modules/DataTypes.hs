module DataTypes where

data Chromosome = IntegerPermutedChromosome Int [Int] deriving (Show)

instance Ord Chromosome where
  compare (IntegerPermutedChromosome f1 _) (IntegerPermutedChromosome f2 _) = compare f1 f2

instance Eq Chromosome where
  (IntegerPermutedChromosome g1 _) == (IntegerPermutedChromosome g2 _) = g1 == g2
  (IntegerPermutedChromosome g1 _) /= (IntegerPermutedChromosome g2 _) = g1 /= g2

type Population = [Chromosome]