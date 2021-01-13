-- | Gradient type
module Grad where

newtype Grad a = Grad {unGrad :: a}

newtype LearningRate = LearningRate {unLearningRate :: Float}
