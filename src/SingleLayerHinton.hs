{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Single layer image recognition network.
--
-- This is an example from the following video:
--     https://www.youtube.com/watch?v=RcgfwZ-FFBI&list=PLoRl3Ht4JOcdU872GhiYWf6jwrk_SNhz9&index=4
--
-- It uses a 2-layer network where the input layer is the pixels in an image,
-- and the output layer is the labels of the digits. All input pixels are
-- connected to all output digits with weights. Learning is achieved by
-- presenting images one at a time. For each image, weights are incremented
-- from the active pixels in the image to the correct class. Then, regardless
-- of whether or not the class was guessed correctly, weights from the active
-- pixels to the guessed class are decremented.
--
-- I'm implementing this primarily for comparison purposes later on.
module SingleLayerHinton where

import Control.Exception (SomeException)
import Data.List.Extra (maximumOn)
import Data.Massiv.Array
  ( Array,
    Comp (Seq),
    DL,
    Elt,
    Ix1,
    Ix2,
    Ix3,
    Lower,
    OuterSlice,
    Source,
    Sz (Sz3),
    Sz3,
    U,
    (!>),
  )
import qualified Data.Massiv.Array as Array
import Data.Massiv.Array.Numeric ((.*), (.*.), (.+.))
import Data.Word (Word32, Word64)
import Grad (LearningRate (LearningRate))
import MNIST (Image (Image), Label)
import qualified MNIST
import qualified System.Random.SplitMix as SplitMix

-- | Weights in the network.
--
-- Index order:
--   Ix3 i j k
--   i - class index
--   j - y index (vertical inside the image)
--   k - x index (horizontal inside the image)
newtype Weights = Weights {unWeights :: Array U Ix3 Float}
  deriving (Show)

-- | Fetch the 2D weights array for a given label.
weightsForLabel :: Label -> Weights -> Array U Ix2 Float
weightsForLabel label weights =
  Array.compute $
    unWeights weights !> (fromIntegral . MNIST.unLabel $ label)

-- | Use weights to predict the label for an image.
predict :: Weights -> Image Float -> Label
predict weights image =
  let pairLabelScore :: Label -> (Label, Float)
      pairLabelScore label = (label, scoreLabel weights image label)

      scores :: [(Label, Float)]
      scores = pairLabelScore . MNIST.Label <$> [0 .. 9]
   in fst $ maximumOn snd scores

-- | Score a single label.
scoreLabel :: Weights -> Image Float -> Label -> Float
scoreLabel weights image label =
  Array.sum . fromRight $
    (weightsForLabel label weights) .*. (MNIST.unImage image)

-- | Adjust weights to learn a single example.
learnSingleExample ::
  -- | Learning rate.
  LearningRate ->
  -- | Floating point input image.
  Image Float ->
  -- | Correct label for the image.
  Label ->
  -- | Initial weights.
  Weights ->
  -- | Improved weights.
  Weights
learnSingleExample learningRate image correctLabel weights =
  let predictedLabel :: Label
      predictedLabel = predict weights image
   in adjustWeights (- learningRate) image predictedLabel
        . adjustWeights learningRate image correctLabel
        $ weights

-- | Adjust weights.
--
-- Weights from the active pixels in the image to the labelled class are
-- adjusted by the learning rate.
adjustWeights ::
  -- | Learning rate (+ve to increment, -ve to decrement).
  LearningRate ->
  -- | Image to use for weight adjustment.
  Image Float ->
  -- | Label to adjust.
  Label ->
  -- | Initial weights.
  Weights ->
  -- | Weights after adjustment.
  Weights
adjustWeights
  (LearningRate epsilon)
  (Image image)
  label
  weights =
    let iLabel :: Int
        iLabel = fromIntegral . MNIST.unLabel $ label

        wArray :: Array U Ix3 Float
        wArray = unWeights weights

        labelWeights :: Array U Ix2 Float
        labelWeights = weightsForLabel label weights

        labelWeights' :: Array U Ix2 Float
        labelWeights' = fromRight $ labelWeights .+. (image .* epsilon)
     in Weights
          . Array.compute
          . replaceSlice iLabel (Array.toManifest $ labelWeights')
          $ wArray

-- | Generate a random set of weights.
--
-- The weights are all between -1.0 and 1.0.
randomWeights ::
  -- | Width and height of the images.
  (Word32, Word32) ->
  -- | Seed value for randomness.
  Word64 ->
  -- | Resulting weights.
  Weights
randomWeights (width, height) seed =
  Weights $
    Array.compute $
      Array.randomArray gen SplitMix.splitSMGen nextFloat Seq sz3
  where
    sz3 :: Sz3
    sz3 = Sz3 10 (fromIntegral height) (fromIntegral width)

    gen :: SplitMix.SMGen
    gen = SplitMix.mkSMGen seed

    -- Float in [-1, 1)
    nextFloat :: SplitMix.SMGen -> (Float, SplitMix.SMGen)
    nextFloat g = mapFst adjustRange (SplitMix.nextFloat g)

    adjustRange :: Float -> Float
    adjustRange x = 2.0 * x - 1.0

    mapFst :: (a -> c) -> (a, b) -> (c, b)
    mapFst f (x, y) = (f x, y)

-- | Replace an outer slice in an array.
replaceSlice ::
  forall r r' ix e.
  ( OuterSlice r ix e,
    Source r' (Lower ix) e,
    Elt r ix e ~ Array r' (Lower ix) e
  ) =>
  -- | Index of the slice to replace.
  Int ->
  -- | New slice.
  Array r' (Lower ix) e ->
  -- | Old array.
  Array r ix e ->
  -- | New array.
  Array DL ix e
replaceSlice replaceIndex newSlice array =
  fromRight
    . Array.stackOuterSlicesM
    . Array.imap getSlice
    . Array.outerSlices
    $ array
  where
    getSlice :: Ix1 -> Array r' (Lower ix) e -> Array r' (Lower ix) e
    getSlice ix e = if ix == replaceIndex then newSlice else e

-- | Errors at runtime. I can make Haskell suck as bad as Python.
fromRight :: Either SomeException a -> a
fromRight (Right x) = x
fromRight (Left s) = error . show $ s
