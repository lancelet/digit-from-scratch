{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Convolutional layer.
module Conv where

import Data.Massiv.Array
  ( Array,
    Comp (Seq),
    Construct,
    Ix2 ((:.)),
    Manifest,
    Sz (Sz2),
    U,
    (!),
  )
import qualified Data.Massiv.Array as Array
import Data.Massiv.Array.Stencil (Stencil)
import qualified Data.Massiv.Array.Stencil as Stencil
import Data.Massiv.Core (Border (Fill))
import Grad (Grad)
import qualified Grad
import MNIST (Image)
import qualified MNIST

-- | Convolution layer.
data Conv = Conv
  { convArray :: !(Array U Ix2 Float),
    convBias :: !Float
  }

-- | Evaluate the convolution layer in a forward direction.
forwardEval :: Conv -> Image Float -> Image Float
forwardEval conv image = MNIST.Image rawImg
  where
    rawImg :: Array U Ix2 Float
    rawImg =
      Array.compute $
        Stencil.applyStencil
          Stencil.noPadding
          biasedStencil
          (MNIST.unImage image)

    biasedStencil :: Stencil Ix2 Float Float
    biasedStencil = fmap (+ convBias conv) rawStencil

    rawStencil :: Stencil Ix2 Float Float
    rawStencil = Stencil.makeConvolutionStencilFromKernel . convArray $ conv

{-
Deep dive into backprop for convolution layers:
  https://towardsdatascience.com/backpropagation-in-a-convolutional-layer-24c8d64d8509
-}

-- | Backpropagation adjustment of the convolution layer weights.
backprop ::
  Image Float ->
  Grad (Image Float) ->
  Conv ->
  (Grad Conv, Grad (Image Float))
backprop input dy conv = (Grad.Grad $ Conv dArray dBias, Grad.Grad dImage)
  where
    -- Gradient of the bias is the sum of the gradient of the actibation map,
    --   dy.
    dBias :: Float
    dBias = Array.sum . MNIST.unImage . Grad.unGrad $ dy

    -- Gradient of the image is the convolution of the zero padded activation
    --   map gradient (zero-padded dy) with the 180-degree rotated weights
    --   matrix.
    dImage :: Image Float
    dImage =
      MNIST.Image $
        Array.compute $
          Stencil.mapStencil
            (Fill 0.0)
            stencil
            (MNIST.unImage . Grad.unGrad $ dy)
      where
        stencil :: Stencil Ix2 Float Float
        stencil =
          Stencil.makeConvolutionStencilFromKernel
            . rot180
            . convArray
            $ conv

    -- Gradient of the weights array is the convolution of the input image
    --   with the gradient of the activation map, dy.
    dArray :: Array U Ix2 Float
    dArray =
      Array.compute $
        Stencil.applyStencil
          Stencil.noPadding
          stencil
          (MNIST.unImage input)
      where
        stencil :: Stencil Ix2 Float Float
        stencil =
          Stencil.makeConvolutionStencilFromKernel
            . MNIST.unImage
            . Grad.unGrad
            $ dy

-- | Rotate a matrix by 180 degrees.
rot180 ::
  forall a r.
  (Manifest r Ix2 a, Construct r Ix2 a) =>
  Array r Ix2 a ->
  Array r Ix2 a
rot180 inArray = Array.makeArray Seq sz f
  where
    f :: Ix2 -> a
    f (j :. i) = inArray ! (w - j - 1) :. (h - i - 1)

    sz :: Sz Ix2
    sz = Array.size inArray

    h, w :: Int
    Sz2 h w = sz
