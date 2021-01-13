-- | Convolutional layer.
module Conv where

import Data.Massiv.Array (Array, Ix2, U)
import qualified Data.Massiv.Array as Array
import Data.Massiv.Array.Stencil (Stencil)
import qualified Data.Massiv.Array.Stencil as Stencil
import MNIST (Image)
import qualified MNIST

-- | Convolution layer.
data Conv = Conv
  { convArray :: !(Array U Ix2 Float),
    convBias :: !Float
  }

eval :: Conv -> Image Float -> Image Float
eval conv image = MNIST.Image rawImg
  where
    rawImg :: Array U Ix2 Float
    rawImg =
      Array.compute $
        Stencil.applyStencil
          Stencil.noPadding
          biasedStencil
          (MNIST.unImage image)

    biasedStencil :: Stencil Ix2 Float Float
    biasedStencil = fmap applyBias rawStencil

    applyBias :: Float -> Float
    applyBias x = convBias conv + x

    rawStencil :: Stencil Ix2 Float Float
    rawStencil = Stencil.makeConvolutionStencilFromKernel . convArray $ conv
