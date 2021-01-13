{-# LANGUAGE ScopedTypeVariables #-}

-- | Decoding MNIST images.
module MNIST where

import qualified Codec.Picture as Picture
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Serialize.Get as Cereal
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Storable as VS
import Data.Word (Word32, Word8)

data Image = Image
  { imageWidth :: {-# UNPACK #-} !Word32,
    imageHeight :: {-# UNPACK #-} !Word32,
    imagePixels :: {-# UNPACK #-} !(VS.Vector Word8)
  }

newtype Label = Label {unLabel :: Word8}

getPixel :: Image -> Word32 -> Word32 -> Word8
getPixel image x _ | x >= imageWidth image = 0
getPixel image _ y | y >= imageHeight image = 0
getPixel image x y = imagePixels image VS.! lindex
  where
    lindex :: Int
    lindex =
      fromIntegral x
        + fromIntegral y * (fromIntegral . imageWidth) image

saveMNISTPNG :: FilePath -> Image -> IO ()
saveMNISTPNG filepath image = do
  let w :: Int = fromIntegral . imageWidth $ image
      h :: Int = fromIntegral . imageWidth $ image
      f x y = getPixel image (fromIntegral x) (fromIntegral y)
      picture = Picture.ImageY8 $ Picture.generateImage f w h
  Picture.savePngImage filepath picture

loadMNISTImages :: FilePath -> IO (Either Text (V.Vector Image))
loadMNISTImages filepath = do
  bs <- BS.readFile filepath
  let es :: Either String (V.Vector Image) = decodeMNISTImages bs
  pure $ first T.pack es

decodeMNISTImages :: ByteString -> Either String (V.Vector Image)
decodeMNISTImages = Cereal.runGet getMNISTImageFile

getMNISTImageFile :: Cereal.Get (V.Vector Image)
getMNISTImageFile = do
  magic <- Cereal.getWord32be
  case magic of
    0x00000803 -> do
      nImages <- Cereal.getWord32be
      height <- Cereal.getWord32be
      width <- Cereal.getWord32be
      V.replicateM (fromIntegral nImages) (getMNISTImage width height)
    _ -> fail "Invalid magic number"

getMNISTImage :: Word32 -> Word32 -> Cereal.Get Image
getMNISTImage width height =
  Image width height <$> VS.replicateM nPixels Cereal.getWord8
  where
    nPixels :: Int
    nPixels = fromIntegral width * fromIntegral height

loadMNISTLabels :: FilePath -> IO (Either Text (V.Vector Label))
loadMNISTLabels filepath = do
  bs <- BS.readFile filepath
  let es :: Either String (V.Vector Label) = decodeMNISTLabelFile bs
  pure $ first T.pack es

decodeMNISTLabelFile :: ByteString -> Either String (V.Vector Label)
decodeMNISTLabelFile = Cereal.runGet getMNISTLabelFile

getMNISTLabelFile :: Cereal.Get (V.Vector Label)
getMNISTLabelFile = do
  magic <- Cereal.getWord32be
  case magic of
    0x00000801 -> do
      nImages <- Cereal.getWord32be
      V.replicateM (fromIntegral nImages) (Label <$> Cereal.getWord8)
    _ -> fail "Invalid magic number"
