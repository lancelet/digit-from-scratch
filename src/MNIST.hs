{-# LANGUAGE ScopedTypeVariables #-}

-- | Decoding MNIST images.
module MNIST where

import qualified Codec.Picture as Picture
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Massiv.Array (Array, B, Comp (Seq), Ix1, Ix2 ((:.)), Sz (Sz1, Sz2), U (U), Unbox, (!))
import qualified Data.Massiv.Array as Array
import qualified Data.Serialize.Get as Cereal
import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word32, Word8)

newtype Image a = Image {unImage :: Array U Ix2 a}

newtype Label = Label {unLabel :: Word8}

getPixel :: (Unbox a) => Image a -> Word32 -> Word32 -> a
getPixel image x y = unImage image ! ix
  where
    ix :: Ix2
    ix = fromIntegral y :. fromIntegral x

imageHeight :: (Unbox a) => Image a -> Word32
imageHeight image = fromIntegral h
  where
    Sz2 h _w = Array.size . unImage $ image

imageWidth :: (Unbox a) => Image a -> Word32
imageWidth image = fromIntegral w
  where
    Sz2 _h w = Array.size . unImage $ image

saveMNISTPNG :: FilePath -> Image Word8 -> IO ()
saveMNISTPNG filepath image = do
  let w :: Int = fromIntegral . imageWidth $ image
      h :: Int = fromIntegral . imageWidth $ image
      f x y = getPixel image (fromIntegral x) (fromIntegral y)
      picture = Picture.ImageY8 $ Picture.generateImage f w h
  Picture.savePngImage filepath picture

loadMNISTImages :: FilePath -> IO (Either Text (Array B Ix1 (Image Word8)))
loadMNISTImages filepath = do
  bs <- BS.readFile filepath
  let es :: Either String (Array B Ix1 (Image Word8)) = decodeMNISTImages bs
  pure $ first T.pack es

decodeMNISTImages :: ByteString -> Either String (Array B Ix1 (Image Word8))
decodeMNISTImages = Cereal.runGet getMNISTImageFile

getMNISTImageFile :: Cereal.Get (Array B Ix1 (Image Word8))
getMNISTImageFile = do
  magic <- Cereal.getWord32be
  case magic of
    0x00000803 -> do
      nImages <- Cereal.getWord32be
      height <- Cereal.getWord32be
      width <- Cereal.getWord32be
      let sz = Sz1 (fromIntegral nImages)
      Array.makeArrayLinearA sz (\_ -> getMNISTImage width height)
    _ -> fail "Invalid magic number"

getMNISTImage :: Word32 -> Word32 -> Cereal.Get (Image Word8)
getMNISTImage width height = do
  let w :: Int = fromIntegral width
      h :: Int = fromIntegral height
      nbytes :: Int = w * h
      sz = Sz2 h w
  bs <- Cereal.getBytes nbytes
  pure . Image $ Array.makeArrayLinearR U Seq sz (BS.index bs)

loadMNISTLabels :: FilePath -> IO (Either Text (Array B Ix1 Label))
loadMNISTLabels filepath = do
  bs <- BS.readFile filepath
  let es :: Either String (Array B Ix1 Label) = decodeMNISTLabelFile bs
  pure $ first T.pack es

decodeMNISTLabelFile :: ByteString -> Either String (Array B Ix1 Label)
decodeMNISTLabelFile = Cereal.runGet getMNISTLabelFile

getMNISTLabelFile :: Cereal.Get (Array B Ix1 Label)
getMNISTLabelFile = do
  magic <- Cereal.getWord32be
  case magic of
    0x00000801 -> do
      nImages <- Cereal.getWord32be
      let sz = Sz1 (fromIntegral nImages)
      Array.makeArrayLinearA sz (\_ -> Label <$> Cereal.getWord8)
    _ -> fail "Invalid magic number"
