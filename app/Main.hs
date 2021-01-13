{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Conv (rot180)
import Data.Massiv.Array (Array, B, Ix1, Ix2 ((:.)))
import qualified Data.Massiv.Array as Array
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Word (Word8)
import MNIST (Image, Label)
import qualified MNIST
import System.Exit (exitFailure)
import TextShow (showt)

main :: IO ()
main = do
  trainingLabels <- loadTrainingLabels
  testingLabels <- loadTestingLabels
  trainingImages <- loadTrainingImages
  testingImages <- loadTestingImages
  T.putStrLn "Test dump of an image"
  MNIST.saveMNISTPNG "test.png" (trainingImages Array.! 0)
  T.putStrLn "Done"

  -- Quick check of  180 degree rotation of a matrix

  let testMat :: Array B Ix2 Int = Array.makeArray Array.Seq (Array.Sz2 3 3) (\(j :. i) -> i + j * 3)
  T.putStrLn . T.pack . show $ testMat

  let rotMat = rot180 testMat
  T.putStrLn . T.pack . show $ rotMat

loadTrainingLabels :: IO (Array B Ix1 Label)
loadTrainingLabels = do
  T.putStrLn "Loading MNIST training labels"
  mLabels <- MNIST.loadMNISTLabels "mnist-data/train-labels-idx1-ubyte"
  case mLabels of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST training labels:"
      T.putStrLn errMsg
      exitFailure
    Right labels -> do
      T.putStrLn $ "  loaded " <> showt (Array.elemsCount labels) <> " labels"
      pure labels

loadTestingLabels :: IO (Array B Ix1 Label)
loadTestingLabels = do
  T.putStrLn "Loading MNIST testing labels"
  mLabels <- MNIST.loadMNISTLabels "mnist-data/t10k-labels-idx1-ubyte"
  case mLabels of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST testing labels:"
      T.putStrLn errMsg
      exitFailure
    Right labels -> do
      T.putStrLn $ "  loaded " <> showt (Array.elemsCount labels) <> " labels"
      pure labels

loadTrainingImages :: IO (Array B Ix1 (Image Word8))
loadTrainingImages = do
  T.putStrLn "Loading MNIST training images"
  mImages <- MNIST.loadMNISTImages "mnist-data/train-images-idx3-ubyte"
  case mImages of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST training images:"
      T.putStrLn errMsg
      exitFailure
    Right images -> do
      T.putStrLn $ "  loaded " <> showt (Array.elemsCount images) <> " images"
      pure images

loadTestingImages :: IO (Array B Ix1 (Image Word8))
loadTestingImages = do
  T.putStrLn "Loading MNIST testing images"
  mImages <- MNIST.loadMNISTImages "mnist-data/t10k-images-idx3-ubyte"
  case mImages of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST testing images:"
      T.putStrLn errMsg
      exitFailure
    Right images -> do
      T.putStrLn $ "  loaded " <> showt (Array.elemsCount images) <> " images"
      pure images
