{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text.IO as T
import qualified Data.Vector as V
import MNIST (Image, Label)
import qualified MNIST
import System.Exit (exitFailure)

main :: IO ()
main = do
  trainingLabels <- loadTrainingLabels
  testingLabels <- loadTestingLabels
  trainingImages <- loadTrainingImages
  testingImages <- loadTestingImages
  -- T.putStrLn "Test dump of an image"
  -- MNIST.saveMNISTPNG "test.png" (trainingImages V.! 0)
  T.putStrLn "Done"

loadTrainingLabels :: IO (V.Vector Label)
loadTrainingLabels = do
  T.putStrLn "Loading MNIST training labels"
  mLabels <- MNIST.loadMNISTLabels "mnist-data/train-labels-idx1-ubyte"
  case mLabels of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST training labels:"
      T.putStrLn errMsg
      exitFailure
    Right labels -> pure labels

loadTestingLabels :: IO (V.Vector Label)
loadTestingLabels = do
  T.putStrLn "Loading MNIST testing labels"
  mLabels <- MNIST.loadMNISTLabels "mnist-data/t10k-labels-idx1-ubyte"
  case mLabels of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST testing labels:"
      T.putStrLn errMsg
      exitFailure
    Right labels -> pure labels

loadTrainingImages :: IO (V.Vector Image)
loadTrainingImages = do
  T.putStrLn "Loading MNIST training images"
  mImages <- MNIST.loadMNISTImages "mnist-data/train-images-idx3-ubyte"
  case mImages of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST training images:"
      T.putStrLn errMsg
      exitFailure
    Right images -> pure images

loadTestingImages :: IO (V.Vector Image)
loadTestingImages = do
  T.putStrLn "Loading MNIST testing images"
  mImages <- MNIST.loadMNISTImages "mnist-data/t10k-images-idx3-ubyte"
  case mImages of
    Left errMsg -> do
      T.putStrLn "Could not load MNIST testing images:"
      T.putStrLn errMsg
      exitFailure
    Right images -> pure images
