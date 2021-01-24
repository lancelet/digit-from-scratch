-- |
-- Module      : Main
-- Description : Main entry point for tests.
module Main where

import qualified Hedgehog

main :: IO ()
main = do
  runHedgehogTests

runHedgehogTests :: IO ()
runHedgehogTests = do
  putStr "\n"
  putStrLn "---- Running Hedgehog Tests ----"
  mapM_ Hedgehog.checkParallel hedgehogTests
  putStrLn "---- Finished Hedgehog Tests ----"

hedgehogTests :: [Hedgehog.Group]
hedgehogTests = []
