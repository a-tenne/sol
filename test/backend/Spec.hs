module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

tests = TestList []

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
