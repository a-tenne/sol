module Main where

import Expr (exprTests)
import Stat (statTests)
import System.Exit (exitFailure, exitSuccess)
import Test.HUnit

tests = TestList [statTests, exprTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
