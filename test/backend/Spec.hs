module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Expr (exprTests)

tests = TestList [exprTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
