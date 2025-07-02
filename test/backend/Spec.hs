module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Expr (exprTests)
import Runtime (runtimeTests)

tests = TestList [exprTests, runtimeTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
