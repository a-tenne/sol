module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import Expr (exprTests)
import Runtime (runtimeTests)
import Stat (statTests)

tests = TestList [exprTests, runtimeTests, statTests]

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 then exitFailure else exitSuccess
