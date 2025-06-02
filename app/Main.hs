module Main where

import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

main :: IO ()
main = do
  argv <- getArgs
  if null argv
    then do
      hPutStrLn stderr "Input file required!"
      exitFailure
    else do
      let fileName = last argv
      _file <- readFile fileName
      {- case parse  "Syntax error" file of
        Left err -> print err
        Right expr -> print expr
      -}
      return ()
