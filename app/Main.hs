module Main where

import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

import Parser.Parser(program)
import Text.Parsec (parse)

main :: IO ()
main = do
  argv <- getArgs
  if null argv
    then do
      hPutStrLn stderr "Input file required!"
      exitFailure
    else do
      let fileName = last argv
      file <- readFile fileName
      case parse program  "Syntax error" file of
        Left err -> print err
        Right expr -> print expr
      
      return ()
