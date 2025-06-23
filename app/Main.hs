module Main where

import System.Environment
import System.IO (hPutStrLn, stderr, hPrint)
import System.Exit (exitFailure)

import Parser.Parser(program)
import Text.Parsec (parse)
import Data.Char (isSpace)

trim :: String -> String
trim = f . f
  where f = reverse . dropWhile isSpace


main :: IO ()
main = do
  argv <- getArgs
  if null argv
    then do
      hPutStrLn stderr "Input file required!"
      exitFailure
    else do
      let fileName = last argv
      file <-  readFile fileName
      ast <- case parse program "Syntax error" (trim file) of
        Left err ->  hPrint stderr err >> exitFailure
        Right ast -> return ast

      print ast
