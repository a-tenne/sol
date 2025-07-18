-- | Small module for parsing all Lua numerals
module Parser.Nums where

import AST
import Control.Monad (void)
import Data.Char (toLower)
import Data.Int (Int64)
import Text.Parsec (char, digit, hexDigit, many, many1, oneOf, optionMaybe, try, (<|>))
import Text.Parsec.String (Parser)

-- | Parses a hexadecimal integer value.
hexInt :: Parser Int64
hexInt = do
  void $ try $ char '0' >> (try (char 'x') <|> try (char 'X'))
  digits <- map hexToInt <$> many1 hexDigit
  let hexVals = zipWith (*) [16 ^ x | x <- [0 .. (length digits - 1)]] (reverse digits)
  return $ sum hexVals

-- | Parses an integer value of any kind.
numInt :: Parser Numeric
numInt = do
  val <- try $ hexInt <|> read <$> many1 digit
  return $ NumInt val

-- | Parses the binary exponent and returns a function to apply to the hexadecimal float.
hexDoubleExp :: Parser (Bool, Double -> Double)
hexDoubleExp = do
  pOpt <- optionMaybe $ oneOf "pP"
  case pOpt of
    Just _ -> do
      signOpt <- optionMaybe $ oneOf "+-"
      let sign :: Double -> Double = case signOpt of
            Just x -> if x == '-' then (* (-1.0)) else (* 1.0)
            Nothing -> (* 1.0)
      digits <- many1 digit
      let pNum = if null digits then 0.0 else read digits
      return (True, ((2.0 ** sign pNum) *))
    Nothing -> return (False, ((2.0 ** 0.0) *))

-- | A table to with tuples of hexadecimal digits and their integer value.
hexToIntTable :: [(Char, Int64)]
hexToIntTable = zip ['0' .. '9'] [0 .. 9] ++ zip ['a' .. 'f'] [10 .. 15]

-- | Helper function for hexToInt.
hexToIntHelper :: [(Char, Int64)] -> Char -> Int64
hexToIntHelper [] x = error $ "Internal error: Invalid hex digit -> " ++ show x
hexToIntHelper ((x, y) : xs) z
  | x == toLower z = y
  | otherwise = hexToIntHelper xs z

-- | A function for getting the integer value for a hex digit.
hexToInt :: Char -> Int64
hexToInt = hexToIntHelper hexToIntTable

-- | Parses a hexadecimal float64 or, if it does not qualify for one, returns it as an int64 numeral.
hexDouble :: Parser Numeric
hexDouble = do
  integerPart <- hexInt
  let start :: Double = fromIntegral integerPart
  middle <- optionMaybe $ char '.'
  case middle of
    Just _ -> do
      precision <- many1 hexDigit
      (_, expFn) <- hexDoubleExp
      let precisionList :: [Double] = zipWith (\a b -> fromIntegral (hexToInt b) / a) (map (16.0 **) [1.0 .. fromIntegral (length $ show precision)]) precision
      let afterPrecision = foldl (+) start precisionList
      return $ NumDouble $ expFn afterPrecision
    Nothing -> do
      (pConsumed, expFn) <- hexDoubleExp
      if pConsumed then return $ NumDouble $ expFn start else return $ NumInt integerPart

-- | Parses a float64 numeral's exponent.
numDoubleExp :: Parser String
numDoubleExp = do
  sign <- optionMaybe $ oneOf "+-"
  digits <- many digit
  let end = if null digits then "0" else digits
  case sign of
    Just x -> return $ x : end
    Nothing -> return end

-- | Parses a float64 numeral. Note that this function can return a int64 numeral, if the hexadecimal number in hexDouble does not qualify as a float64.
numDouble :: Parser Numeric
numDouble =
  try hexDouble <|> do
    firstHalf <- many1 digit
    middle <- optionMaybe $ char '.'
    secondHalf <- case middle of
      Just x -> do
        numbers <- many1 digit
        return $ x : numbers
      Nothing -> return ""
    end :: Maybe Char <-
      if null secondHalf
        then do
          Just <$> oneOf "eE"
        else optionMaybe $ oneOf "eE"
    case end of
      Just x -> do
        doubleExp <- numDoubleExp
        return $ NumDouble $ read $ firstHalf ++ secondHalf ++ [x] ++ doubleExp
      Nothing -> return $ NumDouble $ read $ firstHalf ++ secondHalf
