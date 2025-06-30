module Helpers where
import Text.Parsec.String (Parser)
import Text.Parsec (ParseError, parse)
import Test.HUnit

parseWrapper :: Parser a -> String -> Either ParseError a
parseWrapper x = parse x ""

testTemplate :: (Eq a, Show a) => String -> a -> Parser a -> String -> Test
testTemplate msg expected parser parseStr = TestCase $ assertEqual msg (Right expected) (parseWrapper parser parseStr)
