module Parser.Parser where

import AST
import Control.Monad (void)
import Parser.Nums
import Parser.Op
import Parser.Helpers
import Text.Parsec (alphaNum, anyChar, char, getInput, letter, lookAhead, many, manyTill, oneOf, satisfy, string, try, (<|>), optionMaybe)
import Text.Parsec.String (Parser)

validNext :: [String]
validNext = [" ", "<=", ">=", "<", ">", "~=", "==", "|", "~", "&", "<<", ">>", "+", "-", "*", "/", "//", "%", "^", "\n", ")", "}", ",", "]", ";"]

num :: Parser Expr
num = do
  n <- try numDouble <|> numInt
  check <- checkMulti validNext
  if not check
    then do
      input <- getInput
      if null input
        then
          return $ LiteralExpr (NumLit n)
        else fail "Malformed number"
    else return $ LiteralExpr (NumLit n)

singleLineStr :: Parser Expr
singleLineStr = do
  start <- oneOf "\"'"
  str <- manyTill (satisfy (/= '\n')) (char start)
  return $ LiteralExpr (StringLit str)

multiLineStr :: Parser Expr
multiLineStr =
  char '[' >> do
    levelStr <- many $ char '='
    void $ char '['
    str <- manyTill anyChar (string $ "]" ++ levelStr ++ "]")
    return $ LiteralExpr (StringLit str)

litHelper :: String -> Expr -> Parser Expr
litHelper litName exType = do
  void $ string litName
  check <- checkMulti validNext
  if not check
    then do
      input <- getInput
      if null input
        then
          return exType
        else fail $ "Expected: " ++ litName
    else return exType

tripleDot :: Parser Expr
tripleDot = litHelper "..." TRIPLE_DOT

trueEx :: Parser Expr
trueEx = litHelper "true" TRUE

falseEx :: Parser Expr
falseEx = litHelper "false" FALSE

nilEx :: Parser Expr
nilEx = litHelper "nil" NIL

literalExpr :: Parser Expr
literalExpr = skipJunk >> try (num <|> singleLineStr <|> multiLineStr <|> trueEx <|> falseEx <|> tripleDot <|> nilEx)

ex1 :: Parser Expr
ex1 = skipJunk >> ex2 >>= ex1'

ex1' :: Expr -> Parser Expr
ex1' l = do
  skipJunk
  check <- checkSingle "or"
  if not check
    then return l
    else do
      op <- oper1
      r <- ex2
      ex1' $ BinExpr l op r

ex2 :: Parser Expr
ex2 = skipJunk >> ex3 >>= ex2'

ex2' :: Expr -> Parser Expr
ex2' l = do
  skipJunk
  check <- checkSingle "and"
  if not check
    then return l
    else do
      op <- oper2
      r <- ex3
      ex2' $ BinExpr l op r

ex3 :: Parser Expr
ex3 = skipJunk >> ex4 >>= ex3'

ex3' :: Expr -> Parser Expr
ex3' l = do
  skipJunk
  check <- checkMulti ["<=", ">=", "<", ">", "~=", "=="]
  if not check
    then return l
    else do
      op <- oper3
      r <- ex4
      ex3' $ BinExpr l op r

ex4 :: Parser Expr
ex4 = skipJunk >> ex5 >>= ex4'

ex4' :: Expr -> Parser Expr
ex4' l = do
  skipJunk
  check <- checkSingle "|"
  if not check
    then return l
    else do
      op <- oper4
      r <- ex5
      ex4' $ BinExpr l op r

ex5 :: Parser Expr
ex5 = skipJunk >> ex6 >>= ex5'

ex5' :: Expr -> Parser Expr
ex5' l = do
  skipJunk
  check <- checkSingle "~"
  if not check
    then return l
    else do
      op <- oper5
      r <- ex6
      ex5' $ BinExpr l op r

ex6 :: Parser Expr
ex6 = skipJunk >> ex7 >>= ex6'

ex6' :: Expr -> Parser Expr
ex6' l = do
  skipJunk
  check <- checkSingle "&"
  if not check
    then return l
    else do
      op <- oper6
      r <- ex7
      ex6' $ BinExpr l op r

ex7 :: Parser Expr
ex7 = skipJunk >> ex8 >>= ex7'

ex7' :: Expr -> Parser Expr
ex7' l = do
  skipJunk
  check <- checkMulti ["<<", ">>"]
  if not check
    then return l
    else do
      op <- oper7
      r <- ex8
      ex7' $ BinExpr l op r

ex8 :: Parser Expr
ex8 = skipJunk >> ex9 >>= ex8'

ex8' :: Expr -> Parser Expr
ex8' l = do
  skipJunk
  check <- checkSingle ".."
  if not check
    then return l
    else do
      op <- oper8
      r <- ex9
      ex8' $ BinExpr l op r

ex9 :: Parser Expr
ex9 = skipJunk >> ex10 >>= ex9'

ex9' :: Expr -> Parser Expr
ex9' l = do
  skipJunk
  check <- checkMulti ["+", "-"]
  if not check
    then return l
    else do
      op <- oper9
      r <- ex10
      ex9' $ BinExpr l op r

ex10 :: Parser Expr
ex10 = skipJunk >> ex11 >>= ex10'

ex10' :: Expr -> Parser Expr
ex10' l = do
  skipJunk
  check <- checkMulti ["*", "/", "//", "%"]
  if not check
    then return l
    else do
      op <- oper10
      r <- ex11
      ex10' $ BinExpr l op r

ex11 :: Parser Expr
ex11 = do
  skipJunk
  check <- checkMulti ["not ", "#", "-", "~"]
  if not check
    then ex12
    else UnaryExpr <$> oper11 <*> ex11

ex12 :: Parser Expr
ex12 = do
  skipJunk
  l <- try (tableEx <|> functionDef <|> literalExpr <|> preEx)
  ex12' l

ex12' :: Expr -> Parser Expr
ex12' l = do
  skipJunk
  check <- checkSingle "^"
  if not check
    then return l
    else do
      op <- oper12
      r <- literalExpr
      ex12' $ BinExpr l op r

subEx :: Parser Expr
subEx = do
  skipJunk
  void $ char '('
  expr <- ex1
  skipJunk
  void $ char ')'
  return expr

name :: Parser Name
name = do
  skipJunk
  first <- try $ letter <|> char '_'
  rest <- many (alphaNum <|> char '_')
  let full = first : rest
  if full `elem` reservedKW
    then fail $ "Cannot use reserved keyword \"" ++ full ++ "\""
    else return full

checkName :: Parser Bool
checkName = lookAhead (try $ name >> return True) <|> return False

preEx :: Parser Expr
preEx = PreExpr <$> prefixEx

prefixEx :: Parser PrefixExpr
prefixEx = do
  skipJunk
  checkBrace <- checkChar '('
  if checkBrace
    then do
      lhs <- subEx
      PrefixSub lhs <$> prefixEx'
    else do
      lhs <- name
      PrefixName lhs <$> prefixEx'

tableIndex :: Parser PrefixExpr'
tableIndex = do
  skipJunk
  void $ char '['
  lhs <- ex1
  skipJunk
  void $ char ']'
  TableIndex lhs <$> prefixEx'

dotIndex :: Parser PrefixExpr'
dotIndex = do
  skipJunk
  void $ char '.'
  lhs <- name
  DotIndex lhs <$> prefixEx'

argList :: Parser Args
argList = do
  skipJunk
  void $ char '('
  checkEmpty <- checkChar ')'
  if checkEmpty
    then
      return $ ArgList $ ExprList []
    else do
      l <- exList
      skipJunk
      void $ char ')'
      return $ ArgList l

argTable :: Parser Args
argTable = ArgTable <$> tableConstructor

argString :: Parser Args
argString = ArgString <$> (try singleLineStr <|> multiLineStr)

args :: Parser Args
args = do
  skipJunk
  try (argList <|> argTable) <|> argString

callArgs :: Parser PrefixExpr'
callArgs = do
  skipJunk
  a <- args
  CallArgs a <$> prefixEx'

methodArgs :: Parser PrefixExpr'
methodArgs = do
  skipJunk
  void $ char ':'
  n <- name
  a <- args
  MethodArgs n a <$> prefixEx'

prefixEx' :: Parser PrefixExpr'
prefixEx' = do
  input <- getInput
  if null input
    then
      return PrefixEmpty
    else do
      skipJunk
      nextChar <- peekChar
      case nextChar of
        '[' -> tableIndex <|> callArgs
        '.' -> dotIndex
        '(' -> callArgs
        '{' -> callArgs
        ':' -> methodArgs
        _ -> return PrefixEmpty

exField :: Parser Field
exField = do
  skipJunk
  void $ char '['
  lhs <- ex1
  skipJunk
  void $ char ']'
  skipJunk
  void $ char '='
  ExField lhs <$> ex1

namedField :: Parser Field
namedField = do
  skipJunk
  lhs <- name
  skipJunk
  void $ char '='
  NamedField lhs <$> ex1

singleExField :: Parser Field
singleExField = SingleExField <$> ex1

checkField :: Parser Bool
checkField = lookAhead (try $ field >> return True) <|> return False

field :: Parser Field
field = exField <|> namedField <|> singleExField

fieldList :: Parser [Field]
fieldList = do
  skipJunk
  isField <- checkField
  if not isField
    then
      return []
    else do
      skipJunk
      lhs <- field
      isComma <- checkChar ','
      if isComma
        then do
          void $ char ','
          rhs <- fieldList
          return $ lhs : rhs
        else do
          return [lhs]

tableConstructor :: Parser TableConstructor
tableConstructor = do
  skipJunk
  void $ char '{'
  fields <- fieldList
  skipJunk
  void $ char '}'
  return $ TableConstructor fields

tableEx :: Parser Expr
tableEx = TableExpr <$> tableConstructor

exList :: Parser ExprList
exList = do
  skipJunk
  start <- ex1
  end <- many (skipJunk >> char ',' >> skipJunk >> ex1)
  return $ ExprList $ start : end

varList :: Parser VarList
varList = do
  skipJunk
  start <- var
  end <- many (skipJunk >> char ',' >> skipJunk >> var)
  return $ VarList $ start : end

isVarHelper :: PrefixExpr' -> Bool
isVarHelper (TableIndex _ PrefixEmpty) = True
isVarHelper (DotIndex _ PrefixEmpty) = True
isVarHelper (TableIndex _ r) = isVarHelper r
isVarHelper (DotIndex _ r) = isVarHelper r
isVarHelper (CallArgs _ r) = isVarHelper r
isVarHelper (MethodArgs _ _ r) = isVarHelper r
isVarHelper PrefixEmpty = False

isVar :: PrefixExpr -> Bool
isVar (PrefixName _ PrefixEmpty) = True
isVar (PrefixName _ r) = isVarHelper r
isVar (PrefixSub _ r) = isVarHelper r

isCallHelper :: PrefixExpr' -> Bool
isCallHelper (MethodArgs _ _ PrefixEmpty) = True
isCallHelper (CallArgs _ PrefixEmpty) = True
isCallHelper (TableIndex _ r) = isCallHelper r
isCallHelper (DotIndex _ r) = isCallHelper r
isCallHelper (CallArgs _ r) = isCallHelper r
isCallHelper (MethodArgs _ _ r) = isCallHelper r
isCallHelper PrefixEmpty = False

isCall :: PrefixExpr -> Bool
isCall (PrefixName _ PrefixEmpty) = False
isCall (PrefixName _ r) = isCallHelper r
isCall (PrefixSub _ r) = isCallHelper r

var :: Parser Var
var = do
  skipJunk
  x <- prefixEx
  if isVar x then return $ Var x else fail $ "Expression " ++ show x ++ " does not evaluate to a variable"

funcCall :: Parser FuncCall
funcCall = do
  skipJunk
  x <- prefixEx
  if isCall x then return $ FuncCall x else fail $ "Expression " ++ show x ++ " does not evaluate to a function call"

nameList :: Parser NameList
nameList = do
  skipJunk
  start <- name
  end <- many (skipJunk >> char ',' >> skipJunk >> name)
  return $ NameList $ start : end

paramList :: Parser ParamList
paramList = do
  skipJunk
  void $ char '('
  check1 <- checkSingle "..."
  if check1 then do
    void $ string "..."
    skipJunk
    void $ char ')'
    return $ ParamList (NameList []) $ Just VarArg
  else do
    nameMaybe <- optionMaybe nameList
    let namedParams = case nameMaybe of
          Just names -> names
          Nothing -> NameList []
    skipJunk
    check2 <- checkChar ','
    if check2 then do
      void $ char ','
      skipJunk
      void $ string "..."
      skipJunk
      void $ char ')'
      return $ ParamList namedParams $ Just VarArg
    else do
      void $ char ')'
      return $ ParamList namedParams Nothing

functionDef :: Parser Expr
functionDef = do
  skipJunk
  isName <- checkName
  if isName then fail "" else do
    void $ string "function"
    skipJunk
    params <- paramList
    skipJunk
    body <- block
    skipJunk
    void $ string "end"
    return $ FunctionDef $ FuncBody params body

asgn :: Parser Stat
asgn = do
  skipJunk
  lhs <- varList
  skipJunk
  void $ char '='
  skipJunk
  Asgn lhs <$> exList

semic :: Parser Stat
semic = skipJunk >> char ';' >> return Semic

stat :: Parser Stat
stat = try (asgn <|> semic)

statList :: Parser StatList
statList = StatList <$> many stat

retStat :: Parser RetStat
retStat = do
  skipJunk
  isName <- checkName
  if isName
    then fail ""
    else do
      void $ string "return"
      skipJunk
      check <- checkChar ';'
      if check
        then do
          void $ char ';'
          return $ RetStat $ ExprList []
        else do
          ret <- exList
          void $ optionMaybe $ char ';'
          return $ RetStat ret
block :: Parser Block
block = do
  skipJunk
  retStart <- optionMaybe retStat
  case retStart of
    Just _ -> do
      skipJunk
      input <- getInput
      if null input
        then do
          return $ Block (StatList []) retStart
        else fail ""
    Nothing -> do
      stats <- statList
      ret <- optionMaybe retStat
      return $ Block stats ret

chunk :: Parser Chunk
chunk = Chunk <$> block

program :: Parser Chunk
program = do
  prog <- chunk
  skipJunk
  input <- getInput
  if null input then return prog else fail "expected <EOF>"
