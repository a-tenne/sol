module Parser.Parser where

import AST
import Control.Monad (void)
import Parser.Helpers
import Parser.Nums
import Parser.Op
import Text.Parsec (alphaNum, anyChar, char, eof, getInput, letter, lookAhead, many, manyTill, oneOf, optionMaybe, satisfy, string, try, (<|>))
import Text.Parsec.String (Parser)

validNext :: [String]
validNext = [" ", "<=", ">=", "<", ">", "~=", "==", "|", "~", "&", "<<", ">>", "+", "-", "*", "/", "//", "%", "^", "\n", ")", "}", ",", "]", ";", ".."]

num :: Parser Expr
num = do
  n <- try numDouble <|> numInt
  input <- getInput
  if not (null input)
    then do
      check <- checkMulti validNext
      if check
        then
          return $ LiteralExpr (NumLit n)
        else fail "Malformed number"
    else return $ LiteralExpr (NumLit n)

singleLineStr :: Parser Expr
singleLineStr = do
  start <- oneOf "\"'"
  str <- manyTill (satisfy (/= '\n')) (char start)
  return $ LiteralExpr (StringLit $ unescape str)
    where
      unescape :: String -> String
      unescape s = read ("\"" ++ s ++ "\"")
        

multiLineStr :: Parser Expr
multiLineStr =
  char '[' >> do
    levelStr <- many $ try $ char '='
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
literalExpr = skipJunk >> (try num <|> try singleLineStr <|> try multiLineStr <|> try trueEx <|> try falseEx <|> try tripleDot <|> try nilEx)

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
  check2 <- checkSingle "~="
  if not check || check2
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
      r <- ex9 >>= ex8' 
      return $ BinExpr l op r

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
  l <- try literalExpr<|>try preEx <|>  try tableEx <|> try functionDef
  ex12' l

ex12' :: Expr -> Parser Expr
ex12' l = do
  skipJunk
  check <- checkSingle "^"
  if not check
    then return l
    else do
      op <- oper12
      r <- (try literalExpr<|>try preEx <|>  try tableEx <|> try functionDef) >>= ex12'
      return $ BinExpr l op r

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
  first <- try letter <|> try (char '_')
  rest <- many $ try (alphaNum <|> char '_')
  let full = first : rest
  if full `elem` reservedKW
    then fail $ "Cannot use reserved keyword \"" ++ full ++ "\""
    else return full

checkName :: Parser Bool
checkName = lookAhead (try $ name >> return True) <|> return False

keyword :: String -> Parser ()
keyword kw = do
  skipJunk
  isName <- checkName
  if isName then fail "" else void $ string kw

preEx :: Parser Expr
preEx = PreExpr <$> prefixEx

prefixEx :: Parser PrefixExpr
prefixEx = do
  skipJunk
  checkBrace <- checkChar '('
  if checkBrace
    then do
      lhs <- subEx
      skipJunk
      PrefixSub lhs <$> prefixEx'
    else do
      lhs <- name
      skipJunk
      PrefixName lhs <$> prefixEx'

prefixEx' :: Parser PrefixExpr'
prefixEx' = do
  input <- getInput
  if null input
    then
      return PrefixEmpty
    else do
      skipJunk
      nextChar <- peekChar
      isConcat <- checkSingle ".."
      if isConcat
        then return PrefixEmpty
        else case nextChar of
          '[' -> try tableIndex <|> try callArgs
          '.' -> dotIndex
          '(' -> callArgs
          '{' -> callArgs
          '"' -> callArgs
          '\'' -> callArgs
          ':' -> methodArgs
          _ -> return PrefixEmpty

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
  skipJunk
  checkEmpty <- checkChar ')'
  if checkEmpty
    then do
      void $ char ')'
      return $ ArgList $ ExprList []
    else do
      l <- exList
      skipJunk
      void $ char ')'
      return $ ArgList l

argTable :: Parser Args
argTable = ArgTable <$> tableConstructor

argString :: Parser Args
argString = ArgString <$> (try singleLineStr <|> try multiLineStr)

args :: Parser Args
args = do
  skipJunk
  try argList <|> try argTable <|> try argString

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
field = try exField <|> try namedField <|> try singleExField

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
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> ex1)
  return $ ExprList $ start : end

varList :: Parser VarList
varList = do
  skipJunk
  start <- var
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> var)
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
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> name)
  return $ NameList $ start : end

paramList :: Parser ParamList
paramList = do
  void $ skipJunk >> char '('
  check1 <- skipJunk >> checkSingle "..."
  if check1
    then do
      void $ string "..." >> skipJunk >> char ')'
      return $ ParamList (NameList []) $ Just VarArg
    else do
      nameMaybe <- optionMaybe $ try nameList
      let namedParams = case nameMaybe of
            Just names -> names
            Nothing -> NameList []
      check2 <- skipJunk >> checkChar ','
      if check2
        then do
          void $ char ',' >> skipJunk >> string "..." >> skipJunk >> char ')'
          return $ ParamList namedParams $ Just VarArg
        else do
          void $ char ')'
          return $ ParamList namedParams Nothing

funcBody :: Parser FuncBody
funcBody = do
  params <- skipJunk >> paramList
  body <- skipJunk >> block
  skipJunk >> keyword "end"
  return $ FuncBody params body

functionDef :: Parser Expr
functionDef = do
  skipJunk >> keyword "function"
  FunctionDef <$> (skipJunk >> funcBody)

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

labelStat :: Parser Stat
labelStat = do
  x <- skipJunk >> string "::" >> skipJunk >> name
  void $ skipJunk >> string "::"
  return $ Label x

breakStat :: Parser Stat
breakStat = skipJunk >> keyword "break" >> return Break

goto :: Parser Stat
goto = skipJunk >> keyword "goto" >> skipJunk >> Goto <$> name

doStat :: Parser Stat
doStat = do
  skipJunk >> keyword "do"
  b <- skipJunk >> block
  skipJunk >> keyword "end"
  return $ Do b

whileDo :: Parser Stat
whileDo = do
  skipJunk >> keyword "while"
  ex <- skipJunk >> ex1
  (Do b) <- skipJunk >> doStat
  return $ WhileDo ex b

repeatUntil :: Parser Stat
repeatUntil = do
  skipJunk >> keyword "repeat"
  b <- skipJunk >> block
  skipJunk >> keyword "until"
  RepeatUntil b <$> (skipJunk >> ex1)

funcCallStat :: Parser Stat
funcCallStat = skipJunk >> FuncCallStat <$> funcCall

elseIf :: Parser ElseIf
elseIf = do
  skipJunk >> keyword "elseif"
  ex <- skipJunk >> ex1
  skipJunk >> keyword "then"
  ElseIf ex <$> (skipJunk >> block)

elseIfList :: Parser ElseIfList
elseIfList = ElseIfList <$> many (try elseIf)

elseStat :: Parser Else
elseStat = do
  skipJunk >> keyword "else"
  Else <$> (skipJunk >> block)

ifStat :: Parser Stat
ifStat = do
  skipJunk >> keyword "if"
  ex <- skipJunk >> ex1
  skipJunk >> keyword "then"
  b <- skipJunk >> block
  elif <- skipJunk >> elseIfList
  el <- optionMaybe $ skipJunk >> try elseStat
  skipJunk >> keyword "end"
  return $ IfStat ex b elif el

forStat :: Parser Stat
forStat = do
  skipJunk >> keyword "for"
  n <- skipJunk >> name
  void $ skipJunk >> char '='
  e1 <- skipJunk >> ex1
  void $ skipJunk >> char ','
  e2 <- skipJunk >> ex1
  e3 <- optionMaybe $ skipJunk >> try (char ',' >> skipJunk >> ex1)
  (Do b) <- skipJunk >> doStat
  return $ ForStat n e1 e2 e3 b

forIn :: Parser Stat
forIn = do
  skipJunk
  keyword "for"
  skipJunk
  nl <- nameList
  skipJunk
  keyword "in"
  skipJunk
  elist <- exList
  skipJunk
  (Do b) <- doStat
  return $ ForIn nl elist b

funcName :: Parser FuncName
funcName = do
  skipJunk
  n1 <- name
  nl <- many $ try (char '.' >> skipJunk >> name)
  n2 <- optionMaybe $ try (char ':' >> skipJunk >> name)
  return $ FuncName n1 nl n2

funcDefStat :: Parser Stat
funcDefStat = do
  skipJunk
  keyword "function"
  skipJunk
  n <- funcName
  skipJunk
  FuncDefStat n <$> funcBody

localFuncStat :: Parser Stat
localFuncStat = do
  skipJunk
  keyword "local"
  skipJunk
  keyword "function"
  skipJunk
  n <- name
  skipJunk
  LocalFuncStat n <$> funcBody

attrib :: Parser Attrib
attrib = do
  n <- skipJunk >> char '<' >> skipJunk >> name
  void $ char '>'
  return $ Attrib n

attrName :: Parser (Name, Maybe Attrib)
attrName = do
  skipJunk
  n <- name
  skipJunk
  a <- optionMaybe $ try attrib
  return (n, a)

attrNameList :: Parser AttrNameList
attrNameList = do
  skipJunk
  start <- attrName
  skipJunk
  end <- many $ try $ skipJunk >> char ',' >> attrName
  return $ AttrNameList $ start : end

localAsgn :: Parser Stat
localAsgn = do
  skipJunk
  keyword "local"
  skipJunk
  anl <- attrNameList
  skipJunk
  el <- optionMaybe $ try $ char '=' >> skipJunk >> exList
  return $ LocalAsgn anl el

stat :: Parser Stat
stat = skipJunk >> (try funcCallStat <|> try asgn <|> try semic <|> try labelStat <|> try breakStat <|> try goto <|> try doStat <|> try whileDo <|> try repeatUntil <|> try ifStat <|> try forStat <|> try forIn <|> try funcDefStat <|> try localFuncStat <|> try localAsgn)

statList :: Parser StatList
statList = do
  skipJunk
  sl <- many (try stat)
  skipJunk
  return $ StatList sl

retStat :: Parser RetStat
retStat = do
  skipJunk
  try $ keyword "return"
  check <- skipJunk >> checkChar ';'
  if check
    then do
      void $ char ';'
      return $ RetStat $ ExprList []
    else do
      check2 <- lookAhead (try ex1 >> return True) <|> return False
      ret <- if check2 then exList else return $ ExprList []
      void $ optionMaybe $ try $ char ';'
      return $ RetStat ret

block :: Parser Block
block = do
  skipJunk
  stats <- statList
  ret <- optionMaybe $ try retStat
  return $ Block stats ret

chunk :: Parser Chunk
chunk = Chunk <$> block

program :: Parser AST
program = AST <$> chunk <* eof
