-- | Main parser module for the Sol Lua interpreter
module Parser.Parser where

import AST
import Control.Monad (void)
import Parser.Helpers
import Parser.Nums
import Parser.Op
import Text.Parsec (alphaNum, anyChar, char, eof, getInput, letter, lookAhead, many, manyTill, oneOf, optionMaybe, satisfy, string, try, (<|>))
import Text.Parsec.String (Parser)

-- | A list containing characters that are valid after a literal value is read. Primarily used to detect if a number is malformed.
validNext :: [String]
validNext = [" ", "<=", ">=", "<", ">", "~=", "==", "|", "~", "&", "<<", ">>", "+", "-", "*", "/", "//", "%", "^", "\n", ")", "}", ",", "]", ";", ".."]

-- | Parses int64 and float64 numerals.
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

-- | Parses a single line string.
singleLineStr :: Parser Expr
singleLineStr = do
  start <- oneOf "\"'"
  str <- manyTill (satisfy (/= '\n')) (char start)
  return $ LiteralExpr (StringLit $ unescape str)
  where
    unescape :: String -> String
    unescape s = read ("\"" ++ s ++ "\"")

-- | Parses a raw multi line string.
multiLineStr :: Parser Expr
multiLineStr =
  char '[' >> do
    levelStr <- many $ try $ char '='
    void $ char '['
    str <- manyTill anyChar (string $ "]" ++ levelStr ++ "]")
    return $ LiteralExpr (StringLit str)

-- | A helper function that is used to check if a value is a literal or an identifier. If it's an identifier, it fails.
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

-- | Parses the the varargs symbol @...@
tripleDot :: Parser Expr
tripleDot = litHelper "..." TRIPLE_DOT

-- | Parses the @true@ literal.
trueEx :: Parser Expr
trueEx = litHelper "true" TRUE

-- | Parses the @false@ literal.
falseEx :: Parser Expr
falseEx = litHelper "false" FALSE

-- | Parses the @nil@ literal.
nilEx :: Parser Expr
nilEx = litHelper "nil" NIL

-- | Parses literal expressions. Does consume not input, except for whitespace and comments.
literalExpr :: Parser Expr
literalExpr = skipJunk >> (try num <|> try singleLineStr <|> try multiLineStr <|> try trueEx <|> try falseEx <|> try tripleDot <|> try nilEx)

-- | Parses an ex2 expression and passes it to the ex1' parser as its left side.
ex1 :: Parser Expr
ex1 = skipJunk >> ex2 >>= ex1'

-- | Tries parsing a binary expression with the "or" operator @or@. Returns its left side on failure.
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

-- | Parses an ex3 expression and passes it to the ex2' parser as its left side.
ex2 :: Parser Expr
ex2 = skipJunk >> ex3 >>= ex2'

-- | Tries parsing a binary expression with the "and" operator @and@. Returns its left side on failure.
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

-- | Parses an ex4 expression and passes it to the ex3' parser as its left side.
ex3 :: Parser Expr
ex3 = skipJunk >> ex4 >>= ex3'

-- | Tries parsing a binary expression with the comparison operators @<=@, @>=@, @<@, @>@, @~=@, and @==@. Returns its left side on failure.
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

-- | Parses an ex5 expression and passes it to the ex4' parser as its left side.
ex4 :: Parser Expr
ex4 = skipJunk >> ex5 >>= ex4'

-- | Tries parsing a binary expression with the binary "or" operator @|@. Returns its left side on failure.
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

-- | Parses an ex6 expression and passes it to the ex5' parser as its left side.
ex5 :: Parser Expr
ex5 = skipJunk >> ex6 >>= ex5'

-- | Tries parsing a binary expression with the binary "xor" operator @~@. Returns its left side on failure.
ex5' :: Expr -> Parser Expr
ex5' l = do
  skipJunk
  -- We have to check if the next symbol would be a ~=, because it's lower precedence and it might get parsed wrongly here.
  check <- checkSingle "~"
  check2 <- checkSingle "~="
  if not check || check2
    then return l
    else do
      op <- oper5
      r <- ex6
      ex5' $ BinExpr l op r

-- | Parses an ex7 expression and passes it to the ex6' parser as its left side.
ex6 :: Parser Expr
ex6 = skipJunk >> ex7 >>= ex6'

-- | Tries parsing a binary expression with the binary "and" operator @&@. Returns its left side on failure.
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

-- | Parses an ex8 expression and passes it to the ex7' parser as its left side.
ex7 :: Parser Expr
ex7 = skipJunk >> ex8 >>= ex7'

-- | Left associative parser for expressions with the bitshift operators @<<@ and @>>@
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

-- | Parses an ex9 expression and passes it to the ex8' parser as its left side.
ex8 :: Parser Expr
ex8 = skipJunk >> ex9 >>= ex8'

-- | Tries parsing a binary expression with the right associative concat operator @..@. Returns its left side on failure.
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

-- | Parses an ex10 expression and passes it to the ex9' parser as its left side.
ex9 :: Parser Expr
ex9 = skipJunk >> ex10 >>= ex9'

-- | Tries parsing a binary expression with the arithmetic operators @+@ and @-@. Returns its left side on failure.
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

-- | Parses an ex11 expression and passes it to the ex10' parser as its left side.
ex10 :: Parser Expr
ex10 = skipJunk >> ex11 >>= ex10'

-- | Tries parsing a binary expression with the arithmetic operators @*@, @/@, @//@ and @%@. Returns its left side on failure.
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

-- | Tries parsing a unary expression with the operators @not@, @#@, @-@ and @~@. Parses ex12 on failure.
ex11 :: Parser Expr
ex11 = do
  skipJunk
  check <- checkMulti ["not ", "#", "-", "~"]
  if not check
    then ex12
    else UnaryExpr <$> oper11 <*> ex11

-- | Parses a literal, prefix-expression, table constructor or function definition and passes it to the ex12' parser as its left side.
ex12 :: Parser Expr
ex12 = do
  skipJunk
  l <- try literalExpr <|> try preEx <|> try tableEx <|> try functionDef
  ex12' l

-- | Tries parsing a binary expression with the right associative exponent operator @^@. Returns its left side on failure.
ex12' :: Expr -> Parser Expr
ex12' l = do
  skipJunk
  check <- checkSingle "^"
  if not check
    then return l
    else do
      op <- oper12
      r <- (try literalExpr <|> try preEx <|> try tableEx <|> try functionDef) >>= ex12'
      return $ BinExpr l op r

-- | Parses a subexpression: (@exp@)
subEx :: Parser Expr
subEx = do
  skipJunk
  void $ char '('
  expr <- ex1
  skipJunk
  void $ char ')'
  return expr

-- | Parses a variable name.
name :: Parser Name
name = do
  skipJunk
  first <- try letter <|> try (char '_')
  rest <- many $ try (alphaNum <|> char '_')
  let full = first : rest
  if full `elem` reservedKW
    then fail $ "Cannot use reserved keyword \"" ++ full ++ "\""
    else return full

-- | Lookahead parser that checks for a valid name. Does not consume input.
checkName :: Parser Bool
checkName = lookAhead (try $ name >> return True) <|> return False

-- | Checks if the next string would be a valid name/identifier. If not, then it parses the input keyword.
keyword :: String -> Parser ()
keyword kw = do
  skipJunk
  isName <- checkName
  if isName then fail "" else void $ string kw

-- | Parser wrapper for prefixEx.
preEx :: Parser Expr
preEx = PreExpr <$> prefixEx

-- | Parses a prefix expression, which can either end up being a sub expression, a variable or a chain of table indexes and function calls.
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

-- | Parses the suffix of a prefix expression.
prefixEx' :: Parser PrefixExpr'
prefixEx' = do
  input <- getInput
  if null input
    then
      return PrefixEmpty
    else do
      skipJunk
      nextChar <- peekChar
      -- The concat symbol might get parsed as a single dot if we're not careful.
      isConcat <- checkSingle ".."
      if isConcat
        then return PrefixEmpty
        else case nextChar of
          -- First case can be either a table index or a multi line string, that's why we try callArgs.
          '[' -> try tableIndex <|> try callArgs
          '.' -> dotIndex
          '(' -> callArgs
          -- Table constructor incoming.
          '{' -> callArgs
          -- String literals
          '"' -> callArgs
          '\'' -> callArgs

          ':' -> methodArgs
          _ -> return PrefixEmpty

-- | Parses a table index via square brackets.
tableIndex :: Parser PrefixExpr'
tableIndex = do
  skipJunk
  void $ char '['
  lhs <- ex1
  skipJunk
  void $ char ']'
  TableIndex lhs <$> prefixEx'

-- | Parses a table index via dot notation.
dotIndex :: Parser PrefixExpr'
dotIndex = do
  skipJunk
  void $ char '.'
  lhs <- name
  DotIndex lhs <$> prefixEx'

-- | Parses an argument list.
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

-- | Parses a table constructor as a single function argument.
argTable :: Parser Args
argTable = ArgTable <$> tableConstructor

-- | Parses a string as a single function argument.
argString :: Parser Args
argString = ArgString <$> (try singleLineStr <|> try multiLineStr)

-- | Parses function arguments.
args :: Parser Args
args = do
  skipJunk
  try argList <|> try argTable <|> try argString

-- | Parses a function call.
callArgs :: Parser PrefixExpr'
callArgs = do
  skipJunk
  a <- args
  CallArgs a <$> prefixEx'

-- | Parses a method call.
methodArgs :: Parser PrefixExpr'
methodArgs = do
  skipJunk
  void $ char ':'
  n <- name
  a <- args
  MethodArgs n a <$> prefixEx'

-- | Parses an expression table field. Example inside table constructor: {[10] = "value"}
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

-- | Parses a named table field. Example inside table constructor: {x = "value"}
namedField :: Parser Field
namedField = do
  skipJunk
  lhs <- name
  skipJunk
  void $ char '='
  NamedField lhs <$> ex1

-- | Parses a simple expression table field. Exampel inside table constructor: {"value"}
singleExField :: Parser Field
singleExField = SingleExField <$> ex1

-- | Lookahead parser that checks for a valid field.
checkField :: Parser Bool
checkField = lookAhead (try $ field >> return True) <|> return False

-- | Parses a field within a table constructor.
field :: Parser Field
field = try exField <|> try namedField <|> try singleExField

-- | Parses the field list within a table constructor.
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

-- | Parses a table constructor.
tableConstructor :: Parser TableConstructor
tableConstructor = do
  skipJunk
  void $ char '{'
  fields <- fieldList
  skipJunk
  void $ char '}'
  return $ TableConstructor fields

-- | Parser wrapper for tableConstructor
tableEx :: Parser Expr
tableEx = TableExpr <$> tableConstructor

-- | Parses an expression list (cannot be empty).
exList :: Parser ExprList
exList = do
  skipJunk
  start <- ex1
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> ex1)
  return $ ExprList $ start : end

-- | Parses a variable list (cannot be empty).
varList :: Parser VarList
varList = do
  skipJunk
  start <- var
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> var)
  return $ VarList $ start : end

-- | Helper function for checking wether a prefix expression is a variable.
isVarHelper :: PrefixExpr' -> Bool
isVarHelper (TableIndex _ PrefixEmpty) = True
isVarHelper (DotIndex _ PrefixEmpty) = True
isVarHelper (TableIndex _ r) = isVarHelper r
isVarHelper (DotIndex _ r) = isVarHelper r
isVarHelper (CallArgs _ r) = isVarHelper r
isVarHelper (MethodArgs _ _ r) = isVarHelper r
isVarHelper PrefixEmpty = False

-- | Checks wether a prefix expression is a variable.
isVar :: PrefixExpr -> Bool
isVar (PrefixName _ PrefixEmpty) = True
isVar (PrefixName _ r) = isVarHelper r
isVar (PrefixSub _ r) = isVarHelper r

-- | Helper function for checking wether a prefix expression is a function call.
isCallHelper :: PrefixExpr' -> Bool
isCallHelper (MethodArgs _ _ PrefixEmpty) = True
isCallHelper (CallArgs _ PrefixEmpty) = True
isCallHelper (TableIndex _ r) = isCallHelper r
isCallHelper (DotIndex _ r) = isCallHelper r
isCallHelper (CallArgs _ r) = isCallHelper r
isCallHelper (MethodArgs _ _ r) = isCallHelper r
isCallHelper PrefixEmpty = False

-- | Checks wether a prefix expression is a function call.
isCall :: PrefixExpr -> Bool
isCall (PrefixName _ PrefixEmpty) = False
isCall (PrefixName _ r) = isCallHelper r
isCall (PrefixSub _ r) = isCallHelper r

-- | Parses a variable.
var :: Parser Var
var = do
  skipJunk
  x <- prefixEx
  if isVar x then return $ Var x else fail $ "Expression " ++ show x ++ " does not evaluate to a variable"

-- | Parses a function call.
funcCall :: Parser FuncCall
funcCall = do
  skipJunk
  x <- prefixEx
  if isCall x then return $ FuncCall x else fail $ "Expression " ++ show x ++ " does not evaluate to a function call"

-- | Parses a variable name list.
nameList :: Parser NameList
nameList = do
  skipJunk
  start <- name
  end <- many $ try (skipJunk >> char ',' >> skipJunk >> name)
  return $ NameList $ start : end

-- | Parses a function's parameter list.
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

-- | Parses a function's body.
funcBody :: Parser FuncBody
funcBody = do
  params <- skipJunk >> paramList
  body <- skipJunk >> block
  skipJunk >> keyword "end"
  return $ FuncBody params body

-- | Parses a function definition.
functionDef :: Parser Expr
functionDef = do
  skipJunk >> keyword "function"
  FunctionDef <$> (skipJunk >> funcBody)

-- | Parses a regular assignment statement.
asgn :: Parser Stat
asgn = do
  skipJunk
  lhs <- varList
  skipJunk
  void $ char '='
  skipJunk
  Asgn lhs <$> exList

-- | Parses a semicolon (empty statement).
semic :: Parser Stat
semic = skipJunk >> char ';' >> return Semic

-- | Parses a goto label.
labelStat :: Parser Stat
labelStat = do
  x <- skipJunk >> string "::" >> skipJunk >> name
  void $ skipJunk >> string "::"
  return $ Label x

-- | Parses a break statement.
breakStat :: Parser Stat
breakStat = skipJunk >> keyword "break" >> return Break

-- | Parses a goto statement.
goto :: Parser Stat
goto = skipJunk >> keyword "goto" >> skipJunk >> Goto <$> name

-- | Parses a do block statement.
doStat :: Parser Stat
doStat = do
  skipJunk >> keyword "do"
  b <- skipJunk >> block
  skipJunk >> keyword "end"
  return $ Do b

-- | Parses a while do statement.
whileDo :: Parser Stat
whileDo = do
  skipJunk >> keyword "while"
  ex <- skipJunk >> ex1
  (Do b) <- skipJunk >> doStat
  return $ WhileDo ex b

-- | Parses a repeat until statement.
repeatUntil :: Parser Stat
repeatUntil = do
  skipJunk >> keyword "repeat"
  b <- skipJunk >> block
  skipJunk >> keyword "until"
  RepeatUntil b <$> (skipJunk >> ex1)

-- | Parses a function call statement.
funcCallStat :: Parser Stat
funcCallStat = skipJunk >> FuncCallStat <$> funcCall

-- | Parses an else if clause.
elseIf :: Parser ElseIf
elseIf = do
  skipJunk >> keyword "elseif"
  ex <- skipJunk >> ex1
  skipJunk >> keyword "then"
  ElseIf ex <$> (skipJunk >> block)

-- | Parses an else if list.
elseIfList :: Parser ElseIfList
elseIfList = ElseIfList <$> many (try elseIf)

-- | Parses an else clause.
elseStat :: Parser Else
elseStat = do
  skipJunk >> keyword "else"
  Else <$> (skipJunk >> block)

-- | Parses an if statement.
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

-- | Parses a regular for loop.
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

-- | Parses a for ... in loop.
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

-- | Parses a qualified function name.
funcName :: Parser FuncName
funcName = do
  skipJunk
  n1 <- name
  nl <- many $ try (char '.' >> skipJunk >> name)
  n2 <- optionMaybe $ try (char ':' >> skipJunk >> name)
  return $ FuncName n1 nl n2

-- | Parses a global function definition statement.
funcDefStat :: Parser Stat
funcDefStat = do
  skipJunk
  keyword "function"
  skipJunk
  n <- funcName
  skipJunk
  FuncDefStat n <$> funcBody

-- | Parses a local function definition statement.
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

-- | Parses a variable attribute.
attrib :: Parser Attrib
attrib = do
  n <- skipJunk >> char '<' >> skipJunk >> name
  void $ char '>'
  return $ Attrib n

-- | Parses an a variable name along with an optional attribute.
attrName :: Parser (Name, Maybe Attrib)
attrName = do
  skipJunk
  n <- name
  skipJunk
  a <- optionMaybe $ try attrib
  return (n, a)

-- | Parses an attribute name list.
attrNameList :: Parser AttrNameList
attrNameList = do
  skipJunk
  start <- attrName
  skipJunk
  end <- many $ try $ skipJunk >> char ',' >> attrName
  return $ AttrNameList $ start : end

-- | Parses a local assignment statement.
localAsgn :: Parser Stat
localAsgn = do
  skipJunk
  keyword "local"
  skipJunk
  anl <- attrNameList
  skipJunk
  el <- optionMaybe $ try $ char '=' >> skipJunk >> exList
  return $ LocalAsgn anl el

-- | Parses a statement.
stat :: Parser Stat
stat = skipJunk >> (try funcCallStat <|> try asgn <|> try semic <|> try labelStat <|> try breakStat <|> try goto <|> try doStat <|> try whileDo <|> try repeatUntil <|> try ifStat <|> try forStat <|> try forIn <|> try funcDefStat <|> try localFuncStat <|> try localAsgn)

-- | Parses a list of statements.
statList :: Parser StatList
statList = do
  skipJunk
  sl <- many (try stat)
  skipJunk
  return $ StatList sl

-- | Parses a return statement.
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

-- | Parses a block.
block :: Parser Block
block = do
  skipJunk
  stats <- statList
  ret <- optionMaybe $ try retStat
  return $ Block stats ret

-- | Parses a chunk.
chunk :: Parser Chunk
chunk = Chunk <$> block

-- | Parses the program and ends with EOF.
program :: Parser AST
program = AST <$> chunk <* eof
