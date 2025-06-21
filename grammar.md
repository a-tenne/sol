# Old

exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
		 prefixexp | tableconstructor | exp binop exp | unop exp 

var ::=  Name | prefixexp ‘[’ exp ‘]’ | prefixexp ‘.’ Name 

prefixexp ::= var | functioncall | ‘(’ exp ‘)’

functioncall ::=  prefixexp args | prefixexp ‘:’ Name args

args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString

# New

exp ::=  nil | false | true | Numeral | LiteralString | ‘...’ | functiondef | 
		 prefixexp | tableconstructor | exp binop exp | unop exp 

var ::= prefixexp

functioncall ::= prefixexp

prefixexp ::= Name prefixexp' | ‘(’ exp ‘)’ prefixexp'

prefixexp' ::= ‘[’ exp ‘]’ prefixexp' | ‘.’ Name prefixexp' | args prefixexp' | ‘:’ Name args prefixexp' | ε

args ::=  ‘(’ [explist] ‘)’ | tableconstructor | LiteralString
