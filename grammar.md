# [Original Grammar](https://www.lua.org/manual/5.4/manual.html#9)

chunk ::= block

block ::= {stat} \[retstat\]

stat ::=  ‘;’ | 
     varlist ‘=’ explist | 
     functioncall | 
     label | 
     **break** | 
     **goto** Name | 
     **do** block **end** | 
     **while** exp **do** block **end** | 
     **repeat** block **until** exp | 
     **if** exp **then** block {elseif exp **then** block} \[**else** block\] **end** | 
     **for** Name ‘=’ exp ‘,’ exp \[‘,’ exp\] **do** block **end** | 
     **for** namelist **in** explist **do** block **end** | 
     **function** funcname funcbody | 
     **local** **function** Name funcbody | 
     **local** attnamelist \[‘=’ explist\] 

attnamelist ::=  Name attrib {‘,’ Name attrib}

attrib ::= \[‘<’ Name ‘>’\]

retstat ::= return \[explist\] \[‘;’\]

label ::= ‘::’ Name ‘::’

funcname ::= Name {‘.’ Name} \[‘:’ Name\]

varlist ::= var {‘,’ var}

var ::=  Name | prefixexp ‘\[’ exp ‘\]’ | prefixexp ‘.’ Name 

namelist ::= Name {‘,’ Name}

explist ::= exp {‘,’ exp}

exp ::=  **nil** | **false** | **true** | Numeral | LiteralString | ‘...’ | functiondef | 
     prefixexp | tableconstructor | exp binop exp | unop exp 

prefixexp ::= var | functioncall | ‘(’ exp ‘)’

functioncall ::=  prefixexp args | prefixexp ‘:’ Name args 

args ::=  ‘(’ \[explist\] ‘)’ | tableconstructor | LiteralString 

functiondef ::= function funcbody

funcbody ::= ‘(’ \[parlist\] ‘)’ block end

parlist ::= namelist \[‘,’ ‘...’\] | ‘...’

tableconstructor ::= ‘{’ \[fieldlist\] ‘}’

fieldlist ::= field {fieldsep field} \[fieldsep\]

field ::= ‘\[’ exp ‘\]’ ‘=’ exp | Name ‘=’ exp | exp

fieldsep ::= ‘,’ | ‘;’

binop ::=  ‘+’ | ‘-’ | ‘*’ | ‘/’ | ‘//’ | ‘^’ | ‘%’ | 
     ‘&’ | ‘~’ | ‘|’ | ‘>>’ | ‘<<’ | ‘..’ | 
     ‘<’ | ‘<=’ | ‘>’ | ‘>=’ | ‘==’ | ‘~=’ | 
     **and** | **or**

unop ::= ‘-’ | not | ‘#’ | ‘~’

# Right Recursive Rewrite

chunk ::= block

block ::= {stat} \[retstat\]

stat ::=  ‘;’ | 
     varlist ‘=’ explist | 
     functioncall | 
     label | 
     **break** | 
     **goto** Name | 
     **do** block **end** | 
     **while** exp **do** block **end** | 
     **repeat** block **until** exp | 
     **if** exp **then** block {elseif exp **then** block} \[**else** block\] **end** | 
     **for** Name ‘=’ exp ‘,’ exp \[‘,’ exp\] **do** block **end** | 
     **for** namelist **in** explist **do** block **end** | 
     **function** funcname funcbody | 
     **local** **function** Name funcbody | 
     **local** attnamelist \[‘=’ explist\] 

attnamelist ::=  Name attrib {‘,’ Name attrib}

attrib ::= \[‘<’ Name ‘>’\]

retstat ::= return \[explist\] \[‘;’\]

label ::= ‘::’ Name ‘::’

funcname ::= Name {‘.’ Name} \[‘:’ Name\]

varlist ::= var {‘,’ var}

var ::=  Name | prefixexp ‘\[’ exp ‘\]’ | prefixexp ‘.’ Name 

namelist ::= Name {‘,’ Name}

explist ::= exp {‘,’ exp}

op1 ::= **or**

op2 ::= **and**

op3 ::= ‘<=’ | ‘>=’ | ‘<’ | ‘>’ | ‘~=’ | ‘==’

op4 ::= ‘|’

op5 ::= ‘~’

op6 ::= ‘&’

op7 ::= ‘<<’ | ‘>>’

op8 ::= ‘..’

op9 ::= ‘+’ | ‘-’

op10 ::= ‘*’ | ‘/’ | ‘//’ | ‘%’

op11 ::= **not** | ‘#’ | ‘-’ | ‘~’

op12 ::= ‘^’


exp ::= exp2 {op1 exp2}

exp2 ::= exp3 {op2 exp3}

exp3 ::= exp4 {op3 exp4}

exp4 ::= exp5 {op4 exp5}

exp5 ::= exp6 {op5 exp6}

exp6 ::= exp7 {op6 exp7}

exp7 ::= exp8 {op7 exp8}

exp8 ::= exp9 | exp9 op8 exp8

exp9 ::= exp10 {op9 exp10}

exp10 ::= exp11 {op10 exp11}

exp11 ::= exp12 | op11 exp12

exp12 ::= exp13 op12 exp12

ex13 ::= **nil** | **false** | **true** | Numeral | LiteralString | ‘...’ | functiondef | 
     prefixexp | tableconstructor


> [!NOTE]
> The next two rules are not as strict as they are in the official grammar.
> The restrictions imposed by them, however, are adressed in the parser.
var ::= prefixexp

functioncall ::= prefixexp

prefixexp ::= Name prefixexp' | ‘(’ exp ‘)’ prefixexp'

prefixexp' ::= ‘\[’ exp ‘\]’ prefixexp' | ‘.’ Name prefixexp' | args prefixexp' | ‘:’ Name args prefixexp' | ε

args ::=  ‘(’ \[explist\] ‘)’ | tableconstructor | LiteralString

functiondef ::= function funcbody

funcbody ::= ‘(’ \[parlist\] ‘)’ block end

parlist ::= namelist \[‘,’ ‘...’\] | ‘...’

tableconstructor ::= ‘{’ \[fieldlist\] ‘}’

fieldlist ::= field {fieldsep field} \[fieldsep\]

field ::= ‘\[’ exp ‘\]’ ‘=’ exp | Name ‘=’ exp | exp

fieldsep ::= ‘,’ | ‘;’
