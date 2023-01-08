grammar Latte;

program
    : topDef+
    ;

topDef
    : funDef                                # TopFnDef
    | 'class' ID classBlock                 # BaseCls
    | 'class' ID 'extends' ID classBlock    # DerivCls
    ;

funDef
    : type_ ID '(' params ')' block
    ;

params
    : param ( ',' param )*
    | /* no params */
    ;

param
    : nonvoid_type ID
    ;

classBlock
    : '{' classItem* '}'
    ;

classItem
    : nonvoid_type ID ';'               # Field
    | funDef                            # Method
    ;

decl
    : nonvoid_type items ';'
    ;

items
    : item ( ',' item )*
    ;
    
item
    : ID                                # DeclItemUninit
    | ID '=' expr                       # DeclItemInit
    ;

block
    : '{' stmt* '}'
    ;

stmt
    : ';'                                           # Empty
    | block                                         # BlockStmt
    | decl                                          # VarDecl
    | lval '=' expr ';'                             # Ass
    | lval '++' ';'                                 # Incr
    | lval '--' ';'                                 # Decr
    | 'return' expr ';'                             # Ret
    | 'return' ';'                                  # VRet
    | 'if' '(' expr ')' stmt                        # Cond
    | 'if' '(' expr ')' stmt 'else' stmt            # CondElse
    | 'while' '(' expr ')' stmt                     # While
    | 'for' '(' nonvoid_type ID ':' expr ')' stmt   # For
    | expr ';'                                      # SExp
    ;

lval
    : ID '(' args ')'               # LFunCall
    | lval '.' ID '(' args ')'      # LMetCall
    | lval '.' ID                   # LField
    | lval '[' expr ']'             # LArr
    | 'new' newtype                 # LNew
    | ID                            # LID
    | '(' lval ')'                  # LParen
    ;

type_
    : 'void'        # Void
    | nonvoid_type  # Nonvoid
    ;
    
nonvoid_type
    : 'int' '[]'        # IntArr
    | 'string' '[]'     # StrArr
    | 'boolean' '[]'    # BooleanArr
    | ID '[]'           # ClassArr
    | 'int'             # Int
    | 'string'          # Str
    | 'boolean'         # Bool
    | ID                # Class
    ;

newtype
    : 'int' '[' expr ']'        # NIntArr
    | 'string' '[' expr ']'     # NStrArr
    | 'boolean' '[' expr ']'    # NBooleanArr
    | ID '[' expr ']'           # NClassArr
    | ID                        # NClass
    ;

expr
    : unOp expr                                 # EUnOp
    | expr mulOp expr                           # EMulOp
    | expr addOp expr                           # EAddOp
    | expr relOp expr                           # ERelOp
    | <assoc=right> expr '&&' expr              # EAnd
    | <assoc=right> expr '||' expr              # EOr
    | INT                                       # EInt
    | 'true'                                    # ETrue
    | 'false'                                   # EFalse
    | STR                                       # EStr
    | '(' nonvoid_type ')' 'null'               # ENull
    | lval                                      # ELVal
    | '(' expr ')'                              # EParen
    ;

args
    : ( arg ( ',' arg )* )?
    | /* no args */
    ;

arg
    : expr
    ;

unOp
    : '!'
    | '-'
    ;

addOp
    : '+'
    | '-'
    ;

mulOp
    : '*'
    | '/'
    | '%'
    ;

relOp
    : '<'
    | '<='
    | '>'
    | '>='
    | '=='
    | '!='
    ;

COMMENT : ('#' ~[\r?\n]* | '//' ~[\r?\n]*) -> skip;
MULTICOMMENT : '/*' .*? '*/' -> skip;

fragment Letter  : Capital | Small ;
fragment Capital : [A-Z\u00C0-\u00D6\u00D8-\u00DE] ;
fragment Small   : [a-z\u00DF-\u00F6\u00F8-\u00FF] ;
fragment Digit : [0-9] ;

INT : Digit+ ;
fragment ID_First : Letter | '_';
ID
    : ID_First (ID_First | Digit)* 
    ;

WS : (' ' | '\r' | '\t' | '\n')+ ->  skip;

STR
    :   '"' StringCharacters? '"'
    ;
fragment StringCharacters
    :   StringCharacter+
    ;
fragment
StringCharacter
    :   ~["\\]
    |   '\\' [tnr"\\]
    ;
