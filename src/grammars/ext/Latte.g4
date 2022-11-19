grammar Latte;

program
    : topDef+
    ;

topDef
    : funDef                            # FnDef
    | 'class' ID classBlock                  # BaseCls
    | 'class' ID 'extends' ID classBlock     # DerivCls
    ;

funDef
    : type_ ID '(' arg? ')' block
    ;

arg
    : type_ ID ( ',' type_ ID )*
    ;

classBlock
    : '{' classItem* '}'
    ;

classItem
    : decl                              # Field
    | funDef                            # Method
    ;

decl
    : type_ item ( ',' item )* ';'
    ;

block
    : '{' stmt* '}'
    ;

stmt
    : ';'                                # Empty
    | block                              # BlockStmt
    | decl                               # VarDecl
    | ID '=' expr ';'                    # Ass
    | ID '++' ';'                        # Incr
    | ID '--' ';'                        # Decr
    | 'return' expr ';'                  # Ret
    | 'return' ';'                       # VRet
    | 'if' '(' expr ')' stmt             # Cond
    | 'if' '(' expr ')' stmt 'else' stmt # CondElse
    | 'while' '(' expr ')' stmt          # While
    | expr ';'                           # SExp
    ;

type_
    : 'int'         # Int
    | 'string'      # Str
    | 'boolean'     # Bool
    | 'void'        # Void

    | type_ '[]'    # Arr
    | ID            # Class
    ;

item
    : ID
    | ID '=' expr
    ;

expr
    : ('-'|'!') expr                                        # EUnOp
    | expr mulOp expr                                       # EMulOp
    | expr addOp expr                                       # EAddOp
    | expr relOp expr                                       # ERelOp
    | <assoc=right> expr '&&' expr                          # EAnd
    | <assoc=right> expr '||' expr                          # EOr
    | ID                                                    # EId
    | INT                                                   # EInt
    | 'true'                                                # ETrue
    | 'false'                                               # EFalse
    | ID '(' ( expr ( ',' expr )* )? ')'                    # EFunCall
    | STR                                                   # EStr
    | '(' type_ ')' 'null'                                  # ENull
    | '(' expr ')'                                          # EParen
    | expr '[expr]'                                         # EArrSub
    | <assoc=right> ID '.' ID                               # EField
    | <assoc=right> ID . ID '(' ( expr ( ',' expr )* )? ')' # EMetCall
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
