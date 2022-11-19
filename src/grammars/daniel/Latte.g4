/*
 * Parser Rules
 */

grammar Latte;

program             : topdef+
                    ;

topdef              : typ IDENT LPAREN (arg (COMMA arg)*)? RPAREN block
                    ;

arg                 : typ IDENT
                    ;

typ                : BOOLT #bool_t
                    | INTT #int_t
                    | VOIDT #void_t
                    | STRINGT #string_t
                    | typ LPAREN (typ (COMMA typ)*)? RPAREN #fun_t
                    ;

stmt                : SEMICOLON #empty
                    | block #blockStmt
                    | typ item (COMMA item)* SEMICOLON #decl
                    | IDENT EQUALS expr SEMICOLON #ass
                    | IDENT INCREMENT SEMICOLON #incr
                    | IDENT DECREMENT SEMICOLON #decr
                    | RETURN expr SEMICOLON #ret
                    | RETURN SEMICOLON #retV
                    | IF LPAREN expr RPAREN stmt #cond
                    | IF LPAREN expr RPAREN stmt ELSE stmt #condElse
                    | WHILE LPAREN expr RPAREN stmt #while
                    | expr SEMICOLON #expS
                    ;

block               : LBRACE stmt* RBRACE
                    ;

item                : IDENT #noInit
                    | IDENT EQUALS expr #init
                    ;

expr                : LPAREN expr RPAREN       # bracket
                    | STRING             # string
                    | IDENT              # id
                    | INT                # int
                    | TRUE               # true
                    | FALSE              # false
                    | IDENT LPAREN (expr  (COMMA expr)*)? RPAREN # funApp
                    | NOT expr           # not
                    | SUB expr           # neg
                    | expr mulOp expr    # mulDivMod
                    | expr addOp expr    # addSub
                    | expr relOp expr    # relation
                    | <assoc=right> expr AND expr # and
                    | <assoc=right> expr OR expr # or
                    ;

addOp               : ADD | SUB
                    ;

mulOp               : MUL | DIV | MOD
                    ;

relOp               : LTH | LE | GTH | GE | EQU | NE
                    ;

/*
 * Lexer Rules
 */
COMMENT     : '/*' .*? '*/' -> skip                     ;
LINE_COMMENT: ('//'|'#') ~[\r\n]* -> skip               ;
WS          : [ \t\r\n]+ -> skip                        ;
INT         : [0-9]+                                    ;
ADD         : '+'                                       ;
COMMA       : ','                                       ;
LPAREN      : '('                                       ;
RPAREN      : ')'                                       ;
LBRACE      : '{'                                       ;
RBRACE      : '}'                                       ;
SEMICOLON   : ';'                                       ;
EQUALS      : '='                                       ;
SUB         : '-'                                       ;
MUL         : '*'                                       ;
DIV         : '/'                                       ;
MOD         : '%'                                       ;
LTH         : '<'                                       ;
LE          : '<='                                      ;
GTH         : '>'                                       ;
GE          : '>='                                      ;
EQU         : '=='                                      ;
NE          : '!='                                      ;
TRUE        : 'true'                                    ;
FALSE       : 'false'                                   ;
AND         : '&&'                                      ;
OR          : '||'                                      ;
NOT         : '!'                                       ;
STRING      : '"' ((~["\\]) | ('\\' ["\\tnrf]))* '"'    ;
BOOLT       : 'bool'                                    ;
VOIDT       : 'void'                                    ;
INTT        : 'int'                                     ;
STRINGT     : 'string'                                  ;
INCREMENT   : '++'                                      ;
DECREMENT   : '--'                                      ;
RETURN      : 'return'                                  ;
IF          : 'if'                                      ;
ELSE        : 'else'                                    ;
WHILE       : 'while'                                   ;
IDENT       : [a-z]([a-zA-Z0-9\\_]*)                    ;
