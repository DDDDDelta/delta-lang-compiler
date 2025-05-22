pub const LEXER_RULES: &'static str = r#"
FN : 'fn';
LET : 'let';
I32 : 'i32';
I8 : 'i8';
RETURN : 'return';

PLUS : '+';
MINUS : '-';
STAR : '*';
SLASH : '/';
PERCENT : '%';
EQ : '=' ;
COMMA : ',' ;
SEMI : ';' ;
LPAREN : '(' ;
RPAREN : ')' ;
LBRACE : '{' ;
RBRACE : '}' ;

STR : '"' (ESC | ~["\\\r\n])* '"'
    ;

fragment ESC
    : '\\' [btnfr"\\]
    ;
    
INT : [0-9]+;
ID: [a-zA-Z_][a-zA-Z_0-9]*;
WS: [ \t\n\r\f]+ -> skip ;
"#;
