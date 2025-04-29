/*
 * *** Lexer Rule ***
 * 
 * FN : 'fn';
 * LET : 'let';
 * I32 : 'i32';

 * PLUS : '+';
 * MINUS : '-';
 * STAR : '*';
 * SLASH : '/';
 * AMP : '%';
 * EQ : '=' ;
 * COMMA : ',' ;
 * SEMI : ';' ;
 * LPAREN : '(' ;
 * RPAREN : ')' ;
 * LCURLY : '{' ;
 * RCURLY : '}' ;
 * 
 * STR : '"' [a-zA-Z_0-9]* '"';
 * INT : [0-9]+;
 * ID: [a-zA-Z_][a-zA-Z_0-9]*;
 * WS: [ \t\n\r\f]+ -> skip ;
 * 
 */
pub struct Lexer {

}
