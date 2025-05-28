pub mod token;
pub mod lexer;
mod keyword;
pub mod cached_lexer;

use crate::lex::lexer::Lexer;
use crate::lex::token::{ Token, TokenKind::{ self, * } };

pub const LEXER_RULES: &'static str = r#"
FN : 'fn';
LET : 'let';
VOID : 'void';
PRINT : 'print';
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

pub fn lex_all(src: &str) -> Vec<Token> {
    let mut lx = Lexer::new(src);
    let mut out = Vec::new();

    while let Some(tok) = lx.lex() {
        out.push(tok.clone());
        if *tok.kind() == EOF { break; }
    }
    out
}
