pub mod token;
pub mod lexer;
mod keyword;
pub mod cached_lexer;
mod rules;

use crate::lex::lexer::Lexer;
use crate::lex::token::{ Token, TokenKind::{ self, * } };

pub use crate::lex::rules::LEXER_RULES;

pub fn lex_all(src: &str) -> Vec<Token> {
    let mut lx = Lexer::new(src);
    let mut out = Vec::new();

    while let Some(tok) = lx.lex() {
        out.push(tok.clone());
        if *tok.kind() == EOF { break; }
    }
    out
}
