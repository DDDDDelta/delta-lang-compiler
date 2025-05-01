#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

use cpluspluswocaonima::lex::lexer::Lexer;
use cpluspluswocaonima::lex::token::TokenKind::{ self, * };
use cpluspluswocaonima::lex::token::Token;

/// Collect tokens until the first EOF **or** until the lexer
/// reports an error (`lex()` returns `None`).
fn collect(src: &str) -> Vec<Token> {
    let mut lx = Lexer::new(src);
    let mut out = Vec::new();

    while let Some(tok) = lx.lex() {
        out.push(tok.clone());
        if *tok.kind() == EOF { break; }
    }
    out
}

/* --- basic lexical categories --- */

#[test]
fn keywords_and_identifiers() {
    let got = collect("fn let i32 return foo _bar");
    let exp = vec![
        Token::new(FN,     "fn"),
        Token::new(LET,    "let"),
        Token::new(I32,    "i32"),
        Token::new(RETURN, "return"),
        Token::new(ID,     "foo"),
        Token::new(ID,     "_bar"),
        Token::new(EOF,    ""),
    ];
    assert_eq!(got, exp);
}

#[test]
fn numeric_and_string_literals() {
    let got = collect(r#"123 "abc""#);
    let exp = vec![
        Token::new(INT, "123"),
        Token::new(STR, r#""abc""#),
        Token::new(EOF, ""),
    ];
    assert_eq!(got, exp);
}

#[test]
fn operators_and_delimiters() {
    let got = collect("+-*/%=,;(){}");
    let exp = vec![
        Token::new(PLUS,   "+"),
        Token::new(MINUS,  "-"),
        Token::new(STAR,   "*"),
        Token::new(SLASH,  "/"),
        Token::new(AMP,    "%"),
        Token::new(EQ,     "="),
        Token::new(COMMA,  ","),
        Token::new(SEMI,   ";"),
        Token::new(LPAREN, "("),
        Token::new(RPAREN, ")"),
        Token::new(LCURLY, "{"),
        Token::new(RCURLY, "}"),
        Token::new(EOF,    ""),
    ];
    assert_eq!(got, exp);
}

#[test]
fn whitespace_is_ignored() {
    let got = collect(" \t let\nx\t=\r10 ");
    let exp = vec![
        Token::new(LET, "let"),
        Token::new(ID,  "x"),
        Token::new(EQ,  "="),
        Token::new(INT, "10"),
        Token::new(EOF, ""),
    ];
    assert_eq!(got, exp);
}

/* --- EOF / error behaviour --- */

#[test]
fn eof_is_reported_once() {
    let mut lx = Lexer::new("");
    assert_eq!(*lx.lex().unwrap().kind(), EOF);
    assert!(lx.lex().is_none(), "lexer must yield None after EOF");
}

#[test]
fn unknown_character_sets_error() {
    let mut lx = Lexer::new("@");
    assert!(lx.lex().is_none(),  "first call should error");
    assert!(!lx.ok(),            "error flag must be set");
    assert!(lx.lex().is_none(),  "subsequent calls still None");
}

/* --- extra corner cases --- */

#[test]
fn empty_string_literal() {
    assert_eq!(
        collect(r#""""#),
        vec![Token::new(STR, r#""""#), Token::new(EOF, "")],
    );
}

#[test]
fn identifier_with_leading_underscore() {
    assert_eq!(
        collect("_foo123 99"),
        vec![
            Token::new(ID,  "_foo123"),
            Token::new(INT, "99"),
            Token::new(EOF, ""),
        ],
    );
}

#[test]
fn consecutive_operators_without_spaces() {
    assert_eq!(
        collect("a+-*/b"),
        vec![
            Token::new(ID,   "a"),
            Token::new(PLUS, "+"),
            Token::new(MINUS,"-"),
            Token::new(STAR, "*"),
            Token::new(SLASH,"/"),
            Token::new(ID,   "b"),
            Token::new(EOF,  ""),
        ],
    );
}

#[test]
fn unterminated_string_sets_error() {
    let mut lx = Lexer::new(r#""abc"#);          // missing closing quote
    assert!(lx.lex().is_none());
    assert!(!lx.ok());
}

#[test]
fn leading_zero_and_pseudo_hex() {
    assert_eq!(
        collect("007 0x10"), // currently do not support hex literals
        vec![
            Token::new(INT, "007"),
            /* lexer splits “0x10” into INT '0' + ID 'x10' at present */
            Token::new(INT, "0"),
            Token::new(ID,  "x10"),
            Token::new(EOF, ""),
        ],
    );
}
