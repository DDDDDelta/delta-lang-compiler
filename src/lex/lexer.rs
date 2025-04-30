use std::cmp::{ max, min };

use crate::lex::token::{ Token, TokenKind };
use crate::lex::keyword::KeywordMatcher;

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
pub struct Lexer<'s> {
    input: &'s str,
    pos: LexState<'s>,
    keyword_matcher: KeywordMatcher,
    eof: bool,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Lexer {
            input,
            pos: LexState::new(input, 0),
            keyword_matcher: KeywordMatcher::new(),
            eof: false,
        }
    }

    fn handle_eof(&mut self) -> Option<Token<'s>> {
        if self.eof {
            eprintln!("Unexpected EOF");
            None
        } 
        else {
            self.eof = true;
            Some(Token::new(TokenKind::EOF, ""))
        }
    }
    
    pub fn lex(&mut self) -> Option<Token<'s>> {
        // we make sure c is the first character of the next token
        // skips whitespace until a valid character or return None / EOF if EOF is reach
        let mut c: char;
        let start: LexState<'s>;
        loop {
            let o = self.pos.get();
            if o.is_none() {
                return self.handle_eof();
            }

            c = o.unwrap();
            if !c.is_whitespace() {
                start = self.pos.pre_inc();
                break;
            }
        }

        match c {
            '"' => {
                loop {
                    if let Some(curr) = self.pos.get_inc() {
                        if curr == '"' {
                            return Some(start.form_token(&self.pos, TokenKind::STR));
                        }
                    } 
                    else {
                        return self.handle_eof();
                    }
                }
            }

            '0'..='9' => {
                loop {
                    if let Some(curr) = self.pos.get_inc() {
                        if !curr.is_digit(10) {
                            return Some(start.form_token(&self.pos, TokenKind::INT));
                        }
                    } 
                    else {
                        return self.handle_eof();
                    }
                }
            }


            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    if let Some(curr) = self.pos.get_inc() {
                        if !curr.is_alphanumeric() && curr != '_' {
                            
                            let s = start.form_str(&self.pos);
                            let kind = self.keyword_matcher.search_str(s)
                                .unwrap_or(TokenKind::ID);
                            
                            return Some(start.form_token(&self.pos, kind));
                        }
                    } 
                    else {
                        return self.handle_eof();
                    }
                }
            }

            '+' => Some(self.pos.form_token(&start, TokenKind::PLUS)),

            '-' => Some(self.pos.form_token(&start, TokenKind::MINUS)),

            '*' => Some(self.pos.form_token(&start, TokenKind::STAR)),

            '/' => Some(self.pos.form_token(&start, TokenKind::SLASH)),

            '%' => Some(self.pos.form_token(&start, TokenKind::AMP)),

            '=' => Some(self.pos.form_token(&start, TokenKind::EQ)),

            ',' => Some(self.pos.form_token(&start, TokenKind::COMMA)),

            ';' => Some(self.pos.form_token(&start, TokenKind::SEMI)),

            '(' => Some(self.pos.form_token(&start, TokenKind::LPAREN)),

            ')' => Some(self.pos.form_token(&start, TokenKind::RPAREN)),

            '{' => Some(self.pos.form_token(&start, TokenKind::LCURLY)),

            '}' => Some(self.pos.form_token(&start, TokenKind::RCURLY)),

            _ => {
                eprintln!("unrecognized character: {}", c);
                None
            }
        }
    }
}

#[derive(Copy, Clone)]
struct LexState<'s> {
    input: &'s str,
    current: usize, 
}

impl<'s> PartialEq for LexState<'s> {
    fn eq(&self, other: &Self) -> bool {
        self.current == other.current
    }
}

impl<'s> Eq for LexState<'s> {}

impl<'s> PartialOrd for LexState<'s> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.current.partial_cmp(&other.current)
    }
}

impl<'s> Ord for LexState<'s> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.current.cmp(&other.current)
    }
}

impl<'s> LexState<'s> {
    fn new(input: &'s str, pos: usize) -> Self {
        LexState { input, current: pos }
    }

    fn valid(&self) -> bool {
        self.current < self.input.len()
    }
    

    fn getn(&self, n: usize) -> Option<char> {
        self.input[self.current..]
            .chars()
            .nth(n)
    }

    fn get(&self) -> Option<char> {
        self.getn(0)
    }

    fn pre_inc(&mut self) -> Self {
        let pre = self.clone();

        if let Some(c) = self.get() {
            self.current += c.len_utf8();
        }

        pre
    }

    fn inc(&mut self) -> &mut Self {
        if let Some(c) = self.get() {
            self.current += c.len_utf8();
        }

        self
    }

    fn get_inc(&mut self) -> Option<char> {
        let ret = self.get();
        self.inc();
        ret
    }

    fn form_str(&self, other: &Self) -> &'s str {
        let left = min(self.current, other.current);
        let right = max(self.current, other.current);

        &self.input[left..right]
    }

    fn form_token(&self, other: &Self, kind: TokenKind) -> Token<'s> {
        Token::new(kind, self.form_str(other))
    }
}
