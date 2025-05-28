use std::cmp::{ max, min };

use crate::lex::token::{ Token, TokenKind };
use crate::lex::keyword::KeywordMatcher;

pub struct Lexer<'s> {
    input: &'s str,
    pos: LexState<'s>,
    keyword_matcher: KeywordMatcher,
    eof: bool,
    errored: bool,
}

impl<'s> Lexer<'s> {
    pub fn new(input: &'s str) -> Self {
        Lexer {
            input,
            pos: LexState::new(input, 0),
            keyword_matcher: KeywordMatcher::new(),
            eof: false,
            errored: false,
        }
    }

    pub fn ok(&self) -> bool {
        !self.errored
    }

    fn handle_eof(&mut self) -> Option<Token<'s>> {
        if self.eof {
            eprintln!("Unexpected EOF");
            self.errored = true;
            None
        } 
        else {
            self.eof = true;
            Some(Token::new(TokenKind::EOF, ""))
        }
    }
    
    pub fn lex(&mut self) -> Option<Token<'s>> {
        if self.errored {
            return None;
        }

        // we make sure c is the first character of the next token
        // and pos is pointing to the next character after c
        // skips whitespace until a valid character or return None / EOF if EOF is reach
        let mut curr: char;
        let start: LexState<'s>;
        loop {
            // there is no character available, EOF reached
            let Some(c) = self.pos.get() else {
                return self.handle_eof();
            };

            curr = c;
            if !curr.is_whitespace() {
                start = self.pos.pre_inc();
                break;
            }

            // skips the current whitespace character
            self.pos.inc();
        }

        match curr {
            '"' => {
                let mut escaped = false;
                loop {
                    // the next character will be consumed anyways
                    // it is either a closing quote or a part of the string
                    if let Some(next) = self.pos.get_inc() {
                        if next == '\n' || next == '\r' {
                            eprintln!("unclosed string literal before line break");
                            self.errored = true;
                            return None;
                        }

                        if escaped {
                            // the next character is part of the string
                            // escape sequence validation is done in the parser
                            escaped = false;
                            continue;
                        }

                        if next == '\\' {
                            escaped = true;
                            continue;
                        }

                        if next == '"' {
                            return Some(start.form_token(&self.pos, TokenKind::STR));
                        }
                    } 
                    else {
                        // EOF reached before closing quote, this is an error
                        eprintln!("unclosed string literal before EOF");
                        self.errored = true;
                        return None;
                    }
                }
            }

            '0'..='9' => {
                loop {
                    if let Some(next) = self.pos.get() {
                        if next.is_digit(10) {
                            self.pos.inc();
                            continue;
                        }
                    }

                    // either the next character is not a digit => end of the number
                    // or EOF reached, return the last token of the file, this is not an error (lexically)
                    return Some(start.form_token(&self.pos, TokenKind::INT));
                }
            }


            'a'..='z' | 'A'..='Z' | '_' => {
                loop {
                    if let Some(next) = self.pos.get() {
                        if next.is_alphanumeric() || next == '_' {
                            self.pos.inc();
                            continue;
                        }
                    }

                    // same as above 
                    let s = start.form_str(&self.pos);
                    let kind = self.keyword_matcher.search_str(s)
                        .unwrap_or(TokenKind::ID);
                    
                    return Some(start.form_token(&self.pos, kind));
                }
            }

            '+' => Some(self.pos.form_token(&start, TokenKind::PLUS)),

            '-' => Some(self.pos.form_token(&start, TokenKind::MINUS)),

            '*' => Some(self.pos.form_token(&start, TokenKind::STAR)),

            '/' => Some(self.pos.form_token(&start, TokenKind::SLASH)),

            '%' => Some(self.pos.form_token(&start, TokenKind::PERCENT)),

            '&' => Some(self.pos.form_token(&start, TokenKind::AMP)),

            '=' => Some(self.pos.form_token(&start, TokenKind::EQ)),

            ',' => Some(self.pos.form_token(&start, TokenKind::COMMA)),

            ';' => Some(self.pos.form_token(&start, TokenKind::SEMI)),

            '(' => Some(self.pos.form_token(&start, TokenKind::LPAREN)),

            ')' => Some(self.pos.form_token(&start, TokenKind::RPAREN)),

            '{' => Some(self.pos.form_token(&start, TokenKind::LCURLY)),

            '}' => Some(self.pos.form_token(&start, TokenKind::RCURLY)),

            _ => {
                eprintln!("unrecognized character: {}", curr);
                self.errored = true;
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

        self.inc();

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
