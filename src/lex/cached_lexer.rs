use std::collections::VecDeque;

use crate::lex::lexer::Lexer;
use crate::lex::token::Token;

pub struct CachedLexer<'s> {
    lexer: Lexer<'s>,
    cache: VecDeque<Token<'s>>,
}

impl<'s> CachedLexer<'s> {
    pub fn new(input: &'s str) -> Self {
        CachedLexer {
            lexer: Lexer::new(input),
            cache: VecDeque::new(),
        }
    }

    pub fn ncached(&self) -> usize {
        self.cache.len()
    }

    pub fn ok(&self) -> bool {
        self.lexer.ok()
    }

    pub fn lex(&mut self) -> Option<Token<'s>> {
        if let Some(token) = self.cache.pop_front() {
            Some(token)
        }
        else {
            self.lexer.lex()
        }
    }

    // consumes n tokens and returns the last one
    pub fn eatn(&mut self, n: usize) -> Option<Token<'s>> {
        let mut ret = None;
        for _ in 0..n {
            ret = self.lex();
        }

        ret
    }

    pub fn peek(&mut self) -> Option<&Token<'s>> {
        if self.cache.is_empty() {
            if let Some(token) = self.lexer.lex() {
                self.cache.push_back(token);
            }
        }

        self.cache.front()
    }

    pub fn peekn(&mut self, n: usize) -> Option<&Token<'s>> {
        while self.cache.len() <= n {
            if let Some(token) = self.lexer.lex() {
                self.cache.push_back(token);
            }
            else {
                // if we go pass EOF, clear the cache and return None
                // since the underlying lexer is no longer in a valid state
                // i.e. self.lexer.ok() == false
                self.cache.clear();
                return None;
            }
        }

        self.cache.get(n)
    }
}
