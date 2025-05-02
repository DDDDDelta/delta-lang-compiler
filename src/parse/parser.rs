use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::ptr::null;

use crate::lex::cached_lexer::CachedLexer;
use crate::lex::token::{ Token, TokenKind };
use crate::ast::decl::{ Decl, FnDecl, Named, NamedDecl, TopLevelDecl, VarDecl };

/* 
 * *** Parser Rule ***
 * 
 * program
 *     : decl* EOF
 *     ;
 * 
 * decl: var_decl
 *     | fn_decl
 *     ;
 * 
 * type: I32
 *     ;
 * 
 * declarator
 *     : ID type
 *     ;
 * 
 * declarator_list
 *     : (declarator (COMMA declarator)*)?
 *     ;
 *     
 * var_decl
 *     : LET declarator EQ expr SEMI
 *     ;
 * 
 * fn_decl
 *     : FN ID LPAREN declarator_list RPAREN LBRACE
 *     stmt*
 *     RBRACE
 *     ;
 *     
 * stmt: var_decl
 *     | expr SEMI
 *     | return_stmt
 *     ;
 *     
 * return_stmt
 *     : RETURN expr SEMI
 *     ;
 * 
 * expr: assign_expr
 *     ;
 *     
 * expr_list
 *     : (expr (COMMA expr)*)?
 *     ;
 *     
 * primary_expr
 *     : ID
 *     | INT
 *     | STR
 *     | paren_expr
 *     ;
 *     
 * paren_expr
 *     : LPAREN expr RPAREN
 *     ;
 *     
 * postfix_expr
 *     : primary_expr (LPAREN expr_list RPAREN)*
 *     ;
 *     
 * mul_expr
 *     : mul_expr (STAR | SLASH | AMP) postfix_expr
 *     | postfix_expr
 *     ;
 *     
 * add_expr
 *     : add_expr (PLUS | MINUS) mul_expr
 *     | mul_expr
 *     ;
 *     
 * assign_expr
 *     : add_expr EQ assign_expr
 *     | add_expr
 *     ;
 */
pub struct Parser<'s> {
    lexer: CachedLexer<'s>,
}

impl<'s> Parser<'s> {
    pub fn new(lexer: CachedLexer<'s>) -> Self {
        Self { lexer }
    }

    pub fn parse_tu(&mut self) -> Option<Vec<TopLevelDecl>> {
        let mut decls = Vec::new();
        while let Some(tok) = self.lexer.lex() {
            if *tok.kind() == TokenKind::EOF {
                break;
            }

            decls.push(self.parse_top_level_decl()?);
        }

        None
    }

    fn parse_top_level_decl(&mut self) -> Option<TopLevelDecl> {
        unimplemented!()
    }

    fn parse_var_decl(&mut self) -> Option<VarDecl> {
        // Implementation of variable declaration parsing
        unimplemented!()
    }

    fn parse_fn_decl(&mut self) -> Option<FnDecl> {
        // Implementation of function declaration parsing
        unimplemented!()
    }
}

struct NameTracker {
    pub prev: *const Self,
    pub decls: HashMap<String, NamedDecl>,
}

impl NameTracker {
    pub fn new(prev: *const Self) -> Self {
        Self { prev, decls: HashMap::new() }
    }

    pub fn add(&mut self, decl: &NamedDecl) -> bool {
        let mut res = false;

        self.decls.entry(decl.name().to_string())
            .or_insert_with(|| { res = true; decl.clone() });

        res
    }

    pub fn find(&self, name: &str) -> Option<NamedDecl> {
        if let Some(d) = self.decls.get(name) {
            return Some(d.clone());
        }

        if self.prev != null() {
            unsafe { &*self.prev } .find(name)
        }
        else {
            None
        }
    }
}
