use std::collections::hash_map::Entry;
use std::rc::Rc;
use std::collections::HashMap;
use std::ptr::null;

use crate::lex::cached_lexer::CachedLexer;
use crate::lex::token::{ Token, TokenKind };
use crate::ast::decl::{ Decl, FnDecl, Named, NamedDecl, TopLevelDecl, VarDecl };
use crate::ast::expr_type::Type;
use crate::ast::expr::{ Expr, ValueCategory };

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

    fn expect(&mut self, kind: TokenKind) -> Option<Token<'s>> {
        let tok = self.lexer.lex()?;
        if *tok.kind() == kind {
            Some(tok)
        } 
        else {
            eprintln!("Unexpected token: expected {:?}, found {:?}", kind, tok.kind());
            None
        }
    }

    pub fn parse_all(&mut self) -> Option<Vec<TopLevelDecl>> {
        let mut tracker = NameTracker::empty();
        let mut decls = Vec::new();
        while let Some(tok) = self.lexer.lex() {
            if *tok.kind() == TokenKind::EOF {
                break;
            }

            decls.push(self.parse_top_level_decl(&mut tracker)?);
        }

        None
    }

    fn parse_top_level_decl(&mut self, tracker: &mut NameTracker) -> Option<TopLevelDecl> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::LET => 
                Some(TopLevelDecl::Var(self.parse_var_decl(true, tracker)?)),

            TokenKind::FN => 
                Some(TopLevelDecl::Fn(self.parse_fn_decl()?)),

            _ => None,
        }
    }

    fn is_constinit(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Int(_) => true,
            _ => false,
        }
    }

    fn parse_var_decl(&mut self, top_lv: bool, tracker: &mut NameTracker) -> Option<Rc<VarDecl>> {
        self.expect(TokenKind::LET)?;
        let (name, decl_type) = self.parse_declarator(tracker)?;
        self.expect(TokenKind::EQ)?;
        let expr = self.parse_expr(tracker)?;
        self.expect(TokenKind::SEMI)?;

        if top_lv {
            if !self.is_constinit(&expr) {
                eprintln!("Top-level variable declarations must be initialized with a constant expression");
                return None;
            }
        }
        let decl = Rc::new(VarDecl::new(name.to_string(), expr));

        if tracker.add(&NamedDecl::Var(decl.clone())) {
            Some(decl)
        }
        else {
            eprintln!("Duplicate variable declaration: {}", name);
            None
        }
    }

    pub fn parse_expr(&mut self, tracker: &NameTracker) -> Option<Expr> {
        let tok = self.lexer.peek()?.clone();

        match tok.kind() {
            TokenKind::ID => {
                self.lexer.lex().unwrap();
                let id = tok.lexeme();

                if let Some(decl) = tracker.find(id) {
                    Some(Expr::DeclRef(decl.into()))
                }
                else {
                    None
                }
            }

            TokenKind::INT => {
                let int = self.expect(TokenKind::INT)?;
                Some(Expr::Int(int.lexeme().parse::<i32>().unwrap()))
            }

            TokenKind::STR => {
                let str_lit = self.expect(TokenKind::STR)?;
                Some(Expr::Str(str_lit.lexeme().to_string()))
            }

            _ => None,
        }
    }

    fn parse_declarator(&mut self, tracker: &NameTracker) -> Option<(&'s str, Type)> {
        let tok = self.expect(TokenKind::ID)?;
        // TODO: make this properly parse a type
        let decl_type = self.expect(TokenKind::I32)?;

        Some((tok.lexeme(), Type::I32))
    }

    fn parse_fn_decl(&mut self) -> Option<Rc<FnDecl>> {
        // Implementation of function declaration parsing
        unimplemented!()
    }
}

pub struct NameTracker {
    pub prev: *const Self,
    pub decls: HashMap<String, NamedDecl>,
}

impl NameTracker {
    pub fn empty() -> Self {
        Self { prev: null(), decls: HashMap::new() }
    }

    pub fn new(prev: &Self) -> Self {
        Self { prev: prev as *const Self, decls: HashMap::new() }
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
