use std::collections::hash_map::Entry;
use std::rc::Rc;
use std::collections::HashMap;
use std::ptr::{ null, null_mut };

use crate::lex::cached_lexer::CachedLexer;
use crate::lex::token::{ BinaryOpKind, Token, TokenKind };
use crate::ast::decl::{ Decl, Declarator, FnDecl, LocalDecl, Named, NamedDecl, ParamDecl, TopLevelDecl, VarDecl };
use crate::ast::expr_type::{FnType, Type};
use crate::ast::expr::{ AssignExpr, BinaryExpr, BinaryOp, Expr, RValueCastExpr, ValueCategory };
use crate::ast::stmt::{ Stmt, ReturnStmt };

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
    prec_table: BinOpPrec,
}

macro_rules! expr_peek {
    () => {
        crate::lex::token::TokenKind::ID | 
        crate::lex::token::TokenKind::INT | 
        crate::lex::token::TokenKind::STR | 
        crate::lex::token::TokenKind::LPAREN
    };
}

impl<'s> Parser<'s> {
    pub fn new(lexer: CachedLexer<'s>) -> Self {
        Self { lexer, prec_table: BinOpPrec::new() }
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
        let mut scope = Scope::empty();
        let mut decls = Vec::new();
        while let Some(tok) = self.lexer.lex() {
            if *tok.kind() == TokenKind::EOF {
                break;
            }

            decls.push(self.parse_top_level_decl(&mut scope)?);
        }

        Some(decls)
    }

    pub fn parse_top_level_decl(&mut self, scope: &mut Scope) -> Option<TopLevelDecl> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::LET => 
                Some(TopLevelDecl::Var(self.parse_var_decl(true, scope)?)),

            TokenKind::FN => 
                Some(TopLevelDecl::Fn(self.parse_fn_decl(scope)?)),            

            _ => {
                eprintln!("Unexpected token {:?} when parsing top-level declaration", tok); 
                None 
            }
        }
    }

    fn is_constinit(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Int(_) => true,
            _ => false,
        }
    }

    fn parse_var_decl(&mut self, top_lv: bool, scope: &mut Scope) -> Option<Rc<VarDecl>> {
        self.expect(TokenKind::LET)?;
        let declarator = self.parse_declarator(scope)?;
        self.expect(TokenKind::EQ)?;
        let expr = self.parse_expr(scope)?;
        self.expect(TokenKind::SEMI)?;

        if top_lv {
            if !self.is_constinit(&expr) {
                eprintln!("Top-level variable declarations must be initialized with a constant expression");
                return None;
            }
        }
        let decl = Rc::new(VarDecl::new(declarator.clone(), expr));

        if scope.add(&NamedDecl::Var(decl.clone())) {
            Some(decl)
        }
        else {
            eprintln!("Duplicate variable declaration: {}", declarator.name);
            None
        }
    }

    pub fn parse_expr(&mut self, scope: &Scope) -> Option<Expr> {
        self.parse_assign_expr(scope)
    }

    fn parse_primary_expr(&mut self, scope: &Scope) -> Option<Expr> {
        let tok = self.lexer.peek()?.clone();

        match tok.kind() {
            TokenKind::ID => {
                self.lexer.lex().unwrap();
                let id = tok.lexeme();

                if let Some(decl) = scope.find(id) {
                    Some(Expr::DeclRef(decl.into()))
                }
                else {
                    eprintln!("Unable to resolve reference to: {}", id);
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

            TokenKind::LPAREN => {
                self.lexer.lex()?;
                let expr = self.parse_expr(scope)?;
                self.expect(TokenKind::RPAREN)?;
                Some(expr)
            }

            _ => {
                eprintln!("Unexpected token {:?} when parsing expression", tok);
                None
            }
        }
    }
    
    fn cast_ifn_rvalue(&mut self, expr: Expr) -> Expr {
        if expr.is_lvalue() {
            Expr::RValueCast(Box::new(RValueCastExpr::new(expr)))
        }
        else {
            expr
        }
    }

    fn parse_assign_expr(&mut self, scope: &Scope) -> Option<Expr> {
        let lhs = self.parse_binary_expr(0, scope)?;

        if *self.lexer.peek()?.kind() != TokenKind::EQ {
            Some(lhs)
        }
        else {
            self.lexer.lex()?;
            let rhs = self.parse_assign_expr(scope)?;

            if lhs.is_lvalue() {
                Some(Expr::Assign(Box::new(AssignExpr::new(
                    lhs, self.cast_ifn_rvalue(rhs)
                ))))
            }
            else {
                eprintln!("Left-hand side of assignment must be an lvalue");
                None
            }
        }
    }

    fn parse_binary_expr(&mut self, prec: usize, scope: &Scope) -> Option<Expr> {
        let parse_next_prec: Box<dyn Fn(&mut Parser<'s>, &Scope) -> Option<Expr>> = 
            if prec == self.prec_table.max_prec() {
                Box::new(|this: &mut Self, scope: &Scope| { this.parse_primary_expr(scope) })
            } 
            else {
                Box::new(|this: &mut Self, scope: &Scope| { this.parse_binary_expr(prec + 1, scope) })
            };

        let mut lhs = parse_next_prec(self, scope)?;
        
        while let Ok(op_kind) = TryInto::<BinaryOpKind>::try_into(self.lexer.peek()?.kind().clone()) {
            if !self.prec_table.ops_with_prec(prec).contains(&op_kind.into()) {
                break;
            }

            self.lexer.lex()?;
            let rhs = parse_next_prec(self, scope)?;

            lhs = Expr::Binary(Box::new(
                BinaryExpr::new(
                    op_kind.into(), 
                    // binary expressions expect both sides to be rvalues
                    self.cast_ifn_rvalue(lhs), 
                    self.cast_ifn_rvalue(rhs)
                )
            ));
        }

        Some(lhs)
    }

    fn parse_declarator(&mut self, tracker: &mut Scope) -> Option<Declarator> {
        let tok = self.expect(TokenKind::ID)?;
        // TODO: validate the identifier in the current scope

        let ty = self.parse_type()?;

        Declarator::new(tok.lexeme().to_string(), ty.clone()).into()
    }

    // TODO: make this properly parse a type
    fn parse_type(&mut self) -> Option<Type> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::I32 => {
                self.lexer.lex()?;
                Type::I32.into()
            }

            _ => { 
                eprintln!("Unexpected token {:?} when parsing type", tok);
                None
            }
        }
    }

    fn parse_list_of<F, T>(
        &mut self, parse_action: &F, 
        delim: TokenKind, scope: &mut Scope
    ) -> Option<Vec<T>> 
    where 
        F: Fn(&mut Self, &mut Scope) -> Option<T>,
    {
        let mut list = Vec::new();
        loop {
            list.push(parse_action(self, scope)?);

            let d = self.lexer.peek()?;
            if *d.kind() == delim {
                self.lexer.lex()?;
            }
            else {
                break;
            }
        }

        list.into()
    }

    fn parse_optional_list_of<F, T>(
        &mut self, parse_action: &F, delim: TokenKind,
        start: TokenKind, end: TokenKind, scope: &mut Scope
    ) -> Option<Vec<T>> 
    where 
        F: Fn(&mut Self, &mut Scope) -> Option<T>,
    {
        self.expect(start)?;
        
        if let Some(tok) = self.lexer.peek() {
            if *tok.kind() == end {
                self.lexer.lex()?;
                return Some(Vec::new());
            }
        }

        let ret = self.parse_list_of(parse_action, delim, scope)?;

        self.expect(end)?;

        ret.into()
    }

    pub fn parse_fn_decl(&mut self, s: &mut Scope) -> Option<Rc<FnDecl>> {
        self.expect(TokenKind::FN)?;
        let tok = self.expect(TokenKind::ID)?;
        let name = tok.lexeme();

        if s.find(name).is_some() {
            eprintln!("Duplicate function declaration: {}", name);
            return None;
        }

        let param_action = 
            |this: &mut Self, scope: &mut Scope| { this.parse_declarator(scope) }; // maybe refactor this

        let mut curr_scope = Scope::new(s); 

        let params = self.parse_optional_list_of(
            &param_action, 
            TokenKind::COMMA, TokenKind::LPAREN, TokenKind::RPAREN,
            &mut curr_scope
        )?;

        let ret_ty: Type;
        if self.lexer.peek()?.kind() != &TokenKind::LCURLY {
            ret_ty = self.parse_type()?;
        }
        else {
            ret_ty = Type::I32;
        }

        let mut param_decls: Vec<Rc<ParamDecl>> = Vec::new();
        for param in &params {
            if param.name == name {
                eprintln!("Function parameter cannot have the same name as the function");
                return None;
            }

            if curr_scope.find_local(&param.name).is_none() {
                let decl = Rc::new(ParamDecl::new(param.clone()));

                param_decls.push(decl.clone());
                curr_scope.add(&NamedDecl::Param(decl.clone()));
            }
            else {
                eprintln!("Duplicate parameter declaration: {}", param.name);
                return None;
            }
        }

        let mut fn_decl = FnDecl::new(
            Declarator { 
                name: name.to_string(), 
                ty: Type::Fn(Box::new(FnType::new(
                    params.into_iter().map(|d| d.ty).collect(), 
                    ret_ty.clone()
                ))) 
            },
            param_decls
        );

        self.expect(TokenKind::LCURLY)?;
        let mut body = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            if *tok.kind() == TokenKind::RCURLY {
                break;
            }

            let stmt = self.parse_stmt(&mut curr_scope)?;
            if let Stmt::Return(ret) = &stmt {
                let actual_ret: Type;
                if let Some(expr) = ret.returned() {
                    actual_ret = expr.ty();
                }
                else {
                    actual_ret = Type::I32;
                }

                if actual_ret != ret_ty {
                    eprintln!("Function {} has inconsistent return type", fn_decl.name());
                    return None;
                }
            }

            body.push(stmt);
        }
        // consume the closing curly brace
        // do not use self.expect here, since if lexer errored, it generates extra error messages
        self.lexer.lex()?;

        *fn_decl.body_mut() = Some(body);
        Rc::new(fn_decl).into()
    }

    pub fn parse_stmt(&mut self, scope: &mut Scope) -> Option<Stmt> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::LET => {
                Some(Stmt::LocalDecl(LocalDecl::Var(self.parse_var_decl(false, scope)?)))
            }

            TokenKind::RETURN => {
                self.lexer.lex()?;
                match self.lexer.peek()?.kind() {
                    expr_peek!() => {
                        let expr = self.parse_expr(scope)?;
                        self.expect(TokenKind::SEMI)?;
                        Some(Stmt::Return(ReturnStmt::new(expr.into()).into()))
                    }

                    TokenKind::SEMI => {
                        self.lexer.lex()?;
                        Some(Stmt::Return(ReturnStmt::new(None).into()))
                    }

                    _ => {
                        eprintln!("unexpected token {:?} after return", self.lexer.lex()?);
                        None
                    }
                }
            }

            expr_peek!() => {
                let expr = self.parse_expr(scope)?;
                self.expect(TokenKind::SEMI)?;
                Some(Stmt::Expr(expr))
            }

            _ => {
                eprintln!("unexpected token {:?}", self.lexer.lex()?);
                None
            }
        }
    }
}

struct BinOpPrec {
    prec_table: Vec<Vec<BinaryOp>>,
}

impl BinOpPrec {
    pub fn new() -> Self {
        let table = vec![
            vec![BinaryOp::Add, BinaryOp::Sub],
            vec![BinaryOp::Mul, BinaryOp::Div, BinaryOp::Mod],
        ];

        Self { prec_table: table }
    }

    pub fn ops_with_prec(&self, prec: usize) -> &Vec<BinaryOp> {
        &self.prec_table[prec as usize]
    }

    pub fn max_prec(&self) -> usize {
        self.prec_table.len() - 1
    }
}

#[derive(Clone)]
pub struct Scope {
    prev: *mut Self,
    decls: HashMap<String, NamedDecl>,
}

impl Scope {
    pub fn empty() -> Self {
        Self { prev: null_mut(), decls: HashMap::new() }
    }

    pub fn new(prev: &mut Self) -> Self {
        Self { prev: prev as *mut Self, decls: HashMap::new() }
    }

    pub fn add(&mut self, decl: &NamedDecl) -> bool {
        let mut res = false;

        self.decls.entry(decl.name().to_string())
            .or_insert_with(|| { res = true; decl.clone() });

        res
    }

    pub fn find_local(&self, name: &str) -> Option<NamedDecl> {
        if let Some(d) = self.decls.get(name) {
            return Some(d.clone());
        }
        None
    }

    pub fn find_local_mut(&mut self, name: &str) -> Option<&mut NamedDecl> {
        if let Some(d) = self.decls.get_mut(name) {
            return Some(d);
        }
        None
    }

    pub fn find(&self, name: &str) -> Option<NamedDecl> {
        if let Some(d) = self.find_local(name) {
            return Some(d.clone());
        }

        if self.prev != null_mut() {
            unsafe { &*self.prev } .find(name)
        }
        else {
            None
        }
    }

    pub fn find_mut(&mut self, name: &str) -> Option<&mut NamedDecl> {
        let new = self.clone();
        
        if let Some(d) = self.find_local_mut(name) {
            return Some(d);
        }

        if new.prev != null_mut() {
            unsafe { &mut *(new.prev as *mut Self) } .find_mut(name)
        }
        else {
            None
        }
    }
}
