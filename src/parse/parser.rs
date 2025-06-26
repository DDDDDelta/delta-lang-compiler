use std::rc::Rc;
use std::collections::HashMap;
use std::ptr::null_mut;

use crate::lex::cached_lexer::CachedLexer;
use crate::lex::token::{ BinaryOpKind, Token, TokenKind };
use crate::ast::decl::{ Decl, Declarator, FnDecl, LocalDecl, Named, NamedDecl, ParamDecl, TopLevelDecl, VarDecl };
use crate::ast::expr_type::{ FnType, PtrType, Type };
use crate::ast::expr::{ 
    AssignExpr, 
    BinaryExpr, 
    BinaryOp, 
    CallExpr, 
    Expr, 
    RValueCastExpr, 
    UnaryExpr, 
    UnaryOp, 
};
use crate::ast::stmt::{ ElseBranch, IfStmt, PrintStmt, ReturnStmt, Stmt, WhileStmt };
use crate::parse::literals::parse_string_literal;

pub struct Parser<'s> {
    lexer: CachedLexer<'s>,
    prec_table: BinOpPrec,
}

macro_rules! expr_peek {
    () => {
        crate::lex::token::TokenKind::ID | 
        crate::lex::token::TokenKind::INT | 
        crate::lex::token::TokenKind::STR | 
        crate::lex::token::TokenKind::LPAREN |
        crate::lex::token::TokenKind::MINUS |
        crate::lex::token::TokenKind::PLUS |
        crate::lex::token::TokenKind::AMP |
        crate::lex::token::TokenKind::STAR |
        crate::lex::token::TokenKind::TRUE |
        crate::lex::token::TokenKind::FALSE |
        crate::lex::token::TokenKind::BANG
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
            eprintln!("unexpected token: expected {:?}, found {:?}", kind, tok.kind());
            None
        }
    }

    pub fn parse_all(&mut self) -> Option<Vec<TopLevelDecl>> {
        let mut scope = Scope::empty();
        let mut decls = Vec::new();
        while let Some(tok) = self.lexer.peek() {
            if *tok.kind() == TokenKind::EOF {
                self.lexer.lex()?;
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

            TokenKind::EXTERN => 
                self.parse_extern_decl(scope),

            _ => {
                eprintln!("unexpected token {:?} when parsing top-level declaration", tok); 
                None 
            }
        }
    }

    pub fn parse_extern_decl(&mut self, scope: &mut Scope) -> Option<TopLevelDecl> {
        self.expect(TokenKind::EXTERN)?;
        match self.lexer.peek()?.kind() {
            TokenKind::FN => {
                let fn_decl = self.parse_fn_decl_sig(
                    scope, 
                    &mut Scope::empty(), 
                    TokenKind::SEMI
                )?;
                self.expect(TokenKind::SEMI)?;
                TopLevelDecl::Fn(fn_decl).into()
            }
            
            _ => {
                eprintln!("expected function declaration after extern, found {:?}", self.lexer.peek()?);
                None
            }
        }
    }

    fn is_constinit(&self, expr: &Expr) -> bool {
        match expr {
            Expr::Int(_) | Expr::Str(_) => true,
            _ => false,
        }
    }

    fn parse_var_decl(&mut self, top_lv: bool, scope: &mut Scope) -> Option<Rc<VarDecl>> {
        self.expect(TokenKind::LET)?;
        let declarator = self.parse_declarator()?;
        if matches!(*declarator.ty(), Type::Void | Type::Fn(_)) {
            eprintln!("variable cannot have void or function type");
            return None;
        }
        self.expect(TokenKind::EQ)?;
        let expr = self.parse_expr(scope)?;
        self.expect(TokenKind::SEMI)?;

        if top_lv {
            if !self.is_constinit(&expr) {
                eprintln!("top-level variable declarations must be initialized with a constant expression");
                return None;
            }
        }
        let decl = Rc::new(VarDecl::new(declarator.clone(), expr));

        if scope.add(&NamedDecl::Var(decl.clone())) {
            Some(decl)
        }
        else {
            eprintln!("duplicate variable declaration: {}", declarator.name());
            None
        }
    }

    pub fn parse_expr(&mut self, scope: &mut Scope) -> Option<Expr> {
        self.parse_assign_expr(scope)
    }

    fn parse_primary_expr(&mut self, scope: &mut Scope) -> Option<Expr> {
        let tok = self.lexer.peek()?.clone();

        match tok.kind() {
            TokenKind::ID => {
                self.lexer.lex().unwrap();
                let id = tok.lexeme();

                if let Some(decl) = scope.find(id) {
                    Some(Expr::DeclRef(decl.into()))
                }
                else {
                    eprintln!("unable to resolve reference to: {}", id);
                    None
                }
            }

            TokenKind::INT => {
                let int = self.expect(TokenKind::INT)?;
                Some(Expr::Int(int.lexeme().parse::<i32>().unwrap()))
            }

            TokenKind::STR => {
                let str_lit = self.expect(TokenKind::STR)?;
                Some(Expr::Str(parse_string_literal(str_lit.lexeme())?))
            }

            TokenKind::LPAREN => {
                self.lexer.lex()?;
                let expr = self.parse_expr(scope)?;
                self.expect(TokenKind::RPAREN)?;
                Some(expr)
            }

            TokenKind::TRUE => {
                self.lexer.lex()?;
                Some(Expr::Bool(true))
            }

            TokenKind::FALSE => {
                self.lexer.lex()?;
                Some(Expr::Bool(false))
            }

            _ => {
                eprintln!("unexpected token {:?} when parsing expression", tok);
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

    fn parse_assign_expr(&mut self, scope: &mut Scope) -> Option<Expr> {
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
                eprintln!("left-hand side of assignment must be an lvalue");
                None
            }
        }
    }

    fn parse_binary_expr(&mut self, prec: usize, scope: &mut Scope) -> Option<Expr> {
        let parse_next_prec: Box<dyn Fn(&mut Parser<'s>, &mut Scope) -> Option<Expr>> = 
            if prec == self.prec_table.max_prec() {
                Box::new(|this: &mut Self, scope: &mut Scope| { this.parse_unary_expr(scope) })
            } 
            else {
                Box::new(|this: &mut Self, scope: &mut Scope| { this.parse_binary_expr(prec + 1, scope) })
            };

        let mut lhs = parse_next_prec(self, scope)?;
        
        while let Ok(op_kind) = TryInto::<BinaryOpKind>::try_into(self.lexer.peek()?.kind().clone()) {
            let op_kind = op_kind.to_op();

            if !self.prec_table.ops_with_prec(prec).contains(&op_kind) {
                break;
            }

            self.lexer.lex()?;
            let rhs = parse_next_prec(self, scope)?;

            match (lhs.ty(), rhs.ty()) {
                (Type::Bool, Type::Bool) if matches!(op_kind, BinaryOp::LAnd | BinaryOp::LOr) => {},
                (Type::I32, Type::I32) if !matches!(op_kind, BinaryOp::LAnd | BinaryOp::LOr) => {}, 
                (Type::Ptr(_), Type::Ptr(_)) if op_kind == BinaryOp::Eq => {},

                (Type::Void, _) | (_, Type::Void) => {
                    eprintln!("Binary operator cannot be applied to void type");
                    return None;
                }
                
                _ => {
                    eprintln!("Invalid types for binary operation: {:?} {:?} {:?}", lhs.ty(), op_kind, rhs.ty());
                    return None;
                }
            }

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

    fn parse_unary_expr(&mut self, scope: &mut Scope) -> Option<Expr> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::MINUS | TokenKind::PLUS => {
                self.lexer.lex()?;
                let expr = self.parse_unary_expr(scope)?;
                if expr.ty() != Type::I32 {
                    eprintln!("Unary minus operator expects i32 type");
                    return None;
                }

                Some(Expr::Unary(UnaryExpr::new(
                    UnaryOp::Neg.into(), 
                    self.cast_ifn_rvalue(expr)
                ).into()))
            }

            TokenKind::STAR => {
                self.lexer.lex()?;
                let expr = self.parse_unary_expr(scope)?;
                if !expr.is_ptr() {
                    eprintln!("Dereferencing a non-pointer type");
                    return None;
                }
                
                Some(Expr::Unary(UnaryExpr::new(UnaryOp::Deref, self.cast_ifn_rvalue(expr)).into()))
            }

            TokenKind::AMP => {
                self.lexer.lex()?;
                let expr = self.parse_unary_expr(scope)?;
                if expr.is_lvalue() {
                    Some(Expr::Unary(UnaryExpr::new(UnaryOp::AddressOf, expr).into()))
                }
                else {
                    eprintln!("Address-of operator expects an lvalue");
                    None
                }
            }

            TokenKind::BANG => {
                self.lexer.lex()?;
                let expr = self.parse_unary_expr(scope)?;
                if expr.ty() != Type::Bool {
                    eprintln!("Logical NOT operator expects a boolean type");
                    return None;
                }

                Some(Expr::Unary(UnaryExpr::new(UnaryOp::Not, self.cast_ifn_rvalue(expr)).into()))
            }

            _ => self.parse_postfix_expr(scope),
        }
    }

    fn parse_postfix_expr(&mut self, scope: &mut Scope) -> Option<Expr> {
        let mut lhs = self.parse_primary_expr(scope)?;

        while let Some(tok) = self.lexer.peek() {
            if *tok.kind() == TokenKind::LPAREN {
                if !matches!(lhs.ty(), Type::Fn(_)) {
                    eprintln!("Cannot call non-function type");
                    return None;
                }

                let args = self.parse_optional_list_of(
                    &|this: &mut Self, scope: &mut Scope| { this.parse_expr(scope) }, 
                    TokenKind::COMMA, TokenKind::LPAREN, TokenKind::RPAREN, scope
                )?;

                let args = args.into_iter()
                    .map(|arg| self.cast_ifn_rvalue(arg))
                    .collect::<Vec<_>>();

                match lhs {
                    Expr::DeclRef(ref decl) => {
                        if let Decl::Fn(fn_decl) = decl.clone() {
                            if *fn_decl.ty().param_ty() == args.iter().map(|arg| arg.ty()).collect::<Vec<_>>() {
                                lhs = Expr::Call(Box::new(CallExpr::new(
                                    lhs, args
                                )));
                            }
                            else {
                                eprintln!("Function {} called with incorrect argument types", fn_decl.name());
                                return None;
                            }
                        }
                    }

                    _ => {
                        eprintln!("Currently only direct call is supported");
                        return None;
                    }
                }

            }
            else {
                return Some(lhs);
            }
        }

        None
    }

    fn parse_declarator(&mut self) -> Option<Declarator> {
        let tok = self.expect(TokenKind::ID)?;
        // TODO: validate the identifier in the current scope

        let ty = self.parse_type()?;

        Declarator::new(tok.lexeme().to_string(), ty.clone()).into()
    }

    fn parse_type(&mut self) -> Option<Type> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::I32 => {
                self.lexer.lex()?;
                Type::I32.into()
            }

            TokenKind::I8 => {
                self.lexer.lex()?;
                Type::I8.into()
            }

            TokenKind::BOOL => {
                self.lexer.lex()?;
                Type::Bool.into()
            }

            TokenKind::STAR => {
                self.lexer.lex()?;
                let ty = self.parse_type()?;
                if ty == Type::Void {
                    eprintln!("void pointer is not implemented (for now)");
                    return None;
                }
                Type::Ptr(Box::new(PtrType::new(ty))).into()
            }

            TokenKind::VOID => {
                self.lexer.lex()?;
                Type::Void.into()
            }

            _ => { 
                eprintln!("unexpected token {:?} when parsing type", tok);
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

    fn parse_fn_decl_sig(&mut self, s: &mut Scope, curr_scope: &mut Scope, lookahead: TokenKind) -> Option<Rc<FnDecl>> {
        self.expect(TokenKind::FN)?;
        let tok = self.expect(TokenKind::ID)?;
        let name = tok.lexeme();

        if s.find(name).is_some() {
            eprintln!("duplicate function declaration: {}", name);
            return None;
        }

        let param_action = 
            |this: &mut Self, scope: &mut Scope| { let _ = scope; this.parse_declarator() }; // maybe refactor this

        let params = self.parse_optional_list_of(
            &param_action, 
            TokenKind::COMMA, TokenKind::LPAREN, TokenKind::RPAREN,
            curr_scope
        )?;

        for param in &params {
            if *param.ty() == Type::Void {
                eprintln!("function parameter cannot have void type");
                return None;
            }
        }

        let ret_ty: Type;
        if *self.lexer.peek()?.kind() != lookahead {
            ret_ty = self.parse_type()?;
        }
        else {
            ret_ty = Type::Void;
        }

        let mut param_decls: Vec<Rc<ParamDecl>> = Vec::new();
        for param in &params {
            if param.name() == name {
                eprintln!("function parameter cannot have the same name as the function");
                return None;
            }

            if curr_scope.find_local(param.name()).is_none() {
                let decl = Rc::new(ParamDecl::new(param.clone()));

                param_decls.push(decl.clone());
                curr_scope.add(&NamedDecl::Param(decl.clone()));
            }
            else {
                eprintln!("duplicate parameter declaration: {}", param.name());
                return None;
            }
        }

        let ret = Rc::new(FnDecl::new(
            Declarator::new( 
                name.to_string(), 
                Type::Fn(Box::new(FnType::new(
                    params.into_iter().map(|d| d.ty().clone()).collect(), 
                    ret_ty.clone()
                ))) 
            ),
            param_decls
        ));
        s.add(&NamedDecl::Fn(ret.clone()));
        Some(ret)
    }

    pub fn parse_fn_decl(&mut self, s: &mut Scope) -> Option<Rc<FnDecl>> {
        let mut curr_scope = Scope::new(s); 
        let fn_decl = self.parse_fn_decl_sig(s, &mut curr_scope, TokenKind::LCURLY)?;
        curr_scope.set_associated_fn(fn_decl.clone());

        fn_decl.set_body(self.parse_block_stmt(&mut curr_scope)?);
        fn_decl.into()
    }

    pub fn parse_block_stmt(&mut self, scope: &mut Scope) -> Option<Vec<Stmt>> {
        self.expect(TokenKind::LCURLY)?;
        let mut stmts = Vec::new();

        while let Some(tok) = self.lexer.peek() {
            if *tok.kind() == TokenKind::RCURLY {
                break;
            }

            if let Some(stmt) = self.parse_stmt(scope) {
                stmts.push(stmt);
            }
        }
        // consume the closing curly brace
        // do not use self.expect here, since if lexer errored, it generates extra error messages
        // I forgot how this works, but I guess I will keep it. 6/7/2025
        self.lexer.lex()?;

        Some(stmts)
    }

    pub fn parse_if_stmt(&mut self, scope: &mut Scope) -> Option<IfStmt> {
        self.lexer.lex()?;
        let cond = self.parse_expr(scope)?;

        if cond.ty() != Type::Bool {
            eprintln!("if condition must be of type bool");
            return None;
        }

        let if_body = self.parse_block_stmt(&mut Scope::new(scope))?;

        if *self.lexer.peek()?.kind() != TokenKind::ELSE {
            return IfStmt::new(cond, if_body, ElseBranch::Nothing).into();
        }

        self.lexer.lex()?; // consume the ELSE token
        let else_body = 
            if *self.lexer.peek()?.kind() == TokenKind::IF {
                ElseBranch::ElseIf(Box::new(self.parse_if_stmt(scope)?))
            }
            else if *self.lexer.peek()?.kind() == TokenKind::LCURLY {
                let else_body_stmts = self.parse_block_stmt(&mut Scope::new(scope))?;
                ElseBranch::Else(else_body_stmts).into()
            }
            else {
                eprintln!("expected if or block statement after 'else'");
                return None;
            };

        IfStmt::new(cond, if_body, else_body).into()
    }

    pub fn parse_stmt(&mut self, scope: &mut Scope) -> Option<Stmt> {
        let tok = self.lexer.peek()?;
        match tok.kind() {
            TokenKind::LET => {
                Some(Stmt::LocalDecl(LocalDecl::Var(self.parse_var_decl(false, scope)?)))
            }

            TokenKind::RETURN => {
                self.lexer.lex()?;
                let ret = match self.lexer.peek()?.kind() {
                    expr_peek!() => {
                        let expr = self.parse_expr(scope)?;
                        self.expect(TokenKind::SEMI)?;
                        ReturnStmt::new(self.cast_ifn_rvalue(expr).into())
                    }

                    TokenKind::SEMI => {
                        self.lexer.lex()?;
                        ReturnStmt::new(None)
                    }

                    _ => {
                        eprintln!("unexpected token {:?} after return", self.lexer.lex()?);
                        return None;
                    }
                };

                let actual_ret = if let Some(expr) = ret.returned() {
                    expr.ty()
                }
                else {
                    Type::Void
                };

                let fn_decl = scope.associated_fn()
                    .expect("return statement outside of function scope");
                if actual_ret != *fn_decl.ty().ret_ty() {
                    eprintln!("function {} has inconsistent return type", fn_decl.name());
                    return None;
                }

                Stmt::Return(Box::new(ret)).into()
            }

            TokenKind::PRINT => {
                self.lexer.lex()?;
                let format = self.parse_expr(scope)?;
                
                if format.ty() != Type::Ptr(Box::new(PtrType::new(Type::I8))) {
                    eprintln!("print expects a string as first argument");
                    return None;
                }

                if *self.lexer.peek()?.kind() == TokenKind::SEMI {
                    self.lexer.lex()?;
                    return Some(Stmt::Print(PrintStmt::new(format, Vec::new()).into()));
                }

                self.expect(TokenKind::COMMA)?;

                let args = self.parse_list_of(
                    &|this: &mut Self, scope: &mut Scope| this.parse_expr(scope), 
                    TokenKind::COMMA, scope
                )?;

                let args = args.into_iter()
                    .map(|arg| self.cast_ifn_rvalue(arg))
                    .collect::<Vec<_>>();

                self.expect(TokenKind::SEMI)?;

                Some(Stmt::Print(PrintStmt::new(format, args).into()))
            }

            TokenKind::IF => {
                let if_stmt = self.parse_if_stmt(scope)?;
                Some(Stmt::If(Box::new(if_stmt)))
            }

            TokenKind::WHILE => {
                self.lexer.lex()?;
                let cond = self.parse_expr(scope)?;
                
                if cond.ty() != Type::Bool {
                    eprintln!("while condition must be of type bool");
                    return None;
                }

                let body = self.parse_block_stmt(&mut Scope::new(scope))?;
                Some(Stmt::While(Box::new(WhileStmt::new(cond, body))))
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
            vec![BinaryOp::LOr],
            vec![BinaryOp::LAnd],
            vec![BinaryOp::Eq, BinaryOp::NEq],
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
    associated_fn: Option<Rc<FnDecl>>,
}

impl Scope {
    pub fn empty() -> Self {
        Self { prev: null_mut(), decls: HashMap::new(), associated_fn: None }
    }

    pub fn new(prev: &mut Self) -> Self {
        Self { prev: prev as *mut Self, decls: HashMap::new(), associated_fn: prev.associated_fn.clone() }
    }

    pub fn set_associated_fn(&mut self, fn_decl: Rc<FnDecl>) {
        self.associated_fn = Some(fn_decl);
    }

    pub fn associated_fn(&self) -> Option<Rc<FnDecl>> {
        self.associated_fn.clone()
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
