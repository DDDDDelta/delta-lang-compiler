use subenum::subenum;

use crate::ast::stmt::Stmt;
use crate::ast::expr::Expr;

#[subenum(TopLevelDecl, LocalDecl)]
pub enum Decl {
    #[subenum(TopLevelDecl)]
    Fn(Box<FnDecl>),
    
    #[subenum(TopLevelDecl, LocalDecl)]
    Var(Box<VarDecl>),
}


pub trait NamedDecl {
    fn name(&self) -> &str;
}

pub struct FnDecl {
    name: String,
    body: Vec<Stmt>,
}

impl FnDecl {
    pub fn new(name: String, body: Vec<Stmt>) -> Self {
        FnDecl { name, body }
    }

    pub fn body(&self) -> &Vec<Stmt> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.body
    }
}

impl NamedDecl for FnDecl {
    fn name(&self) -> &str {
        &self.name
    }
}

pub struct VarDecl {
    name: String,
    initializer: Expr,
}

impl VarDecl {
    pub fn new(name: String, initializer: Expr) -> Self {
        VarDecl { name, initializer }
    }

    pub fn initializer(&self) -> &Expr {
        &self.initializer
    }

    pub fn initializer_mut(&mut self) -> &mut Expr {
        &mut self.initializer
    }
}

impl NamedDecl for VarDecl {
    fn name(&self) -> &str {
        &self.name
    }
}
