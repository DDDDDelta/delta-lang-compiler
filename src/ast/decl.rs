use std::rc::Rc;

use subenum::subenum;

use crate::ast::stmt::Stmt;
use crate::ast::expr::Expr;


#[subenum(
    TopLevelDecl, 
    LocalDecl, 
    NamedDecl
)]
#[derive(Clone)]
pub enum Decl {
    #[subenum(TopLevelDecl, NamedDecl)]
    Fn(Rc<FnDecl>),
    
    #[subenum(TopLevelDecl, LocalDecl, NamedDecl)]
    Var(Rc<VarDecl>),
}


impl Decl {
    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (Decl::Fn(a), Decl::Fn(b)) => Rc::ptr_eq(a, b),
            (Decl::Var(a), Decl::Var(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl LocalDecl {
    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (LocalDecl::Var(a), LocalDecl::Var(b)) => Rc::ptr_eq(a, b),
            _ => false,
        }
    }
}

impl Named for NamedDecl {
    fn name(&self) -> &str {
        match self {
            NamedDecl::Fn(decl) => decl.name(),
            NamedDecl::Var(decl) => decl.name(),
        }
    }
}

pub trait Named {
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

impl Named for FnDecl {
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

impl Named for VarDecl {
    fn name(&self) -> &str {
        &self.name
    }
}
