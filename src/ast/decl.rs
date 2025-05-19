use std::rc::Rc;

use subenum::subenum;

use crate::ast::stmt::Stmt;
use crate::ast::expr::Expr;
use crate::ast::expr_type::{ Type, FnType };

#[subenum(
    TopLevelDecl, 
    LocalDecl, 
    NamedDecl
)]
#[derive(Clone, Debug)]
pub enum Decl {
    #[subenum(TopLevelDecl, NamedDecl)]
    Fn(Rc<FnDecl>),
    
    #[subenum(TopLevelDecl, LocalDecl, NamedDecl)]
    Var(Rc<VarDecl>),

    #[subenum(NamedDecl)]
    Param(Rc<ParamDecl>),
}

impl Decl {
    pub fn ptr_eq(lhs: &Self, rhs: &Self) -> bool {
        match (lhs, rhs) {
            (Decl::Fn(a), Decl::Fn(b)) => Rc::ptr_eq(a, b),
            (Decl::Var(a), Decl::Var(b)) => Rc::ptr_eq(a, b),
            (Decl::Param(a), Decl::Param(b)) => Rc::ptr_eq(a, b),
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
    fn declarator(&self) -> &Declarator {
        match self {
            NamedDecl::Fn(fn_decl) => fn_decl.declarator(),
            NamedDecl::Var(var_decl) => var_decl.declarator(),
            NamedDecl::Param(param_decl) => param_decl.declarator(),
        }
    }
}

pub trait Named {
    fn declarator(&self) -> &Declarator;
    fn name(&self) -> &str {
        self.declarator().name.as_str()
    }
}

#[derive(Debug)]
pub struct FnDecl {
    declarator: Declarator,
    params: Vec<Rc<ParamDecl>>,
    body: Option<Vec<Stmt>>,
}

impl FnDecl {
    pub fn new(declarator: Declarator, params: Vec<Rc<ParamDecl>>) -> Self {
        FnDecl { 
            declarator,
            params, 
            body: None  
        }
    }

    pub fn new_with_body(declarator: Declarator, params: Vec<Rc<ParamDecl>>, body: Vec<Stmt>) -> Self {
        FnDecl { declarator, params, body: Some(body) }
    }

    pub fn params(&self) -> &Vec<Rc<ParamDecl>> {
        &self.params
    }

    pub fn ty(&self) -> &FnType {
        match &self.declarator.ty {
            Type::Fn(ty) => ty,
            _ => panic!("Expected function type"),
        }
    }

    pub fn has_body(&self) -> bool {
        self.body.is_some()
    }

    pub fn body(&self) -> &Option<Vec<Stmt>> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Option<Vec<Stmt>> {
        &mut self.body
    }
}

impl Named for FnDecl {
    fn declarator(&self) -> &Declarator {
        &self.declarator
    }
}

#[derive(Clone, Debug)]
pub struct Declarator {
    pub name: String,
    pub ty: Type,
}

impl Declarator {
    pub fn new(name: String, ty: Type) -> Self {
        Declarator { name, ty }
    }

    pub fn name(&self) -> &str {
        self.name.as_str()
    }

    pub fn ty(&self) -> &Type {
        &self.ty
    }
}

#[derive(Debug)]
pub struct ParamDecl {
    declarator: Declarator,
}

impl ParamDecl {
    pub fn new(declarator: Declarator) -> Self {
        ParamDecl { declarator }
    }

    pub fn ty(&self) -> &Type {
        &self.declarator.ty
    }
}

impl Named for ParamDecl {
    fn declarator(&self) -> &Declarator {
        &self.declarator
    }
}

#[derive(Debug)]
pub struct VarDecl {
    declarator: Declarator,
    initializer: Expr,
}

impl VarDecl {
    pub fn new(declarator: Declarator, initializer: Expr) -> Self {
        VarDecl { declarator, initializer  }
    }

    pub fn ty(&self) -> &Type {
        &self.declarator.ty
    }

    pub fn initializer(&self) -> &Expr {
        &self.initializer
    }

    pub fn initializer_mut(&mut self) -> &mut Expr {
        &mut self.initializer
    }
}

impl Named for VarDecl {
    fn declarator(&self) -> &Declarator {
        &self.declarator
    }
}
