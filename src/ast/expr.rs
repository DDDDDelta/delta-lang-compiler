use std::rc::Rc;

use crate::ast::decl::Decl;

pub enum Expr {
    Int(i32),
    Str(String),
    Call(Box<CallExpr>),
    DeclRef(Decl),
}

pub struct CallExpr {
    callee: String,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: String, args: Vec<Expr>) -> Self {
        CallExpr { callee, args }
    }

    pub fn callee(&self) -> &str {
        &self.callee
    }

    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }

    pub fn args_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.args
    }
}
