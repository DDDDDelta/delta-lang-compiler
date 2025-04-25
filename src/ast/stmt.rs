use crate::ast::expr::Expr;

use crate::ast::decl::LocalDecl;

pub enum Stmt {
    Expr(Expr),
    Return(Box<ReturnStmt>),
    LocalDecl(LocalDecl),
}

pub struct ReturnStmt {
    returned: Option<Expr>,
}

impl ReturnStmt {
    pub fn new(returned: Option<Expr>) -> Self {
        ReturnStmt { returned }
    }

    pub fn returned(&self) -> &Option<Expr> {
        &self.returned
    }

    pub fn returned_as_mut(&mut self) -> Option<&mut Expr> {
        self.returned.as_mut()
    }
}
