use crate::ast::expr::Expr;

pub enum Stmt {
    Expr(Expr),
    Return(Box<ReturnStmt>),
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
