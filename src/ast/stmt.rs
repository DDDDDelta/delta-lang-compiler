use crate::ast::expr::Expr;
use crate::ast::decl::LocalDecl;

// consider this a union of smart pointers
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Return(Box<ReturnStmt>),
    LocalDecl(LocalDecl),
    Print(Box<PrintStmt>),
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct PrintStmt {
    format: Expr,
    args: Vec<Expr>,
}

impl PrintStmt {
    pub fn new(format: Expr, args: Vec<Expr>) -> Self {
        PrintStmt { format, args }
    }

    pub fn format(&self) -> &Expr {
        &self.format
    }

    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }

    pub fn args_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.args
    }
}
