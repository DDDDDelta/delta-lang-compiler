use crate::ast::expr::Expr;
use crate::ast::decl::LocalDecl;

// consider this a union of smart pointers
#[derive(Debug)]
pub enum Stmt {
    Expr(Expr),
    Return(Box<ReturnStmt>),
    LocalDecl(LocalDecl),
    Print(Box<PrintStmt>),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
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

#[derive(Debug)]
pub enum ElseBranch {
    Else(Vec<Stmt>),
    ElseIf(Box<IfStmt>),
    Nothing,
}

#[derive(Debug)]
pub struct IfStmt {
    condition: Expr,
    then_branch: Vec<Stmt>,
    else_branch: ElseBranch,
}

impl IfStmt {
    pub fn new(condition: Expr, then_branch: Vec<Stmt>, else_branch: ElseBranch) -> Self {
        IfStmt { condition, then_branch, else_branch }
    }

    pub fn cond(&self) -> &Expr {
        &self.condition
    }

    pub fn then(&self) -> &Vec<Stmt> {
        &self.then_branch
    }

    pub fn elze(&self) -> &ElseBranch {
        &self.else_branch
    }
}

#[derive(Debug)]
pub struct WhileStmt {
    condition: Expr,
    body: Vec<Stmt>,
}

impl WhileStmt {
    pub fn new(condition: Expr, body: Vec<Stmt>) -> Self {
        WhileStmt { condition, body }
    }

    pub fn cond(&self) -> &Expr {
        &self.condition
    }

    pub fn body(&self) -> &Vec<Stmt> {
        &self.body
    }

    pub fn body_mut(&mut self) -> &mut Vec<Stmt> {
        &mut self.body
    }
}
