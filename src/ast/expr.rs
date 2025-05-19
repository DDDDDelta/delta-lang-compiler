use std::rc::Rc;

use crate::ast::decl::Decl;
use crate::ast::expr_type::Type;

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Str(String),
    Call(Box<CallExpr>),
    RValueCast(Box<RValueCastExpr>),
    DeclRef(Decl),
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
}

impl Expr {
    pub fn value_category(&self) -> ValueCategory {
        match self {
            Expr::Int(_) | Expr::Str(_) | Expr::Call(_) | 
            Expr::RValueCast(_) | Expr::Binary(_)
                => ValueCategory::RValue,
                
            Expr::DeclRef(_) | Expr::Assign(_) => ValueCategory::LValue,
        }
    }

    pub fn is_lvalue(&self) -> bool {
        self.value_category().is_lvalue()
    }

    pub fn is_rvalue(&self) -> bool {
        self.value_category().is_rvalue()
    }

    pub fn ty(&self) -> Type {
        match self {
            Expr::Int(_) => Type::I32,
            Expr::Str(_) => Type::I32,
            Expr::Call(call) => Type::I32, // Placeholder, should be the return type of the function
            Expr::RValueCast(_) => Type::I32,
            Expr::DeclRef(decl) => {
                match decl {
                    Decl::Var(var_decl) => var_decl.ty().clone(),
                    Decl::Fn(fn_decl) => Type::Fn(Box::new(fn_decl.ty().clone())),
                    Decl::Param(param_decl) => param_decl.ty().clone(),
                }
            }
            Expr::Assign(_) => Type::I32,
            Expr::Binary(_) => Type::I32,
        }
    }
}

#[derive(Clone)]
pub enum ValueCategory {
    LValue,
    RValue,
}

impl ValueCategory {
    pub fn is_lvalue(&self) -> bool {
        matches!(self, ValueCategory::LValue)
    }

    pub fn is_rvalue(&self) -> bool {
        matches!(self, ValueCategory::RValue)
    }
}

#[derive(Debug)]
pub struct RValueCastExpr {
    operand: Expr,
}

impl RValueCastExpr {
    pub fn new(operand: Expr) -> Self {
        assert!(operand.is_lvalue());
        RValueCastExpr { operand }
    }

    pub fn operand(&self) -> &Expr {
        &self.operand
    }

    pub fn operand_mut(&mut self) -> &mut Expr {
        &mut self.operand
    }
}

#[derive(Debug)]
pub struct CallExpr {
    callee: String,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: String, args: Vec<Expr>) -> Self {
        assert!(args.iter().all(|arg| arg.is_rvalue()));
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

#[derive(Debug)]
pub struct AssignExpr {
    exprs: [Expr; 2],
}

impl AssignExpr {
    pub fn new(lhs: Expr, rhs: Expr) -> Self {  
        assert!(lhs.is_lvalue());
        assert!(rhs.is_rvalue());
        AssignExpr { exprs: [lhs, rhs] }
    }

    pub fn lhs(&self) -> &Expr {
        &self.exprs[0]
    }

    pub fn rhs(&self) -> &Expr {
        &self.exprs[1]
    }

    pub fn lhs_mut(&mut self) -> &mut Expr {
        &mut self.exprs[0]
    }

    pub fn rhs_mut(&mut self) -> &mut Expr {
        &mut self.exprs[1]
    }
}

#[derive(Debug)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}

#[derive(Debug)]
pub struct BinaryExpr {
    op: BinaryOp,
    exprs: [Expr; 2],
}

impl BinaryExpr {
    pub fn new(op: BinaryOp, lhs: Expr, rhs: Expr) -> Self {
        assert!(lhs.is_rvalue());
        assert!(rhs.is_rvalue());
        BinaryExpr { op, exprs: [lhs, rhs] }
    }

    pub fn op(&self) -> &BinaryOp {
        &self.op
    }

    pub fn lhs(&self) -> &Expr {
        &self.exprs[0]
    }

    pub fn rhs(&self) -> &Expr {
        &self.exprs[1]
    }

    pub fn lhs_mut(&mut self) -> &mut Expr {
        &mut self.exprs[0]
    }

    pub fn rhs_mut(&mut self) -> &mut Expr {
        &mut self.exprs[1]
    }
}
