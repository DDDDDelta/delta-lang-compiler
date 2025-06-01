use std::rc::Rc;

use crate::ast::decl::Decl;
use crate::ast::expr_type::{ Type, PtrType };
use crate::lex::token::{TokenKind, UnaryOpKind};

use super::decl::{Named, NamedDecl};

#[derive(Debug)]
pub enum Expr {
    Int(i32),
    Str(String),
    Bool(bool),
    Call(Box<CallExpr>),
    RValueCast(Box<RValueCastExpr>),
    DeclRef(Decl),
    Assign(Box<AssignExpr>),
    Binary(Box<BinaryExpr>),
    Unary(Box<UnaryExpr>),
}

impl Expr {
    pub fn value_category(&self) -> ValueCategory {
        match self {
            Expr::Int(_) | Expr::Str(_) | Expr::Call(_) | 
            Expr::RValueCast(_) | Expr::Binary(_) | Expr::Bool(_)
                => ValueCategory::RValue,

            Expr::Unary(u) => if matches!(u.op(), UnaryOp::Deref) {
                ValueCategory::LValue
            } else {
                ValueCategory::RValue
            },
                
            Expr::DeclRef(_) | Expr::Assign(_) => ValueCategory::LValue,
        }
    }

    pub fn is_lvalue(&self) -> bool {
        self.value_category().is_lvalue()
    }

    pub fn is_rvalue(&self) -> bool {
        self.value_category().is_rvalue()
    }

    pub fn is_ptr(&self) -> bool {
        matches!(self.ty(), Type::Ptr(_))
    }

    pub fn as_declref(&self) -> &Decl {
        if let Expr::DeclRef(decl) = self {
            decl
        } 
        else {
            panic!("Expected DeclRef, found {:?}", self);
        }
    }

    pub fn as_string(&self) -> &String {
        if let Expr::Str(s) = self {
            s
        } 
        else {
            panic!("Expected String, found {:?}", self);
        }
    }

    // TODO: this must be refactored to dispatch logic to the specific expression type
    pub fn ty(&self) -> Type {
        match self {
            Expr::Int(_) => Type::I32,
            Expr::Str(_) => Type::Ptr(Box::new(PtrType::new(Type::I8))),
            Expr::Bool(_) => Type::Bool,
            Expr::Call(call) => 
                // currently we support only direct calls so this works
                call.ty(),
            Expr::RValueCast(expr) => expr.ty(),
            Expr::DeclRef(decl) => {
                match decl {
                    Decl::Var(var_decl) => var_decl.ty().clone(),
                    Decl::Fn(fn_decl) => Type::Fn(Box::new(fn_decl.ty().clone())),
                    Decl::Param(param_decl) => param_decl.ty().clone(),
                }
            },
            Expr::Unary(unary) => {
                match unary.op() {
                    UnaryOp::AddressOf => Type::Ptr(PtrType::new(unary.operand().ty()).into()),
                    UnaryOp::Deref => unary.operand().ty().clone(),
                    UnaryOp::Pos | UnaryOp::Neg => Type::I32,
                }
            },
            Expr::Assign(assign) => assign.lhs().ty().clone(),
            Expr::Binary(binary) => {
                match binary.op() {
                    BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => Type::I32,
                    BinaryOp::LAnd | BinaryOp::LOr | BinaryOp::Eq => Type::Bool,
                }
            },
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

    pub fn ty(&self) -> Type {
        self.operand.ty()
    }
}

#[derive(Debug)]
pub struct CallExpr {
    callee: Expr,
    args: Vec<Expr>,
}

impl CallExpr {
    pub fn new(callee: Expr, args: Vec<Expr>) -> Self {
        assert!(args.iter().all(|arg| arg.is_rvalue()));
        CallExpr { callee, args }
    }

    pub fn callee(&self) -> &Expr {
        &self.callee
    }

    pub fn args(&self) -> &Vec<Expr> {
        &self.args
    }

    pub fn args_mut(&mut self) -> &mut Vec<Expr> {
        &mut self.args
    }

    pub fn ty(&self) -> Type {
        self.callee().as_declref().as_fn().declarator().ty().clone().into_fn_ty().ret_ty().clone()
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

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LAnd,
    LOr,
    Eq,
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

    pub fn op(&self) -> BinaryOp {
        self.op
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

    pub fn ty(&self) -> Type {
        use BinaryOp::*;

        match self.op {
            LAnd | LOr | Eq => Type::Bool,
            Add | Sub | Mul | Div | Mod => Type::I32,
        }
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum UnaryOp {
    AddressOf,
    Deref,
    Pos,
    Neg,
}

#[derive(Debug)]
pub struct UnaryExpr {
    op: UnaryOp,
    operand: Expr,
}

impl UnaryExpr {
    pub fn new(op: UnaryOp, operand: Expr) -> Self {
        UnaryExpr { op, operand }
    }

    pub fn op(&self) -> &UnaryOp {
        &self.op
    }

    pub fn operand(&self) -> &Expr {
        &self.operand
    }

    pub fn operand_mut(&mut self) -> &mut Expr {
        &mut self.operand
    }
}
