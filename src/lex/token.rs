use subenum::subenum;

use crate::ast::expr::{ BinaryOp, UnaryOp };

#[subenum(KeywordKind, BinaryOpKind, UnaryOpKind)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    #[subenum(KeywordKind)]
    FN,
    
    #[subenum(KeywordKind)]
    LET,

    #[subenum(KeywordKind)]
    I32,

    #[subenum(KeywordKind)]
    I8,

    #[subenum(KeywordKind)]
    RETURN,

    #[subenum(KeywordKind)]
    PRINT,

    #[subenum(KeywordKind)]
    VOID,

    #[subenum(KeywordKind)]
    BOOL,

    #[subenum(KeywordKind)]
    TRUE,

    #[subenum(KeywordKind)]
    FALSE,

    #[subenum(KeywordKind)]
    EXTERN,

    #[subenum(KeywordKind)]
    IF,

    #[subenum(KeywordKind)]
    ELSE,

    #[subenum(KeywordKind)]
    WHILE,
    
    #[subenum(BinaryOpKind, UnaryOpKind)]
    PLUS,

    #[subenum(BinaryOpKind, UnaryOpKind)]
    MINUS,

    #[subenum(BinaryOpKind, UnaryOpKind)]
    STAR,

    #[subenum(BinaryOpKind)]
    SLASH,

    #[subenum(BinaryOpKind)]
    PERCENT,

    #[subenum(UnaryOpKind)]
    AMP,

    #[subenum(UnaryOpKind)]
    BANG,

    #[subenum(BinaryOpKind)]
    EQEQ,

    #[subenum(BinaryOpKind)]
    BANGEQ,

    #[subenum(BinaryOpKind)]
    AMPAMP,

    #[subenum(BinaryOpKind)]
    PIPEPIPE,

    EQ,
    COMMA,
    SEMI,
    LPAREN,
    RPAREN,
    LCURLY,
    RCURLY,
    
    STR,
    INT,
    ID,
    EOF,
}

impl KeywordKind {
    pub fn spelling(&self) -> &'static str {
        match self {
            KeywordKind::FN => "fn",
            KeywordKind::EXTERN => "extern",
            KeywordKind::LET => "let",
            KeywordKind::I32 => "i32",
            KeywordKind::I8 => "i8",
            KeywordKind::VOID => "void",
            KeywordKind::RETURN => "return",
            KeywordKind::PRINT => "print",
            KeywordKind::BOOL => "bool",
            KeywordKind::TRUE => "true",
            KeywordKind::FALSE => "false",
            KeywordKind::IF => "if",
            KeywordKind::ELSE => "else",
            KeywordKind::WHILE => "while",
        }
    }
}

impl BinaryOpKind {
    pub fn to_op(self) -> BinaryOp {
        use crate::ast::expr::BinaryOp::*;
        use self::BinaryOpKind::*;

        match self {
            PLUS => Add,
            MINUS => Sub,
            STAR => Mul,
            SLASH => Div,
            PERCENT => Mod,
            AMPAMP => LAnd,
            PIPEPIPE => LOr,
            EQEQ => Eq,
            BANGEQ => NEq,
        }
    }
}

impl Into<BinaryOp> for BinaryOpKind {
    fn into(self) -> BinaryOp {
        self.to_op()
    }
}

impl UnaryOpKind {
    pub fn to_op(self) -> UnaryOp {
        use crate::ast::expr::UnaryOp::*;
        use self::UnaryOpKind::*;

        match self {
            PLUS => Pos,
            MINUS => Neg,
            STAR => Deref,
            AMP => AddressOf,
            BANG => Not,
        }
    }
}

impl Into<UnaryOp> for UnaryOpKind {
    fn into(self) -> UnaryOp {
        self.to_op()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token<'s> {
    kind: TokenKind,
    lexeme: &'s str,
}

impl<'s> Token<'s> {
    pub fn new(kind: TokenKind, lexeme: &'s str) -> Self {
        Token { kind, lexeme }
    }

    pub fn kind(&self) -> &TokenKind {
        &self.kind
    }

    pub fn lexeme(&self) -> &'s str {
        self.lexeme
    }
}
