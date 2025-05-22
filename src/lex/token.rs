use subenum::subenum;

use crate::ast::expr::BinaryOp;

#[subenum(KeywordKind, BinaryOpKind)]
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
    
    #[subenum(BinaryOpKind)]
    PLUS,

    #[subenum(BinaryOpKind)]
    MINUS,

    #[subenum(BinaryOpKind)]
    STAR,

    #[subenum(BinaryOpKind)]
    SLASH,

    #[subenum(BinaryOpKind)]
    PERCENT,

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
            KeywordKind::LET => "let",
            KeywordKind::I32 => "i32",
            KeywordKind::I8 => "i8",
            KeywordKind::RETURN => "return",
        }
    }
}

impl BinaryOpKind {
    pub fn to_op(self) -> BinaryOp {
        let s: Self = TokenKind::PLUS.try_into().unwrap();

        use crate::ast::expr::BinaryOp::*;
        use self::BinaryOpKind::*;

        match self {
            PLUS => Add,
            MINUS => Sub,
            STAR => Mul,
            SLASH => Div,
            PERCENT => Mod,
        }
    }
}

impl Into<BinaryOp> for BinaryOpKind {
    fn into(self) -> BinaryOp {
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
