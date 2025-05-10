use subenum::subenum;

#[subenum(KeywordKind)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenKind {
    #[subenum(KeywordKind)]
    FN,
    
    #[subenum(KeywordKind)]
    LET,

    #[subenum(KeywordKind)]
    I32,

    #[subenum(KeywordKind)]
    RETURN,
   
    PLUS,
    MINUS,
    STAR,
    SLASH,
    AMP,
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
            KeywordKind::RETURN => "return",
        }
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
