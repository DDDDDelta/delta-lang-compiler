#![allow(unused_variables)]
#![allow(unused_imports)]

use std::rc::Rc;

use cpluspluswocaonima::ast::expr::Expr;
use cpluspluswocaonima::parse::parser::{ Parser, Scope };
use cpluspluswocaonima::lex::cached_lexer::CachedLexer;
use cpluspluswocaonima::lex::lexer::Lexer;
use cpluspluswocaonima::lex::token::{ Token, TokenKind };
use cpluspluswocaonima::ast::decl::{ Named, NamedDecl, TopLevelDecl, VarDecl };

#[test]
fn parse_integer_literal_expr() {
    let lexer = CachedLexer::new("123");
    let mut parser = Parser::new(lexer);
    let scope = Scope::empty();

    let expr = parser
        .parse_expr(&scope)
        .expect("integer literal should parse");

    assert!(matches!(expr, Expr::Int(123)));
}

#[test]
fn identifier_resolves_to_decl() {
    // put `a` in scope first
    let mut scope = Scope::empty();
    scope.add(&NamedDecl::Var(Rc::new(VarDecl::new("a".into(), Expr::Int(0)))));

    let lexer = CachedLexer::new("a");
    let mut parser = Parser::new(lexer);

    let expr = parser
        .parse_expr(&scope)
        .expect("identifier should parse");

    match expr {
        Expr::DeclRef(decl) => assert_eq!(TryInto::<NamedDecl>::try_into(decl).unwrap().name(), "a"),
        _ => panic!("expected DeclRef"),
    }
}

#[test]
fn unknown_identifier_returns_none() {
    let lexer = CachedLexer::new("b");
    let mut parser = Parser::new(lexer);
    let scope = Scope::empty();

    assert!(parser.parse_expr(&scope).is_none());
}

#[test]
fn top_level_const_var_decl_ok() {
    // let x i32 = 42;
    let src = "let x i32 = 42;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut tracker = Scope::empty();

    let tl = parser
        .parse_top_level_decl(&mut tracker)
        .expect("declaration should parse");

    match tl {
        TopLevelDecl::Var(v) => {
            assert_eq!(v.name(), "x");
            assert!(matches!(&v.initializer(), Expr::Int(42)));
        }
        _ => panic!("expected TopLevelDecl::Var"),
    }
}

#[test]
fn top_level_non_const_init_rejected() {
    // let y i32 = z;   // RHS is not a constant literal → should fail
    let src = "let y i32 = z;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut tracker = Scope::empty();

    assert!(parser.parse_top_level_decl(&mut tracker).is_none());
}

#[test]
fn name_tracker_detects_duplicates() {
    let mut nt = Scope::empty();
    let v1 = NamedDecl::Var(Rc::new(VarDecl::new("x".into(), Expr::Int(1))));
    let v2 = NamedDecl::Var(Rc::new(VarDecl::new("x".into(), Expr::Int(2))));

    assert!(nt.add(&v1));   // first insert succeeds
    assert!(!nt.add(&v2));  // duplicate rejected
    assert!(nt.find("x").is_some());
}
