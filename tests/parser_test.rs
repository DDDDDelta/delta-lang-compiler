#![allow(unused_variables)]
#![allow(unused_imports)]

use std::rc::Rc;

use cpluspluswocaonima::ast::expr::{BinaryOp, Expr};
use cpluspluswocaonima::ast::expr_type::Type;
use cpluspluswocaonima::ast::stmt::{ Stmt, ReturnStmt };
use cpluspluswocaonima::parse::parser::{ Parser, Scope };
use cpluspluswocaonima::lex::cached_lexer::CachedLexer;
use cpluspluswocaonima::lex::lexer::Lexer;
use cpluspluswocaonima::lex::token::{ Token, TokenKind };
use cpluspluswocaonima::ast::decl::{ Declarator, LocalDecl, Named, NamedDecl, TopLevelDecl, VarDecl };

#[test]
fn parse_integer_literal_expr() {
    let lexer = CachedLexer::new("123");
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let expr = parser
        .parse_expr(&mut scope)
        .expect("integer literal should parse");

    assert!(matches!(expr, Expr::Int(123)));
}

#[test]
fn identifier_resolves_to_decl() {
    // put `a` in scope first
    let mut scope = Scope::empty();
    scope.add(&NamedDecl::Var(Rc::new(VarDecl::new(
        Declarator::new("a".to_string(), Type::I32), 
        Expr::Int(0)
    ))));

    let lexer = CachedLexer::new("a");
    let mut parser = Parser::new(lexer);

    let expr = parser
        .parse_expr(&mut scope)
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
    let mut scope = Scope::empty();

    assert!(parser.parse_expr(&mut scope).is_none());
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
    let v1 = NamedDecl::Var(Rc::new(VarDecl::new(
        Declarator::new("x".to_string(), Type::I32), Expr::Int(1)
    )));
    let v2 = NamedDecl::Var(Rc::new(VarDecl::new(
        Declarator::new("x".to_string(), Type::I32), Expr::Int(2)
    )));

    assert!(nt.add(&v1));   // first insert succeeds
    assert!(!nt.add(&v2));  // duplicate rejected
    assert!(nt.find("x").is_some());
}

#[test]
fn parse_function_declaration_basic() {
    let src = "fn my_function(x i32) { let y i32 = 10; return y; }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser
        .parse_fn_decl(&mut scope)
        .expect("function declaration should parse");

    assert_eq!(func.name(), "my_function");
    assert_eq!(func.body().as_ref().expect("the function has a body").len(), 2); 
    // the two statements: var declaration and return statement
}

#[test]
fn parse_function_declaration_empty_body() {
    let src = "fn my_empty_function() { }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser
        .parse_fn_decl(&mut scope)
        .expect("function declaration should parse");

    assert_eq!(func.name(), "my_empty_function");
    assert_eq!(func.body().as_ref().expect("the function has a body").len(), 0); // no statements inside
}

#[test]
fn parse_function_declaration_with_multiple_params() {
    let src = "fn add(x i32, y i32) { y; return x; }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser
        .parse_fn_decl(&mut scope)
        .expect("function declaration with parameters should parse");

    assert_eq!(func.name(), "add");
    assert_eq!(func.body().as_ref().expect("the function has a body").len(), 2); // two statement: y and return
}

#[test]
fn parse_invalid_function_declaration_missing_param_type() {
    let src = "fn invalid_function(x) { return x; }"; // missing type for x
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    assert!(parser.parse_fn_decl(&mut scope).is_none());
}

#[test]
#[ignore = "reason: not implemented different function return type yet"]
fn parse_function_with_invalid_return_type() {
    let src = "fn invalid_return_type() { let x i32 = 10; return x + 1; }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser
        .parse_fn_decl(&mut scope)
        .expect("function with invalid return type should fail");

    // Expect failure here, handle type mismatch if needed
}

#[test]
fn parse_function_with_no_return_stmt() {
    let src = "fn no_return() { let x i32 = 42; }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser
        .parse_fn_decl(&mut scope)
        .expect("function without return statement should parse");

    assert_eq!(func.name(), "no_return");
    assert_eq!(func.body().as_ref().expect("the function has a body").len(), 1); // only a variable declaration
}

#[test]
fn parse_var_decl_in_statement() {
    let src = "let a i32 = 5;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let stmt = parser
        .parse_stmt(&mut scope)
        .expect("var declaration should parse as statement");

    match stmt {
        Stmt::LocalDecl(local_decl) => {
            match local_decl {
                LocalDecl::Var(var_decl) => {
                    assert_eq!(var_decl.name(), "a");
                    assert!(matches!(&var_decl.initializer(), Expr::Int(5)));
                }
                _ => panic!("expected VarDecl"),
            }
        }
        _ => panic!("expected LocalDecl in statement"),
    }
}

#[test]
fn parse_var_decl_with_non_constant_init() {
    let src = "let x i32 = a;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    assert!(parser.parse_stmt(&mut scope).is_none());
}

#[test]
fn parse_return_stmt_with_expression() {
    let src = "return 42;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let stmt = parser
        .parse_stmt(&mut scope)
        .expect("return statement with expression should parse");

    match stmt {
        Stmt::Return(return_stmt) => {
            assert!(matches!(&return_stmt.returned(), Some(Expr::Int(42))));
        }
        _ => panic!("expected ReturnStmt"),
    }
}

#[test]
fn parse_return_stmt_without_expression() {
    let src = "return;";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let stmt = parser
        .parse_stmt(&mut scope)
        .expect("return statement without expression should parse");

    match stmt {
        Stmt::Return(return_stmt) => {
            assert!(return_stmt.returned().is_none());
        }
        _ => panic!("expected ReturnStmt"),
    }
}

#[test]
fn binary_precedence_multiplication_bind_tighter_than_addition() {
    // `1 + 2 * 3` ⇒ ADD(1, MUL(2, 3))
    let lexer = CachedLexer::new("1 + 2 * 3");
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();
    let expr = parser.parse_expr(&mut scope).expect("expression should parse");

    match expr {
        Expr::Binary(add) => {
            assert!(matches!(add.op(), BinaryOp::Add));
            assert!(matches!(add.lhs(), Expr::Int(1)));
            match add.rhs() {
                Expr::Binary(mul) => {
                    assert!(matches!(mul.op(), BinaryOp::Mul));
                    assert!(matches!(mul.lhs(), Expr::Int(2)));
                    assert!(matches!(mul.rhs(), Expr::Int(3)));
                }
                _ => panic!("right-hand side of add must be a multiplication")
            }
        }
        _ => panic!("root must be an addition binary-expr"),
    }
}

#[test]
fn binary_left_associative_for_same_precedence_ops() {
    // `4 - 3 - 2` ⇒ SUB(SUB(4,3), 2)
    let lexer = CachedLexer::new("4 - 3 - 2");
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();
    let expr = parser.parse_expr(&mut scope).expect("expression should parse");

    match expr {
        Expr::Binary(outer_sub) => {
            assert!(matches!(outer_sub.op(), BinaryOp::Sub));
            assert!(matches!(outer_sub.rhs(), Expr::Int(2)));
            match outer_sub.lhs() {
                Expr::Binary(inner_sub) => {
                    assert!(matches!(inner_sub.op(), BinaryOp::Sub));
                    assert!(matches!(inner_sub.lhs(), Expr::Int(4)));
                    assert!(matches!(inner_sub.rhs(), Expr::Int(3)));
                }
                _ => panic!("left-hand side of outer subtraction must be another subtraction")
            }
        }
        _ => panic!("root must be a subtraction binary-expr"),
    }
}

#[test]
fn binary_parentheses_override_precedence() {
    // `(1 + 2) * 3` ⇒ MUL(ADD(1,2),3)
    let lexer = CachedLexer::new("(1 + 2) * 3");
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();
    let expr = parser.parse_expr(&mut scope).expect("expression should parse");

    match expr {
        Expr::Binary(mul) => {
            assert!(matches!(mul.op(), BinaryOp::Mul));
            assert!(matches!(mul.rhs(), Expr::Int(3)));
            match mul.lhs() {
                Expr::Binary(add) => {
                    assert!(matches!(add.op(), BinaryOp::Add));
                    assert!(matches!(&*add.lhs(), Expr::Int(1)));
                    assert!(matches!(&*add.rhs(), Expr::Int(2)));
                }
                _ => panic!("left-hand side of multiplication must be an addition")
            }
        }
        _ => panic!("root must be a multiplication binary-expr"),
    }
}

#[test]
fn function_with_explicit_return_type_parses() {
    let src = "fn inc(x i32) i32 { return x + 1; }";
    let lexer = CachedLexer::new(src);
    let mut parser = Parser::new(lexer);
    let mut scope = Scope::empty();

    let func = parser.parse_fn_decl(&mut scope).expect("function should parse");
    assert_eq!(func.name(), "inc");
    // body has one return statement
    assert_eq!(func.body().as_ref().unwrap().len(), 1);
}
