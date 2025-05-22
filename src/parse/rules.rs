pub const PARSER_RULES: &'static str = r#"
program
    : decl* EOF
    ;

decl: var_decl
    | fn_decl
    ;

type: I32
    | I8
    | STAR type
    ;

declarator
    : ID type
    ;

declarator_list
    : (declarator (COMMA declarator)*)?
    ;
    
var_decl
    : LET declarator EQ expr SEMI
    ;

fn_decl
    : FN ID LPAREN declarator_list RPAREN type LBRACE
    stmt*
    RBRACE
    ;
    
stmt: var_decl
    | expr SEMI
    | return_stmt
    ;
    
return_stmt
    : RETURN expr SEMI
    ;

expr: assign_expr
    ;
    
expr_list
    : (expr (COMMA expr)*)?
    ;
    
primary_expr
    : ID
    | INT
    | STR
    | paren_expr
    ;
    
paren_expr
    : LPAREN expr RPAREN
    ;
    
postfix_expr
    : primary_expr (LPAREN expr_list RPAREN)*
    ;
    
mul_expr
    : mul_expr (STAR | SLASH | PERCENT) postfix_expr
    | postfix_expr
    ;
    
add_expr
    : add_expr (PLUS | MINUS) mul_expr
    | mul_expr
    ;
    
assign_expr
    : add_expr EQ assign_expr
    | add_expr
    ;
"#;
