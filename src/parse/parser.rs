use crate::lex::lexer::Lexer;
use crate::lex::token::{ Token, TokenKind };

/* 
 * *** Parser Rule ***
 * 
 * program
 *     : decl* EOF
 *     ;
 * 
 * decl: var_decl
 *     | fn_decl
 *     ;
 * 
 * type: I32
 *     ;
 * 
 * declarator
 *     : ID type
 *     ;
 * 
 * declarator_list
 *     : (declarator (COMMA declarator)*)?
 *     ;
 *     
 * var_decl
 *     : LET declarator EQ expr SEMI
 *     ;
 * 
 * fn_decl
 *     : FN ID LPAREN declarator_list RPAREN LBRACE
 *     stmt*
 *     RBRACE
 *     ;
 *     
 * stmt: var_decl
 *     | expr SEMI
 *     | return_stmt
 *     ;
 *     
 * return_stmt
 *     : RETURN expr SEMI
 *     ;
 * 
 * expr: assign_expr
 *     ;
 *     
 * expr_list
 *     : (expr (COMMA expr)*)?
 *     ;
 *     
 * primary_expr
 *     : ID
 *     | INT
 *     | STR
 *     | paren_expr
 *     ;
 *     
 * paren_expr
 *     : LPAREN expr RPAREN
 *     ;
 *     
 * postfix_expr
 *     : primary_expr (LPAREN expr_list RPAREN)*
 *     ;
 *     
 * mul_expr
 *     : mul_expr (STAR | SLASH | AMP) postfix_expr
 *     | postfix_expr
 *     ;
 *     
 * add_expr
 *     : add_expr (PLUS | MINUS) mul_expr
 *     | mul_expr
 *     ;
 *     
 * assign_expr
 *     : add_expr EQ assign_expr
 *     | add_expr
 *     ;
 */
pub struct Parser {
    lexer: Lexer,
}
