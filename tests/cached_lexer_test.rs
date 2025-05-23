#![allow(unused_imports)]

use deltac::lex::cached_lexer::CachedLexer;
use deltac::lex::token::TokenKind::{ self, * };

/* -------------------------------------------------------------------------- */
/*                peek() must *not* consume the underlying token              */
/* -------------------------------------------------------------------------- */

#[test]
fn peek_does_not_consume() {
    let mut lx = CachedLexer::new("let x");
    // first peek fills the cache but leaves the stream untouched
    let t1 = lx.peek().expect("peek must yield a token");
    assert_eq!(*t1.kind(), LET);
    assert_eq!(lx.ncached(), 1, "peek should cache exactly one token");

    // first lex() now *consumes* that cached token
    let t1_again = lx.lex().expect("lex after peek should return same token");
    assert_eq!(*t1_again.kind(), LET);
    assert_eq!(lx.ncached(), 0, "cache should be empty after consuming");
}

/* -------------------------------------------------------------------------- */
/*                 peekn() fills the cache up to the requested n              */
/* -------------------------------------------------------------------------- */

#[test]
fn peekn_populates_cache() {
    let mut lx = CachedLexer::new("let x i32");
    // 0-based index: peekn(2) should give the *third* token (`I32`)
    let tn = lx.peekn(2).expect("source has at least three tokens");
    assert_eq!(*tn.kind(), I32);

    // Cache must now contain three tokens (indices 0,1,2)
    assert_eq!(lx.ncached(), 3);

    // Sequential lex() calls should return those same three tokens in order
    assert_eq!(*lx.lex().unwrap().kind(), LET);
    assert_eq!(*lx.lex().unwrap().kind(), ID);
    assert_eq!(*lx.lex().unwrap().kind(), I32);
    assert_eq!(lx.ncached(), 0);
}

/* -------------------------------------------------------------------------- */
/*                        eatn() consumes *exactly* n tokens                  */
/* -------------------------------------------------------------------------- */

#[test]
fn eatn_consumes_exact_count() {
    let mut lx = CachedLexer::new("fn foo");
    // eat the first two tokens; should return the second one (`ID`)
    let last = lx.eatn(2).expect("stream has two tokens");
    assert_eq!(*last.kind(), ID);

    // next lex() should now yield EOF
    assert_eq!(*lx.lex().unwrap().kind(), EOF);
    // and the stream is exhausted
    assert!(lx.lex().is_none());
}

/* -------------------------------------------------------------------------- */
/*                    ok() mirrors the underlying lexer state                 */
/* -------------------------------------------------------------------------- */

#[test]
fn ok_reflects_lexer_error_flag() {
    let mut lx_ok   = CachedLexer::new("123");
    let _ = lx_ok.lex();            // consume INT
    assert!(lx_ok.ok(), "valid input must keep ok() true");

    let mut lx_err  = CachedLexer::new("@"); // invalid char triggers error
    assert!(lx_err.lex().is_none(),  "first lex() should fail");
    assert!(!lx_err.ok(),            "ok() must propagate lexer error flag");
}

/* -------------------------------------------------------------------------- */
/*                    peek after EOF must keep returning EOF                  */
/* -------------------------------------------------------------------------- */

#[test]
fn repeated_peek_after_eof() {
    let mut lx = CachedLexer::new("");
    // first peek injects EOF into cache
    assert_eq!(*lx.peek().unwrap().kind(), EOF);
    // further peeks *and* lex() must keep reporting EOF or None
    assert_eq!(*lx.peek().unwrap().kind(), EOF);
    assert_eq!(*lx.lex().unwrap().kind(), EOF);
    assert!(lx.lex().is_none(), "after EOF token, stream must end");
}

/* -------------------------------------------------------------------------- */
/*         peekn past the end of stream returns None and leaves cache intact  */
/* -------------------------------------------------------------------------- */

#[test]
fn peekn_past_eof_returns_none() {
    let mut lx = CachedLexer::new("let");
    // cache the single real token
    assert!(lx.peekn(0).is_some());
    assert_eq!(lx.ncached(), 1);

    // ask for a token beyond EOF
    assert!(lx.peekn(5).is_none(), "peekn past end should return None");
    // cache should be cleared since the underlying lexer is no longer valid
    assert_eq!(lx.ncached(), 0);
    assert!(!lx.ok(), "peekn past end should set lexer error flag");
}
