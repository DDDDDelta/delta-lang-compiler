#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

mod code_gen;
mod ast;
mod parse;
mod lex;
mod compiler;

fn main() -> std::process::ExitCode {
    let exit_code = crate::compiler::driver::compile(std::env::args().collect());

    println!("compiler exited with code {}", exit_code);
    std::process::ExitCode::from(exit_code as u8)
}
