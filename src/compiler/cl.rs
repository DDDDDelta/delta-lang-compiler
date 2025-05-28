use std::path::PathBuf;
use clap::Parser;

#[derive(Debug, Parser)]
#[command(author, version, about)]
pub struct CLOpt {
    #[arg(value_name = "FILE", num_args = 1..)]
    files: Vec<PathBuf>,

    #[arg(short, long, default_value = "a.out", value_name = "PATH")]
    output: PathBuf,

    #[arg(long)]
    emit_llvm: bool,

    #[arg(short, long = "compile")]
    compile_only: bool,

    #[arg(long)]
    verify_llvm: bool,

    #[arg(long)]
    dump_ast: bool,
}

impl CLOpt {
    pub fn files(&self) -> &Vec<PathBuf> {
        &self.files
    }

    pub fn output(&self) -> &PathBuf {
        &self.output
    }

    pub fn emit_llvm(&self) -> bool {
        self.emit_llvm
    }

    pub fn compile_only(&self) -> bool {
        self.compile_only
    }
    
    pub fn verify_llvm(&self) -> bool {
        self.verify_llvm
    }

    pub fn dump_ast(&self) -> bool {
        self.dump_ast
    }
}
