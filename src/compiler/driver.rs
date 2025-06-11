use std::fs;
use std::path::Path;
use std::process::Command;

use clap::Parser as _;
use inkwell::context::Context;
use inkwell::targets::{ FileType, InitializationConfig, Target, TargetTriple };
use inkwell::OptimizationLevel;
use tempfile::{ tempdir, Builder };

use crate::lex::cached_lexer::CachedLexer;
use crate::parse::parser::Parser;
use crate::code_gen::ir_gen::IRGen;
use crate::compiler::cl::CLOpt;

pub fn compile(args: Vec<String>) -> i32 {
    // parse args
    let args = match CLOpt::try_parse_from(args) {
        Ok(parsed_args) => parsed_args,
        Err(e) => {
            eprintln!("{}", e);
            return 2;
        }
    };

    if args.files().len() != 1 {
        eprintln!("Currently only one file is supported");
        return 1;
    }

    // create temporary directory and object file
    let Ok(temp_dir) = tempdir() else {
        eprintln!("Failed to create temporary directory");
        return 1;
    };
    let Ok(obj_file) = Builder::new()
        .suffix(".o")
        .tempfile_in(temp_dir.path()) else {
            eprintln!("Failed to create temporary object file");
            return 1;
        };

    // preperations
    let input_file = &args.files()[0];
    let object_file = obj_file.path();
    let Ok(code) = fs::read_to_string(input_file) else {
        eprintln!("Error reading file {}", input_file.display());
        return 3;
    };

    // language frontend
    let lexer = CachedLexer::new(code.as_str());
    let mut parser = Parser::new(lexer);
    let Some(program) = parser.parse_all() else {
        eprintln!("Error analyzing file {}", input_file.display());
        return 4;
    };

    if args.dump_ast() {
        println!("{:?}", program);
    }

    // code gen init
    Target::initialize_all(&InitializationConfig::default());
    
    let triple = TargetTriple::create("x86_64-unknown-linux-gnu");
    let target = Target::from_triple(&triple).expect("Failed to get target");
    let target_machine = target
        .create_target_machine(
            &triple, 
            "x86-64", 
            "", 
            OptimizationLevel::None, 
            Default::default(), 
            Default::default()
        )
        .expect("failed to create target machine.");

    // ir generation
    let context = Context::create();
    let ir_gen = IRGen::new(
        &context, 
        &target_machine,
        input_file.to_str().unwrap_or_default(),
    );
    
    let module = ir_gen.module();

    ir_gen.gen_program(&program);

    if args.emit_llvm() {
        println!("{}", module.print_to_string().to_string_lossy());
    }

    if args.verify_llvm() {
        match module.verify() {
            Ok(_) => println!("Module verified successfully"),
            Err(e) => {
                eprintln!("Module verification failed: {}", e.to_string());
                return 5;
            }
        }
    }

    // object code emittion
    match target_machine
        .write_to_file(
            &module, 
            FileType::Object, 
            Path::new(&object_file)
        ) {
        
        Ok(_) => println!("Object file created successfully"),
        Err(e) => {
            eprintln!("Failed to create object file: {}", e.to_string_lossy());
            return 6;
        }
    }

    // linking
    let output = Command::new("ld")
        .arg("-o")
        .arg(args.output())
        .arg(object_file)
        .arg("/usr/lib/x86_64-linux-gnu/crt1.o")
        .arg("/usr/lib/x86_64-linux-gnu/crti.o")
        .arg("/usr/lib/x86_64-linux-gnu/crtn.o")
        .arg("-lc")
        .arg("-dynamic-linker")
        .arg("/lib64/ld-linux-x86-64.so.2")
        .output();

    match output {
        Ok(output) => {
            if !output.status.success() {
                eprintln!(
                    "Error creating executable, the linker returned the following:\n{}", 
                    String::from_utf8_lossy(&output.stderr)
                );
                return 6;
            }
        }
        Err(e) => {
            eprintln!("Failed to execute command: {}", e);
            return 7;
        }
    }

    println!("Executable created successfully");
    0
}
