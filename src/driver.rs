use std::fs;
use std::path::Path;
use std::process::Command;

use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::{ InitializationConfig, Target, TargetTriple };
use inkwell::OptimizationLevel;

use crate::lex::cached_lexer::CachedLexer;
use crate::lex::lex_all;
use crate::parse::parser::Parser;
use crate::code_gen::ir_gen::IRGen;

pub fn compile(input_file: &str, output_file: &str) -> i32 {
    // preperations
    let object_file = input_file.to_string() + ".o";
    let Ok(code) = fs::read_to_string(input_file) else {
        eprintln!("Error reading file {}", input_file);
        return 3;
    };

    // language frontend
    let lexer = CachedLexer::new(code.as_str());
    let mut parser = Parser::new(lexer);
    let Some(program) = parser.parse_all() else {
        eprintln!("Error analyzing file {}", input_file);
        return 4;
    };

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
        .expect("Failed to create target machine.");

    // ir generation
    let context = Context::create();
    let ir_gen = IRGen::new(
        &context, 
        &target_machine,
        input_file
    );
    
    let module = ir_gen.module();
    let builder = ir_gen.builder();

    ir_gen.gen_program(&program);

    match module.verify() {
        Ok(_) => println!("Module verified successfully"),
        Err(e) => {
            eprintln!("Module verification failed: {}", e);
            return 5;
        }
    }

    // object code emittion
    match target_machine
        .write_to_file(
            &module, 
            inkwell::targets::FileType::Object, 
            Path::new(&object_file)
        ) {
        
        Ok(_) => println!("Object file created successfully"),
        Err(e) => {
            eprintln!("Failed to create object file: {}", e);
            return 6;
        }
    }

    // linking
    let output = Command::new("ld")
        .arg("-o")
        .arg(output_file)
        .arg(object_file)
        .arg("/usr/lib/x86_64-linux-gnu/crt1.o")
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
