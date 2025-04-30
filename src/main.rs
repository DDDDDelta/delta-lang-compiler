#![allow(unused_variables)]
#![allow(unused_imports)]
#![allow(dead_code)]

use std::io::Write;
use std::path::Path;
use std::process::Command;
use std::rc::Rc;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{ Linkage, Module };
use inkwell::types::{ BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType };
use inkwell::targets::{ Target, TargetMachine, InitializationConfig, TargetTriple };
use inkwell::values::{ BasicValueEnum, FunctionValue, InstructionValue, PointerValue };
use inkwell::{ AddressSpace, OptimizationLevel };

use crate::ast::decl::{ Decl, LocalDecl, TopLevelDecl, VarDecl, FnDecl };
use crate::ast::stmt::{ Stmt, ReturnStmt };
use crate::ast::expr::{ Expr, CallExpr, AssignExpr, BinaryExpr, BinaryOp, RValueCastExpr };
use crate::parse::parser::Parser;
use crate::code_gen::ir_gen::IRGen;

mod code_gen;
mod ast;
mod parse;
mod lex;

fn main() {
    // arg processing
    let input_file = "hello.mtxx";
    let output_file = "hello";
    let object_file = output_file.to_string() + ".o";

    // ast
    let localvardecl = Decl::Var(Rc::new(VarDecl::new(
        "local".to_string(),
        Expr::Int(15)
    )));

    let globalvardecl = Decl::Var(Rc::new(VarDecl::new(
        "global".to_string(),
        Expr::Int(20)
    )));

    let localvardecl2 = Decl::Var(Rc::new(VarDecl::new(
        "local2".to_string(),
        Expr::Int(25)
    )));

    let declreflocal = Expr::DeclRef(localvardecl.clone());
    let declrefglobal = Expr::DeclRef(globalvardecl.clone());

    let stmt_list = vec![
        Stmt::LocalDecl(localvardecl.try_into().unwrap()),
        Stmt::Expr(
            Expr::Call(
                Box::new(CallExpr::new(
                    "printf".to_string(), 
                    vec![
                        Expr::Str("Hello, World!\n".to_string()),
                    ]
                ))
            )
        ),
        Stmt::Expr(
            Expr::Call(
                Box::new(CallExpr::new(
                    "printf".to_string(), 
                    vec![
                        Expr::Str("local: %d\nglobal: %d\n".to_string()),
                        Expr::RValueCast(
                            Box::new(RValueCastExpr::new(
                                Expr::Assign(Box::new(AssignExpr::new(
                                    Expr::Assign(Box::new(AssignExpr::new(
                                        declreflocal,
                                        Expr::Int(-1)
                                    ))),
                                    Expr::Binary(Box::new(BinaryExpr::new(
                                        BinaryOp::Add,
                                        Expr::Int(2),
                                        Expr::RValueCast(Box::new(RValueCastExpr::new(
                                            Expr::DeclRef(globalvardecl.clone())
                                        )))
                                    )))
                                )))
                            ))
                        ),
                        Expr::RValueCast(
                            Box::new(RValueCastExpr::new(declrefglobal))
                        ),
                    ]
                ))
            )
        ),
        Stmt::LocalDecl(localvardecl2.clone().try_into().unwrap()),
        Stmt::Expr(
            Expr::Call(
                Box::new(CallExpr::new(
                    "printf".to_string(), 
                    vec![
                        Expr::Str("local2: %d\n".to_string()),
                        Expr::RValueCast(Box::new(RValueCastExpr::new(Expr::DeclRef(localvardecl2)))),
                    ]
                ))
            )
        ),
        Stmt::Return(
            Box::new(
                ReturnStmt::new(
                    Some(
                        Expr::Int(0)
                    )
                )
            )
        )
    ];
    let fn_decl = FnDecl::new(
        "main".to_string(), 
        stmt_list
    );

    let vardecl = VarDecl::new(
        "v1".to_string(),
        Expr::Int(15)
    );

    let localvardecl = VarDecl::new(
        "v2".to_string(),
        Expr::Int(20)
    );

    let local_fn_decl = FnDecl::new(
        "local_var".to_string(), 
        vec![
            Stmt::LocalDecl(
                LocalDecl::Var(Rc::new(localvardecl))
            ),
            Stmt::Return(
                Box::new(
                    ReturnStmt::new(
                        Some(
                            Expr::Int(0)
                        )
                    )
                )
            )
        ]
    );

    let program: Vec<TopLevelDecl> = vec![
        globalvardecl.try_into().unwrap(),
        TopLevelDecl::Fn(Rc::new(fn_decl)),
        TopLevelDecl::Fn(Rc::new(local_fn_decl)),
        TopLevelDecl::Var(Rc::new(vardecl))
    ];

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

    let var_decl_fn = {
        let fn_type = context.i32_type().fn_type(&[], false);
        module.add_function("vardecl", fn_type, Some(Linkage::External))
    };

    let entry = context.append_basic_block(var_decl_fn, "entry");
    builder.position_at_end(entry);
    
    let i32_type = context.i32_type();
    let str_type = context.i8_type().array_type(3);

    let s = module.add_global(
        str_type, 
        None, 
        "msg"
    );
    
    s.set_initializer(
        &context.const_string(
            b"%d", 
            true
        )
    );

    let alloca = builder.build_alloca(
        i32_type, 
        "x"
    )
    .unwrap();
    let stored = builder.build_store(
        alloca, 
        i32_type.const_int(42, true)
    )
    .unwrap();
    let loaded = builder.build_load(
        i32_type,
        alloca, 
        "x_val"
    )
    .unwrap();
    let _ = builder.build_call(
        module.get_function("printf").unwrap(),
        &[s.as_pointer_value().into(), loaded.into()],
        "call"
    );
    let stored = builder.build_store(
        alloca, 
        i32_type.const_int(43, true)
    )
    .unwrap();
    let loaded = builder.build_load(
        i32_type,
        alloca, 
        "x_val"
    ).unwrap();
    let _ = builder.build_call(
        module.get_function("printf").unwrap(),
        &[s.as_pointer_value().into(), loaded.into()],
        "call.1"
    ).unwrap();
    let _ = builder.build_return(
        Some(
            &i32_type.const_int(0, true)
        )
    ).unwrap();

    // let _ = ir_gen.gen_function(&fn_decl);

    ir_gen.gen_program(&program);

    /*
    let context = Context::create();
    let module = context.create_module(input_file);
    let builder = context.create_builder();

    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    let ptr_type = context.ptr_type(AddressSpace::default());
    let printf_type = i32_type.fn_type(
        &[ptr_type.into()], 
        true
    );
    let rand_type = i32_type.fn_type(
        &[], 
        false
    );

    module.add_function("printf", printf_type, Some(Linkage::External));
    module.add_function("rand", rand_type, Some(Linkage::External));

    let global_str = module.add_global(
        i8_type.array_type(14), 
        None, 
        "msg"
    );

    global_str.set_initializer(
        &context.const_string(
            b"Hello, World!", 
            true
        )
    );

    let main_fn = module.add_function(
        "main", 
        i32_type.fn_type(&[], false), 
        None
    );

    let entry = context.append_basic_block(main_fn, "entry");
    
    builder.position_at_end(entry);
    let _ = builder.build_call(
        module.get_function("printf").unwrap(),
        &[global_str.as_pointer_value().into()],
        "call"
    );

    let _ = builder.build_return(
        Some(
            &i32_type.const_int(0, true)
        )
    );

    */

    println!("{}", module.print_to_string().to_string());

    match module.verify() {
        Ok(_) => println!("Module verified successfully"),
        Err(e) => {
            eprintln!("Module verification failed: {}", e);
            return;
        }
    }

    println!("");

    // asm insepction
    let buffer = target_machine
        .write_to_memory_buffer(
            &module,
            inkwell::targets::FileType::Assembly, 
        )
        .expect("Failed to create assembly buffer.");

    std::io::stdout()
        .write_all(buffer.as_slice())
        .expect("Failed to write assembly buffer to stdout.");

    // object code emittion
    let result = target_machine
        .write_to_file(
            &module, 
            inkwell::targets::FileType::Object, 
            Path::new(&object_file)
        )
        .expect("Object code emittion failed.");

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
            if output.status.success() {
                println!("Executable created successfully");
            } else {
                eprintln!("Error creating executable, the linker returned the following\n{}", String::from_utf8_lossy(&output.stderr));
            }
        }
        Err(e) => {
            eprintln!("Failed to execute command: {}", e);
        }
    }
}
