use std::collections::HashMap;

use either::Either;

use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::data_layout::DataLayout;
use inkwell::module::{ Linkage, Module };
use inkwell::targets::TargetMachine;
use inkwell::types::{ BasicMetadataTypeEnum, BasicType, BasicTypeEnum, IntType };
use inkwell::values::{ 
    AnyValue, BasicMetadataValueEnum, BasicValue, 
    BasicValueEnum, FunctionValue, GlobalValue, 
    InstructionValue, PointerValue 
};
use inkwell::{ AddressSpace };

use crate::ast::decl::{ FnDecl, NamedDecl, TopLevelDecl, VarDecl, LocalDecl };
use crate::ast::stmt::{ Stmt, ReturnStmt };
use crate::ast::expr::{ Expr };

pub struct IRGen<'ctx> {
    context: &'ctx Context, 
    module: Module<'ctx>, 
    builder: Builder<'ctx>,
}

impl<'ctx> IRGen<'ctx> {
    pub fn new(ctx: &'ctx Context, target: &TargetMachine, module_name: &str) -> Self {
        let module = ctx.create_module(module_name);
        module.set_triple(&target.get_triple());
        module.set_data_layout(&target.get_target_data().get_data_layout());

        let builder = ctx.create_builder();
        let ret = IRGen { context: ctx, module, builder };
        ret.setup_builtin_decls();
        ret
    }

    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    pub fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    fn setup_builtin_decls(&self) {
        self.declare_function(
            "printf", 
            &self.context.i32_type(), 
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true
        );

        self.declare_function(
            "rand", 
            &self.context.i32_type(), 
            &[], 
            false
        );
    }

    pub fn gen_program(
        &self,
        decls: &[TopLevelDecl]
    ) {
        for decl in decls {
            match decl {
                TopLevelDecl::Fn(decl) => {
                    self.gen_function(decl);
                }
                TopLevelDecl::Var(decl) => {
                    self.gen_global_var(decl);
                }
            }
        }
    }

    pub fn gen_global_var(
        &self,
        decl: &VarDecl
    ) -> GlobalValue<'ctx> {
        let initializer = self.gen_expr_non_void(decl.initializer());
        let ret = self.gen_global_var_impl(
            decl.name(), 
            self.context.i32_type(), 
            &initializer
        );

        ret
    }

    fn declare_global_var<T: BasicType<'ctx>>(
        &self,
        name: &str, 
        ty: T
    ) -> GlobalValue<'ctx> {
        let ret = self.module.add_global(
            ty, 
            None, 
            name
        );
        ret.set_linkage(Linkage::External);
        ret
    }

    fn gen_global_var_impl<T: BasicType<'ctx>>(
        &self,
        name: &str, 
        ty: T,
        initializer: &BasicValueEnum<'ctx>
    ) -> GlobalValue<'ctx> {
        let ret = self.declare_global_var(
            name, 
            ty
        );

        ret.set_initializer(initializer);
        ret
    }
    
    fn declare_function<T: BasicType<'ctx>>(
        &self, 
        name: &str, 
        ret_type: &T, 
        param_types: &[BasicMetadataTypeEnum<'ctx>],
        is_var_arg: bool
    ) -> FunctionValue<'ctx> {
        let fn_type = ret_type.fn_type(param_types, is_var_arg);
        self.module.add_function(name, fn_type, Some(Linkage::External))
    }

    pub fn gen_local_decl(
        &self,
        decl: &LocalDecl
    ) {
        match decl {
            LocalDecl::Var(var_decl) => {
                self.gen_var(var_decl);
            }
        }
    }

    pub fn gen_var(
        &self,
        decl: &VarDecl
    ) -> PointerValue<'ctx> {
        let alloca = self.builder.build_alloca(
            self.context.i32_type(), 
            decl.name()
        )
        .unwrap();

        let initializer = self.gen_expr_non_void(decl.initializer());
        let _ = self.builder.build_store(alloca, initializer);

        alloca
    }

    pub fn gen_function(
        &self,
        decl: &FnDecl
    ) -> FunctionValue<'ctx> {
        let func = self.declare_function(
            decl.name(),
             &self.context.i32_type(), 
            &[],
            false
        );

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);

        for stmt in decl.body() {
            self.gen_stmt(stmt);
        }

        func
    }

    pub fn gen_stmt(
        &self,
        stmt: &Stmt
    ) {
        match stmt {
            Stmt::Return(return_stmt) => {
                if let Some(expr) = return_stmt.returned() {
                    let ret = self.gen_expr(expr);
                    self.builder.build_return(Some(&ret.unwrap())).unwrap();
                }
                else { 
                    self.builder.build_return(None).unwrap();
                }
            }
            Stmt::Expr(expr) => {
                let _ = self.gen_expr(expr);
            }
            Stmt::LocalDecl(decl) => {
                self.gen_local_decl(decl);
            }
        }
    }

    pub fn gen_expr_non_void(
        &self,
        expr: &Expr
    ) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Int(i) => 
                self.context.i32_type().const_int(*i as u64, true).into(),

            Expr::Str(s) => {
                let global_str = self.module.add_global(
                    self.context.i8_type().array_type(s.len() as u32 + 1), 
                    None, 
                    "msg"
                );
                global_str.set_initializer(&self.context.const_string(s.as_bytes(), true));
                global_str.as_pointer_value().into()
            }

            _ => self.gen_expr(expr).unwrap()
        }
    }

    // helper, only for expressions that can be void
    fn gen_expr_voidable_only(
        &self,
        expr: &Expr
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Call(call) => {
                let callee = call.callee();
                let mut arg_values: Vec<BasicMetadataValueEnum> = Vec::new();

                for arg in call.args() {
                    // obviously, we cannot call a function with an expression with value type void
                    // frontend must catch this
                    arg_values.push(self.gen_expr_non_void(arg).into());
                }

                let fn_value = self.module.get_function(callee).unwrap();
                // calling a function can result in void type
                match self.builder.build_call(fn_value, &arg_values.as_slice(), "")
                    .unwrap().try_as_basic_value() {
                        Either::Left(v) => Some(v),
                        Either::Right(_) => None
                    }
            }

            _ => panic!("expression type can not be void, use gen_expr_non_void() for it"),
        }
    }

    // this function returns None if the expr has a void type
    pub fn gen_expr(
        &self,
        expr: &Expr
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Int(_) | Expr::Str(_) => 
                Some(self.gen_expr_non_void(expr)),

            Expr::Call(_) => 
                self.gen_expr_voidable_only(expr),
        }
    }
}
