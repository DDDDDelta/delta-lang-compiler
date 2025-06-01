use std::cell::RefCell;
use std::collections::HashMap;
use std::ffi::CString;
use std::fmt::Binary;
use std::ptr::{null, null_mut};
use std::rc::Rc;
use std::mem;

use either::Either;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::{AsContextRef, Context};
use inkwell::data_layout::DataLayout;
use inkwell::module::{ Linkage, Module };
use inkwell::targets::TargetMachine;
use inkwell::types::{ AnyTypeEnum, AsTypeRef, BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType, IntType };
use inkwell::values::{ 
    AnyValue, 
    AsValueRef, 
    BasicMetadataValueEnum, 
    BasicValue, 
    BasicValueEnum, 
    FunctionValue, 
    GlobalValue, 
    InstructionValue, 
    IntValue, 
    PointerValue 
};
use inkwell::AddressSpace;
use llvm_sys::core::{ 
    LLVMAppendBasicBlockInContext, 
    LLVMBuildICmp, 
    LLVMBuildLoad2, 
    LLVMGetBasicBlockParent, 
    LLVMGetNextBasicBlock, 
    LLVMInsertBasicBlockInContext 
};
use llvm_sys::prelude::LLVMBasicBlockRef;
use llvm_sys::LLVMIntPredicate;

use crate::ast::decl::{ Decl, FnDecl, LocalDecl, Named, NamedDecl, TopLevelDecl, VarDecl };
use crate::ast::expr_type::{ FnType, Type };
use crate::ast::stmt::{ Stmt, ReturnStmt };
use crate::ast::expr::{ BinaryExpr, BinaryOp, Expr, UnaryExpr, UnaryOp };

pub struct IRGen<'ctx> {
    context: &'ctx Context, 
    module: Module<'ctx>, 
    builder: Builder<'ctx>,
    globals: GlobalTracker<'ctx>,
}

#[derive(Debug)]
struct GlobalTracker<'ctx> {
    // TODO: refactor this bs, maybe make all IRGen methods mut
    pub globals: RefCell<Vec<(Decl, GlobalValue<'ctx>)>>,
    pub functions: RefCell<Vec<(Decl, FunctionValue<'ctx>)>>,
}

impl<'ctx> GlobalTracker<'ctx> {
    pub fn new() -> Self {
        GlobalTracker { globals: RefCell::new(Vec::new()), functions: RefCell::new(Vec::new()) }
    }

    pub fn add_global(&self, decl: Decl, value: GlobalValue<'ctx>) {
        self.globals.borrow_mut().push((decl, value));
    }

    pub fn add_function(&self, decl: Decl, value: FunctionValue<'ctx>) {
        self.functions.borrow_mut().push((decl, value));
    }

    pub fn get_global(&self, decl: &Decl) -> Option<GlobalValue<'ctx>> {
        if let Some(fdecl) = self.get_function(decl) {
            return Some(fdecl.as_global_value());
        }
        if let Some(vdecl) = self.get_global_var(decl) {
            return Some(vdecl);
        }

        None
    }

    pub fn get_function(&self, decl: &Decl) -> Option<FunctionValue<'ctx>> {
        for (d, v) in self.functions.borrow().iter() {
            if Decl::ptr_eq(d, decl) {
                return Some(*v);
            }
        }

        None
    }

    pub fn get_global_var(&self, decl: &Decl) -> Option<GlobalValue<'ctx>> {
        for (d, v) in self.globals.borrow().iter() {
            if Decl::ptr_eq(d, decl) {
                return Some(*v);
            }
        }

        None
    }
}

// tree structure for value tracking, must insure prev lives loneger than self
#[derive(Debug)]
pub struct LocalTracker<'ctx> {
    pub prev: *const LocalTracker<'ctx>,
    pub vars: Vec<(Decl, PointerValue<'ctx>)>,
}

impl<'ctx> LocalTracker<'ctx> {
    pub fn empty() -> Self {
        LocalTracker { prev: null(), vars: Vec::new() }
    }

    pub fn new(prev: &LocalTracker<'ctx>, curr_func: FunctionValue<'ctx>) -> Self {
        LocalTracker { prev, vars: Vec::new() }
    }

    pub fn add(&mut self, decl: Decl, value: PointerValue<'ctx>) {
        self.vars.push((decl, value));
    }

    pub fn get(&self, decl: &Decl) -> Option<PointerValue<'ctx>> {
        for (d, v) in &self.vars {
            if Decl::ptr_eq(d, decl) {
                return Some(*v);
            }
        }

        if self.prev != null() {
            unsafe { (*self.prev).get(decl) }
        }
        else {
            None
        }
    }
}

// builder returns Result::Error(BuilderError) mostly for unset position
// we generally assume we always set the position before building, so we can unwrap
impl<'ctx> IRGen<'ctx> {
    pub fn new(ctx: &'ctx Context, target: &TargetMachine, module_name: &str) -> Self {
        let module = ctx.create_module(module_name);
        module.set_triple(&target.get_triple());
        module.set_data_layout(&target.get_target_data().get_data_layout());

        let builder = ctx.create_builder();
        let ret = IRGen { 
            context: ctx, module, builder, 
            globals: GlobalTracker::new(),
        };
        ret.setup_builtin_decls();
        ret
    }

    pub fn module(&self) -> &Module<'ctx> {
        &self.module
    }

    pub fn builder(&self) -> &Builder<'ctx> {
        &self.builder
    }

    pub fn map_to_llvm_type(
        &self,  
        ty: &Type
    ) -> AnyTypeEnum<'ctx> {
        match ty {
            Type::Void => self.context.void_type().into(),
            Type::I32 => self.context.i32_type().into(),
            Type::I8 => self.context.i8_type().into(),
            Type::Bool => self.context.bool_type().into(),
            Type::Ptr(_) => self.context.ptr_type(AddressSpace::default()).into(),
            Type::Fn(fnty) => {
                let ret_type = self.map_to_llvm_type(fnty.ret_ty());

                let param_types = fnty.param_ty().iter()
                    .map(
                        |p| 
                            self.map_to_llvm_type(p).try_into()
                                .expect("function parameter type must be a basic metadata type")
                    )
                    .collect::<Vec<_>>();
                

                let Ok(ret): Result<BasicTypeEnum, _> = ret_type.try_into() else {
                    // only possible return type is void if the function is not returning basic type
                    return ret_type.into_void_type().fn_type(param_types.as_slice(), false).into();                   
                };

                ret.fn_type(param_types.as_slice(), false).into()
            }
        }
    }

    fn setup_builtin_decls(&self) {
        self.declare_function(
            "printf", 
            &self.context.i32_type(), 
            &[self.context.ptr_type(AddressSpace::default()).into()],
            true
        );
    }

    pub fn gen_program(
        &self,
        decls: &[TopLevelDecl]
    ) {
        for decl in decls {
            match decl {
                TopLevelDecl::Fn(fn_decl) => {
                    let func = self.gen_function(fn_decl);
                    self.globals.add_function(
                        decl.clone().into(), 
                        func
                    );
                }
                TopLevelDecl::Var(var_decl) => {
                    let value = self.gen_global_var(var_decl);
                    self.globals.add_global(
                        decl.clone().into(), 
                        value
                    );
                }
            }
        }
    }

    pub fn gen_global_var(
        &self,
        decl: &VarDecl
    ) -> GlobalValue<'ctx> {
        let initializer = self.gen_expr_non_void(
            decl.initializer(), 
            &LocalTracker::empty() // no local value needed to be tracked for global vars
        );
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

    fn declare_with_fn_type(
        &self, 
        name: &str, 
        fn_ty: FunctionType<'ctx>,
        is_var_arg: bool
    ) -> FunctionValue<'ctx> {
        self.module.add_function(name, fn_ty, Some(Linkage::External))
    }

    pub fn gen_local_decl(
        &self,
        decl: &LocalDecl,
        tracker: &mut LocalTracker<'ctx>
    ) {
        match decl {
            LocalDecl::Var(var_decl) => {
                self.gen_var(var_decl, var_decl.clone(), tracker);
            }
        }
    }

    pub fn gen_var(
        &self,
        decl: &VarDecl,
        decl_enum: Rc<VarDecl>,
        tracker: &mut LocalTracker<'ctx>
    ) -> PointerValue<'ctx> {
        let alloca = self.gen_alloca_store(
            // vardecl must declare a basic type
            self.map_to_llvm_type(decl.ty()).try_into().unwrap(),
            decl.name(),
            // initializer must be a non-void expression
            self.gen_expr_non_void(decl.initializer(), tracker)
        );

        tracker.add(Decl::Var(decl_enum), alloca);

        alloca
    }

    pub fn gen_alloca_store(
        &self,
        decl: BasicTypeEnum<'ctx>,
        name: &str,
        value: BasicValueEnum<'ctx>,
    ) -> PointerValue<'ctx> {
        let alloca = self.builder.build_alloca(
            decl, 
            name
        ).unwrap();

        self.builder.build_store(alloca, value).unwrap();
        alloca
    }

    pub fn gen_function(
        &self,
        decl: &FnDecl
    ) -> FunctionValue<'ctx> {
        let fn_ty = self.map_to_llvm_type(
            &Type::Fn(Box::new(decl.ty().clone()))
        ).into_function_type();

        // TODO: support vararg functions
        let func = self.declare_with_fn_type(
            decl.name(),
            fn_ty,
            false
        );

        let mut tracker = LocalTracker::empty();

        let entry = self.context.append_basic_block(func, "entry");
        self.builder.position_at_end(entry);

        for (i, param) in func.get_params().iter().enumerate() {
            let astdecl = &decl.params()[i];
            param.set_name(astdecl.name());
            
            tracker.add(
                Decl::Param(astdecl.clone()), 
                self.gen_alloca_store(
                    param.get_type(), 
                    astdecl.name(), 
                    param.clone()
                )
            );
        }

        for stmt in decl.body().as_ref().unwrap() {
            self.gen_stmt(stmt, &mut tracker);
        }

        func
    }

    pub fn gen_stmt(
        &self,
        stmt: &Stmt,
        tracker: &mut LocalTracker<'ctx>
    ) {
        match stmt {
            Stmt::Return(return_stmt) => {
                if let Some(expr) = return_stmt.returned() {
                    let ret = self.gen_expr(expr, tracker);
                    self.builder.build_return(Some(&ret.unwrap())).unwrap();
                }
                else { 
                    // this returns void
                    self.builder.build_return(None).unwrap();
                }
            }
            Stmt::Expr(expr) => {
                let _ = self.gen_expr(expr, tracker);
            }
            Stmt::LocalDecl(decl) => {
                self.gen_local_decl(decl, tracker);
            }
            Stmt::Print(print_stmt) => {
                let format = self.gen_expr_non_void(
                    print_stmt.format(), 
                    tracker
                );
                let args = print_stmt.args()
                    .iter()
                    .map(|arg| self.gen_expr_non_void(arg, tracker))
                    .collect::<Vec<_>>();

                let mut arg_values: Vec<BasicMetadataValueEnum<'ctx>> = Vec::new();
                arg_values.push(format.into());

                for arg in args {
                    arg_values.push(arg.into());
                }

                let fn_value = self.module.get_function("printf")
                    .expect("this should be a builtin function");
                
                let _ = self.builder.build_call(
                    fn_value, 
                    &arg_values.as_slice(), 
                    "printf_res"
                ).unwrap();
            }
        }
    }

    fn gen_binary_expr(
        &self,
        expr: &BinaryExpr,
        tracker: &LocalTracker<'ctx>
    ) -> BasicValueEnum<'ctx> {
        match expr.op() {
            BinaryOp::LAnd | BinaryOp::LOr => {
                let short_on_lhs = matches!(expr.op(), BinaryOp::LAnd);

                let v = self.gen_short_circuit(expr.lhs(), expr.rhs(), short_on_lhs, tracker);
                v.into()
            }
            BinaryOp::Eq => {
                assert!(expr.lhs().ty() == expr.rhs().ty());
                let lhs = self.gen_expr_non_void(expr.lhs(), tracker);
                let rhs = self.gen_expr_non_void(expr.rhs(), tracker);
                if matches!(expr.lhs().ty(), Type::Ptr(_)) &&
                   matches!(expr.rhs().ty(), Type::Ptr(_)) {
                    // pointer equality
                    unsafe {
                        // fucking inkwell does not allow me to use icmp on pointers wtf
                        BasicValueEnum::new(LLVMBuildICmp(self.builder.as_mut_ptr(), 
                            LLVMIntPredicate::LLVMIntEQ, 
                            lhs.into_pointer_value().as_value_ref(), 
                            rhs.into_pointer_value().as_value_ref(), 
                            CString::new("eq").unwrap().as_ptr()
                        ))
                    }
                }
                else {
                    self.builder.build_int_compare(
                        inkwell::IntPredicate::EQ, 
                        lhs.into_int_value(), 
                        rhs.into_int_value(), 
                        "eq"
                    ).unwrap().into()
                }
            }
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div | BinaryOp::Mod => {
                let ast_lhs = expr.lhs();
                let ast_rhs = expr.rhs();
                match (ast_lhs.ty(), ast_rhs.ty()) {
                    (Type::I32, Type::I32) => { 
                        self.gen_arith_binary_expr(expr, tracker)
                    },
                    _ => panic!("invalid binary expression types: {:?} and {:?}", ast_lhs.ty(), ast_rhs.ty()),
                }
            }
        }
    }

    fn insert_bb_after(
        &self, 
        name: &str,
        after: BasicBlock<'ctx>
    ) -> BasicBlock<'ctx> {
        let c_name = CString::new(name).unwrap();
        let next_bb = unsafe { LLVMGetNextBasicBlock(after.as_mut_ptr()) };
        let ret: LLVMBasicBlockRef;
        if next_bb != null_mut() {
            ret = unsafe {
                LLVMInsertBasicBlockInContext(
                    self.context.as_ctx_ref(), next_bb, 
                    c_name.as_ptr()
                )
            };
        }
        else {
            ret = unsafe {
                let func   = LLVMGetBasicBlockParent(after.as_mut_ptr());
                LLVMAppendBasicBlockInContext(
                    self.context.as_ctx_ref(), func, c_name.as_ptr()
                )
            };
        }

        // inkwell::basic_block::BasicBlock::new is private (╭∩╮ಠ益ಠ)🔥
        assert!(mem::size_of::<LLVMBasicBlockRef>() == mem::size_of::<BasicBlock>());
        unsafe { mem::transmute(ret) } // wtf
    }

    fn gen_short_circuit(
        &self,
        lhs: &Expr,
        rhs: &Expr,
        short: bool,
        tracker: &LocalTracker<'ctx>
    ) -> IntValue<'ctx> {
        let bool_ty  = self.context.bool_type();

        // this must be a bool
        let lhs_val = self.gen_expr_non_void(lhs, tracker).into_int_value();

        let parent = self.builder.get_insert_block().unwrap();

        let mut rhs_block = self.insert_bb_after("rhs", parent);
        let merge_block = self.insert_bb_after("merge", rhs_block);

        if short {
            // && : false -> merge  |  true -> rhs
            self.builder.build_conditional_branch(
                lhs_val, rhs_block, merge_block
            ).unwrap();
        } else {
            // || : true -> merge  |  false -> rhs
            self.builder.build_conditional_branch(
                lhs_val, merge_block, rhs_block
            ).unwrap();
        }

        self.builder.position_at_end(rhs_block);
        let rhs_val = self.gen_expr_non_void(rhs, tracker).into_int_value();
        self.builder.build_unconditional_branch(merge_block).unwrap();

        // retrieve the actual basic block of the generated value
        rhs_block = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_block);
        let phi = self.builder.build_phi(bool_ty, "sc_phi").unwrap();

        if short {
            phi.add_incoming(&[
                (&bool_ty.const_zero(), parent),     // short-circuit happened for &&, must be false
                (&rhs_val,              rhs_block),
            ]);
        }
        else {
            phi.add_incoming(&[
                (&bool_ty.const_all_ones(), parent), // short-circuit happened for ||, must be true
                (&rhs_val,                  rhs_block),
            ]);
        }

        phi.as_basic_value().into_int_value()
    }

    pub fn gen_arith_binary_expr(
        &self,
        expr: &BinaryExpr,
        tracker: &LocalTracker<'ctx>
    ) -> BasicValueEnum<'ctx> {
        let lhs = self.gen_expr_non_void(expr.lhs(), tracker);
        let rhs = self.gen_expr_non_void(expr.rhs(), tracker);
        let lhs = match lhs {
            BasicValueEnum::IntValue(v) => v,
            _ => panic!("expect an integer value for arithmetic binary expression")
        };
        let rhs = match rhs {
            BasicValueEnum::IntValue(v) => v,
            _ => panic!("expect an integer value for arithmetic binary expression")
        };

        let op = expr.op();

        match op {
            BinaryOp::Add => 
                self.builder.build_int_add(
                    lhs, rhs, "add"
                ).unwrap().into(),

            BinaryOp::Sub => 
                self.builder.build_int_sub(
                    lhs, rhs, "sub"
                ).unwrap().into(),

            BinaryOp::Mul => 
                self.builder.build_int_mul(
                    lhs, rhs, "mul"
                ).unwrap().into(),

            BinaryOp::Div => 
                self.builder.build_int_signed_div(
                    lhs, rhs, "div"
                ).unwrap().into(),

            BinaryOp::Mod =>
                self.builder.build_int_signed_rem(
                    lhs, rhs, "mod"
                ).unwrap().into(),
            
            _ => panic!("other cases should be handled separately"),
        }
    }

    pub fn gen_expr_non_void(
        &self,
        expr: &Expr,
        tracker: &LocalTracker<'ctx>
    ) -> BasicValueEnum<'ctx> {
        match expr {
            Expr::Int(i) => 
                self.context.i32_type().const_int(*i as u64, true).into(),

            Expr::Str(s) => {
                let global_str = self.module.add_global(
                    self.context.i8_type().array_type(s.len() as u32 + 1), 
                    None, 
                    "string_literal"
                );
                global_str.set_initializer(&self.context.const_string(s.as_bytes(), true));
                global_str.as_pointer_value().into()
            }

            Expr::Bool(b) => {
                let v = self.context.bool_type().const_int(*b as u64, false);
                v.set_name(b.to_string().as_str());
                v.into()
            }

            // support variable reference only
            Expr::DeclRef(decl) => {
                match decl {
                    // reference on vardecl is a lvalue represented as a pointer
                    Decl::Var(var_decl) => {
                        if let Some(value) = tracker.get(&Decl::Var(var_decl.clone())) {
                            return value.into();
                        }
                        else if let Some(value) = self.globals.get_global_var(decl) { 
                            return value.as_pointer_value().into();
                        }
                        else {
                            panic!("variable `{}` not found", var_decl.name());
                        }
                    }

                    Decl::Param(param_decl) => {
                        if let Some(value) = tracker.get(&Decl::Param(param_decl.clone())) {
                            return value.into();
                        }
                        else {
                            panic!("parameter `{}` not found", param_decl.name());
                        }
                    }
                    
                    Decl::Fn(_) => panic!("function reference not supported")
                }
            }

            Expr::RValueCast(cast) => {
                let operand = cast.operand();
                // expects a pointer value which is obviously non-void
                let value = self.gen_expr_non_void(operand, tracker);
                match value {
                    BasicValueEnum::PointerValue(v) => {
                        // rvalue cast represents a load from a pointer
                        // inkwell does not support constructing a load with typeenum wtf
                        let loaded = unsafe {
                            BasicValueEnum::new(LLVMBuildLoad2(
                                self.builder.as_mut_ptr(),
                                self.map_to_llvm_type(&cast.operand().ty()).as_type_ref(),
                                v.as_value_ref(),
                                CString::new("rvalue_cast").unwrap().as_ptr(),
                            ))
                        };
                        return loaded;
                    },
                    _ => panic!("expect a pointer value (represents an lvalue) for a rvalue cast"),
                }
            }

            Expr::Assign(assign) => {
                let lhs = assign.lhs();
                let rhs = assign.rhs();

                // expects a pointer value which is obviously non-void
                let value = self.gen_expr_non_void(lhs, tracker);
                match value {
                    BasicValueEnum::PointerValue(v) => {
                        let rhs_value = self.gen_expr_non_void(rhs, tracker);
                        let _ = self.builder.build_store(v, rhs_value);
                        return v.into();
                    },
                    _ => panic!("expect a pointer value (represents a lvalue) for an assignment"),
                }
            }

            Expr::Binary(binary) => {
                return self.gen_binary_expr(binary, tracker);
            }

            Expr::Unary(unary) => {
                let operand = unary.operand();
                let value = self.gen_expr_non_void(operand, tracker);
                match unary.op() {
                    UnaryOp::AddressOf | UnaryOp::Deref => {
                        return value.into();
                    }

                    UnaryOp::Pos => value, // no-op

                    UnaryOp::Neg => {
                        assert!(value.is_int_value());
                        return self.builder.build_int_neg(
                            value.into_int_value(), "neg"
                        ).unwrap().into()
                    },
                }
            }

            _ => self.gen_expr(expr, tracker).expect("this expression is assumed to be non-void"),
        }
    }

    // helper, only for expressions that can be void
    fn gen_expr_voidable_only(
        &self,
        expr: &Expr,
        tracker: &LocalTracker<'ctx>
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Call(call) => {
                let callee = call.callee();
                let mut arg_values: Vec<BasicMetadataValueEnum> = Vec::new();

                for arg in call.args() {
                    // obviously, we cannot call a function with an expression with value type void
                    // frontend must catch this
                    arg_values.push(self.gen_expr_non_void(arg, tracker).into());
                }

                let fn_value = self.module.get_function(
                    TryInto::<NamedDecl>::try_into(callee.as_declref().clone())
                        .expect("it must be a function decl ref").name()
                ).expect("frontend should assure this function exists");

                // calling a function can result in void type
                match self.builder.build_call(fn_value, &arg_values.as_slice(), "")
                    .unwrap().try_as_basic_value() {
                        Either::Left(v) => Some(v),
                        Either::Right(_) => None // the function returns void, TODO: check this
                    }
            }

            _ => panic!("expression type can not be void, use gen_expr_non_void() for it"),
        }
    }

    // this function returns None if the expr has a void type
    pub fn gen_expr(
        &self,
        expr: &Expr,
        tracker: &LocalTracker<'ctx>
    ) -> Option<BasicValueEnum<'ctx>> {
        match expr {
            Expr::Int(_) | Expr::Str(_) | Expr::DeclRef(_) | Expr::Unary(_) |
            Expr::RValueCast(_) | Expr::Assign(_) | Expr::Binary(_) | Expr::Bool(_) => 
                Some(self.gen_expr_non_void(expr, tracker)),

            Expr::Call(_) => 
                self.gen_expr_voidable_only(expr, tracker),
        }
    }
}
