use std::collections::HashMap;

use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValue, BasicValueEnum, FunctionValue, IntValue, PointerValue},
    AddressSpace,
};

use crate::{
    anyhow_500, bail_500, bail_type_err,
    compiler::{
        parser::{self, CompileResult},
        CompileOpts,
    },
    signature::Signature,
};

use super::{
    super::{
        parser::python_ast::{Arg, BinOp, ConstantAST, ExpressionAST, FunctionAST, StatementAST},
        Type,
    },
    Typed,
};

/// This is currently for only one function
pub struct CodeGen<'ctx, 'm> {
    context: &'ctx Context,
    module: &'m Module<'ctx>,
    builder: Builder<'ctx>,

    /// Variables in the function
    variables: HashMap<String, VarData<'ctx>>,

    tmp_var_counter: u64,
}

#[derive(Debug, Clone, Copy)]
enum VarData<'ctx> {
    Const(Typed<IntValue<'ctx>, IntValue<'ctx>>),
    Var(Typed<PointerValue<'ctx>, PointerValue<'ctx>>),
}

#[derive(Debug, Clone, Copy)]
struct Expr<'ctx>(pub Typed<IntValue<'ctx>, IntValue<'ctx>>);

impl<'ctx> From<Typed<IntValue<'ctx>, IntValue<'ctx>>> for Expr<'ctx> {
    fn from(val: Typed<IntValue<'ctx>, IntValue<'ctx>>) -> Self {
        Self(val)
    }
}

impl<'ctx> Expr<'ctx> {
    fn into_dyn(&self) -> &dyn BasicValue<'ctx> {
        match &self.0 {
            Typed::I64(val) => val,
            Typed::Bool(val) => val,
        }
    }
}

impl<'ctx, 'm> CodeGen<'ctx, 'm> {
    fn new_tmp_var_name(&mut self) -> String {
        let name = format!("_tmp_var_{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;
        name
    }

    pub fn new(context: &'ctx Context, module: &'m Module<'ctx>) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            variables: HashMap::new(),
            tmp_var_counter: 0,
        }
    }

    fn jit_compile_expr(&mut self, val: ExpressionAST) -> CompileResult<Expr<'ctx>> {
        match val {
            ExpressionAST::BinOp(lhs, op, rhs) => {
                let lhs = self.jit_compile_expr(*lhs)?;
                let rhs = self.jit_compile_expr(*rhs)?;
                let name = self.new_tmp_var_name();

                Ok(Expr(match (lhs.0, op, rhs.0) {
                    (Typed::I64(lhs), BinOp::Add, Typed::I64(rhs)) => {
                        Typed::I64(self.builder.build_int_add(lhs, rhs, &name)?)
                    }
                    (Typed::I64(lhs), BinOp::Sub, Typed::I64(rhs)) => {
                        Typed::I64(self.builder.build_int_sub(lhs, rhs, &name)?)
                    }
                    (Typed::Bool(_), BinOp::Add | BinOp::Sub, _) => {
                        bail_type_err!("Add/Sub not supported for bool")
                    }
                    (_, BinOp::Add | BinOp::Sub, Typed::Bool(_)) => {
                        bail_type_err!("Add/Sub not supported for bool")
                    }
                }))
            }
            ExpressionAST::Name(name) => {
                // only i64 for now. load var
                let data = self
                    .variables
                    .get(&name)
                    .copied()
                    .ok_or_else(|| anyhow_500!("Unknown variable: {}", name))?;

                Ok(Expr(match data {
                    VarData::Const(val) => val,
                    VarData::Var(Typed::I64(ptr)) => {
                        let tmp_var = self.new_tmp_var_name();
                        let ty = self.context.i64_type();
                        let BasicValueEnum::IntValue(value) =
                            self.builder.build_load(ty, ptr, &tmp_var)?
                        else {
                            bail_500!("Expected IntValue, this is a bug");
                        };
                        Typed::I64(value)
                    }
                    VarData::Var(Typed::Bool(ptr)) => {
                        let tmp_var = self.new_tmp_var_name();
                        let ty = self.context.bool_type();
                        let BasicValueEnum::IntValue(value) =
                            self.builder.build_load(ty, ptr, &tmp_var)?
                        else {
                            bail_500!("Expected IntValue, this is a bug");
                        };
                        Typed::Bool(value)
                    }
                }))
            }
            ExpressionAST::Constant(const_) => {
                match const_ {
                    ConstantAST::I64(val) => {
                        // idk about sign_extend, but we are passing in 64 bits.
                        // even though it says u64, it still does it signed
                        Ok(Expr(Typed::I64(
                            self.context.i64_type().const_int(val as u64, false),
                        )))
                    }
                    ConstantAST::Bool(val) => Ok(Expr(Typed::Bool(
                        self.context.bool_type().const_int(val as u64, false),
                    ))),
                }
            }
            ExpressionAST::UnaryOp(op, val) => {
                let val = self.jit_compile_expr(*val)?;
                let name = self.new_tmp_var_name();

                Ok(Expr(match (val.0, op) {
                    (Typed::I64(val), parser::python_ast::UnaryOp::USub) => {
                        Typed::I64(self.builder.build_int_neg(val, &name)?)
                    }
                    (Typed::Bool(_), parser::python_ast::UnaryOp::USub) => {
                        bail_type_err!("Unary minus on bool not supported")
                    }
                }))
            }
        }
    }

    fn jit_compile_stmt(&mut self, val: StatementAST, return_type: Type) -> CompileResult<()> {
        match val {
            StatementAST::Return(val) => {
                let val = val.map(|val| self.jit_compile_expr(val)).transpose()?;

                let val = match val {
                    Some(val) => match (return_type, val.0) {
                        (Type::I64, Typed::I64(_)) => val,
                        (Type::Bool, Typed::Bool(_)) => val,
                        (ret_ty, val) => bail_type_err!(ret_ty, val.into()),
                    },
                    None => Expr(match return_type {
                        Type::I64 => Typed::I64(self.context.i64_type().const_zero()).into(),
                        Type::Bool => Typed::Bool(self.context.bool_type().const_zero()).into(),
                    }),
                };

                self.builder.build_return(Some(val.into_dyn()))?;
            }
            StatementAST::Assign { target, value } => {
                let existing_var = self.variables.get(&target).cloned();

                let value = self.jit_compile_expr(value)?;

                match existing_var {
                    Some(var) => match var {
                        VarData::Const(_) => {
                            bail_500!("Cannot assign to a constant. This is a bug")
                        }
                        VarData::Var(Typed::I64(ptr)) => {
                            let Ok(value) = value.0.into_i64() else {
                                bail_type_err!(Type::I64, value.0.into())
                            };

                            self.builder.build_store(ptr, value)?;
                        }
                        VarData::Var(Typed::Bool(ptr)) => {
                            let Ok(value) = value.0.into_bool() else {
                                bail_type_err!(Type::Bool, value.0.into())
                            };

                            self.builder.build_store(ptr, value)?;
                        }
                    },
                    None => {
                        let typed = match value.0 {
                            Typed::I64(value) => {
                                let ptr = self
                                    .builder
                                    .build_alloca(self.context.i64_type(), &target)?;
                                self.builder.build_store(ptr, value)?;
                                Typed::I64(ptr)
                            }
                            Typed::Bool(value) => {
                                let ptr = self
                                    .builder
                                    .build_alloca(self.context.bool_type(), &target)?;
                                self.builder.build_store(ptr, value)?;
                                Typed::Bool(ptr)
                            }
                        };

                        self.variables.insert(target, VarData::Var(typed));
                    }
                }
            }
        }

        Ok(())
    }

    fn jit_compile_body(
        &mut self,
        body: Vec<StatementAST>,
        return_type: Type,
    ) -> CompileResult<()> {
        for stmt in body {
            self.jit_compile_stmt(stmt, return_type)?;
        }

        Ok(())
    }

    fn jit_compile_args(
        &mut self,
        args: Vec<Arg>,
        function: &FunctionValue<'ctx>,
        code_analysis: &CodeAnalysis,
    ) -> CompileResult<()> {
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();

        for (i, arg) in args.into_iter().enumerate() {
            let var_info = code_analysis
                .variable_info
                .get(&arg.arg)
                .ok_or_else(|| anyhow_500!("Variable info not found. This is a bug"))?;

            let raw_val = function
                .get_nth_param(i as u32)
                .ok_or_else(|| anyhow_500!("Argument count off. This is a bug"))?
                .into_pointer_value();

            let x_ptr = match arg.type_ {
                Type::I64 => Typed::I64(raw_val),
                Type::Bool => Typed::Bool(raw_val),
            };

            if var_info.is_assigned {
                let name = arg.arg.clone();
                self.variables.insert(name, VarData::Var(x_ptr));
            } else {
                let tmp_var = self.new_tmp_var_name();
                self.variables.insert(
                    arg.arg,
                    VarData::Const(match arg.type_ {
                        Type::I64 => Typed::I64(
                            self.builder
                                .build_load(
                                    i64_type,
                                    x_ptr.into_i64().expect("This is a bug"),
                                    &tmp_var,
                                )?
                                .into_int_value(),
                        ),
                        Type::Bool => Typed::Bool(
                            self.builder
                                .build_load(
                                    bool_type,
                                    x_ptr.into_bool().expect("This is a bug"),
                                    &tmp_var,
                                )?
                                .into_int_value(),
                        ),
                    }),
                );
            }
        }

        Ok(())
    }

    pub fn jit_compile_function(
        mut self,
        func: FunctionAST,
        compile_opts: CompileOpts,
    ) -> CompileResult<(FunctionValue<'ctx>, Signature)> {
        let i64_type = self.context.i64_type();
        let bool_type = self.context.bool_type();
        // let ptr_type = self.context.ptr_type(6);

        // Context::ptr
        let ptr_type = self.context.ptr_type(AddressSpace::default());

        let code_analysis = CodeAnalysis::analyze_function(&func);

        let FunctionAST {
            name,
            args,
            body,
            return_type,
        } = func;

        let mut arg_types_llvm = vec![];
        let mut arg_types = vec![];
        for arg in &args {
            arg_types_llvm.push(ptr_type.into());
            arg_types.push(arg.type_)
        }

        let sig = Signature::new(arg_types, return_type);

        let fn_type = match return_type {
            Type::I64 => i64_type.fn_type(&arg_types_llvm, false),
            Type::Bool => bool_type.fn_type(&arg_types_llvm, false),
        };

        let function = self.module.add_function(&name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        self.jit_compile_args(args, &function, &code_analysis)?;

        self.jit_compile_body(body, return_type)?;

        if compile_opts.dump_ir {
            eprintln!("");
            eprintln!(">>> IR for function {}", name);
            function.print_to_stderr();
            eprintln!("<<<");
            eprintln!("");
        }

        Ok((function, sig))
    }
}

struct CodeAnalysis {
    pub variable_info: HashMap<String, VariableInfo>,
}

#[derive(Debug)]
struct VariableInfo {
    /// false until proven otherwise
    is_assigned: bool,
}

impl CodeAnalysis {
    fn analyze_function(func: &FunctionAST) -> Self {
        let FunctionAST { args, body, .. } = func;

        let mut variable_info: HashMap<String, VariableInfo> = HashMap::new();

        for arg in args {
            variable_info.insert(arg.arg.clone(), VariableInfo { is_assigned: false });
        }

        for stmt in body {
            match stmt {
                StatementAST::Assign { target, .. } => {
                    if let Some(var) = variable_info.get_mut(target) {
                        var.is_assigned = true;
                    } else {
                        variable_info.insert(target.clone(), VariableInfo { is_assigned: true });
                    }
                }
                StatementAST::Return(_) => {}
            }
        }

        Self { variable_info }
    }
}
