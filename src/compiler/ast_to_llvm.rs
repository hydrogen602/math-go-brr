use std::collections::HashMap;

use anyhow::{anyhow, bail};
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{BasicValueEnum, FunctionValue, IntValue, PointerValue},
};

use crate::signature::Signature;

use super::{
    parser::python_ast::{Arg, BinOp, ConstantAST, ExpressionAST, FunctionAST, StatementAST},
    ArgType,
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
    ConstInt(IntValue<'ctx>),
    VarInt(PointerValue<'ctx>),
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

    fn jit_compile_expr(&mut self, val: ExpressionAST) -> anyhow::Result<IntValue<'ctx>> {
        match val {
            ExpressionAST::BinOp(lhs, op, rhs) => {
                let lhs = self.jit_compile_expr(*lhs)?;
                let rhs = self.jit_compile_expr(*rhs)?;
                let name = self.new_tmp_var_name();

                Ok(match op {
                    BinOp::Add => self.builder.build_int_add(lhs, rhs, &name)?,
                    BinOp::Sub => self.builder.build_int_sub(lhs, rhs, &name)?,
                })
            }
            ExpressionAST::Name(name) => {
                // only i64 for now. load var
                let data = self
                    .variables
                    .get(&name)
                    .copied()
                    .ok_or_else(|| anyhow!("Unknown variable: {}", name))?;

                match data {
                    VarData::ConstInt(val) => Ok(val),
                    VarData::VarInt(ptr) => Ok({
                        let tmp_var = self.new_tmp_var_name();
                        let ty = self.context.i64_type();
                        let BasicValueEnum::IntValue(value) =
                            self.builder.build_load(ty, ptr, &tmp_var)?
                        else {
                            bail!("Expected IntValue, this is a bug");
                        };
                        value
                    }),
                }
            }
            ExpressionAST::Constant(ConstantAST::I64(val)) => {
                // idk about sign_extend, but we are passing in 64 bits.
                // even though it says u64, it still does it signed
                Ok(self.context.i64_type().const_int(val as u64, false))
            }
            ExpressionAST::UnaryOp(op, val) => {
                let val = self.jit_compile_expr(*val)?;
                let name = self.new_tmp_var_name();

                Ok(match op {
                    super::parser::python_ast::UnaryOp::USub => {
                        self.builder.build_int_neg(val, &name)?
                    }
                })
            }
        }
    }

    fn jit_compile_stmt(
        &mut self,
        val: StatementAST,
        code_analysis: &CodeAnalysis,
    ) -> anyhow::Result<()> {
        match val {
            StatementAST::Return(val) => {
                let val = match val {
                    Some(val) => self.jit_compile_expr(val)?,
                    None => self.context.i64_type().const_zero(),
                };

                self.builder.build_return(Some(&val))?;
            }
            StatementAST::Assign { target, value } => {
                let existing_var = self.variables.get(&target).cloned();

                let value = self.jit_compile_expr(value)?;

                match existing_var {
                    Some(var) => match var {
                        VarData::ConstInt(_) => {
                            bail!("Cannot assign to a constant. This is a bug")
                        }
                        VarData::VarInt(ptr) => {
                            self.builder.build_store(ptr, value)?;
                        }
                    },
                    None => {
                        // TODO: only supports i64 for now
                        let ptr = self
                            .builder
                            .build_alloca(self.context.i64_type(), &target)?;
                        self.builder.build_store(ptr, value)?;

                        self.variables.insert(target, VarData::VarInt(ptr));
                    }
                }
            }
        }

        Ok(())
    }

    fn jit_compile_body(
        &mut self,
        body: Vec<StatementAST>,
        code_analysis: &CodeAnalysis,
    ) -> anyhow::Result<()> {
        for stmt in body {
            self.jit_compile_stmt(stmt, &code_analysis)?;
        }

        Ok(())
    }

    fn jit_compile_args(
        &mut self,
        args: Vec<Arg>,
        function: &FunctionValue<'ctx>,
        code_analysis: &CodeAnalysis,
    ) -> anyhow::Result<()> {
        for (i, arg) in args.into_iter().enumerate() {
            let x = function
                .get_nth_param(i as u32)
                .ok_or_else(|| anyhow!("Argument count off"))?
                .into_int_value();

            let var_info = code_analysis
                .variable_info
                .get(&arg.arg)
                .ok_or_else(|| anyhow!("Variable info not found. This is a bug"))?;

            if var_info.is_assigned {
                let name = arg.arg.clone();
                let ptr = match arg.type_ {
                    ArgType::I64 => {
                        let ptr = self
                            .builder
                            .build_alloca(self.context.i64_type(), &arg.arg)?;
                        self.builder.build_store(ptr, x)?;
                        ptr
                    }
                };
                self.variables.insert(name, VarData::VarInt(ptr));
            } else {
                self.variables.insert(arg.arg, VarData::ConstInt(x));
            }
        }

        Ok(())
    }

    pub fn jit_compile_function(
        mut self,
        func: FunctionAST,
        compile_opts: super::CompileOpts,
    ) -> anyhow::Result<(FunctionValue<'ctx>, Signature)> {
        let i64_type = self.context.i64_type();

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
            arg_types_llvm.push(match arg.type_ {
                ArgType::I64 => i64_type.into(),
            });
            arg_types.push(arg.type_)
        }

        let sig = Signature::new(arg_types, return_type);

        let fn_type = match return_type {
            ArgType::I64 => i64_type.fn_type(&arg_types_llvm, false),
        };

        let function = self.module.add_function(&name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        self.jit_compile_args(args, &function, &code_analysis)?;

        self.jit_compile_body(body, &code_analysis)?;

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
    /// rn, we only know it for args
    type_: Option<ArgType>,
}

impl CodeAnalysis {
    fn analyze_function(func: &FunctionAST) -> Self {
        let FunctionAST { args, body, .. } = func;

        let mut variable_info: HashMap<String, VariableInfo> = HashMap::new();

        for arg in args {
            variable_info.insert(
                arg.arg.clone(),
                VariableInfo {
                    is_assigned: false,
                    type_: Some(arg.type_),
                },
            );
        }

        for stmt in body {
            match stmt {
                StatementAST::Assign { target, .. } => {
                    if let Some(var) = variable_info.get_mut(target) {
                        var.is_assigned = true;
                    } else {
                        variable_info.insert(
                            target.clone(),
                            VariableInfo {
                                is_assigned: true,
                                type_: None,
                            },
                        );
                    }
                }
                StatementAST::Return(_) => {}
            }
        }

        Self { variable_info }
    }
}
