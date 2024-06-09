use std::collections::HashMap;

use anyhow::anyhow;
use inkwell::{
    builder::Builder,
    context::Context,
    module::Module,
    values::{FunctionValue, IntValue},
};

use crate::signature::Signature;

use super::{
    parser::python_ast::{Arg, BinOp, ConstantAST, ExpressionAST, FunctionAST, StatementAST},
    ArgType,
};

pub struct CodeGen<'ctx, 'm> {
    context: &'ctx Context,
    module: &'m Module<'ctx>,
    builder: Builder<'ctx>,

    variables: HashMap<String, IntValue<'ctx>>,

    tmp_var_counter: u64,
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
                Ok(self
                    .variables
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| anyhow!("Unknown variable: {}", name))?)
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

    fn jit_compile_stmt(&mut self, val: StatementAST) -> anyhow::Result<()> {
        match val {
            StatementAST::Return(val) => {
                let val = match val {
                    Some(val) => self.jit_compile_expr(val)?,
                    None => self.context.i64_type().const_zero(),
                };

                self.builder.build_return(Some(&val))?;
            }
        }

        Ok(())
    }

    fn jit_compile_body(&mut self, body: Vec<StatementAST>) -> anyhow::Result<()> {
        for stmt in body {
            self.jit_compile_stmt(stmt)?;
        }

        Ok(())
    }

    fn jit_compile_args(
        &mut self,
        args: Vec<Arg>,
        function: &FunctionValue<'ctx>,
    ) -> anyhow::Result<()> {
        for (i, arg) in args.into_iter().enumerate() {
            let x = function
                .get_nth_param(i as u32)
                .ok_or_else(|| anyhow!("Argument count off"))?
                .into_int_value();
            self.variables.insert(arg.arg, x);
        }

        Ok(())
    }

    pub fn jit_compile_function(
        &mut self,
        func: FunctionAST,
        compile_opts: super::CompileOpts,
    ) -> anyhow::Result<(FunctionValue<'ctx>, Signature)> {
        let i64_type = self.context.i64_type();

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

        self.jit_compile_args(args, &function)?;

        self.jit_compile_body(body)?;

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
