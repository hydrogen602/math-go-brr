use std::collections::HashMap;

use anyhow::{anyhow, bail};
use inkwell::{
    builder::Builder,
    context::Context,
    execution_engine::{ExecutionEngine, JitFunction},
    module::Module,
    values::{FunctionValue, IntValue},
    OptimizationLevel,
};

use crate::{
    python_ast::{Arg, BinOp, Expression, Function, Statement},
    util::Ext,
};

/// Convenience type alias for the `sum` function.
///
/// Calling this is innately `unsafe` because there's no guarantee it doesn't
/// do `unsafe` operations internally.
type SumFunc = unsafe extern "C" fn(i64, i64) -> i64;

pub struct CodeGen<'ctx, 'm> {
    context: &'ctx Context,
    module: &'m Module<'ctx>,
    builder: Builder<'ctx>,
    execution_engine: ExecutionEngine<'ctx>,

    variables: HashMap<String, IntValue<'ctx>>,

    tmp_var_counter: u64,
}

impl<'ctx, 'm> CodeGen<'ctx, 'm> {
    fn new_tmp_var_name(&mut self) -> String {
        let name = format!("_tmp_var_{}", self.tmp_var_counter);
        self.tmp_var_counter += 1;
        name
    }

    pub fn new(
        context: &'ctx Context,
        module: &'m Module<'ctx>,
        execution_engine: ExecutionEngine<'ctx>,
    ) -> Self {
        Self {
            context,
            module,
            builder: context.create_builder(),
            execution_engine,
            variables: HashMap::new(),
            tmp_var_counter: 0,
        }
    }

    fn jit_compile_expr(&mut self, val: Expression) -> anyhow::Result<IntValue<'ctx>> {
        match val {
            Expression::BinOp(lhs, BinOp::Add, rhs) => {
                let lhs = self.jit_compile_expr(*lhs)?;
                let rhs = self.jit_compile_expr(*rhs)?;
                let name = self.new_tmp_var_name();

                Ok(self.builder.build_int_add(lhs, rhs, &name)?)
            }
            Expression::Name(name) => {
                // only i64 for now. load var
                Ok(self
                    .variables
                    .get(&name)
                    .cloned()
                    .ok_or_else(|| anyhow!("Unknown variable: {}", name))?)
            }
        }
    }

    fn jit_compile_stmt(&mut self, val: Statement) -> anyhow::Result<()> {
        match val {
            Statement::Return(val) => {
                let val = match val {
                    Some(val) => self.jit_compile_expr(val)?,
                    None => self.context.i64_type().const_zero(),
                };

                self.builder.build_return(Some(&val))?;
            }
        }

        Ok(())
    }

    fn jit_compile_body(&mut self, body: Vec<Statement>) -> anyhow::Result<()> {
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

    pub fn jit_compile_function(&mut self, func: Function) -> anyhow::Result<FunctionValue<'ctx>> {
        let i64_type = self.context.i64_type();

        let Function { name, args, body } = func;

        let mut arg_type = vec![];
        for _ in &args {
            arg_type.push(i64_type.into());
        }

        let fn_type = i64_type.fn_type(&arg_type, false);
        let function = self.module.add_function(&name, fn_type, None);
        let basic_block = self.context.append_basic_block(function, "entry");

        self.builder.position_at_end(basic_block);

        self.jit_compile_args(args, &function)?;

        self.jit_compile_body(body)?;

        Ok(function)
    }
}

pub fn func_to_llvm(func: Function) -> anyhow::Result<()> {
    let context = Context::create();
    let module = context.create_module("test_go_brrr");
    let execution_engine = module
        .create_jit_execution_engine(OptimizationLevel::None)
        .err_convert()?;
    let mut codegen = CodeGen {
        context: &context,
        module: &module,
        builder: context.create_builder(),
        execution_engine,
        variables: HashMap::new(),
        tmp_var_counter: 0,
    };

    let f_name = func.name.clone();

    codegen.jit_compile_function(func)?;

    let Some(f): Option<JitFunction<SumFunc>> =
        (unsafe { codegen.execution_engine.get_function(&f_name).ok() })
    else {
        bail!("Function not found")
    };

    let a = 1i64;
    let b = 2i64;

    let result = unsafe { f.call(a, b) };

    println!("{} + {} = {}", a, b, result);

    Ok(())
}
