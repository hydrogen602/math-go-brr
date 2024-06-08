use anyhow::anyhow;
use inkwell::{
    context::Context,
    execution_engine::{ExecutionEngine, UnsafeFunctionPointer},
    module::Module,
    OptimizationLevel,
};

use crate::{ast_to_llvm::CodeGen, parser::python_ast::Function, util::Ext};

#[derive(Debug)]
pub struct LLVMContext(Context);

pub struct LLVM<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl LLVMContext {
    pub fn new() -> Self {
        Self(Context::create())
    }
}

impl<'ctx> LLVM<'ctx> {
    pub fn new(context: &'ctx LLVMContext) -> anyhow::Result<Self> {
        let module = context.0.create_module("test_go_brrr");
        let execution_engine = module
            .create_jit_execution_engine(OptimizationLevel::Default)
            .err_convert()?;

        Ok(Self {
            context: &context.0,
            module,
            execution_engine,
        })
    }

    fn new_codegen<'m>(&'m self) -> CodeGen<'ctx, 'm> {
        CodeGen::new(self.context, &self.module, self.execution_engine.clone())
    }

    pub fn compile_func(
        &self,
        func: Function,
        compile_opts: super::CompileOpts,
    ) -> anyhow::Result<()> {
        let mut codegen = self.new_codegen();

        codegen.jit_compile_function(func, compile_opts)?;

        Ok(())
    }

    pub fn get_func<F: UnsafeFunctionPointer>(
        &self,
        name: &str,
    ) -> anyhow::Result<inkwell::execution_engine::JitFunction<F>> {
        let f = unsafe { self.execution_engine.get_function(name).ok() }
            .ok_or_else(|| anyhow!("Function not found"))?;

        Ok(f)
    }
}
