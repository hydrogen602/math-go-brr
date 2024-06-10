use inkwell::{
    context::Context, execution_engine::ExecutionEngine, module::Module, OptimizationLevel,
};

use crate::signature::Signature;

use super::{ast_to_llvm::CodeGen, parser::python_ast::FunctionAST, util::Ext};

#[derive(Debug)]
/// Note: Context is not thread safe.
/// The docs say: "you should be careful to have one context per thread"
/// https://llvm.org/doxygen/classllvm_1_1LLVMContext.html#details
pub struct LLVMJitContext(Context);

pub struct LLVMModule<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub execution_engine: ExecutionEngine<'ctx>,
}

impl LLVMJitContext {
    pub fn new() -> Self {
        Self(Context::create())
    }
}

impl<'ctx> LLVMModule<'ctx> {
    pub fn new(context: &'ctx LLVMJitContext, module_name: &str) -> anyhow::Result<Self> {
        let module = context.0.create_module(module_name);
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
        CodeGen::new(self.context, &self.module)
    }

    pub fn compile_func(
        &self,
        func: FunctionAST,
        compile_opts: super::CompileOpts,
    ) -> anyhow::Result<Signature> {
        let codegen = self.new_codegen();

        let (_, sig) = codegen.jit_compile_function(func, compile_opts)?;

        Ok(sig)
    }
}
