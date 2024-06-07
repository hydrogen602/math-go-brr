use anyhow::{anyhow, bail};
pub use inkwell::execution_engine::JitFunction;
use python_ast::Function;
use python_ast_json::PyJsonNode;

pub mod ast_to_llvm;
pub mod llvm;
pub mod python_ast;
pub mod python_ast_json;
mod util;

pub fn parse(py_ast_json: &str) -> anyhow::Result<Function> {
    let py_ast = python_ast_json::PyJsonNode::load_from_str(py_ast_json)?;
    let func = python_ast::find_functions_in_module(py_ast)
        .into_iter()
        .next()
        .ok_or_else(|| anyhow!("No function in given code"))?;

    let func = match func {
        func @ PyJsonNode::FunctionDef { .. } => func,
        _ => bail!("Expected function, got {:?}", func),
    };

    let func = python_ast::translate_func(func)?;

    Ok(func)
}

// pub fn func_to_llvm(func: Function) -> anyhow::Result<()> {
//   let context = Context::create();
//   let module = context.create_module("test1");
//   let execution_engine = module
//       .create_jit_execution_engine(OptimizationLevel::None)
//       .err_convert()?;
//   let mut codegen = CodeGen {
//       context: &context,
//       module,
//       builder: context.create_builder(),
//       execution_engine,
//       variables: HashMap::new(),
//       tmp_var_counter: 0,
//   };

//   let f_name = func.name.clone();

//   codegen.jit_compile_function(func)?;

//   let Some(f): Option<JitFunction<SumFunc>> =
//       (unsafe { codegen.execution_engine.get_function(&f_name).ok() })
//   else {
//       bail!("Function not found")
//   };

//   let a = 1i64;
//   let b = 2i64;

//   let result = unsafe { f.call(a, b) };

//   println!("{} + {} = {}", a, b, result);

//   Ok(())
// }
