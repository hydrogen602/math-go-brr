use anyhow::{anyhow, bail};
use parser::python_ast::FunctionAST;
use parser::python_ast_json::PyJsonNode;

mod gen_llvm;
pub mod llvm;
mod parser;
mod util;

pub use parser::python_ast::{ArgType, TypeToArg};

pub fn parse(py_ast_json: &str) -> anyhow::Result<FunctionAST> {
    let py_ast = PyJsonNode::load_from_str(py_ast_json)?;
    let func = parser::python_ast::find_functions_in_module(py_ast)
        .into_iter()
        .next()
        .ok_or_else(|| anyhow!("No function in given code"))?;

    let func = match func {
        func @ PyJsonNode::FunctionDef { .. } => func,
        _ => bail!("Expected function, got {:?}", func),
    };

    let func = parser::python_ast::translate_func(func)?;

    Ok(func)
}

#[derive(Debug, Clone, Copy)]
pub struct CompileOpts {
    pub dump_ir: bool,
}
