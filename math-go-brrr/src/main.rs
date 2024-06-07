use math_go_brrr::{ast_to_llvm, python_ast, python_ast_json};

fn main() -> anyhow::Result<()> {
    let py_ast = python_ast_json::PyJsonNode::load_from_file("tmp.json")?;
    let func = python_ast::find_functions_in_module(py_ast)
        .into_iter()
        .next()
        .unwrap();

    let func = python_ast::translate_func(func)?;

    ast_to_llvm::func_to_llvm(func)?;

    Ok(())
}
