use anyhow::{bail, ensure};

use super::python_ast_json::PyJsonNode;

/// Stricter version (only what we can parse)
pub struct Function {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Vec<Statement>,
}

pub struct Arg {
    pub arg: String,
    pub annotation: Option<String>,
}

pub enum Statement {
    Return(Option<Expression>),
}

pub enum Expression {
    BinOp(Box<Expression>, BinOp, Box<Expression>),
    Name(String),
}

pub enum BinOp {
    Add,
    Sub,
}

pub fn find_functions_in_module(module: PyJsonNode) -> Vec<PyJsonNode> {
    let mut functions = vec![];

    match module {
        PyJsonNode::Module { body, .. } => {
            for node in body {
                match node {
                    func @ PyJsonNode::FunctionDef { .. } => {
                        functions.push(func);
                    }
                    _ => {}
                }
            }
        }
        _ => panic!("Expected module, got {:?}", module),
    }

    functions
}

pub fn translate_func(func: PyJsonNode) -> anyhow::Result<Function> {
    let PyJsonNode::FunctionDef {
        name, args, body, ..
    } = func
    else {
        bail!("Expected FunctionDef, got {:?}", func)
    };

    Ok(Function {
        name,
        args: translate_args(*args)?,
        body: translate_body(body)?,
    })
}

pub fn translate_expression(expr: PyJsonNode) -> anyhow::Result<Expression> {
    match expr {
        PyJsonNode::BinOp {
            left, op, right, ..
        } => Ok(Expression::BinOp(
            Box::new(translate_expression(*left)?),
            match *op {
                PyJsonNode::Add => BinOp::Add,
                PyJsonNode::Sub => BinOp::Sub,
                _ => bail!("Unsupported binop: {:?}", op),
            },
            Box::new(translate_expression(*right)?),
        )),
        PyJsonNode::Name { id, .. } => Ok(Expression::Name(id)),
        _ => bail!("Unsupported expression: {:?}", expr),
    }
}

pub fn translate_body(body: Vec<PyJsonNode>) -> anyhow::Result<Vec<Statement>> {
    let mut statements = vec![];

    for node in body {
        match node {
            PyJsonNode::Return { value, .. } => {
                statements.push(Statement::Return(match value {
                    Some(expr) => Some(translate_expression(*expr)?),
                    None => None,
                }));
            }
            _ => bail!("Unsupported statement: {:?}", node),
        }
    }

    Ok(statements)
}

pub fn translate_args(args: PyJsonNode) -> anyhow::Result<Vec<Arg>> {
    let PyJsonNode::Arguments {
        args,
        kwonlyargs,
        vararg,
        kwarg,
        defaults,
        kw_defaults,
        posonlyargs,
    } = args
    else {
        bail!("Expected Arguments, got {:?}", args)
    };

    ensure!(kwonlyargs.len() == 0, "kwonlyargs not supported yet");
    ensure!(defaults.len() == 0, "defaults not supported yet");
    ensure!(kw_defaults.len() == 0, "kw_defaults not supported yet");
    ensure!(posonlyargs.len() == 0, "posonlyargs not supported yet");
    ensure!(vararg.is_none(), "vararg not supported yet");
    ensure!(kwarg.is_none(), "kwarg not supported yet");

    let mut arg_names = vec![];

    for arg in args {
        let PyJsonNode::Arg {
            arg, annotation, ..
        } = arg
        else {
            panic!("Expected Arg, got {:?}", arg)
        };

        arg_names.push(Arg {
            arg,
            annotation: match annotation.as_deref() {
                Some(PyJsonNode::Name { id, .. }) => Some(id.clone()),
                None => None,
                _ => bail!("Expected Name, got {:?}", annotation),
            },
        });
    }

    Ok(arg_names)
}
