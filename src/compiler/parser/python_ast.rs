use anyhow::{bail, ensure};

use super::python_ast_json::PyJsonNode;

/// Stricter version (only what we can parse)
#[derive(Debug)]
pub struct FunctionAST {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Vec<StatementAST>,
    pub return_type: ArgType,
}

#[derive(Debug)]
pub struct Arg {
    pub arg: String,
    pub type_: ArgType,
}

#[derive(Debug)]
pub enum StatementAST {
    Return(Option<ExpressionAST>),
}

#[derive(Debug)]
pub enum ExpressionAST {
    BinOp(Box<ExpressionAST>, BinOp, Box<ExpressionAST>),
    Name(String),
    Constant(ConstantAST),
    UnaryOp(UnaryOp, Box<ExpressionAST>),
}

#[derive(Debug)]
pub enum UnaryOp {
    USub,
}

#[derive(Debug)]
pub enum ConstantAST {
    I64(i64), // idk why, but -4 in py ast is unaryop { usub, 4 }
              // TODO: F64(f64),
}

#[derive(Debug)]
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

pub fn translate_func(func: PyJsonNode) -> anyhow::Result<FunctionAST> {
    let PyJsonNode::FunctionDef {
        name,
        args,
        body,
        returns,
        ..
    } = func
    else {
        bail!("Expected FunctionDef, got {:?}", func)
    };

    let return_type = match returns.as_deref() {
        Some(PyJsonNode::Name { id, .. }) => translate_arg_type(&id),
        _ => bail!("Expected Name, got {:?} for return type", returns),
    };

    Ok(FunctionAST {
        name,
        args: translate_args(*args)?,
        body: translate_body(body)?,
        return_type,
    })
}

fn translate_expression(expr: PyJsonNode) -> anyhow::Result<ExpressionAST> {
    match expr {
        PyJsonNode::BinOp {
            left, op, right, ..
        } => Ok(ExpressionAST::BinOp(
            Box::new(translate_expression(*left)?),
            match *op {
                PyJsonNode::Add => BinOp::Add,
                PyJsonNode::Sub => BinOp::Sub,
                _ => bail!("Unsupported binop: {:?}", op),
            },
            Box::new(translate_expression(*right)?),
        )),
        PyJsonNode::Name { id, .. } => Ok(ExpressionAST::Name(id)),
        PyJsonNode::Constant { value, .. } => match value {
            Some(serde_json::Value::Number(n)) => {
                if let Some(i) = n.as_i64() {
                    Ok(ExpressionAST::Constant(ConstantAST::I64(i)))
                } else {
                    bail!("TODO: Unsupported number: {:?}", n)
                }
            }
            _ => bail!("Unsupported constant: {:?}", value),
        },
        PyJsonNode::UnaryOp { op, operand, .. } => {
            let operand = translate_expression(*operand)?;
            match *op {
                PyJsonNode::USub => Ok(ExpressionAST::UnaryOp(UnaryOp::USub, Box::new(operand))),
                _ => bail!("Unsupported unary op: {:?}", op),
            }
        }
        _ => bail!("Unsupported expression: {:?}", expr),
    }
}

fn translate_body(body: Vec<PyJsonNode>) -> anyhow::Result<Vec<StatementAST>> {
    let mut statements = vec![];

    for node in body {
        match node {
            PyJsonNode::Return { value, .. } => {
                statements.push(StatementAST::Return(match value {
                    Some(expr) => Some(translate_expression(*expr)?),
                    None => None,
                }));
            }
            _ => bail!("Unsupported statement: {:?}", node),
        }
    }

    Ok(statements)
}

fn translate_args(args: PyJsonNode) -> anyhow::Result<Vec<Arg>> {
    let PyJsonNode::Arguments {
        args,
        kwonlyargs,
        vararg,
        kwarg,
        defaults,
        kw_defaults,
        posonlyargs: _,
    } = args
    else {
        bail!("Expected Arguments, got {:?}", args)
    };

    ensure!(kwonlyargs.len() == 0, "kwonlyargs not supported yet");
    ensure!(defaults.len() == 0, "defaults not supported yet");
    ensure!(kw_defaults.len() == 0, "kw_defaults not supported yet");
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
            type_: match annotation.as_deref() {
                Some(PyJsonNode::Name { id, .. }) => translate_arg_type(id),
                _ => bail!("Expected Name, got {:?} for argument type", annotation),
            },
        });
    }

    Ok(arg_names)
}

fn translate_arg_type(arg_type: &str) -> ArgType {
    match arg_type {
        "int" => ArgType::I64,
        _ => panic!("Unsupported type: {}", arg_type),
    }
}

pub trait TypeToArg {
    const ARG: ArgType;
}

macro_rules! impl_type_to_arg {
    ($t:ty, $l:expr) => {
        impl TypeToArg for $t {
            const ARG: ArgType = $l;
        }
    };
    () => {};
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArgType {
    I64,
}

impl_type_to_arg!(i64, ArgType::I64);
