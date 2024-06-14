use core::fmt;

use anyhow::{bail, ensure};

use crate::compiler::gen_llvm::Type;

use super::python_ast_json::PyJsonNode;

/// Stricter version (only what we can parse)
#[derive(Debug)]
pub struct FunctionAST {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Vec<StatementAST>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct Arg {
    pub arg: String,
    pub type_: Type,
}

#[derive(Debug)]
pub enum StatementAST {
    Return(Option<ExpressionAST>),
    Assign {
        target: String,
        value: ExpressionAST,
    },
    If {
        if_block: Vec<StatementAST>,
        else_block: Vec<StatementAST>,
        condition: ExpressionAST,
    },
    While {
        body: Vec<StatementAST>,
        condition: ExpressionAST,
    },
}

#[derive(Debug)]
pub enum ExpressionAST {
    BinOp(Box<ExpressionAST>, BinOp, Box<ExpressionAST>),
    Name(String),
    Constant(ConstantAST),
    UnaryOp(UnaryOp, Box<ExpressionAST>),
    MultiOp(Box<ExpressionAST>, Vec<(CompareOp, ExpressionAST)>),
}

impl fmt::Display for ExpressionAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionAST::BinOp(left, op, right) => write!(f, "({} {} {})", left, op, right),
            ExpressionAST::Name(name) => write!(f, "{}", name),
            ExpressionAST::Constant(c) => write!(f, "{}", c),
            ExpressionAST::UnaryOp(op, operand) => write!(f, "({} {})", op, operand),
            ExpressionAST::MultiOp(expr, pairs) => {
                write!(f, "({}", expr)?;
                for (op, expr) in pairs {
                    write!(f, " {} {}", op, expr)?;
                }
                write!(f, ")")
            }
        }
    }
}

#[derive(Debug)]
pub enum UnaryOp {
    USub,
    Not,
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::USub => write!(f, "-"),
            UnaryOp::Not => write!(f, "not"),
        }
    }
}

#[derive(Debug)]
pub enum ConstantAST {
    I64(i64), // idk why, but -4 in py ast is unaryop { usub, 4 }
    // TODO: F64(f64),
    Bool(bool),
}

impl fmt::Display for ConstantAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ConstantAST::I64(i) => write!(f, "{}", i),
            ConstantAST::Bool(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    And,
    Or,
}

/// Python is special sometimes
///
/// ```a < b``` and ```a + b```
/// are syntactically different constructs
#[derive(Debug, Clone, Copy)]
pub enum CompareOp {
    Eq,
    NotEq,
    Lt,
    LtE,
    Gt,
    GtE,
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
            BinOp::And => write!(f, "and"),
            BinOp::Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for CompareOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompareOp::Eq => write!(f, "=="),
            CompareOp::NotEq => write!(f, "!="),
            CompareOp::Lt => write!(f, "<"),
            CompareOp::LtE => write!(f, "<="),
            CompareOp::Gt => write!(f, ">"),
            CompareOp::GtE => write!(f, ">="),
        }
    }
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
        Some(PyJsonNode::Name { id, .. }) => translate_arg_py_type(&id),
        None => bail!("Expected return type, got None"),
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
            Some(serde_json::Value::Bool(b)) => Ok(ExpressionAST::Constant(ConstantAST::Bool(b))),
            _ => bail!("Unsupported constant: {:?}", value),
        },
        PyJsonNode::UnaryOp { op, operand, .. } => {
            let operand = translate_expression(*operand)?;
            match *op {
                PyJsonNode::USub => Ok(ExpressionAST::UnaryOp(UnaryOp::USub, Box::new(operand))),
                PyJsonNode::Not => Ok(ExpressionAST::UnaryOp(UnaryOp::Not, Box::new(operand))),
                _ => bail!("Unsupported unary op: {:?}", op),
            }
        }
        PyJsonNode::BoolOp { op, values, .. } => {
            let values = values
                .into_iter()
                .map(|v| translate_expression(v))
                .collect::<anyhow::Result<Vec<ExpressionAST>>>()?;

            let op = match *op {
                PyJsonNode::And => BinOp::And,
                PyJsonNode::Or => BinOp::Or,
                _ => bail!("Unsupported boolop: {:?}", op),
            };

            let Some(expr) = values.into_iter().fold(None, |acc, v| match acc {
                None => Some(v),
                Some(acc) => Some(ExpressionAST::BinOp(Box::new(acc), op, Box::new(v))),
            }) else {
                bail!("Expected at least one value in boolop")
            };

            Ok(expr)
        }
        PyJsonNode::Compare {
            left,
            ops,
            comparators,
            ..
        } => {
            ensure!(
                comparators.len() == ops.len(),
                "Mismatched number of compare ops and comparators: {} vs {}",
                ops.len(),
                comparators.len()
            );

            let left = translate_expression(*left)?;
            let comparators = comparators.into_iter().map(|c| translate_expression(c));
            let ops = ops.into_iter().map(|op| match op {
                PyJsonNode::Eq => Ok(CompareOp::Eq),
                PyJsonNode::NotEq => Ok(CompareOp::NotEq),
                PyJsonNode::Lt => Ok(CompareOp::Lt),
                PyJsonNode::LtE => Ok(CompareOp::LtE),
                PyJsonNode::Gt => Ok(CompareOp::Gt),
                PyJsonNode::GtE => Ok(CompareOp::GtE),
                _ => bail!("Unsupported compare op: {:?}", op),
            });

            let pairs = ops.zip(comparators).map(|(op, expr)| Ok((op?, expr?)));

            Ok(ExpressionAST::MultiOp(
                Box::new(left),
                pairs.collect::<anyhow::Result<_>>()?,
            ))
        }
        _ => bail!("Unsupported expression: {:?}", expr),
    }
}

fn translate_body(body: Vec<PyJsonNode>) -> anyhow::Result<Vec<StatementAST>> {
    let mut statements = vec![];

    for node in body {
        match node {
            PyJsonNode::If {
                body, orelse, test, ..
            } => {
                let condition = translate_expression(*test)?;

                let if_true = translate_body(body)?;
                let if_false = translate_body(orelse)?;

                statements.push(StatementAST::If {
                    if_block: if_true,
                    else_block: if_false,
                    condition,
                });
            }
            PyJsonNode::Return { value, .. } => {
                statements.push(StatementAST::Return(match value {
                    Some(expr) => Some(translate_expression(*expr)?),
                    None => None,
                }));
            }
            PyJsonNode::Assign { targets, value, .. } => {
                ensure!(
                    targets.len() == 1,
                    "Only single target assign currently supported"
                );
                let target = targets.into_iter().next().unwrap();

                let PyJsonNode::Name {
                    id: target, ctx, ..
                } = target
                else {
                    bail!("Expected Name, got {:?}", target)
                };
                ensure!(
                    matches!(*ctx, PyJsonNode::Store),
                    "Expected Store, got {:?}",
                    ctx
                );

                statements.push(StatementAST::Assign {
                    target,
                    value: translate_expression(*value)?,
                });
            }
            PyJsonNode::Pass { .. } => {
                // no-op
            }
            PyJsonNode::While { body, test, .. } => {
                let condition = translate_expression(*test)?;
                let body = translate_body(body)?;

                statements.push(StatementAST::While { body, condition });
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
                Some(PyJsonNode::Name { id, .. }) => translate_arg_py_type(id),
                None => bail!("Expected annotation, got None"),
                _ => bail!("Expected Name, got {:?} for argument type", annotation),
            },
        });
    }

    Ok(arg_names)
}

fn translate_arg_py_type(arg_type: &str) -> Type {
    match arg_type {
        "int" => Type::I64,
        "bool" => Type::Bool,
        _ => panic!("Unsupported type: {}", arg_type),
    }
}
