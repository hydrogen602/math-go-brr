use anyhow::{anyhow, bail, ensure};

use crate::{compiler::gen_llvm::Type, Location};

use super::{python_ast::*, python_ast_json::*};

impl From<PyLocation> for Location {
    fn from(loc: PyLocation) -> Self {
        Location {
            lineno: loc.lineno,
            offset: loc.col_offset,
            end_offset: Some(loc.end_col_offset),
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

fn translate_expression(expr: PyJsonNode) -> anyhow::Result<Located<ExpressionAST>> {
    Ok(match expr {
        PyJsonNode::BinOp {
            left,
            op,
            right,
            location,
        } => ExpressionAST::BinOp(
            Box::new(translate_expression(*left)?),
            (*op)
                .try_into()
                .map_err(|op| anyhow!("Unsupported binop: {:?}", op))?,
            Box::new(translate_expression(*right)?),
        )
        .with_loc(location),
        PyJsonNode::Name { id, location, ctx } => {
            ensure!(
                matches!(*ctx, PyJsonNode::Load),
                "Expected Load, got {:?}",
                ctx
            );
            ExpressionAST::Name(id).with_loc(location)
        }
        PyJsonNode::Constant { value, location } => (match value {
            Some(serde_json::Value::Number(n)) => {
                if let Some(i) = n.as_i64() {
                    ExpressionAST::Constant(ConstantAST::I64(i))
                } else {
                    bail!("TODO: Unsupported number: {:?}", n)
                }
            }
            Some(serde_json::Value::Bool(b)) => ExpressionAST::Constant(ConstantAST::Bool(b)),
            _ => bail!("Unsupported constant: {:?}", value),
        })
        .with_loc(location),
        PyJsonNode::UnaryOp {
            op,
            operand,
            location,
        } => {
            let operand = translate_expression(*operand)?;

            let op = (*op)
                .try_into()
                .map_err(|op| anyhow!("Unsupported unary op: {:?}", op))?;

            ExpressionAST::UnaryOp(op, Box::new(operand)).with_loc(location)
        }
        PyJsonNode::BoolOp {
            op,
            values,
            location,
        } => {
            let values = values
                .into_iter()
                .map(|v| translate_expression(v))
                .collect::<anyhow::Result<Vec<_>>>()?;

            let op: BoolBinOp = (*op)
                .try_into()
                .map_err(|op| anyhow!("Unsupported boolop: {:?}", op))?;

            let Some(expr) = values.into_iter().fold(None, |acc, v| match acc {
                None => Some(v),
                Some(acc) => Some(
                    ExpressionAST::BoolBinOp(Box::new(acc), op, Box::new(v)).with_loc(location),
                ),
            }) else {
                bail!("Expected at least one value in boolop")
            };

            expr
        }
        PyJsonNode::Compare {
            left,
            ops,
            comparators,
            location,
        } => {
            ensure!(
                comparators.len() == ops.len(),
                "Mismatched number of compare ops and comparators: {} vs {}",
                ops.len(),
                comparators.len()
            );

            let left = translate_expression(*left)?;
            let comparators = comparators.into_iter().map(|c| translate_expression(c));
            let ops = ops.into_iter().map(|op| {
                op.try_into()
                    .map_err(|op| anyhow!("Unsupported compare op: {:?}", op))
            });

            let pairs = ops.zip(comparators).map(|(op, expr)| Ok((op?, expr?)));

            ExpressionAST::MultiOp(Box::new(left), pairs.collect::<anyhow::Result<_>>()?)
                .with_loc(location)
        }
        _ => bail!("Unsupported expression: {:?}", expr),
    })
}

fn translate_body(body: Vec<PyJsonNode>) -> anyhow::Result<Vec<Located<StatementAST>>> {
    let mut statements = vec![];

    for node in body {
        match node {
            PyJsonNode::If {
                body,
                orelse,
                test,
                location,
            } => {
                let condition = translate_expression(*test)?;

                let if_true = translate_body(body)?;
                let if_false = translate_body(orelse)?;

                statements.push(
                    StatementAST::If {
                        if_block: if_true,
                        else_block: if_false,
                        condition,
                    }
                    .with_loc(location),
                );
            }
            PyJsonNode::Return { value, location } => {
                statements.push(
                    StatementAST::Return(match value {
                        Some(expr) => Some(translate_expression(*expr)?),
                        None => None,
                    })
                    .with_loc(location),
                );
            }
            PyJsonNode::Assign {
                targets,
                value,
                location,
            } => {
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

                statements.push(
                    StatementAST::Assign {
                        target,
                        value: translate_expression(*value)?,
                    }
                    .with_loc(location),
                );
            }
            PyJsonNode::AugAssign {
                target,
                op,
                value,
                location,
            } => {
                let PyJsonNode::Name {
                    id: target, ctx, ..
                } = *target
                else {
                    bail!("Expected Name, got {:?}", target)
                };
                ensure!(
                    matches!(*ctx, PyJsonNode::Store),
                    "Expected Store, got {:?}",
                    ctx
                );

                let value = translate_expression(*value)?;

                let op: BinOp = (*op)
                    .try_into()
                    .map_err(|op| anyhow!("Unsupported binop: {:?}", op))?;

                let expr = ExpressionAST::BinOp(
                    Box::new(ExpressionAST::Name(target.clone()).with_loc(location)),
                    op,
                    Box::new(value),
                )
                .with_loc(location);

                statements.push(
                    StatementAST::Assign {
                        target,
                        value: expr,
                    }
                    .with_loc(location),
                );
            }
            PyJsonNode::Pass { .. } => {
                // no-op
            }
            PyJsonNode::While {
                body,
                test,
                location,
            } => {
                let condition = translate_expression(*test)?;
                let body = translate_body(body)?;

                statements.push(StatementAST::While { body, condition }.with_loc(location));
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
