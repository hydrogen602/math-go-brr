use core::fmt;

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
    BoolBinOp(Box<ExpressionAST>, BoolBinOp, Box<ExpressionAST>),
    Name(String),
    Constant(ConstantAST),
    UnaryOp(UnaryOp, Box<ExpressionAST>),
    MultiOp(Box<ExpressionAST>, Vec<(CompareOp, ExpressionAST)>),
}

impl fmt::Display for ExpressionAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionAST::BinOp(left, op, right) => write!(f, "({} {} {})", left, op, right),
            ExpressionAST::BoolBinOp(left, op, right) => write!(f, "({} {} {})", left, op, right),
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

impl TryFrom<PyJsonNode> for UnaryOp {
    type Error = PyJsonNode;

    fn try_from(value: PyJsonNode) -> Result<Self, Self::Error> {
        match value {
            PyJsonNode::USub => Ok(UnaryOp::USub),
            PyJsonNode::Not => Ok(UnaryOp::Not),
            other => Err(other),
        }
    }
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
}

impl TryFrom<PyJsonNode> for BinOp {
    type Error = PyJsonNode;

    fn try_from(value: PyJsonNode) -> Result<Self, Self::Error> {
        match value {
            PyJsonNode::Add => Ok(BinOp::Add),
            PyJsonNode::Sub => Ok(BinOp::Sub),
            other => Err(other),
        }
    }
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

impl TryFrom<PyJsonNode> for CompareOp {
    type Error = PyJsonNode;

    fn try_from(value: PyJsonNode) -> Result<Self, Self::Error> {
        match value {
            PyJsonNode::Eq => Ok(CompareOp::Eq),
            PyJsonNode::NotEq => Ok(CompareOp::NotEq),
            PyJsonNode::Lt => Ok(CompareOp::Lt),
            PyJsonNode::LtE => Ok(CompareOp::LtE),
            PyJsonNode::Gt => Ok(CompareOp::Gt),
            PyJsonNode::GtE => Ok(CompareOp::GtE),
            other => Err(other),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum BoolBinOp {
    And,
    Or,
}

impl TryFrom<PyJsonNode> for BoolBinOp {
    type Error = PyJsonNode;

    fn try_from(value: PyJsonNode) -> Result<Self, Self::Error> {
        match value {
            PyJsonNode::And => Ok(BoolBinOp::And),
            PyJsonNode::Or => Ok(BoolBinOp::Or),
            other => Err(other),
        }
    }
}

impl fmt::Display for BoolBinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BoolBinOp::And => write!(f, "and"),
            BoolBinOp::Or => write!(f, "or"),
        }
    }
}

impl fmt::Display for BinOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinOp::Add => write!(f, "+"),
            BinOp::Sub => write!(f, "-"),
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
