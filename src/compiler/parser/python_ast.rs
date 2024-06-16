use core::fmt;
use std::{
    borrow::{Borrow, BorrowMut},
    fmt::write,
    ops::{Deref, DerefMut},
};

use crate::{compiler::gen_llvm::Type, Location};

use super::python_ast_json::{PyJsonNode, PyLocation};

#[derive(Debug)]
pub struct Located<T> {
    pub value: T,
    pub location: Location,
}

impl<T> Borrow<T> for Located<T> {
    fn borrow(&self) -> &T {
        &self.value
    }
}

impl<T> BorrowMut<T> for Located<T> {
    fn borrow_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> AsRef<T> for Located<T> {
    fn as_ref(&self) -> &T {
        &self.value
    }
}

impl<T> AsMut<T> for Located<T> {
    fn as_mut(&mut self) -> &mut T {
        &mut self.value
    }
}

impl<T> Deref for Located<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Located<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

pub trait LocatedAdder<L> {
    fn with_loc(self, location: L) -> Located<Self>
    where
        Self: Sized;
}

impl<T> LocatedAdder<Location> for T {
    fn with_loc(self, location: Location) -> Located<Self> {
        Located {
            value: self,
            location,
        }
    }
}

impl<T> LocatedAdder<PyLocation> for T {
    fn with_loc(self, location: PyLocation) -> Located<Self> {
        Located {
            value: self,
            location: location.into(),
        }
    }
}

/// Stricter version (only what we can parse)
#[derive(Debug)]
pub struct FunctionAST {
    pub name: String,
    pub args: Vec<Arg>,
    pub body: Vec<Located<StatementAST>>,
    pub return_type: Type,
}

#[derive(Debug)]
pub struct Arg {
    pub arg: String,
    pub type_: Type,
}

#[derive(Debug)]
pub enum StatementAST {
    Return(Option<Located<ExpressionAST>>),
    Assign {
        target: String,
        value: Located<ExpressionAST>,
    },
    If {
        if_block: Vec<Located<StatementAST>>,
        else_block: Vec<Located<StatementAST>>,
        condition: Located<ExpressionAST>,
    },
    While {
        body: Vec<Located<StatementAST>>,
        condition: Located<ExpressionAST>,
    },
}

#[derive(Debug)]
pub enum ExpressionAST {
    BinOp(
        Box<Located<ExpressionAST>>,
        BinOp,
        Box<Located<ExpressionAST>>,
    ),
    BoolBinOp(
        Box<Located<ExpressionAST>>,
        BoolBinOp,
        Box<Located<ExpressionAST>>,
    ),
    Name(String),
    Constant(ConstantAST),
    UnaryOp(UnaryOp, Box<Located<ExpressionAST>>),
    MultiOp(
        Box<Located<ExpressionAST>>,
        Vec<(CompareOp, Located<ExpressionAST>)>,
    ),
}

impl fmt::Display for ExpressionAST {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ExpressionAST::BinOp(left, op, right) => write!(f, "({} {} {})", ***left, op, ***right),
            ExpressionAST::BoolBinOp(left, op, right) => {
                write!(f, "({} {} {})", ***left, op, ***right)
            }
            ExpressionAST::Name(name) => write!(f, "{}", name),
            ExpressionAST::Constant(c) => write!(f, "{}", c),
            ExpressionAST::UnaryOp(op, operand) => write!(f, "({} {})", op, ***operand),
            ExpressionAST::MultiOp(expr, pairs) => {
                write!(f, "({}", ***expr)?;
                for (op, expr) in pairs {
                    write!(f, " {} {}", op, **expr)?;
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
    Mult,
}

impl TryFrom<PyJsonNode> for BinOp {
    type Error = PyJsonNode;

    fn try_from(value: PyJsonNode) -> Result<Self, Self::Error> {
        match value {
            PyJsonNode::Add => Ok(BinOp::Add),
            PyJsonNode::Sub => Ok(BinOp::Sub),
            PyJsonNode::Mult => Ok(BinOp::Mult),
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
            BinOp::Mult => write!(f, "*"),
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
