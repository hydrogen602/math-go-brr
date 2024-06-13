mod ast_to_llvm;
use core::fmt;

pub use ast_to_llvm::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Typed<I, B> {
    I64(I),
    Bool(B),
}

impl<I, B> Typed<I, B> {
    fn into_i64(self) -> Result<I, Self> {
        match self {
            Typed::I64(i) => Ok(i),
            Typed::Bool(_) => Err(self),
        }
    }

    fn into_bool(self) -> Result<B, Self> {
        match self {
            Typed::I64(_) => Err(self),
            Typed::Bool(b) => Ok(b),
        }
    }

    pub fn py_type_name(&self) -> &'static str {
        match self {
            Typed::I64(_) => "int",
            Typed::Bool(_) => "bool",
        }
    }
}

impl<I, B> From<Typed<I, B>> for Type {
    fn from(typed: Typed<I, B>) -> Self {
        match typed {
            Typed::I64(_) => Type::I64,
            Typed::Bool(_) => Type::Bool,
        }
    }
}

pub trait TypeToArg {
    const ARG: Type;
}

macro_rules! impl_type_to_arg {
    ($t:ty, $l:expr) => {
        impl TypeToArg for $t {
            const ARG: Type = $l;
        }
    };
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[allow(dead_code)]
pub struct Type(Typed<(), ()>);

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.py_type_name())
    }
}

#[allow(non_upper_case_globals)]
impl Type {
    pub const I64: Self = Self(Typed::I64(()));
    pub const Bool: Self = Self(Typed::Bool(()));
}

impl AsRef<Typed<(), ()>> for Type {
    fn as_ref(&self) -> &Typed<(), ()> {
        &self.0
    }
}

impl From<Type> for Typed<(), ()> {
    fn from(arg_type: Type) -> Self {
        arg_type.0
    }
}

impl_type_to_arg!(i64, Type::I64);
impl_type_to_arg!(bool, Type::Bool);
