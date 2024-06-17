mod ast_to_llvm;
mod misc_converters;
mod runtime_err;
mod runtime_py_err;
use core::fmt;

pub use ast_to_llvm::*;
use inkwell::{context::Context, values::IntValue};
pub use runtime_err::RuntimeError;
pub use runtime_py_err::JITRuntimeError;

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

// single type

#[derive(Debug, Clone, Copy)]
pub struct Bool<T>(pub T);

impl<'ctx> Bool<IntValue<'ctx>> {
    pub fn get_true(ctx: &'ctx Context) -> Self {
        Bool(ctx.bool_type().const_all_ones())
    }

    pub fn get_false(ctx: &'ctx Context) -> Self {
        Bool(ctx.bool_type().const_zero())
    }
}

impl<I, B> TryFrom<Typed<I, B>> for Bool<B> {
    type Error = Typed<I, B>;

    fn try_from(value: Typed<I, B>) -> Result<Self, Self::Error> {
        match value {
            Typed::Bool(b) => Ok(Bool(b)),
            other => Err(other),
        }
    }
}

impl<I, B> From<Bool<B>> for Typed<I, B> {
    fn from(b: Bool<B>) -> Self {
        Typed::Bool(b.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct I64<T>(pub T);

impl<'ctx> I64<IntValue<'ctx>> {
    pub fn from_i64(ctx: &'ctx Context, i: i64) -> Self {
        // idk about sign_extend, but we are passing in 64 bits.
        // even though it says u64, it still does it signed
        I64(ctx.i64_type().const_int(i as u64, false))
    }
}

impl<I, B> TryFrom<Typed<I, B>> for I64<I> {
    type Error = Typed<I, B>;

    fn try_from(value: Typed<I, B>) -> Result<Self, Self::Error> {
        match value {
            Typed::I64(i) => Ok(I64(i)),
            other => Err(other),
        }
    }
}

impl<I, B> From<I64<I>> for Typed<I, B> {
    fn from(i: I64<I>) -> Self {
        Typed::I64(i.0)
    }
}
