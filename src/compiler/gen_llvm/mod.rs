mod ast_to_llvm;
pub use ast_to_llvm::*;

#[derive(Debug, Clone, Copy)]
enum Typed<I, B> {
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
}
