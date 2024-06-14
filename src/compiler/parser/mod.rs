use inkwell::builder::BuilderError;
use pyo3::{exceptions, PyErr};
use python_ast_json::Location;

use super::Type;

pub mod python_ast;
pub mod python_ast_json;
pub mod translation;

#[derive(Debug, thiserror::Error)]
pub enum CompileError {
    #[error("Parse error: {msg}")]
    ParseError {
        msg: String,
        location: Option<Location>,
    },
    #[error("Type error: expected {expected} but got {got}")]
    TypeMismatchError { expected: Type, got: Type },
    #[error("Type error: {0}")]
    TypeInvalidOperationError(String),
    #[error("Internal error: {0}")]
    InternalError(anyhow::Error),
}

pub type CompileResult<T> = Result<T, CompileError>;

#[macro_export]
macro_rules! anyhow_500 {
    ($err:expr) => {
        $crate::compiler::CompileError::InternalError(anyhow::anyhow!($err))
    };
    ($msg:literal $(,)?) => {
        $crate::compiler::CompileError::InternalError(anyhow::anyhow!($msg))
    };
    ($fmt:literal, $($arg:tt)*) => {
        $crate::compiler::CompileError::InternalError(anyhow::anyhow!($fmt, $($arg)*))
    };
}

#[macro_export]
macro_rules! bail_500 {
    ($err:expr) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::InternalError(anyhow::anyhow!($err)))
    };
    ($msg:literal $(,)?) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::InternalError(anyhow::anyhow!($msg)))
    };
    ($fmt:literal, $($arg:tt)*) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::InternalError(anyhow::anyhow!($fmt, $($arg)*)))
    };
}

#[macro_export]
macro_rules! bail_400 {
    ($err:expr) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::ParseError{ msg: ($err).to_string(), location: None })
    };
    ($msg:literal $(,)?) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::ParseError{ msg: ($err).to_string(), location: None })
    };
    ($fmt:literal, $($arg:tt)*) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::ParseError{ msg: format!($fmt, $($arg)*), location: None })
    };
}

#[macro_export]
macro_rules! bail_type_err {
    ($msg:literal $(,)?) => {
      return ::core::result::Result::Err($crate::compiler::CompileError::TypeInvalidOperationError(
        ($msg).to_string()))
    };
    ($fmt:literal, $($arg:tt)*) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::TypeInvalidOperationError(format!($fmt, $($arg)*)))
    };
    ($expected:expr => $got:expr) => {
        return ::core::result::Result::Err($crate::compiler::CompileError::TypeMismatchError {
            expected: $expected,
            got: $got,
        })
    };

}

// #[macro_export]
// macro_rules! bail {
//   ($msg:literal $(,)?) => {
//       return ::core::result::Result::Err($crate::parser::CompileError::InternalError(($msg).to_string()))
//   };
//   ($err:expr $(,)?) => {
//       return ::core::result::Result::Err($crate::parser::CompileError::InternalError(($msg).to_string()))
//   };
//   ($fmt:expr, $($arg:tt)*) => {
//       return ::core::result::Result::Err($crate::parser::CompileError::InternalError(format!($fmt, $($arg)*)))
//   };
// }

impl From<BuilderError> for CompileError {
    fn from(e: BuilderError) -> Self {
        CompileError::InternalError(e.into())
    }
}

impl From<CompileError> for PyErr {
    fn from(e: CompileError) -> Self {
        match e {
            CompileError::ParseError { msg, location } => {
                let msg = if let Some(location) = location {
                    format!("{} at {:?}", msg, location)
                } else {
                    msg
                };
                // TODO: proper location setting
                exceptions::PySyntaxError::new_err(msg)
            }
            CompileError::TypeMismatchError { expected, got } => {
                exceptions::PyTypeError::new_err(format!("{} expected, got {}", expected, got))
            }
            CompileError::TypeInvalidOperationError(msg) => exceptions::PyTypeError::new_err(msg),
            CompileError::InternalError(msg) => {
                exceptions::PyRuntimeError::new_err(msg.to_string())
            }
        }
    }
}
