use inkwell::execution_engine::{FunctionLookupError, JitFunction, UnsafeFunctionPointer};
use pyo3::{exceptions::PyRuntimeError, PyResult};

use crate::{compiler::ArgType, compiler::TypeToArg, ContextAndLLVM};

#[derive(Debug)]
pub struct Signature {
    args: Vec<ArgType>,
    ret: ArgType,
}

impl Signature {
    pub fn new(args: Vec<ArgType>, ret: ArgType) -> Self {
        Self { args, ret }
    }

    // unsafe extern "C" fn(i64, i64) -> i64
    pub unsafe fn call(&self, context_and_module: &ContextAndLLVM, args: &[i64]) -> PyResult<i64> {
        assert_eq!(
            self.args.len(),
            args.len(),
            "Argument count mismatch, this is a bug"
        );

        match (self.ret, &self.args[..]) {
            (i64::ARG, sl) if sl.is_empty() => {
                let f = Self::call_helper::<unsafe extern "C" fn() -> i64>(context_and_module)?;
                Ok(f.call())
            }
            (i64::ARG, &[i64::ARG]) => {
                let f = Self::call_helper::<unsafe extern "C" fn(i64) -> i64>(context_and_module)?;
                Ok(f.call(args[0]))
            }
            (i64::ARG, &[i64::ARG, i64::ARG]) => {
                let f =
                    Self::call_helper::<unsafe extern "C" fn(i64, i64) -> i64>(context_and_module)?;
                Ok(f.call(args[0], args[1]))
            }
            (i64::ARG, &[i64::ARG, i64::ARG, i64::ARG]) => {
                let f = Self::call_helper::<unsafe extern "C" fn(i64, i64, i64) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(args[0], args[1], args[2]))
            }
            (i64::ARG, &[i64::ARG, i64::ARG, i64::ARG, i64::ARG]) => {
                let f = Self::call_helper::<unsafe extern "C" fn(i64, i64, i64, i64) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(args[0], args[1], args[2], args[3]))
            }
            (i64::ARG, &[i64::ARG, i64::ARG, i64::ARG, i64::ARG, i64::ARG]) => {
                let f = Self::call_helper::<unsafe extern "C" fn(i64, i64, i64, i64, i64) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(args[0], args[1], args[2], args[3], args[4]))
            }
            (i64::ARG, &[i64::ARG, i64::ARG, i64::ARG, i64::ARG, i64::ARG, i64::ARG]) => {
                let f = Self::call_helper::<
                    unsafe extern "C" fn(i64, i64, i64, i64, i64, i64) -> i64,
                >(context_and_module)?;
                Ok(f.call(args[0], args[1], args[2], args[3], args[4], args[5]))
            }
            (ret, args) => {
                return Err(PyRuntimeError::new_err(format!(
                    "Signature not yet supported: {:?} -> {:?}",
                    args, ret
                )))
            }
        }
    }

    unsafe fn call_helper<F>(context_and_module: &ContextAndLLVM) -> PyResult<JitFunction<F>>
    where
        F: UnsafeFunctionPointer,
    {
        context_and_module
            .module
            .execution_engine
            .get_function(&context_and_module.func_name)
            .map_err(|e| match e {
                FunctionLookupError::JITNotEnabled => {
                    PyRuntimeError::new_err("FATAL: JIT not enabled")
                }
                FunctionLookupError::FunctionNotFound => PyRuntimeError::new_err(
                    "Function not found? This is a bug and should be reported",
                ),
            })
    }
}
