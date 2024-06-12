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

    /// args is mut as we pass pointers to llvm function and we don't know what llvm does with them
    pub unsafe fn call(
        &self,
        context_and_module: &ContextAndLLVM,
        args: &mut [i64],
    ) -> PyResult<i64> {
        assert_eq!(
            self.args.len(),
            args.len(),
            "Argument count mismatch, this is a bug"
        );

        // Safety: don't call this function with the same arg twice (would create overlapping mut pointers)
        let arg_to_ptr = |idx: usize| &args[idx] as *const i64 as *mut ();

        type Ptr = *mut ();

        match (self.ret, self.args.len()) {
            (i64::ARG, 0) => {
                let f = Self::call_helper::<unsafe extern "C" fn() -> i64>(context_and_module)?;
                Ok(f.call())
            }
            (i64::ARG, 1) => {
                let f = Self::call_helper::<unsafe extern "C" fn(Ptr) -> i64>(context_and_module)?;
                Ok(f.call(arg_to_ptr(0)))
            }
            (i64::ARG, 2) => {
                let f =
                    Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr) -> i64>(context_and_module)?;
                Ok(f.call(arg_to_ptr(0), arg_to_ptr(1)))
            }
            (i64::ARG, 3) => {
                let f = Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr, Ptr) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(arg_to_ptr(0), arg_to_ptr(1), arg_to_ptr(2)))
            }
            (i64::ARG, 4) => {
                let f = Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(arg_to_ptr(0), arg_to_ptr(1), arg_to_ptr(2), arg_to_ptr(3)))
            }
            (i64::ARG, 5) => {
                let f = Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr, Ptr) -> i64>(
                    context_and_module,
                )?;
                Ok(f.call(
                    arg_to_ptr(0),
                    arg_to_ptr(1),
                    arg_to_ptr(2),
                    arg_to_ptr(3),
                    arg_to_ptr(4),
                ))
            }
            (i64::ARG, 6) => {
                let f = Self::call_helper::<
                    unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr, Ptr, Ptr) -> i64,
                >(context_and_module)?;
                Ok(f.call(
                    arg_to_ptr(0),
                    arg_to_ptr(1),
                    arg_to_ptr(2),
                    arg_to_ptr(3),
                    arg_to_ptr(4),
                    arg_to_ptr(5),
                ))
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
