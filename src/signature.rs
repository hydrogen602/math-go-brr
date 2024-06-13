use inkwell::execution_engine::{FunctionLookupError, JitFunction, UnsafeFunctionPointer};
use pyo3::{exceptions::PyRuntimeError, types::PyTuple, Bound, FromPyObject, PyResult};

use crate::{
    compiler::{Type, TypeToArg, Typed},
    ContextAndLLVM,
};

#[derive(Debug)]
pub struct Signature {
    args: Vec<Type>,
    ret: Type,
}

impl Signature {
    pub fn new(args: Vec<Type>, ret: Type) -> Self {
        Self { args, ret }
    }

    /// args is mut as we pass pointers to llvm function and we don't know what llvm does with them
    pub unsafe fn call(
        &self,
        context_and_module: &ContextAndLLVM,
        py_args: &Bound<'_, PyTuple>,
    ) -> PyResult<Typed<i64, bool>> {
        #[allow(unused_mut)] // we pass out pointers to llvm that can be mutated
        let mut args: Vec<Typed<i64, bool>> = py_args
            .into_iter()
            .zip(self.args.iter())
            .map(|(py_arg, arg_ty)| -> PyResult<Typed<i64, bool>> {
                match arg_ty.as_ref() {
                    Typed::I64(_) => Ok(Typed::I64(FromPyObject::extract_bound(&py_arg)?)),
                    Typed::Bool(_) => Ok(Typed::Bool(FromPyObject::extract_bound(&py_arg)?)),
                }
            })
            .collect::<PyResult<Vec<_>>>()?;

        assert_eq!(
            self.args.len(),
            args.len(),
            "Argument count mismatch, this is a bug"
        );

        // Safety: don't call this function with the same arg twice (would create overlapping mut pointers)
        let arg_to_ptr = |idx: usize| match &args[idx] {
            Typed::I64(i) => i as *const i64 as *mut (),
            Typed::Bool(b) => b as *const bool as *mut (),
        };

        type Ptr = *mut ();

        macro_rules! match_arms_for_return_type {
            ($ret:ty, $matcher:expr) => {
                match $matcher {
                    (<$ret>::ARG, 0) => {
                        let f = Self::call_helper::<unsafe extern "C" fn() -> $ret>(
                            context_and_module,
                        )?;
                        Some(Ok(f.call()))
                    }
                    (<$ret>::ARG, 1) => {
                        let f = Self::call_helper::<unsafe extern "C" fn(Ptr) -> $ret>(
                            context_and_module,
                        )?;
                        Some(Ok(f.call(arg_to_ptr(0))))
                    }
                    (<$ret>::ARG, 2) => {
                        let f = Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr) -> $ret>(
                            context_and_module,
                        )?;
                        Some(Ok(f.call(arg_to_ptr(0), arg_to_ptr(1))))
                    }
                    (<$ret>::ARG, 3) => {
                        let f = Self::call_helper::<unsafe extern "C" fn(Ptr, Ptr, Ptr) -> $ret>(
                            context_and_module,
                        )?;
                        Some(Ok(f.call(arg_to_ptr(0), arg_to_ptr(1), arg_to_ptr(2))))
                    }
                    (<$ret>::ARG, 4) => {
                        let f = Self::call_helper::<
                            unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr) -> $ret,
                        >(context_and_module)?;
                        Some(Ok(f.call(
                            arg_to_ptr(0),
                            arg_to_ptr(1),
                            arg_to_ptr(2),
                            arg_to_ptr(3),
                        )))
                    }
                    (<$ret>::ARG, 5) => {
                        let f = Self::call_helper::<
                            unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr, Ptr) -> $ret,
                        >(context_and_module)?;
                        Some(Ok(f.call(
                            arg_to_ptr(0),
                            arg_to_ptr(1),
                            arg_to_ptr(2),
                            arg_to_ptr(3),
                            arg_to_ptr(4),
                        )))
                    }
                    (<$ret>::ARG, 6) => {
                        let f = Self::call_helper::<
                            unsafe extern "C" fn(Ptr, Ptr, Ptr, Ptr, Ptr, Ptr) -> $ret,
                        >(context_and_module)?;
                        Some(Ok(f.call(
                            arg_to_ptr(0),
                            arg_to_ptr(1),
                            arg_to_ptr(2),
                            arg_to_ptr(3),
                            arg_to_ptr(4),
                            arg_to_ptr(5),
                        )))
                    }
                    (_, _) => None,
                }
            };
        }

        if let Some(result) = match_arms_for_return_type!(i64, (self.ret, self.args.len())) {
            return result.map(Typed::I64);
        }
        if let Some(result) = match_arms_for_return_type!(bool, (self.ret, self.args.len())) {
            return result.map(Typed::Bool);
        }

        return Err(PyRuntimeError::new_err(format!(
            "Signature not yet supported: {:?} -> {:?}",
            args, self.ret
        )));
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
