use std::sync::{Arc, Mutex};

use aliasable::boxed::AliasableBox;
use pyo3::{
    exceptions::{PyRuntimeError, PyTypeError},
    prelude::*,
    types::PyTuple,
};

use compiler::llvm::{LLVMJitContext, LLVMModule};
use util::Intermediary;

mod compiler;
mod signature;
mod util;

#[allow(dead_code)]
struct ContextAndLLVM {
    /// actually has a lifetime of 'context. It must be declared before context so it gets dropped first
    module: LLVMModule<'static>,
    /// safety: we must never move out of this box as long as llvm is alive
    context: AliasableBox<LLVMJitContext>,
    func_name: String,
    signature: signature::Signature,
}

#[pyclass]
#[derive(Debug, Clone, Copy)]
pub struct CompileOpts {
    dump_ir: bool,
}

#[pymethods]
impl CompileOpts {
    #[new]
    fn new(dump_ir: bool) -> Self {
        Self { dump_ir }
    }
}

impl From<CompileOpts> for compiler::CompileOpts {
    fn from(opts: CompileOpts) -> Self {
        let CompileOpts { dump_ir } = opts;
        Self { dump_ir }
    }
}

#[pyfunction]
pub fn take_source(src: &str, compile_opts: CompileOpts) -> PyResult<Func> {
    let inner = || -> Result<ContextAndLLVM, Intermediary> {
        let func = compiler::parse(src)?;
        let func_name = func.name.clone();

        let context = AliasableBox::from_unique(Box::new(LLVMJitContext::new()));
        let module = LLVMModule::new(&context, "test_go_brrr")?;

        let signature = module.compile_func(func, compile_opts.into())?;

        // extend the lifetime of 'ctx to 'static
        Ok(ContextAndLLVM {
            module: unsafe { std::mem::transmute(module) },
            context,
            func_name,
            signature,
        })
    };

    Ok(Func {
        llvm: Arc::new(Mutex::new(inner()?)),
    })
}

#[pyclass]
pub struct Func {
    llvm: Arc<Mutex<ContextAndLLVM>>,
}

#[pymethods]
impl Func {
    #[pyo3(signature = (*py_args))]
    fn __call__(&self, py_args: &Bound<'_, PyTuple>) -> PyResult<i64> {
        let lock = self
            .llvm
            .lock()
            .map_err(|e| PyRuntimeError::new_err(e.to_string()))?;

        // TODO: improve with SmallVec<[i64; 5]>
        let args: Vec<i64> = match py_args
            .into_iter()
            .map(|py_arg| FromPyObject::extract_bound(&py_arg))
            .collect()
        {
            Ok(args) => args,
            Err(e) => return Err(PyTypeError::new_err(e.to_string())),
        };

        let out = unsafe { lock.signature.call(&lock, &args) }?;

        Ok(out)
    }
}

// whatever, lets hope Arc<Mutex<...>> deals with it
unsafe impl Send for Func {}

/// A Python module implemented in Rust.
#[pymodule]
fn math_go_brrr(_py: Python, m: &Bound<'_, PyModule>) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(take_source, m)?)?;
    m.add_class::<CompileOpts>()?;
    Ok(())
}
