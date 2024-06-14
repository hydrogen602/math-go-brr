use std::sync::{Arc, Mutex};

use aliasable::boxed::AliasableBox;
use pyo3::{exceptions::PyRuntimeError, prelude::*, types::PyTuple};

use compiler::{
    llvm::{LLVMJitContext, LLVMModule},
    CompileError, Typed,
};

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
pub fn take_source(
    src: &str,
    compile_opts: CompileOpts,
    original_func: Bound<'_, PyAny>,
) -> PyResult<Func> {
    // TODO: add location to error & revamp parse error handling
    let func = compiler::parse(src).map_err(|x| CompileError::ParseError {
        msg: x.to_string(),
        location: None,
    })?;
    let func_name = func.name.clone();

    let context = AliasableBox::from_unique(Box::new(LLVMJitContext::new()));
    let module = LLVMModule::new(&context, "test_go_brrr").map_err(|x| anyhow_500!(x))?;

    let signature = match module.compile_func(func, compile_opts.into()) {
        Ok(sig) => sig,
        Err(e) => return Err(e.into()),
    };

    // extend the lifetime of 'ctx to 'static
    let ctx_obj = ContextAndLLVM {
        module: unsafe { std::mem::transmute(module) },
        context,
        func_name,
        signature,
    };

    Ok(Func {
        llvm: Arc::new(Mutex::new(ctx_obj)),
        original_func: original_func.unbind(),
    })
}

#[pyclass(frozen)]
pub struct Func {
    llvm: Arc<Mutex<ContextAndLLVM>>,
    #[pyo3(get)]
    original_func: Py<PyAny>,
}

#[pymethods]
impl Func {
    #[pyo3(signature = (*py_args))]
    fn __call__(&self, py_args: &Bound<'_, PyTuple>) -> PyResult<Typed<i64, bool>> {
        let lock = self
            .llvm
            .lock()
            .map_err(|e| PyRuntimeError::new_err(e.to_string()))?;

        // TODO: type check - with signature

        let out = unsafe { lock.signature.call(&lock, py_args) }?;

        Ok(out)
    }
}

impl IntoPy<PyObject> for Typed<i64, bool> {
    fn into_py(self, py: Python) -> PyObject {
        match self {
            Typed::I64(i) => i.into_py(py),
            Typed::Bool(b) => b.into_py(py),
        }
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
