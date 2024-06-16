use std::sync::{Arc, Mutex};

use aliasable::boxed::AliasableBox;
use inkwell::OptimizationLevel;
use pyo3::{
    exceptions::{PyRuntimeError, PyTypeError, PyValueError},
    prelude::*,
    types::PyTuple,
};

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
#[derive(Debug, Clone)]
pub struct CompileOpts {
    dump_ir: bool,
    optimization_level: String,
}

impl CompileOpts {
    fn get_opt_level(&self) -> PyResult<OptimizationLevel> {
        match self.optimization_level.as_str() {
            "none" => Ok(OptimizationLevel::None),
            "less" => Ok(OptimizationLevel::Less),
            "default" => Ok(OptimizationLevel::Default),
            "aggressive" => Ok(OptimizationLevel::Aggressive),
            _ => Err(PyValueError::new_err("Invalid optimization level")),
        }
    }
}

#[pymethods]
impl CompileOpts {
    #[new]
    #[pyo3(signature = (dump_ir, optimization_level="default".to_string()))]
    fn new(dump_ir: bool, optimization_level: String) -> Self {
        Self {
            dump_ir,
            optimization_level,
        }
    }
}

impl From<CompileOpts> for compiler::CompileOpts {
    fn from(opts: CompileOpts) -> Self {
        let CompileOpts { dump_ir, .. } = opts;
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
    let module = LLVMModule::new(
        &context,
        &format!("{}_go_brrr", func_name),
        compile_opts.get_opt_level()?,
    )
    .map_err(|x| anyhow_500!(x))?;

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
    m.add_function(wrap_pyfunction!(foo, m)?)?;
    m.add_class::<CompileOpts>()?;
    m.add_class::<CompileTypeError>()?;
    Ok(())
}

#[pyfunction]
/// Basic function implemented in just Rust to compare speed differences
/// @brrr
/// def foo(a: int) -> int:
///     b = 0
///     i = 0
///     while i < a:
///         b = b + i
///         i = i + 1
///
///     return b
pub fn foo(a: i64) -> i64 {
    let mut b = 0;
    let mut i = 0;
    while i < a {
        b = b + i;
        i = i + 1;
    }

    b
}

#[pyclass]
#[derive(Debug, Clone, Copy, Default)]
pub struct Location {
    #[pyo3(get)]
    pub lineno: u64,
    #[pyo3(get)]
    pub offset: u64,
    #[pyo3(get)]
    pub end_offset: Option<u64>,
}

impl Location {
    fn with_end_offset(mut self, end_offset: u64) -> Self {
        self.end_offset = Some(end_offset);
        self
    }
}

#[pyclass(extends=PyTypeError)]
#[derive(Debug)]
pub struct CompileTypeError {
    pub loc: Location,
    #[pyo3(get)]
    pub msg: String,
}

// impl CompileTypeError {
//     fn new(msg: String, loc: Location) -> Self {
//         Self { loc, msg }
//     }
// }

#[pymethods]
impl CompileTypeError {
    #[new]
    fn new(msg: String, loc: Location) -> Self {
        Self { loc, msg }
    }

    #[getter]
    fn lineno(&self) -> u64 {
        self.loc.lineno
    }

    #[getter]
    fn offset(&self) -> u64 {
        self.loc.offset
    }

    #[getter]
    fn width(&self) -> Option<u64> {
        self.loc.end_offset.map(|end| end - self.loc.offset)
    }
}
