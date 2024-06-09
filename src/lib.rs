use std::sync::{Arc, Mutex};

use aliasable::boxed::AliasableBox;
use anyhow::anyhow;
use pyo3::{
    exceptions::{PyRuntimeError, PyTypeError},
    prelude::*,
    types::PyTuple,
};

use compiler::{
    llvm::{LLVMContext, LLVM},
    JitFunction,
};
use util::{Ext, Intermediary};

mod compiler;
mod util;

#[allow(dead_code)]
struct ContextAndLLVM {
    /// actually has a lifetime of 'context. It must be declared before context so it gets dropped first
    llvm: LLVM<'static>,
    /// safety: we must never move out of this box as long as llvm is alive
    context: AliasableBox<LLVMContext>,
    func_name: String,
}

impl ContextAndLLVM {
    pub fn get_func(&self) -> anyhow::Result<JitFunction<unsafe extern "C" fn(i64, i64) -> i64>> {
        let f = unsafe {
            self.llvm
                .execution_engine
                .get_function(&self.func_name)
                .ok()
        }
        .ok_or_else(|| anyhow!("Function not found"))?;

        println!("Function found");

        Ok(f)
    }
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

        let context = AliasableBox::from_unique(Box::new(LLVMContext::new()));
        // extend the lifetime of context to 'static
        let llvm: LLVM<'static> = unsafe { std::mem::transmute(LLVM::new(&context)?) };
        let context_and_llvm = ContextAndLLVM {
            llvm,
            context,
            func_name,
        };

        context_and_llvm
            .llvm
            .compile_func(func, compile_opts.into())?;

        Ok(context_and_llvm)
    };

    let ctx = inner()?;

    Ok(Func {
        llvm: Arc::new(Mutex::new(ctx)),
    })
}

#[pyclass]
pub struct Func {
    llvm: Arc<Mutex<ContextAndLLVM>>,
}

#[pymethods]
impl Func {
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

        // FIXME: make this be able to take any number of arguments
        if args.len() != 2 {
            return Err(PyTypeError::new_err("Expected 2 arguments"));
        }
        let a = args[0];
        let b = args[1];

        let f = lock.get_func().err_convert()?;
        let out = unsafe { f.call(a, b) };
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
