use std::sync::{Arc, Mutex, Once};

use aliasable::boxed::AliasableBox;
use anyhow::anyhow;
use math_go_brrr::{
    self,
    llvm::{LLVMContext, LLVM},
    JitFunction,
};
use once_cell::sync::OnceCell;
use pyo3::{exceptions::PyRuntimeError, prelude::*};
use util::{Ext, Intermediary};

mod util;

#[pyfunction]
pub fn bin_op(a: i64, b: i64) -> PyResult<i64> {
    Ok(a + b)
}

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

#[pyfunction]
pub fn take_source(src: &str) -> PyResult<Func> {
    let inner = || -> Result<ContextAndLLVM, Intermediary> {
        let func = math_go_brrr::parse(src)?;
        let func_name = func.name.clone();

        let context = AliasableBox::from_unique(Box::new(LLVMContext::new()));
        // extend the lifetime of context to 'static
        let llvm: LLVM<'static> = unsafe { std::mem::transmute(LLVM::new(&context)?) };
        let context_and_llvm = ContextAndLLVM {
            llvm,
            context,
            func_name,
        };

        context_and_llvm.llvm.compile_func(func)?;

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
    fn __call__(&self, a: i64, b: i64) -> PyResult<i64> {
        println!("MyClass has been called");
        let lock = self
            .llvm
            .lock()
            .map_err(|e| PyRuntimeError::new_err(e.to_string()))?;

        let f = lock.get_func().err_convert()?;
        let out = unsafe { f.call(a, b) };
        Ok(out)
    }
}

// whatever, lets hope Arc<Mutex<...>> deals with it
unsafe impl Send for Func {}

/// A Python module implemented in Rust.
#[pymodule]
fn math_go_brrr_py(_py: Python, m: &PyModule) -> PyResult<()> {
    // INSTANCE
    //     .set(Arc::new(Mutex::new(LLVMContext::new())))
    //     .expect("Just no");

    m.add_function(wrap_pyfunction!(bin_op, m)?)?;
    m.add_function(wrap_pyfunction!(take_source, m)?)?;
    Ok(())
}
