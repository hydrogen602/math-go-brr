use math_go_brrr;
use pyo3::{exceptions, prelude::*};

#[pyfunction]
pub fn bin_op(a: i64, b: i64) -> PyResult<i64> {
    Ok(a + b)
}

#[pyfunction]
pub fn take_source(src: &str) -> PyResult<()> {
    match math_go_brrr::brrr(src) {
        Ok(_) => Ok(()),
        Err(e) => Err(exceptions::PyRuntimeError::new_err(e.to_string())),
    }
}

/// A Python module implemented in Rust.
#[pymodule]
fn math_go_brrr_py(_py: Python, m: &PyModule) -> PyResult<()> {
    m.add_function(wrap_pyfunction!(bin_op, m)?)?;
    m.add_function(wrap_pyfunction!(take_source, m)?)?;
    Ok(())
}
