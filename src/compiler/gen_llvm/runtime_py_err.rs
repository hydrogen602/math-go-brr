use pyo3::{
    exceptions::{PyRuntimeError, PyZeroDivisionError},
    pyclass, pymethods, Py, PyAny, Python, ToPyObject,
};

use crate::{compiler::gen_llvm::runtime_err::ErrorCode, Location};

use super::runtime_err::RuntimeError;

impl JITRuntimeError {
    pub fn prep_args(err: RuntimeError, py: Python) -> (Location, Py<PyAny>) {
        let loc = Location {
            lineno: err.line().into(),
            offset: err.column().into(),
            end_offset: None,
        };

        let code = err.error_code();
        let inner_err = match code {
            ErrorCode::ZeroDivisionError => {
                PyZeroDivisionError::new_err("division by zero".to_string())
            }
            ErrorCode::UnknownError { extra_info } => {
                PyRuntimeError::new_err(format!("unknown error, extra info: {}", extra_info))
            }
        }
        .to_object(py);

        (loc, inner_err)
    }
}

#[pyclass(extends=PyRuntimeError)]
#[derive(Debug)]
pub struct JITRuntimeError {
    #[pyo3(get)]
    pub loc: Location,
    #[pyo3(get)]
    pub inner_error: Py<PyAny>,
}

#[pymethods]
impl JITRuntimeError {
    #[new]
    fn new(loc: Location, inner_error: Py<PyAny>) -> Self {
        Self { loc, inner_error }
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
