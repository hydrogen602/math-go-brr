use pyo3::{exceptions, PyResult};

pub struct Intermediary(pyo3::prelude::PyErr);

impl From<anyhow::Error> for Intermediary {
    fn from(e: anyhow::Error) -> Self {
        Intermediary(exceptions::PyRuntimeError::new_err(e.to_string()))
    }
}

impl From<Intermediary> for pyo3::prelude::PyErr {
    fn from(e: Intermediary) -> Self {
        e.0
    }
}

// pub trait Ext<T> {
//     fn err_convert(self) -> PyResult<T>;
// }

// impl<T> Ext<T> for anyhow::Result<T> {
//     fn err_convert(self) -> PyResult<T> {
//         self.map_err(|e| exceptions::PyRuntimeError::new_err(e.to_string()))
//     }
// }
