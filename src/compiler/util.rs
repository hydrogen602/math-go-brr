use std::fmt::Display;

use anyhow::anyhow;

pub trait Ext<T, R: Display> {
    fn err_convert(self) -> anyhow::Result<T>;
}

impl<T, R: Display> Ext<T, R> for Result<T, R> {
    fn err_convert(self) -> anyhow::Result<T> {
        self.map_err(|e| anyhow!(e.to_string()))
    }
}
