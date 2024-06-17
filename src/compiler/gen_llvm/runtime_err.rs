use crate::Location;

#[repr(transparent)]
/// Error code
/// So there are 64 bits.
/// The first 8 bits are the error code
/// 8-16 are extra error information, error code specific (unsigned, u8)
/// 16-32 are the column number (unsigned, u16)
/// The final 32 bits are the line number (unsigned, u32)
#[derive(Debug, Clone, Copy)]
pub struct RuntimeError(u64);

pub enum ErrorCode {
    UnknownError { extra_info: u8 },
    ZeroDivisionError,
}

impl ErrorCode {
    fn to_code(&self) -> u8 {
        match self {
            ErrorCode::UnknownError { .. } => 0,
            ErrorCode::ZeroDivisionError => 1,
        }
    }

    fn from_code_and_info(code: u8, extra_info: u8) -> Self {
        match code {
            0 => ErrorCode::UnknownError { extra_info },
            1 => ErrorCode::ZeroDivisionError,
            _ => panic!("Invalid error code. This is a bug"),
        }
    }
}

impl RuntimeError {
    pub fn new(error_code: ErrorCode, extra_info: u8, column: u16, line: u32) -> Self {
        let error_code = error_code.to_code() as u64;
        let extra_info = (extra_info as u64) << 8;
        let column = (column as u64) << 16;
        let line = (line as u64) << 32;
        Self(error_code | extra_info | column | line)
    }

    pub fn zero_division_error(location: Location) -> Self {
        let column = location
            .offset
            .try_into()
            .expect("Column number is too large");
        let line = location
            .lineno
            .try_into()
            .expect("Line number is too large");
        Self::new(ErrorCode::ZeroDivisionError, 0, column, line)
    }

    pub fn error_code(&self) -> ErrorCode {
        let code = (self.0 & 0xFF) as u8;
        let extra_info = self.extra_info();

        ErrorCode::from_code_and_info(code, extra_info)
    }

    fn extra_info(&self) -> u8 {
        ((self.0 >> 8) & 0xFF) as u8
    }

    pub fn column(&self) -> u16 {
        ((self.0 >> 16) & 0xFFFF) as u16
    }

    pub fn line(&self) -> u32 {
        ((self.0 >> 32) & 0xFFFFFFFF) as u32
    }

    pub fn into_raw_value(&self) -> u64 {
        self.0
    }

    pub fn from_raw_value(value: u64) -> Self {
        Self(value)
    }
}
