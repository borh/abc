mod aozora;
mod aat;

use std::error::Error;
use std::fmt;

use serde::Serialize;

pub use aozora::from_aozora_honbun_bytes;
pub use aat::{from_aat_value, visible_text_projection};

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct PlainTextDocument {
    pub text_id: String,
    pub source_format: SourceFormat,
    pub text: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum SourceFormat {
    AozoraHonbun,
    AatVisibleText,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PlainTextError {
    MissingAatWorkId,
}

impl fmt::Display for PlainTextError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            PlainTextError::MissingAatWorkId => write!(f, "AAT is missing string work_id"),
        }
    }
}

impl Error for PlainTextError {}
