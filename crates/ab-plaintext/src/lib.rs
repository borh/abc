mod aat;
mod aozora;

use std::error::Error;
use std::fmt;

use serde::Serialize;

pub use aat::{from_aat_value, visible_text_projection};
pub use aozora::from_aozora_honbun_bytes;

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

pub(crate) fn canonicalize_line_endings(text: impl Into<String>) -> String {
    let text = text.into();
    if !text.as_bytes().contains(&b'\r') {
        return text;
    }

    let mut out = String::with_capacity(text.len());
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '\r' {
            if chars.peek() == Some(&'\n') {
                chars.next();
            }
            out.push('\n');
        } else {
            out.push(ch);
        }
    }
    out
}
