mod aozora;

use serde::Serialize;

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
}
