use std::error::Error;
use std::fmt;
use std::ops::Range;

#[derive(Debug)]
pub enum AnalyzerError {
    DictionaryLoad {
        analyzer: String,
        message: String,
    },
    Tokenize {
        analyzer: String,
        message: String,
    },
    SurfaceMismatch {
        analyzer: String,
        text_id: String,
        cursor: usize,
        expected_surface: String,
    },
    InvalidTokenSpan {
        analyzer: String,
        text_id: String,
        span: Range<usize>,
        message: String,
    },
}

impl fmt::Display for AnalyzerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AnalyzerError::DictionaryLoad { analyzer, message } => {
                write!(f, "{analyzer} dictionary load failed: {message}")
            }
            AnalyzerError::Tokenize { analyzer, message } => {
                write!(f, "{analyzer} tokenization failed: {message}")
            }
            AnalyzerError::SurfaceMismatch {
                analyzer,
                text_id,
                cursor,
                expected_surface,
            } => write!(
                f,
                "{analyzer} emitted surface `{expected_surface}` that did not match text `{text_id}` at byte {cursor}"
            ),
            AnalyzerError::InvalidTokenSpan {
                analyzer,
                text_id,
                span,
                message,
            } => write!(
                f,
                "{analyzer} emitted invalid span {}..{} for text `{text_id}`: {message}",
                span.start, span.end
            ),
        }
    }
}

impl Error for AnalyzerError {}
