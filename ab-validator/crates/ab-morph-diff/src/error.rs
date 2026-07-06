use std::error::Error;
use std::fmt;

use crate::model::{AnalyzerId, TextId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MorphDiffError {
    InvalidInput {
        message: String,
    },
    TextIdMismatch {
        from: TextId,
        to: TextId,
    },
    SourceTextMismatch {
        text_id: TextId,
    },
    OutOfOrderSpan {
        analyzer: AnalyzerId,
        text_id: TextId,
        index: usize,
    },
    OverlappingSpan {
        analyzer: AnalyzerId,
        text_id: TextId,
        previous: usize,
        current: usize,
    },
    InvalidByteSpan {
        analyzer: AnalyzerId,
        text_id: TextId,
        index: usize,
    },
    CharSpanMismatch {
        analyzer: AnalyzerId,
        text_id: TextId,
        index: usize,
    },
    SurfaceMismatch {
        analyzer: AnalyzerId,
        text_id: TextId,
        index: usize,
    },
}

impl fmt::Display for MorphDiffError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::InvalidInput { message } => write!(f, "invalid input: {message}"),
            Self::TextIdMismatch { from, to } => {
                write!(f, "text id mismatch: from={from}, to={to}")
            }
            Self::SourceTextMismatch { text_id } => {
                write!(f, "source text mismatch for text_id={text_id}")
            }
            Self::OutOfOrderSpan {
                analyzer,
                text_id,
                index,
            } => write!(
                f,
                "out-of-order span: analyzer={analyzer}, text_id={text_id}, index={index}"
            ),
            Self::OverlappingSpan {
                analyzer,
                text_id,
                previous,
                current,
            } => write!(
                f,
                "overlapping spans: analyzer={analyzer}, text_id={text_id}, previous={previous}, current={current}"
            ),
            Self::InvalidByteSpan {
                analyzer,
                text_id,
                index,
            } => write!(
                f,
                "invalid byte span: analyzer={analyzer}, text_id={text_id}, index={index}"
            ),
            Self::CharSpanMismatch {
                analyzer,
                text_id,
                index,
            } => write!(
                f,
                "char span mismatch: analyzer={analyzer}, text_id={text_id}, index={index}"
            ),
            Self::SurfaceMismatch {
                analyzer,
                text_id,
                index,
            } => write!(
                f,
                "surface/source mismatch: analyzer={analyzer}, text_id={text_id}, index={index}"
            ),
        }
    }
}

impl Error for MorphDiffError {}
