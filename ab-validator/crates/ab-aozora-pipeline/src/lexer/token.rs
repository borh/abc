//! Lexer token types.
//!
//! The tokenize stage emits source spans for text, newlines, and triggers.

use ab_aozora_syntax::Span;

pub use ab_aozora_spec::TriggerKind;

/// A single lexer event.
#[derive(Debug, Clone, PartialEq, Eq)]
#[non_exhaustive]
pub enum Token {
    /// Text between triggers. `range` is a byte-offset span in the
    /// sanitized source (sanitize-stage output). May be empty if two
    /// triggers are adjacent.
    Text {
        /// Sanitized-source byte span of the run; may be empty.
        range: Span,
    },

    /// A delimiter character. `pos` is the start byte offset of the
    /// token in the sanitized source; `kind` carries its role. For
    /// multi-character triggers (`［＃`) the span covers
    /// all constituent characters.
    Trigger {
        /// Which delimiter this is (`｜`, `《`, `［＃`, …).
        kind: TriggerKind,
        /// Sanitized-source byte span covering every constituent
        /// character of the trigger.
        span: Span,
    },

    /// Line-feed (`\n`). Emitted as its own token rather than folded
    /// into the surrounding Text because line-structure matters for
    /// block-level container recognition (the pair stage pairs
    /// block-opener / block-closer lines by position).
    Newline {
        /// Sanitized-source byte offset of the `\n`.
        pos: u32,
    },
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn single_char_trigger_byte_lens_match_utf8() {
        // Verifies that the re-exported functions produce identical results.
        assert_eq!(TriggerKind::Bar.source_byte_len(), 3);
        assert_eq!(TriggerKind::AngleQuoteOpen.source_byte_len(), 3);
    }
}
