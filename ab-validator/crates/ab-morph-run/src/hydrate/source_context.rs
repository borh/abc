//! Per-source resolution: AAT loading, re-projection, snippet windows,
//! and Aozora-markup reconstruction (spec Layers 1, 3, 4).

use anyhow::{Result, bail};
use serde::Serialize;

/// A snippet window around a region, parts kept separate so JSON consumers
/// can re-mark (spec §Layer 1).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
#[allow(dead_code)]
pub struct Snippet {
    pub before: String,
    pub region: String,
    pub after: String,
    #[serde(skip)]
    at_doc_start: bool,
    #[serde(skip)]
    at_doc_end: bool,
}

impl Snippet {
    /// `…before【region】after…` — the Markdown display form. Ellipses appear
    /// only where the window was clipped short of the document bounds, which
    /// the constructor encodes by leaving `before`/`after` at full context
    /// length; callers never re-check bounds.
    #[must_use]
    #[allow(dead_code)]
    pub fn marked(&self) -> String {
        let lead = if self.at_doc_start { "" } else { "…" };
        let trail = if self.at_doc_end { "" } else { "…" };
        format!(
            "{lead}{}【{}】{}{trail}",
            self.before, self.region, self.after
        )
    }
}

/// Slices `text` by char index (never byte index) into a window of up to
/// `context` chars on each side of `[char_start, char_end)`. Errors when the
/// span is inverted or exceeds the text's char count.
#[allow(dead_code)]
pub fn snippet_window(
    text: &str,
    char_start: u64,
    char_end: u64,
    context: usize,
) -> Result<Snippet> {
    let chars: Vec<char> = text.chars().collect();
    let total = chars.len() as u64;
    if char_start > char_end || char_end > total {
        bail!("span [{char_start}, {char_end}) is out of range for a text of {total} chars");
    }
    let start = char_start as usize;
    let end = char_end as usize;
    let context_start = start.saturating_sub(context);
    let context_end = (end + context).min(chars.len());
    Ok(Snippet {
        before: chars[context_start..start].iter().collect(),
        region: chars[start..end].iter().collect(),
        after: chars[end..context_end].iter().collect(),
        at_doc_start: context_start == 0,
        at_doc_end: context_end == chars.len(),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn snippet_window_slices_by_char_index_with_context() {
        let text = "abc吾輩は猫であるxyz";
        let s = snippet_window(text, 6, 10, 2).unwrap();
        assert_eq!(s.before, "輩は");
        assert_eq!(s.region, "猫である");
        assert_eq!(s.after, "xy");
        assert_eq!(s.marked(), "…輩は【猫である】xy…");
    }

    #[test]
    fn snippet_window_clips_at_document_bounds() {
        let s = snippet_window("猫である", 0, 2, 40).unwrap();
        assert_eq!(s.before, "");
        assert_eq!(s.region, "猫で");
        assert_eq!(s.after, "ある");
        // No leading ellipsis when the window reaches the document start,
        // no trailing ellipsis when it reaches the end.
        assert_eq!(s.marked(), "【猫で】ある");
    }

    #[test]
    fn snippet_window_rejects_out_of_range_span() {
        let err = snippet_window("abc", 0, 10, 0).unwrap_err();
        assert!(err.to_string().contains("out of range"));
    }
}
