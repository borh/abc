//! Sanitize stage — source sanitation.
//!
//! Prepares the raw source text for the downstream lexer stages:
//!
//! 1. **BOM strip** — every leading `U+FEFF` (UTF-8 BOM, 3 bytes each)
//!    is consumed. Both single (`U+FEFF`) and stacked (`U+FEFF`+
//!    `U+FEFF`+…) leading sequences resolve to the same empty prefix
//!    so that `to_source(to_source(x))` round-trips byte-equal — a
//!    single-strip would peel off one BOM per pass and break I3
//!    fixed-point on inputs that carry more than one. Interior
//!    `U+FEFF` (zero-width no-break space) is still preserved.
//! 2. **CR/LF normalization** — `\r\n` → `\n`, lone `\r` → `\n`. Aozora
//!    source comes from a variety of encoders; downstream stages assume
//!    `\n` as the one line terminator so they don't have to handle three
//!    variants each.
//! 3. **Accent decomposition inside `〔...〕`** — ASCII accent digraphs
//!    (`fune`+grave-accent → funèbre, `cafe`+apostrophe → café, …) are
//!    rewritten to their Unicode-combined form before any later stage
//!    sees them. Scope is deliberately restricted to tortoiseshell-
//!    bracket spans; the function is the identity outside them.
//! 4. **Decorative rule isolation** — lines composed entirely of 10 or
//!    more `-`, `=`, or `_` characters (a very common visual separator
//!    in Aozora Bunko prose) are forced to sit on their own stanza by
//!    inserting a blank line before them, so downstream Markdown
//!    layers (e.g. the sibling `afm` repo's `CommonMark` integration)
//!    do not promote the preceding paragraph into a setext heading.
//!
//! Marker identity belongs to the normalized-offset registry. Literal private-use
//! source characters remain text, including codepoints also used by injected markers.
//! The pass borrows its input when no normalization is needed.

use std::borrow::Cow;

use memchr::memmem;

use ab_aozora_syntax::Span;
use ab_aozora_syntax::accent::decompose_fragment_sites;

use ab_aozora_spec::Diagnostic;

/// Tortoiseshell-bracket open character — delimits accent-decomposition
/// spans.
const TORTOISE_OPEN: char = '〔';
/// UTF-8 byte encoding of [`TORTOISE_OPEN`] for `memmem`-based scans.
/// `'〔'` (U+3014) → `0xE3 0x80 0x94`.
const TORTOISE_OPEN_BYTES: &[u8] = "〔".as_bytes();
/// Tortoiseshell-bracket close character.
const TORTOISE_CLOSE: char = '〕';

/// Minimum run length for a `-` / `=` / `_` line to be treated as a
/// decorative rule rather than a setext underline. Nine characters is
/// the longest setext underline observed in the `CommonMark` 0.31.2 spec
/// cases; ten is the first length where Aozora's typical `---...---`
/// separator starts to appear in the 17 k-work corpus.
const DECORATIVE_RULE_MIN_LEN: usize = 10;

/// Output of the sanitize stage. `text` is what downstream stages consume;
/// `diagnostics` carries any non-fatal observations gathered during sanitation.
#[derive(Debug, Clone)]
pub struct SanitizeOutput<'s> {
    pub text: Cow<'s, str>,
    pub diagnostics: Vec<Diagnostic>,
}

/// One non-identity rewrite: source bytes `src_start..src_end` became
/// output bytes `dst_start..dst_end`.
///
/// Recorded in a single transform step's OWN input/output coordinates
/// (offset-map groundwork for span metrics; consumed by
/// [`OffsetMap`] / [`SanitizeMaps`]).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MapEdit {
    pub src_start: usize,
    pub src_end: usize,
    pub dst_start: usize,
    pub dst_end: usize,
}

/// Offset map for ONE transform pass: sorted, non-overlapping edits;
/// offsets between edits translate by the accumulated length delta.
#[derive(Debug, Clone, Default)]
pub struct OffsetMap {
    edits: Vec<MapEdit>,
}

impl OffsetMap {
    fn locate(&self, dst: usize) -> Result<usize, isize> {
        // Returns Ok(edit index) when dst falls inside an edit's dst
        // range, Err(delta) with the accumulated (src - dst) delta of all
        // edits ending at or before dst otherwise.
        let mut delta: isize = 0;
        for (i, e) in self.edits.iter().enumerate() {
            if dst < e.dst_start {
                return Err(delta);
            }
            if dst < e.dst_end {
                return Ok(i);
            }
            delta +=
                (e.src_end - e.src_start).cast_signed() - (e.dst_end - e.dst_start).cast_signed();
        }
        Err(delta)
    }

    #[must_use]
    pub fn to_source_offset(&self, dst: usize) -> usize {
        match self.locate(dst) {
            Ok(i) => self.edits[i].src_start,
            Err(delta) => (dst.cast_signed() + delta).cast_unsigned(),
        }
    }

    #[must_use]
    pub fn to_source_end(&self, dst: usize) -> usize {
        // End-exclusive semantics: an end whose last covered byte
        // (dst - 1) lands inside an edit resolves to the edit's src_end;
        // an end at an edit's dst_start resolves BEFORE the edit. An
        // empty span's end behaves like its start.
        if dst == 0 {
            return self.to_source_offset(0);
        }
        match self.locate(dst - 1) {
            Ok(i) => self.edits[i].src_end,
            Err(delta) => (dst.cast_signed() + delta).cast_unsigned(),
        }
    }
}

/// Composed per-step maps; queries walk the steps in reverse.
#[derive(Debug, Clone, Default)]
pub struct SanitizeMaps {
    steps: Vec<OffsetMap>,
}

impl SanitizeMaps {
    #[must_use]
    pub fn to_source_offset(&self, dst: usize) -> usize {
        self.steps
            .iter()
            .rev()
            .fold(dst, |o, m| m.to_source_offset(o))
    }

    #[must_use]
    pub fn to_source_end(&self, dst: usize) -> usize {
        self.steps.iter().rev().fold(dst, |o, m| m.to_source_end(o))
    }

    /// Test introspection: does `dst` pass through any step's edit?
    #[cfg(test)]
    fn is_edited(&self, dst: usize) -> bool {
        let mut offset = dst;
        for map in self.steps.iter().rev() {
            match map.locate(offset) {
                Ok(_) => return true,
                Err(delta) => offset = (offset.cast_signed() + delta).cast_unsigned(),
            }
        }
        false
    }
}

/// [`sanitize`] plus the composed offset map .
#[derive(Debug)]
pub struct SanitizeMappedOutput<'s> {
    pub text: Cow<'s, str>,
    pub diagnostics: Vec<Diagnostic>,
    pub maps: SanitizeMaps,
}

/// Apply the four sanitation steps and return the result. See module
/// documentation for the step order and rationale.
#[must_use]
pub fn sanitize(source: &str) -> SanitizeOutput<'_> {
    // Strip every leading `U+FEFF`. CommonMark / WHATWG-text-encoding
    // both consider only one BOM, but the `to_source` round-trip would
    // peel one off per pass without this loop, breaking the I3
    // fixed-point invariant `to_source(to_source(x)) == to_source(x)`
    // on inputs that carry stacked BOMs (e.g. `\u{feff}\u{feff}` →
    // first pass yields `\u{feff}`, second yields `""`).
    let mut after_bom = source;
    while let Some(rest) = after_bom.strip_prefix('\u{FEFF}') {
        after_bom = rest;
    }

    let line_normalized: Cow<'_, str> = if after_bom.contains('\r') {
        Cow::Owned(normalize_line_endings(after_bom))
    } else {
        Cow::Borrowed(after_bom)
    };

    let rule_isolated: Cow<'_, str> = if has_long_rule_line(&line_normalized) {
        Cow::Owned(isolate_decorative_rules(&line_normalized))
    } else {
        line_normalized
    };

    // Gate via `memmem::find` on the UTF-8 byte sequence rather than
    // `str::contains(char)`, which falls back to a per-codepoint
    // scan via `Pattern::is_contained_in` and pays full UTF-8 decode
    // cost on every char of the input. memmem uses Two-Way / SIMD on
    // the 3-byte needle and zooms through Japanese prose at memory-
    // bandwidth speed.
    let mut accent_diagnostics: Vec<Diagnostic> = Vec::new();
    let text: Cow<'_, str> =
        if memmem::find(rule_isolated.as_bytes(), TORTOISE_OPEN_BYTES).is_some() {
            let owned = rule_isolated.into_owned();
            Cow::Owned(rewrite_accent_spans_collecting(
                &owned,
                &mut accent_diagnostics,
            ))
        } else {
            rule_isolated
        };

    SanitizeOutput {
        text,
        diagnostics: accent_diagnostics,
    }
}

/// As [`sanitize`], but additionally returns a [`SanitizeMaps`].
///
/// Composes one [`OffsetMap`] per transform step, so a downstream consumer
/// can translate a byte offset in the sanitized `text` back to the
/// corresponding offset in `source` (groundwork for span
/// semantics). Mirrors `sanitize`'s exact step sequence and MUST stay
/// bit-identical to it — both delegate to the same `_core` functions, so
/// there is only one implementation of each step to drift.
#[must_use]
pub fn sanitize_mapped(source: &str) -> SanitizeMappedOutput<'_> {
    let mut steps: Vec<OffsetMap> = Vec::with_capacity(5);

    // Step 1: BOM strip. See `sanitize` for the stacked-BOM rationale.
    let mut after_bom = source;
    let mut bom_bytes = 0usize;
    while let Some(rest) = after_bom.strip_prefix('\u{FEFF}') {
        bom_bytes += after_bom.len() - rest.len();
        after_bom = rest;
    }
    steps.push(if bom_bytes > 0 {
        OffsetMap {
            edits: vec![MapEdit {
                src_start: 0,
                src_end: bom_bytes,
                dst_start: 0,
                dst_end: 0,
            }],
        }
    } else {
        OffsetMap::default()
    });

    // Step 2: CR/LF normalization.
    let line_normalized: Cow<'_, str> = if after_bom.contains('\r') {
        let mut edits = Vec::new();
        let out = normalize_line_endings_core(after_bom, Some(&mut edits));
        steps.push(OffsetMap { edits });
        Cow::Owned(out)
    } else {
        steps.push(OffsetMap::default());
        Cow::Borrowed(after_bom)
    };

    // Step 3: decorative-rule isolation.
    let rule_isolated: Cow<'_, str> = if has_long_rule_line(&line_normalized) {
        let mut edits = Vec::new();
        let out = isolate_decorative_rules_core(&line_normalized, Some(&mut edits));
        steps.push(OffsetMap { edits });
        Cow::Owned(out)
    } else {
        steps.push(OffsetMap::default());
        line_normalized
    };

    // Step 4: accent decomposition inside tortoiseshell brackets.
    let mut accent_diagnostics: Vec<Diagnostic> = Vec::new();
    let text: Cow<'_, str> = if memmem::find(rule_isolated.as_bytes(), TORTOISE_OPEN_BYTES)
        .is_some()
    {
        let owned = rule_isolated.into_owned();
        let mut edits = Vec::new();
        let out =
            rewrite_accent_spans_collecting_core(&owned, &mut accent_diagnostics, Some(&mut edits));
        steps.push(OffsetMap { edits });
        Cow::Owned(out)
    } else {
        steps.push(OffsetMap::default());
        rule_isolated
    };

    SanitizeMappedOutput {
        text,
        diagnostics: accent_diagnostics,
        maps: SanitizeMaps { steps },
    }
}

/// Rewrite complete, unnested `〔...〕` accent scopes within each source line.
/// Converted spans lose their accent-scope brackets; literal bracket text is preserved.
#[doc(hidden)]
#[must_use]
pub fn rewrite_accent_spans(input: &str) -> String {
    // Discard the per-span notes; the public, diagnostic-free entry point
    // keeps its `-> String` shape for existing callers and tests.
    let mut sink = Vec::new();
    rewrite_accent_spans_collecting(input, &mut sink)
}

/// As [`rewrite_accent_spans`], but additionally pushes one
/// [`Diagnostic::accent_decomposition_applied`] (a `Note`) for every
/// digraph **substitution site** inside a `〔…〕` span; a `〔…〕` that
/// contains no accent digraph is silent.
///
/// Spans are reported in **output (post-decomposition) coordinates** and
/// bracket exactly the replacement character. Accent decomposition is
/// *not* byte-length-preserving, so
/// an input-coordinate span would slide once the first digraph changes
/// width. The downstream stages — and the CLI's miette renderer — see the
/// rewritten text, so output coordinates put the caret on the right
/// character.
fn rewrite_accent_spans_collecting(input: &str, diagnostics: &mut Vec<Diagnostic>) -> String {
    rewrite_accent_spans_collecting_core(input, diagnostics, None)
}

/// Core of [`rewrite_accent_spans_collecting`]; when `edits` is `Some`,
/// records delimiter deletions and one [`MapEdit`] per digraph substitution — the source digraph
/// bytes to the replacement character, in THIS step's input/output
/// coordinates. Length-preserving substitutions (`s&` = ß, `s,` = ş) are
/// recorded too: an edit marks "these bytes were rewritten", exactly as the
/// width-equal lone-`\r` → `\n` normalization does. Bytes *between* sites
/// carry no edit, so every unrewritten byte inside a span keeps its exact
/// source position — a whole-span edit here once collapsed every interior
/// fact onto the span start, which manufactured byte-identical
/// classified-source entries out of distinct constructs.
pub(super) fn rewrite_accent_spans_collecting_core(
    input: &str,
    diagnostics: &mut Vec<Diagnostic>,
    mut edits: Option<&mut Vec<MapEdit>>,
) -> String {
    let mut out = String::with_capacity(input.len());
    let mut cursor = 0;

    while cursor < input.len() {
        let Some(open_rel) = input[cursor..].find(TORTOISE_OPEN) else {
            // No more opens — copy the remainder verbatim and finish.
            out.push_str(&input[cursor..]);
            break;
        };
        let open_abs = cursor + open_rel;
        out.push_str(&input[cursor..open_abs]);

        let after_open = open_abs + TORTOISE_OPEN.len_utf8();
        let Some(close_rel) = input[after_open..].find(['\r', '\n', TORTOISE_OPEN, TORTOISE_CLOSE])
        else {
            // Unclosed `〔` — emit the rest verbatim so the author can
            // see the malformed span in the rendered output rather
            // than silently dropping content.
            out.push_str(&input[open_abs..]);
            break;
        };
        let close_abs = after_open + close_rel;
        // Aozora accent scopes close on the same line and cannot nest.
        if !input[close_abs..].starts_with(TORTOISE_CLOSE) {
            out.push_str(&input[open_abs..close_abs]);
            cursor = close_abs;
            continue;
        }

        let body = &input[after_open..close_abs];
        let sites = decompose_fragment_sites(body);
        if sites.is_empty() {
            out.push_str(&input[open_abs..close_abs + TORTOISE_CLOSE.len_utf8()]);
            cursor = close_abs + TORTOISE_CLOSE.len_utf8();
            continue;
        }
        if let Some(e) = edits.as_deref_mut() {
            e.push(MapEdit {
                src_start: open_abs,
                src_end: after_open,
                dst_start: out.len(),
                dst_end: out.len(),
            });
        }
        let mut body_cursor = 0;
        for (site_off, in_len, replacement) in sites {
            out.push_str(&body[body_cursor..site_off]);
            let dst_start = out.len();
            out.push(replacement);
            let dst_end = out.len();
            // `out.len()` fits u32 by the same sanitize-entry length cap
            // at the sanitize boundary; accent decomposition only ever
            // adds a bounded handful of combining bytes per digraph.
            #[allow(
                clippy::cast_possible_truncation,
                reason = "sanitized text length <= u32::MAX is asserted at sanitize entry"
            )]
            diagnostics.push(Diagnostic::accent_decomposition_applied(Span::new(
                dst_start as u32,
                dst_end as u32,
            )));
            if let Some(e) = edits.as_deref_mut() {
                e.push(MapEdit {
                    src_start: after_open + site_off,
                    src_end: after_open + site_off + in_len,
                    dst_start,
                    dst_end,
                });
            }
            body_cursor = site_off + in_len;
        }
        out.push_str(&body[body_cursor..]);
        if let Some(e) = edits.as_deref_mut() {
            e.push(MapEdit {
                src_start: close_abs,
                src_end: close_abs + TORTOISE_CLOSE.len_utf8(),
                dst_start: out.len(),
                dst_end: out.len(),
            });
        }

        cursor = close_abs + TORTOISE_CLOSE.len_utf8();
    }

    out
}

/// Return `true` when at least one line in `input` is a decorative
/// rule (≥ `DECORATIVE_RULE_MIN_LEN` of `-` / `=` / `_`).
///
/// Used as a fast-path gate in [`sanitize`]: when the whole document
/// has no long rule line, the pass is a no-op and [`Cow::Borrowed`]
/// survives.
#[doc(hidden)]
pub fn has_long_rule_line(input: &str) -> bool {
    input.lines().any(is_decorative_rule_line)
}

/// Return `true` when `line` is composed of ≥ `DECORATIVE_RULE_MIN_LEN`
/// repeats of a single `-` / `=` / `_` character with no other content
/// (surrounding whitespace is tolerated to match real-world formatting).
fn is_decorative_rule_line(line: &str) -> bool {
    is_rule_line_trimmed(line.trim())
}

/// Byte-level rule-line check on a string the caller has already
/// trimmed. Used by [`isolate_decorative_rules`] which also needs
/// the trimmed length for the blank-line bookkeeping — sharing the
/// trim avoids the duplicate work the prior split called for.
///
/// `-` / `=` / `_` are ASCII single-byte characters, so the
/// `bytes().all(...)` comparison is a `memcmp`-class scan. For lines
/// whose first byte is multi-byte UTF-8 (every Japanese line in the
/// corpus, the dominant case) the leading `matches!` check rejects
/// in 2–3 ops and the rest of the function is skipped entirely.
///
/// `pub` (and `#[doc(hidden)]`, like the other sanitize helpers) so the
/// in-workspace LSP `ParseCache` can reuse the *exact* decorative-rule
/// predicate when it (a) precomputes which raw lines gained an isolation
/// blank and (b) gates an incremental edit that would create or destroy a
/// rule line — sharing the predicate keeps the rope splice byte-identical
/// to a full sanitize.
#[doc(hidden)]
#[must_use]
pub fn is_rule_line_trimmed(trimmed: &str) -> bool {
    let bytes = trimmed.as_bytes();
    if bytes.len() < DECORATIVE_RULE_MIN_LEN {
        return false;
    }
    let first = bytes[0];
    if !matches!(first, b'-' | b'=' | b'_') {
        return false;
    }
    bytes.iter().all(|&b| b == first)
}

/// Insert a blank line before every decorative rule that would
/// otherwise be interpreted by `CommonMark` as a setext underline for
/// the preceding paragraph. The output differs from the input *only*
/// in the blank lines inserted.
///
/// ## Algorithm
///
/// `memchr::memchr_iter(b'\n', ...)` walks every newline position via
/// SIMD byte scan. For each line we run [`is_decorative_rule_line`]
/// (which exits in O(1) when the trimmed first char isn't `-=_`,
/// covering ≥99% of Aozora lines). Only when a rule line needs an
/// inserted blank line does the algorithm break the running bulk-copy
/// to flush `[copy_from..line_start)` and emit a `\n`.
///
/// Replaces a previous `for line in input.split_inclusive('\n')` /
/// `out.push_str(line)` loop that paid one `push_str` (one `memcpy`)
/// per line. Real Aozora corpora have ~10⁴ short lines per document
/// and typically only 1–5 rule line insertions, so the new path
/// collapses ~10⁴ small `memcpy`s into a small handful of large ones.
#[doc(hidden)]
#[must_use]
pub fn isolate_decorative_rules(input: &str) -> String {
    isolate_decorative_rules_core(input, None)
}

/// Core of [`isolate_decorative_rules`]; when `edits` is `Some`, records
/// one [`MapEdit`] per inserted blank line — an empty source span (the
/// insertion point) to the one-byte `\n` it produced (offset-map
/// groundwork).
fn isolate_decorative_rules_core(input: &str, mut edits: Option<&mut Vec<MapEdit>>) -> String {
    let bytes = input.as_bytes();
    let mut out = String::with_capacity(input.len() + 16);
    let mut line_start: usize = 0;
    let mut copy_from: usize = 0;
    let mut prev_nonblank = false;

    for nl_pos in memchr::memchr_iter(b'\n', bytes) {
        let line_no_eol = &input[line_start..nl_pos];
        // Single trim per line: feed the result to both the rule check
        // and the blank-line bookkeeping. Avoids the double `.trim()`
        // the prior implementation paid on every line.
        let trimmed = line_no_eol.trim();
        if is_rule_line_trimmed(trimmed) && prev_nonblank {
            // Flush the bulk-copy run up to (but not including) this
            // rule line, then inject the separating blank line. The
            // rule line itself stays in the next bulk-copy chunk.
            out.push_str(&input[copy_from..line_start]);
            let dst_start = out.len();
            out.push('\n');
            if let Some(e) = edits.as_deref_mut() {
                e.push(MapEdit {
                    src_start: line_start,
                    src_end: line_start,
                    dst_start,
                    dst_end: dst_start + 1,
                });
            }
            copy_from = line_start;
        }
        // A rule line (or any visible line) keeps `prev_nonblank` true;
        // an empty / whitespace-only line flips it false so the next
        // rule line does not trigger another spurious insertion.
        prev_nonblank = !trimmed.is_empty();
        line_start = nl_pos + 1;
    }
    // Final tail line (no trailing `\n`). Mirrors the per-line check.
    if line_start < bytes.len() {
        let tail = &input[line_start..];
        let tail_trimmed = tail.trim();
        if is_rule_line_trimmed(tail_trimmed) && prev_nonblank {
            out.push_str(&input[copy_from..line_start]);
            let dst_start = out.len();
            out.push('\n');
            if let Some(e) = edits {
                e.push(MapEdit {
                    src_start: line_start,
                    src_end: line_start,
                    dst_start,
                    dst_end: dst_start + 1,
                });
            }
            copy_from = line_start;
        }
    }
    // Single closing flush emits the unmodified tail of the input
    // verbatim. Typical corpus documents take this path with
    // `copy_from == 0` and one big `push_str` of the whole buffer.
    if copy_from < bytes.len() {
        out.push_str(&input[copy_from..]);
    }
    out
}

/// Normalise line endings: every `\r\n` and every standalone `\r`
/// collapses to a single `\n`.
///
/// ## Algorithm
///
/// `memchr::memchr_iter(b'\r', ...)` walks every `\r` position in the
/// input via SIMD-accelerated byte scan, bulk-copying the inter-`\r`
/// runs through `push_str` (one `memcpy` per chunk). At each hit a
/// single-byte lookahead distinguishes `\r\n` (skip both, emit `\n`)
/// from a lone `\r` (skip the `\r`, emit `\n`).
///
/// One pass over the input, one buffer allocation. Replaces the prior
/// `.replace("\r\n", "\n").replace('\r', "\n")` pair which materialised
/// **two** intermediate `String`s and walked the input twice. On the
/// 17 k-document Aozora corpus — where every document arrives with
/// CRLF line endings (the archive's house format) — this sub-pass is
/// the dominant cost in the sanitize stage; the single-pass form is
/// ~2–3× faster at memory-bandwidth ceiling.
///
/// `\r` (0x0D) is ASCII so `memchr` lands cleanly on UTF-8 boundaries;
/// no need for `is_char_boundary` checks.
#[doc(hidden)]
#[must_use]
pub fn normalize_line_endings(input: &str) -> String {
    normalize_line_endings_core(input, None)
}

/// Core of [`normalize_line_endings`]; when `edits` is `Some`, records
/// one [`MapEdit`] per `\r\n`→`\n` or lone `\r`→`\n` substitution in this
/// step's own input/output coordinates (offset-map groundwork).
fn normalize_line_endings_core(input: &str, mut edits: Option<&mut Vec<MapEdit>>) -> String {
    let bytes = input.as_bytes();
    let mut out = String::with_capacity(input.len());
    let mut cursor = 0;
    for cr_pos in memchr::memchr_iter(b'\r', bytes) {
        // Bulk-copy the run between the previous cursor and this `\r`.
        // `push_str` lowers to a single `memcpy` when the chunk is
        // contiguous and non-empty.
        if cr_pos > cursor {
            out.push_str(&input[cursor..cr_pos]);
        }
        // Always emit one `\n` for the line terminator. Skip the
        // following `\n` if this is `\r\n` (the CRLF path); otherwise
        // step past the lone `\r` only.
        let dst_start = out.len();
        out.push('\n');
        let src_end = if bytes.get(cr_pos + 1) == Some(&b'\n') {
            cr_pos + 2
        } else {
            cr_pos + 1
        };
        if let Some(e) = edits.as_deref_mut() {
            e.push(MapEdit {
                src_start: cr_pos,
                src_end,
                dst_start,
                dst_end: dst_start + 1,
            });
        }
        cursor = src_end;
    }
    if cursor < bytes.len() {
        out.push_str(&input[cursor..]);
    }
    out
}

#[cfg(test)]
mod tests {
    use proptest::prelude::*;

    use super::*;

    #[test]
    fn plain_ascii_is_borrowed_and_unchanged() {
        let input = "hello world";
        let out = sanitize(input);
        assert!(matches!(out.text, Cow::Borrowed(_)));
        assert_eq!(out.text.as_ref(), input);
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn leading_bom_is_stripped() {
        let input = "\u{FEFF}hello";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "hello");
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn bom_only_inside_source_is_not_stripped() {
        let input = "abc\u{FEFF}def";
        let out = sanitize(input);
        // Only a *leading* BOM gets stripped; interior U+FEFF is left as
        // zero-width no-break space (the other meaning of the codepoint).
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn stacked_leading_boms_are_all_stripped() {
        // I3 fixed-point regression: `to_source(to_source(x))` must
        // byte-equal `to_source(x)`. Stacked leading BOMs would
        // otherwise peel off one per round-trip pass, so the strip
        // loop has to consume every leading `U+FEFF`.
        let input = "\u{FEFF}\u{FEFF}\u{FEFF}hello";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "hello");
    }

    #[test]
    fn leading_boms_only_resolve_to_empty() {
        // Edge case: an input that is *nothing but* leading BOMs
        // resolves to the empty string. The previous single-strip
        // behaviour produced `""` for one BOM and `"\u{feff}"` for
        // two — the source of the I3 fuzz crash.
        let out = sanitize("\u{FEFF}\u{FEFF}");
        assert_eq!(out.text.as_ref(), "");
    }

    #[test]
    fn crlf_is_normalized_to_lf() {
        let input = "line1\r\nline2\r\nline3";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "line1\nline2\nline3");
        assert!(matches!(out.text, Cow::Owned(_)));
    }

    #[test]
    fn lone_cr_is_normalized_to_lf() {
        let input = "old-mac\rstyle";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "old-mac\nstyle");
    }

    #[test]
    fn mixed_cr_and_crlf_both_become_single_lf() {
        let input = "a\r\nb\rc\r\nd";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "a\nb\nc\nd");
    }

    #[test]
    fn literal_private_use_characters_remain_borrowed_text() {
        let input = "前\u{e000}\u{e001}\u{e002}\u{e003}\u{e004}\u{f8ff}後";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
        assert!(matches!(out.text, Cow::Borrowed(_)));
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn bom_and_newline_normalization_preserve_literal_private_use_characters() {
        let input = "\u{FEFF}hello\r\n\u{E003}world";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "hello\n\u{E003}world");
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn empty_input_produces_empty_output() {
        let out = sanitize("");
        assert!(out.text.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn bom_only_input_produces_empty_output() {
        let out = sanitize("\u{FEFF}");
        assert!(out.text.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    // -----------------------------------------------------------------
    // Accent-decomposition inside 〔...〕.
    // -----------------------------------------------------------------

    #[test]
    fn pure_japanese_is_not_accent_rewritten_and_stays_borrowed() {
        let input = "これはただの日本語の文章です。";
        let out = sanitize(input);
        assert!(matches!(out.text, Cow::Borrowed(_)));
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn plain_commonmark_without_tortoiseshell_stays_borrowed() {
        let input = "# heading\n\nParagraph with `code` and *emph*.\n";
        let out = sanitize(input);
        assert!(matches!(out.text, Cow::Borrowed(_)));
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn accent_digraph_inside_tortoiseshell_is_decomposed() {
        // The 罪と罰 canary: the grave-accent digraph `e`` must collapse
        // to `è` inside the span so the parser never sees the lone backtick.
        let input = "〔oraison fune`bre〕";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "oraison funèbre");
        assert!(!out.text.contains('`'));
    }

    #[test]
    fn literal_tortoiseshell_brackets_without_accent_notation_are_preserved() {
        let input = "〔Où〕";
        let out = sanitize(input);
        assert!(out.text.contains('〔'));
        assert!(out.text.contains('〕'));
    }

    #[test]
    fn text_outside_tortoiseshell_spans_is_not_decomposed() {
        // `text,` stays as-is; only `cafe'` inside the span becomes `café`.
        let input = "text, 〔cafe'〕, rest";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "text, café, rest");
        assert!(out.text.starts_with("text,"));
    }

    #[test]
    fn multiple_tortoiseshell_spans_are_each_rewritten() {
        let input = "前〔a`〕中〔e'〕後";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "前à中é後");
    }

    #[test]
    fn accent_scopes_do_not_capture_later_openings_or_source_lines() {
        for (input, expected) in [
            ("先〔未閉\n次〔Le'on〕後", "先〔未閉\n次Léon後"),
            ("〔cafe'\r\n次〔e'〕", "〔cafe'\n次é"),
            ("\u{feff}〔未閉\r\n次〔Le'on〕", "〔未閉\n次Léon"),
            ("〔外〔cafe'〕後〕", "〔外café後〕"),
            ("〔a'〔e'〕o'〕", "〔a'éo'〕"),
            ("〔本全集〕、〔Le'on Walras〕", "〔本全集〕、Léon Walras"),
        ] {
            let out = sanitize_mapped(input);
            assert_eq!(out.text.as_ref(), expected, "{input:?}");
            assert_eq!(sanitize(input).text.as_ref(), expected);
            assert_eq!(out.diagnostics.len(), expected.matches('é').count());
            for diagnostic in &out.diagnostics {
                if let Diagnostic::AccentDecompositionApplied { span, .. } = diagnostic {
                    let start = usize::try_from(span.start).unwrap();
                    let end = usize::try_from(span.end).unwrap();
                    let source_start = out.maps.to_source_offset(start);
                    let source_end = out.maps.to_source_end(end);
                    assert_eq!(&input[source_start..source_end], "e'");
                }
            }
        }
    }

    #[test]
    fn unclosed_tortoiseshell_span_passes_through_verbatim() {
        // Graceful degradation — don't panic, emit the rest as-is so a
        // later stage can surface a diagnostic.
        let input = "tail 〔fune`bre without close";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn empty_tortoiseshell_span_is_idempotent() {
        let input = "〔〕 empty";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn nested_literal_brackets_without_accent_codes_are_preserved() {
        let input = "〔outer 〔inner`〕〕";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn tortoiseshell_plus_crlf_plus_bom_all_applied() {
        let input = "\u{FEFF}〔fune`bre〕\r\nend";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), "funèbre\nend");
        assert!(!out.text.contains('`'), "grave accent must be consumed");
    }

    // -------------------------------------------------------------
    // Decorative rule isolation — long `-` / `=` / `_` rows must not
    // be misread as setext underlines for a preceding paragraph.
    //
    // Background: Aozora Bunko prose frequently inserts
    // `---------------------------------------------------------`
    // as a visual separator between front matter and body. Without
    // this pass, CommonMark would swallow the front-matter paragraph
    // into an H2. These tests pin both halves of the contract — long
    // runs are isolated, short runs (the genuine setext idiom) are
    // untouched — so future refactors cannot silently regress either
    // direction.
    // -------------------------------------------------------------

    #[test]
    fn long_hyphen_rule_gets_blank_line_before_it() {
        let input = "前置き\n-----------\n本文";
        let out = sanitize(input);
        assert!(
            out.text.contains("前置き\n\n-----------"),
            "expected blank line inserted; got {:?}",
            out.text
        );
    }

    #[test]
    fn long_equals_rule_gets_blank_line_before_it() {
        let input = "前置き\n===============\n本文";
        let out = sanitize(input);
        assert!(
            out.text.contains("前置き\n\n==============="),
            "expected blank line before long-equals rule; got {:?}",
            out.text
        );
    }

    #[test]
    fn long_underscore_rule_gets_blank_line_before_it() {
        let input = "前置き\n____________\n本文";
        let out = sanitize(input);
        assert!(
            out.text.contains("前置き\n\n____________"),
            "expected blank line before long-underscore rule; got {:?}",
            out.text
        );
    }

    #[test]
    fn short_hyphen_setext_underline_is_not_split() {
        // The genuine setext-heading idiom uses `---` or `===` rows
        // of modest length (typically < 10 chars). Those must reach
        // unmodified so the H1/H2 promotion still fires.
        let input = "Heading\n---\nbody";
        let out = sanitize(input);
        assert_eq!(
            out.text.as_ref(),
            input,
            "short setext underline must not gain a blank line"
        );
    }

    #[test]
    fn nine_char_hyphen_row_stays_as_setext_underline() {
        // Nine characters: still inside the setext-heading length
        // range per our DECORATIVE_RULE_MIN_LEN threshold.
        let input = "Heading\n---------\nbody";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn ten_char_hyphen_row_is_isolated() {
        // Ten characters — the first length at which we classify the
        // row as decorative rather than setext.
        let input = "Heading\n----------\nbody";
        let out = sanitize(input);
        assert!(
            out.text.contains("Heading\n\n----------"),
            "expected 10-char rule to be isolated; got {:?}",
            out.text
        );
    }

    #[test]
    fn rule_already_preceded_by_blank_line_is_unchanged() {
        // Idempotence: if the author already put a blank line before
        // the rule, we must not add a second.
        let input = "前置き\n\n-----------\n本文";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn document_without_any_rule_stays_borrowed() {
        // The fast-path gate (`has_long_rule_line`) must keep the
        // common case allocation-free.
        let input = "plain paragraph\n\nsecond paragraph";
        let out = sanitize(input);
        assert!(
            matches!(out.text, Cow::Borrowed(_)),
            "documents without a long rule must pass through borrowed"
        );
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn rule_at_document_start_is_unchanged() {
        // With no preceding non-blank line, the setext-heading
        // confusion cannot arise — no blank line needed.
        let input = "-----------\n本文";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn mixed_character_rule_is_not_isolated() {
        // `---===---` is neither a valid setext underline nor a
        // homogeneous rule; leave it alone so CommonMark handles it
        // as a plain paragraph line.
        let input = "text\n---===---\ntail";
        let out = sanitize(input);
        assert_eq!(out.text.as_ref(), input);
    }

    #[test]
    fn consecutive_rule_rows_each_get_isolated() {
        // Author stacks two rules back-to-back for a thick border.
        // Current policy isolates every decorative rule uniformly;
        // the extra blank line between two rules is a no-op in
        // CommonMark (both become `<hr>` regardless), so the simpler
        // uniform behaviour is preferred over a conditional that
        // special-cases rule-after-rule. Test documents the shape so
        // a future tightening that skips the second isolation has to
        // update this expectation deliberately.
        let input = "前置き\n----------\n==========\n本文";
        let out = sanitize(input);
        assert_eq!(
            out.text.as_ref(),
            "前置き\n\n----------\n\n==========\n本文"
        );
    }

    #[test]
    fn aozora_style_long_rule_fixture_shape() {
        // Direct analogue of the `spec/aozora/fixtures/56656/input.utf8.txt`
        // front-matter: a prose paragraph (here condensed) immediately
        // followed by a 55-char `-` row. The promotion would otherwise
        // turn the prose into a setext H2; the isolation pass must
        // separate them so the paragraph reaches the parser as a
        // paragraph.
        let rule: String = "-".repeat(55);
        let input = format!("凡例です。\n{rule}\n本文");
        let out = sanitize(&input);
        let expected = format!("凡例です。\n\n{rule}\n本文");
        assert_eq!(out.text.as_ref(), expected);
    }

    #[test]
    fn every_backtick_inside_vowel_span_collapses() {
        // Every vowel base + grave accent digraph has a table entry,
        // so no backtick survives inside a `〔<vowel>`〕` span.
        for base in ['a', 'e', 'i', 'o', 'u'] {
            let input = format!("〔x{base}`y〕");
            let out = sanitize(&input);
            assert!(
                !out.text.contains('`'),
                "backtick survived for base {base:?}: {:?}",
                out.text
            );
        }
    }

    // -------------------------------------------------------------
    // `sanitize_mapped` + `OffsetMap` — offset bookkeeping
    // across the five sanitize transform steps.
    // -------------------------------------------------------------

    #[test]
    fn sanitize_mapped_text_equals_sanitize() {
        for src in [
            "\u{feff}あ\r\nい\r う",
            "plain",
            "〔fune`bre〕\r\n----------\nx",
            "あ\u{e001}い",
            "",
        ] {
            let plain = sanitize(src);
            let mapped = sanitize_mapped(src);
            assert_eq!(plain.text, mapped.text, "text drift on {src:?}");
            assert_eq!(plain.diagnostics.len(), mapped.diagnostics.len());
        }
    }

    #[test]
    fn offset_map_translates_through_bom_and_crlf() {
        // source: BOM(3) + "あ\r\nい"  → sanitized: "あ\nい"
        let src = "\u{feff}あ\r\nい";
        let m = sanitize_mapped(src).maps;
        assert_eq!(m.to_source_offset(0), 3); // あ starts after BOM
        assert_eq!(m.to_source_offset(3), 6); // \n ← \r\n start
        assert_eq!(m.to_source_end(4), 8); // end of \n ← end of \r\n
        assert_eq!(m.to_source_offset(4), 8); // い
    }

    #[test]
    fn bytes_between_accent_sites_keep_exact_source_positions() {
        // `〔Henri《ア》 Re'gnier《レ》〕`: one digraph site (`e'` → é). Every
        // byte outside that site — the ruby runs before AND after it — must
        // translate to its own source position, not to the span start. The
        // old whole-span edit collapsed all of them onto `〔`, which
        // manufactured byte-identical ledger entries out of distinct
        // constructs (the corpus-wide duplicate-entry capture failures).
        let src = "〔Henri《ア》 Re'gnier《レ》〕";
        let out = sanitize_mapped(src);
        let dst = out.text.as_ref();
        assert_eq!(dst, "Henri《ア》 Régnier《レ》");
        for needle in ["《ア》", "《レ》", "Henri", "gnier"] {
            let d = dst.find(needle).unwrap();
            let s = src.find(needle).unwrap();
            assert_eq!(
                out.maps.to_source_offset(d),
                s,
                "{needle:?} start must map to its own source position"
            );
            assert_eq!(
                out.maps.to_source_end(d + needle.len()),
                s + needle.len(),
                "{needle:?} end must map to its own source position"
            );
            assert!(!out.maps.is_edited(d), "{needle:?} is not a rewrite site");
        }
        // The site itself: é maps back to exactly the 2-byte digraph.
        let site = dst.find('é').unwrap();
        assert!(out.maps.is_edited(site));
        assert_eq!(out.maps.to_source_offset(site), src.find("e'").unwrap());
        assert_eq!(
            out.maps.to_source_end(site + 'é'.len_utf8()),
            src.find("e'").unwrap() + 2
        );
    }

    #[test]
    fn each_accent_site_gets_its_own_diagnostic_bracketing_the_replacement() {
        // Two digraphs in one span → two Notes, each on its replacement
        // character in output coordinates — not one whole-span Note.
        let out = sanitize("〔ve'rite'〕");
        let dst = out.text.as_ref();
        assert_eq!(dst, "vérité");
        let spans: Vec<(usize, usize)> = out
            .diagnostics
            .iter()
            .filter(|d| d.code() == "aozora::lex::accent_decomposition_applied")
            .map(|d| (d.span().start as usize, d.span().end as usize))
            .collect();
        let expected: Vec<(usize, usize)> = dst
            .match_indices('é')
            .map(|(i, m)| (i, i + m.len()))
            .collect();
        assert_eq!(spans, expected);
        // A span whose body has no digraph stays silent.
        assert!(
            sanitize("〔plain〕")
                .diagnostics
                .iter()
                .all(|d| d.code() != "aozora::lex::accent_decomposition_applied")
        );
    }

    /// Width-equality is not unedited-ness: lone `\r`→`\n` is a
    /// width-equal rewrite (plan test corrected per prose property).
    #[test]
    fn offset_map_is_monotone_and_content_preserving() {
        let src = "\u{feff}前〔e'te'〕中\r\n==========\n後\r尾";
        let out = sanitize_mapped(src);
        let dst = out.text.as_ref();
        let mut prev = 0usize;
        for i in (0..=dst.len()).filter(|i| dst.is_char_boundary(*i)) {
            let s = out.maps.to_source_offset(i);
            assert!(s >= prev && s <= src.len(), "monotonicity at {i}");
            prev = s;
        }
        // Unedited chars must map to a width-equal source region that
        // slices identically; edited chars are exempt entirely.
        for (i, ch) in dst.char_indices() {
            if out.maps.is_edited(i) {
                continue;
            }
            let (s, e) = (
                out.maps.to_source_offset(i),
                out.maps.to_source_end(i + ch.len_utf8()),
            );
            assert_eq!(e - s, ch.len_utf8(), "unedited char changed width at {i}");
            assert_eq!(
                &src[s..e],
                &dst[i..i + ch.len_utf8()],
                "content drift at {i}"
            );
        }
        // Pin the traced rewrite: the `\n` produced from the lone `\r`
        // (between 後 and 尾) IS an edit and maps to the
        // lone `\r`'s exact source range (src 36..37).
        let newline = dst.rfind('\n').unwrap();
        assert!(out.maps.is_edited(newline));
        assert_eq!(out.maps.to_source_offset(newline), 36);
        assert_eq!(out.maps.to_source_end(newline + 1), 37);
        assert_eq!(&src[36..37], "\r");
    }

    proptest! {
        #[test]
        fn mapped_output_always_matches_unmapped(parts in prop::collection::vec(
            prop_oneof![
                Just("あいう".to_owned()),
                Just("\r\n".to_owned()),
                Just("\r".to_owned()),
                Just("〔cafe'〕".to_owned()),
                Just("----------\n".to_owned()),
                Just("\u{feff}".to_owned()),
                Just("\u{e001}".to_owned()),
            ], 0..12)) {
            let src: String = parts.concat();
            let plain = sanitize(&src);
            let mapped = sanitize_mapped(&src);
            prop_assert_eq!(plain.text.as_ref(), mapped.text.as_ref());
            let dst_len = mapped.text.len();
            let mut prev = 0usize;
            for i in (0..=dst_len).filter(|i| mapped.text.is_char_boundary(*i)) {
                let s = mapped.maps.to_source_offset(i);
                prop_assert!(s >= prev && s <= src.len());
                prev = s;
            }
        }
    }
}
