//! Type-state lex pipeline.
//!
//! `Pipeline<'src, S>` makes the lex stage order enforceable at compile time.
//! The state markers [`Source`], [`Sanitized`], [`Tokenized`], [`Paired`] track
//! which stages have run; methods consume `self` and return the next state.
//! Calling `.pair()` on a `Source` is a type error; calling `.tokenize()` twice
//! is a type error; etc.
//!
//! # Two entry shapes
//!
//! - [`Pipeline::run_to_completion`] — one-shot, equivalent to [`crate::lex`].
//!   Used by `Document::parse` and the FFI / WASM / Python drivers.
//! - [`Pipeline::new`] → `.sanitize()` → `.tokenize()` → `.pair()` →
//!   `.build()` — explicit chain. Use for inspection / instrumentation: each
//!   intermediate state exposes accessors (`.sanitized_text()`, `.tokens()`,
//!   `.events()`, `.diagnostics()`) so callers can probe the partial output
//!   without re-running the pipeline.
//!
//! # State carries its own payload
//!
//! Each state marker is a field-bound struct holding exactly the stage outputs
//! it has produced (`Sanitized` carries the sanitized `String`; `Tokenized`
//! adds the token `Vec`; …). Reading `.sanitized_text()` from
//! `Pipeline<'_, Sanitized>` is a field projection on the state struct — no
//! `Option::expect` lives in production code.
//!
//! # Owned, arena-free
//!
//! Every stage operates on owned data: the sanitized text is an owned `String`,
//! the token / event lists are `Vec`s, and the classify stage builds the owned
//! AST directly into an
//! [`Allocator`]'s
//! [`NodeStore`], which threads straight into
//! the returned [`LexOutput`]. There is no bumpalo arena.
//!
//! # Why `build` is the terminal transition
//!
//! The classify stage requires `&mut Allocator`. We collapse the classify
//! stage + the normalize fold into a single terminal `.build()` call —
//! inspection up through `Paired` works freely; the final pass is atomic.

use crate::lexer::{
    ClassifiedSpan, PairEvent, SpanKind, Token, classify, pair, sanitize, tokenize,
};
use ab_aozora_spec::{Diagnostic, PairKind, PairLink};

use ab_aozora_syntax::accent::{compose_accent_dots, decompose_fragment_sites};
use ab_aozora_syntax::alloc::Allocator;
use ab_aozora_syntax::ast::canonicalize_classified_source_facts;
use ab_aozora_syntax::ast::{LexOutput, Node, NodeStore, NonEmptySpan, Registry, Segment};
use ab_aozora_syntax::format::ForwardOrigin;
use ab_aozora_syntax::{ForwardAttr, Span};
use std::collections::BTreeMap;

use crate::fold::Normalizer;

// =====================================================================
// State markers (field-bound — each state carries the stage output it is
// responsible for).
// =====================================================================

/// Initial state — no stage has run yet.
#[derive(Debug, Clone, Copy)]
pub struct Source;

/// The sanitize stage has run; the sanitized text is owned.
#[derive(Debug, Clone)]
pub struct Sanitized {
    sanitized_text: String,
}

/// The tokenize stage has run; the token list is materialised.
#[derive(Debug)]
pub struct Tokenized {
    sanitized_text: String,
    tokens: Vec<Token>,
}

/// The pair stage has run; the event list and the resolved (open, close) link
/// side-table are materialised.
#[derive(Debug)]
pub struct Paired {
    sanitized_text: String,
    events: Vec<PairEvent>,
    links: Vec<PairLink>,
}

// =====================================================================
// Pipeline
// =====================================================================

/// Type-state lex pipeline. Each state's transition method consumes `self`,
/// materialises its stage output into the next state struct, and returns a new
/// pipeline in the next state.
#[derive(Debug)]
pub struct Pipeline<'src, S> {
    source: &'src str,
    diagnostics: Vec<Diagnostic>,
    state: S,
}

// ---------------------------------------------------------------------
// Source
// ---------------------------------------------------------------------

impl<'src> Pipeline<'src, Source> {
    /// Wrap a source string for type-state-driven lex. The sanitize stage has
    /// not yet run; only `source` is set.
    #[must_use]
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            state: Source,
        }
    }

    /// One-shot driver: run every stage and return the final [`LexOutput`].
    /// Equivalent to [`crate::lex`].
    #[must_use]
    pub fn run_to_completion(source: &'src str) -> LexOutput {
        Self::new(source).sanitize().tokenize().pair().build()
    }

    /// Borrow the original source text.
    #[must_use]
    pub fn source(&self) -> &'src str {
        self.source
    }

    /// Run the sanitize stage, materialising the sanitized text as an owned
    /// `String`.
    #[must_use]
    pub fn sanitize(mut self) -> Pipeline<'src, Sanitized> {
        let out = sanitize(self.source);
        self.diagnostics.extend(out.diagnostics);
        Pipeline {
            source: self.source,
            diagnostics: self.diagnostics,
            state: Sanitized {
                sanitized_text: out.text.into_owned(),
            },
        }
    }
}

// ---------------------------------------------------------------------
// Sanitized
// ---------------------------------------------------------------------

impl<'src> Pipeline<'src, Sanitized> {
    /// Parse text whose caller already owns sanitization and its source maps.
    /// Token and source-node spans index these exact bytes. The caller retains
    /// any diagnostics produced by its sanitization pass.
    #[must_use]
    pub fn from_sanitized(source: &'src str) -> Self {
        Self {
            source,
            diagnostics: Vec::new(),
            state: Sanitized {
                sanitized_text: source.to_owned(),
            },
        }
    }

    /// Sanitized text.
    #[must_use]
    pub fn sanitized_text(&self) -> &str {
        &self.state.sanitized_text
    }

    /// Diagnostics accumulated through the sanitize stage.
    #[must_use]
    pub fn diagnostics(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    /// Run the tokenize stage, materialising the token list.
    #[must_use]
    pub fn tokenize(self) -> Pipeline<'src, Tokenized> {
        let tokens: Vec<Token> = tokenize(&self.state.sanitized_text).collect();
        Pipeline {
            source: self.source,
            diagnostics: self.diagnostics,
            state: Tokenized {
                sanitized_text: self.state.sanitized_text,
                tokens,
            },
        }
    }
}

// ---------------------------------------------------------------------
// Tokenized
// ---------------------------------------------------------------------

impl<'src> Pipeline<'src, Tokenized> {
    /// Borrow the materialised token list. Useful for instrumentation.
    #[must_use]
    pub fn tokens(&self) -> &[Token] {
        &self.state.tokens
    }

    /// Run the pair stage, materialising a paired-event stream and the resolved
    /// link side-table. The pair stage's diagnostics are drained into the
    /// pipeline's diagnostic accumulator immediately.
    #[must_use]
    pub fn pair(mut self) -> Pipeline<'src, Paired> {
        let Tokenized {
            sanitized_text,
            tokens,
        } = self.state;
        let mut pair_stream = pair(tokens.into_iter());
        let events: Vec<PairEvent> = (&mut pair_stream).collect();
        self.diagnostics.extend(pair_stream.take_diagnostics());
        let links = pair_stream.take_links();
        Pipeline {
            source: self.source,
            diagnostics: self.diagnostics,
            state: Paired {
                sanitized_text,
                events,
                links,
            },
        }
    }
}

// ---------------------------------------------------------------------
// Paired (terminal)
// ---------------------------------------------------------------------

impl Pipeline<'_, Paired> {
    /// Borrow the materialised pair-event list. Useful for inspection before
    /// `.build()`.
    #[must_use]
    pub fn events(&self) -> &[PairEvent] {
        &self.state.events
    }

    /// Borrow the resolved (open, close) pair side-table.
    #[must_use]
    pub fn links(&self) -> &[PairLink] {
        &self.state.links
    }

    /// Complete multiline accent scopes retained as source text rather than decoded.
    /// Ordinary tortoiseshell punctuation without accent digraphs is excluded.
    #[must_use]
    pub fn retained_multiline_accent_spans(&self) -> Vec<Span> {
        self.state
            .links
            .iter()
            .filter_map(|link| {
                if link.kind != PairKind::Tortoise {
                    return None;
                }
                let body =
                    &self.state.sanitized_text[link.open.end as usize..link.close.start as usize];
                (body.contains(['\r', '\n']) && !decompose_fragment_sites(body).is_empty())
                    .then(|| Span::new(link.open.start, link.close.end))
            })
            .collect()
    }

    /// Drive the classify stage + the owned normalizer fold and return the final
    /// [`LexOutput`]. Terminal transition.
    ///
    /// # Diagnostic order
    ///
    /// Sanitize stage → pair stage (unclosed/unmatched) → classify stage
    /// (unknown annotations etc.) → normalizer (mismatched container close).
    ///
    /// # Panics
    ///
    /// Panics if the sanitized source exceeds `u32::MAX` bytes (the lexer's
    /// `Span` width contract). In practice unreachable.
    #[must_use]
    pub fn build(mut self) -> LexOutput {
        let Paired {
            sanitized_text,
            events,
            links,
        } = self.state;
        let sanitized_len =
            u32::try_from(sanitized_text.len()).expect("sanitized source exceeds u32::MAX bytes");

        let mut alloc = Allocator::new();

        let (normalized, recorder, container_pairs, classify_diagnostics, norm_diagnostics, store) = {
            let mut normalizer = Normalizer::new(&sanitized_text, sanitized_text.len() / 64);

            // Drain the pair events through the streaming `classify` Iterator
            // path; collect the classified spans and the classify diagnostics,
            // then drop the stream so its `&mut alloc` borrow is released before
            // the NORMALIZE (lowering) pass mints its canonical core nodes.
            let mut events_iter = events.into_iter();
            let mut classify_stream = classify(&mut events_iter, &sanitized_text, &mut alloc);
            let spans: Vec<ClassifiedSpan> = (&mut classify_stream).collect();
            let mut classify_diagnostics: Vec<Diagnostic> = classify_stream.take_diagnostics();
            drop(classify_stream);
            let (mut lowered, ruby_base_decorated) =
                lower_spans(spans, &sanitized_text, &mut alloc);
            resolve_adjacent_note_targets(&mut lowered, &sanitized_text, alloc.store(), &links);
            resolve_adjacent_ruby_dots(&mut lowered, &sanitized_text, &mut alloc, &links);
            // Ruby-base forward emphasis: a directive the lowering pass
            // decorated onto a preceding ruby base is no longer an unstyled
            // decline, so drop its `forward_referent_not_stylable` warning. Only
            // the decorated directive spans are suppressed — cross-line /
            // multi-target / prior-construct declines keep their warning.
            if !ruby_base_decorated.is_empty() {
                classify_diagnostics.retain(|d| {
                    !(matches!(d, Diagnostic::ForwardReferentNotStylable { .. })
                        && ruby_base_decorated.contains(&d.span()))
                });
            }
            for span in &lowered {
                normalizer.emit(span);
            }
            normalizer.finish();
            // Move the owned products out, ending the normalizer's borrow of
            // `sanitized_text` so it can be moved into the output below.
            let Normalizer {
                out,
                recorder,
                container_pairs,
                diagnostics: norm_diagnostics,
                ..
            } = normalizer;
            let store = alloc.into_store();
            (
                out,
                recorder,
                container_pairs,
                classify_diagnostics,
                norm_diagnostics,
                store,
            )
        };

        // Classify-stage diagnostics first, then the normalizer's (post-classify)
        // set, so the final vector stays in pipeline-stage order.
        self.diagnostics.extend(classify_diagnostics);
        self.diagnostics.extend(norm_diagnostics);
        let intern_stats = store.interner.stats;

        // Classifier emits in source order, so the recorder's entries are already
        // sorted by position; `from_sorted_slice` skips the redundant sort.
        let registry = Registry::from_sorted_slice(&recorder.entries);
        let classified_source_facts =
            canonicalize_classified_source_facts(recorder.classified_source_facts);

        LexOutput::new(
            normalized,
            sanitized_text,
            registry,
            self.diagnostics,
            sanitized_len,
            links,
            recorder.source_nodes,
            classified_source_facts,
            container_pairs,
            intern_stats,
            store,
        )
    }
}

/// NORMALIZE (lowering) pass over the materialized classified-span list.
///
/// This is the seam the normalization waist is built on. It performs the
/// source-byte **drop-superset** the streaming window did: when a later span's
/// source span is a proper superset of an earlier one — a backward pull-back,
/// e.g. a promoted 大/中/小 heading reclaiming its referent line `序章\n`, or a
/// forward node reclaiming its predecessor literal — the subsumed earlier span
/// is dropped, so the normalizer (which appends in source order) does not emit
/// the reclaimed text twice. This overlap-truncate cured the round-trip
/// pathology; the surviving [`ForwardOrigin`] on each forward leaf is necessary
/// provenance.
///
/// Returns the lowered spans and the set of forward-directive spans the
/// ruby-base emphasis phase decorated — the builder suppresses those
/// directives' `forward_referent_not_stylable` warnings.
fn lower_spans(
    spans: Vec<ClassifiedSpan>,
    source: &str,
    alloc: &mut Allocator,
) -> (Vec<ClassifiedSpan>, Vec<Span>) {
    // Phase 0: resolve forward heading hints whose referent is the bare line
    // directly above the directive into promoted `Heading` nodes.
    let spans = promote_headings(spans, source, alloc);
    let mut out: Vec<ClassifiedSpan> = Vec::with_capacity(spans.len());
    for span in spans {
        while let Some(back) = out.last() {
            let (bs, be) = (back.source_span.start, back.source_span.end);
            let back_is_plain = matches!(back.kind, SpanKind::Plain(_));
            let (ss, se) = (span.source_span.start, span.source_span.end);
            if ss <= bs && be <= se && (ss < bs || se > be) {
                // Full superset: `span` reclaimed all of `back` (a promoted
                // heading swallowing its referent line). Drop `back`.
                out.pop();
            } else if back_is_plain && bs < ss && ss < be {
                // Partial overlap: `span` (a `Reclaimed` forward node) pulled its
                // source region back into the *tail* of a committed plain run —
                // truncate the plain so the literal is emitted once, by the node
                // (issue, unbounded growth).
                if let Some(last) = out.last_mut() {
                    last.source_span.end = ss;
                }
                break;
            } else {
                break;
            }
        }
        out.push(span);
    }
    // Apply a declined forward emphasis onto a preceding ruby base
    // it uniquely names.
    let decorated = decorate_ruby_bases(&mut out, source, alloc.store());
    (out, decorated)
}

/// Apply supplied dot selectors to an adjacent typed ruby base, never its reading.
fn resolve_adjacent_ruby_dots(
    spans: &mut [ClassifiedSpan],
    source: &str,
    alloc: &mut Allocator,
    links: &[PairLink],
) {
    if !spans.windows(2).any(|pair| {
        matches!(pair[0].kind, SpanKind::Aozora(Node::Ruby(_)))
            && matches!(pair[1].kind, SpanKind::Aozora(Node::Directive(_)))
    }) {
        return;
    }
    let pairs: BTreeMap<_, _> = links.iter().map(|pair| (pair.close.end, pair)).collect();
    for index in 1..spans.len() {
        let SpanKind::Aozora(Node::Directive(directive)) = spans[index].kind else {
            continue;
        };
        if directive.kind != ab_aozora_syntax::DirectiveKind::Unknown {
            continue;
        }
        let SpanKind::Aozora(Node::Ruby(mut ruby)) = spans[index - 1].kind else {
            continue;
        };
        if spans[index - 1].source_span.end != spans[index].source_span.start {
            continue;
        }
        let (Some(ruby_pair), Some(marker)) = (
            pairs.get(&spans[index - 1].source_span.end),
            pairs.get(&spans[index].source_span.end),
        ) else {
            continue;
        };
        if ruby_pair.kind != PairKind::Ruby || marker.kind != PairKind::Bracket {
            continue;
        }
        let Some(body) =
            source[marker.open.end as usize..marker.close.start as usize].strip_prefix('＃')
        else {
            continue;
        };
        let (Some(base), Some(reading)) = (
            alloc.store().content_range_as_plain(ruby.base),
            alloc.store().content_range_as_plain(ruby.reading),
        ) else {
            continue;
        };
        if compose_accent_dots(base, body).is_none() {
            continue;
        }
        let base = base.to_owned();
        let reading = reading.to_owned();
        let mut base_start = spans[index - 1].source_span.start;
        if source[base_start as usize..].starts_with('｜') {
            base_start += 3;
        }
        let target = alloc.content_plain(&base);
        let Node::Format(format) = alloc.accent_dot(target, body, ForwardOrigin::Detached) else {
            unreachable!()
        };
        let content = alloc.content_segments(&[Segment::Format {
            value: format,
            source_span: Span::new(base_start, ruby_pair.open.start),
        }]);
        let reading = alloc.content_plain(&reading);
        let Node::Ruby(replacement) = alloc.ruby(content, reading) else {
            unreachable!()
        };
        ruby.base = replacement.base;
        spans[index - 1].kind = SpanKind::Aozora(Node::Ruby(ruby));
        let mut reference = format;
        reference.origin = ForwardOrigin::Referenced;
        spans[index].kind = SpanKind::Aozora(Node::Format(reference));
    }
}

/// Resolve source quotations within the immediately preceding typed base.
fn resolve_adjacent_note_targets(
    spans: &mut [ClassifiedSpan],
    source: &str,
    store: &NodeStore,
    links: &[PairLink],
) {
    if !spans.iter().any(|span| {
        matches!(span.kind,
        SpanKind::Aozora(Node::MarginNote(note)) if note.target_span.is_none())
    }) {
        return;
    }
    let pairs: BTreeMap<_, _> = links.iter().map(|pair| (pair.close.end, pair)).collect();
    for index in 1..spans.len() {
        let SpanKind::Aozora(Node::MarginNote(mut note)) = spans[index].kind else {
            continue;
        };
        if note.target_span.is_some() {
            continue;
        }
        let previous = &spans[index - 1];
        if previous.source_span.end != spans[index].source_span.start {
            continue;
        }
        let expected_pair = match previous.kind {
            SpanKind::Aozora(Node::Ruby(_)) => PairKind::Ruby,
            SpanKind::Aozora(Node::Format(_)) => PairKind::Bracket,
            _ => continue,
        };
        let Some(pair) = pairs.get(&previous.source_span.end) else {
            continue;
        };
        if pair.kind != expected_pair {
            continue;
        }
        let mut start = previous.source_span.start as usize;
        let end = pair.open.start as usize;
        if source
            .get(start..end)
            .is_some_and(|text| text.starts_with('｜'))
        {
            start += '｜'.len_utf8();
        }
        let (Some(base), Some(target)) = (
            source.get(start..end),
            store.content_range_as_plain(note.base),
        ) else {
            continue;
        };
        let mut occurrences = base.match_indices(target);
        let Some((offset, _)) = occurrences.next() else {
            continue;
        };
        if target.is_empty() || occurrences.next().is_some() {
            continue;
        }
        let from = u32::try_from(start + offset).expect("source offset fits u32");
        let to = u32::try_from(start + offset + target.len()).expect("source offset fits u32");
        note.target_span = NonEmptySpan::new(Span::new(from, to));
        spans[index].kind = SpanKind::Aozora(Node::MarginNote(note));
    }
}

/// Whether a forward attribute decorates a whole run as a single emphasis
/// wrapper, so it can style a ruby base. Excludes the sub-character /
/// target-splitting attributes — [`ForwardAttr::AccentDot`] (addresses letters
/// via an interned directive body the ruby cannot carry),
/// [`ForwardAttr::Accent`] (composes a single Latin letter), and
/// [`ForwardAttr::Fraction`] (splits the target on a slash) — which are never
/// meaningful over a kanji base and stay declined.
const fn attr_decorates_ruby_base(attr: ForwardAttr) -> bool {
    !matches!(
        attr,
        ForwardAttr::AccentDot | ForwardAttr::Accent(_) | ForwardAttr::Fraction
    )
}

/// Apply a declined forward emphasis directive (`［＃「X」に傍点/罫囲み/…］`, a
/// [`ForwardOrigin::Referenced`] leaf) onto a preceding ruby whose base is the
/// *unique* referent named `X`. The classifier declines these because a
/// ruby base cannot be pulled into a plain forward leaf (bouten-over-ruby is not
/// representable); instead we set that ruby's `base_emphasis` so the renderer
/// wraps the base in the attribute's emphasis element. The directive leaf stays
/// `Referenced` (serializes the bracket verbatim, renders nothing), so serialize
/// stays byte-identical.
///
/// Uniqueness is load-bearing, not "nearest ruby wins": the target must match
/// **exactly one** preceding ruby base and **no** preceding plain-text run
/// anywhere in the look-back — a plain copy that precedes the ruby (cross-line
/// or same-line, out of the classifier's reset window) is a competing referent,
/// so we decline and keep the honest `forward_referent_not_stylable` warning.
/// Returns the directive spans decorated, so the builder can suppress exactly
/// those warnings.
fn decorate_ruby_bases(out: &mut [ClassifiedSpan], source: &str, store: &NodeStore) -> Vec<Span> {
    let mut decorated: Vec<Span> = Vec::new();
    for idx in 0..out.len() {
        // Fire only on a declined (Referenced) forward directive with a
        // whole-run-decoratable attribute.
        let SpanKind::Aozora(Node::Format(f)) = out[idx].kind else {
            continue;
        };
        if !matches!(f.origin, ForwardOrigin::Referenced)
            || !f.attrs.single().is_some_and(attr_decorates_ruby_base)
        {
            continue;
        }
        // A ruby base is a single `Plain` run; a structured target never matches.
        let Some(target) = store.content_range_as_plain(f.target) else {
            continue;
        };
        // Scan the whole look-back for the unique referent.
        let mut ruby_match: Option<usize> = None;
        let mut ambiguous = false;
        for j in 0..idx {
            match &out[j].kind {
                SpanKind::Aozora(Node::Ruby(r)) => {
                    if store.content_range_as_plain(r.base) == Some(target) {
                        if ruby_match.is_some() {
                            ambiguous = true;
                            break;
                        }
                        ruby_match = Some(j);
                    }
                }
                // Any preceding plain run that carries the target text — before
                // the ruby, so invisible to the classifier's reset window — is a
                // competing referent that forces a decline.
                SpanKind::Plain(_) => {
                    let s =
                        &source[out[j].source_span.start as usize..out[j].source_span.end as usize];
                    if s.contains(target) {
                        ambiguous = true;
                        break;
                    }
                }
                _ => {}
            }
        }
        if ambiguous {
            continue;
        }
        let Some(ruby_idx) = ruby_match else {
            continue;
        };
        let Some(attr) = f.attrs.single() else {
            continue;
        };
        let directive_span = out[idx].source_span;
        if let SpanKind::Aozora(Node::Ruby(ref mut r)) = out[ruby_idx].kind {
            r.base_emphasis = Some(attr);
            decorated.push(directive_span);
        }
    }
    decorated
}

/// Byte position where `target` begins, **only if** it is the bare line
/// immediately preceding the `［` at `bracket_start`. `None` → the hint stays
/// inline.
fn find_heading_predecessor_position_at(
    source: &str,
    bracket_start: u32,
    target: &str,
) -> Option<u32> {
    let bytes = source.as_bytes();
    let cutoff = bracket_start as usize;
    if cutoff == 0 || bytes[cutoff - 1] != b'\n' {
        return None;
    }
    let text_end = cutoff - 1;
    let len = target.len();
    if text_end < len {
        return None;
    }
    let candidate_start = text_end - len;
    if &bytes[candidate_start..text_end] != target.as_bytes() {
        return None;
    }
    if candidate_start != 0 && bytes[candidate_start - 1] != b'\n' {
        return None;
    }
    u32::try_from(candidate_start).ok()
}

/// Promote each forward heading hint whose target is the bare line directly
/// above it into a `Heading`, reaching the span back over `target\n` so the
/// superset-drop pass reclaims the referent line.
fn promote_headings(
    mut spans: Vec<ClassifiedSpan>,
    source: &str,
    alloc: &mut Allocator,
) -> Vec<ClassifiedSpan> {
    for span in &mut spans {
        let SpanKind::Aozora(Node::HeadingHint(hint)) = span.kind else {
            continue;
        };
        // Resolve the interned target to an owned string so the `&store` borrow
        // does not overlap the `&mut alloc` builder calls below.
        let target = alloc.store().resolve_str(hint.target).to_owned();
        let Some(referent_start) =
            find_heading_predecessor_position_at(source, span.source_span.start, &target)
        else {
            continue;
        };
        let text = alloc.content_plain(&target);
        span.kind = SpanKind::Aozora(alloc.aozora_heading(hint.level, hint.style, text));
        span.source_span.start = referent_start;
    }
    spans
}

#[cfg(test)]
mod tests {
    use core::ptr;

    use super::*;

    #[test]
    fn type_state_chain_compiles() {
        let _final = Pipeline::new("｜青梅《おうめ》")
            .sanitize()
            .tokenize()
            .pair()
            .build();
    }

    #[test]
    fn run_to_completion_matches_chain() {
        let chain = Pipeline::new("｜青梅《おうめ》")
            .sanitize()
            .tokenize()
            .pair()
            .build();
        let oneshot = Pipeline::run_to_completion("｜青梅《おうめ》");
        assert_eq!(chain.normalized, oneshot.normalized);
        assert_eq!(chain.sanitized_len, oneshot.sanitized_len);
        assert_eq!(
            chain.registry.count_kind(ab_aozora_spec::Sentinel::Inline),
            oneshot
                .registry
                .count_kind(ab_aozora_spec::Sentinel::Inline)
        );
    }

    #[test]
    fn intermediate_inspection_at_sanitized() {
        let p = Pipeline::new("plain text").sanitize();
        assert_eq!(p.sanitized_text(), "plain text");
        assert!(p.diagnostics().is_empty());
        drop(p.tokenize().pair().build());
    }

    #[test]
    fn intermediate_inspection_at_tokenized() {
        let p = Pipeline::new("a｜b《c》").sanitize().tokenize();
        assert!(p.tokens().len() >= 5);
        drop(p.pair().build());
    }

    #[test]
    fn intermediate_inspection_at_paired() {
        let p = Pipeline::new("a｜b《c》").sanitize().tokenize().pair();
        assert!(!p.events().is_empty());
        drop(p.build());
    }

    #[test]
    fn sanitize_accent_diagnostic_propagates() {
        let out = Pipeline::run_to_completion("abc〔cafe'〕def");
        assert!(
            out.diagnostics
                .iter()
                .any(|d| matches!(d, Diagnostic::AccentDecompositionApplied { .. })),
            "expected AccentDecompositionApplied, got {:?}",
            out.diagnostics
        );
    }

    #[test]
    fn empty_source_round_trips() {
        let out = Pipeline::run_to_completion("");
        assert!(out.normalized.is_empty());
        assert!(out.registry.is_empty());
        assert_eq!(out.sanitized_len, 0);
    }

    #[test]
    fn source_accessor_returns_original() {
        let s = "the original";
        let p = Pipeline::new(s);
        assert!(ptr::eq(p.source(), s));
    }
}
