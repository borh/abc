//! Owned lex API + the single-pass owned normalizer.
//!
//! Produces an [`LexOutput`] whose normalized text, registry, and side
//! tables are owned (lifetime-free, `Send + Sync`). The classify stage builds
//! [`Node`] directly through an
//! [`Allocator`](ab_aozora_syntax::alloc::Allocator); this module's
//! [`Normalizer`] is the PUA-rewriter + position-recorder over those spans.
//! Every interned string lives once, in the allocator's
//! `NodeStore`, which threads straight into
//! the output — no arena, no conversion step.
//!
//! ## Pipeline
//!
//! 1. The sanitize / tokenize / pair stages run as owned-data helpers operating
//!    on byte spans and event indices — they never construct AST.
//! 2. The classify stage is invoked with an
//!    [`Allocator`](ab_aozora_syntax::alloc::Allocator); owned AST
//!    nodes land in its `NodeStore`, strings
//!    interned through the store's [`StrInterner`](ab_aozora_syntax::ast::StrInterner)
//!    so byte-equal content shares a single id.
//! 3. A single fused walk emits the PUA-rewritten text and builds the
//!    position-keyed registry + source-keyed side table, recording each
//!    `Node` (which is `Copy`) directly.

use core::mem::discriminant;

use crate::lexer::{
    BLOCK_CLOSE_SENTINEL, BLOCK_LEAF_SENTINEL, BLOCK_OPEN_SENTINEL, ClassifiedSpan,
    INLINE_SENTINEL, PlainProvenance, SpanKind,
};
use ab_aozora_spec::{Diagnostic, NormalizedOffset, Span};
use ab_aozora_syntax::ast::{
    ClassifiedSourceDisposition as Disposition, ClassifiedSourceEvidenceClass as EvidenceClass,
    ClassifiedSourceFact, ClassifiedSourceRole as Role, ConstructId, ContainerEnd, ContainerPair,
    LexOutput, Node, NodeRef, SourceNode,
};
use ab_aozora_syntax::{
    DirectiveKind, EnclosureKind, Format, ForwardAttr, LineFormat, RegionClose, RegionFormat,
};

/// Run the lex pipeline and materialise the result as an owned, lifetime-free
/// [`LexOutput`] (`Send + Sync`).
///
/// The native owned producer: the classify stage builds the owned tree in one
/// pass, so the
/// returned output owns all its payloads (interned strings, content / segment
/// runs, side tables). This is what `Document::parse` / `Document::lex`
/// call.
#[must_use]
pub fn lex(source: &str) -> LexOutput {
    crate::pipeline::Pipeline::run_to_completion(source)
}

/// Output recorder for the [`Normalizer`] fold.
///
/// Holds the position-keyed registry entries and the source-keyed side table.
/// Each emitted [`Node`] is `Copy`, so recording it is a plain push — no
/// conversion, no second store (the allocator's
/// `NodeStore` is authoritative and threads
/// into the output separately).
#[derive(Debug, Default)]
pub(crate) struct Recorder {
    pub(crate) entries: Vec<(u32, NodeRef)>,
    pub(crate) source_nodes: Vec<SourceNode>,
    pub(crate) classified_source_facts: Vec<ClassifiedSourceFact>,
}

impl Recorder {
    fn with_capacity(hint: usize) -> Self {
        Self {
            entries: Vec::with_capacity(hint),
            source_nodes: Vec::with_capacity(hint),
            classified_source_facts: Vec::with_capacity(hint),
        }
    }

    fn push(&mut self, pos: u32, source_span: Span, nref: NodeRef) {
        self.entries.push((pos, nref));
        self.source_nodes.push(SourceNode {
            source_span,
            node: nref,
        });
    }

    fn record_inline(&mut self, pos: u32, source_span: Span, node: Node) {
        self.push(pos, source_span, NodeRef::Inline(node));
    }

    fn record_block_leaf(&mut self, pos: u32, source_span: Span, node: Node) {
        self.push(pos, source_span, NodeRef::BlockLeaf(node));
    }

    fn record_block_open(&mut self, pos: u32, source_span: Span, region: RegionFormat) {
        self.push(pos, source_span, NodeRef::BlockOpen(region));
    }

    fn record_block_close(&mut self, pos: u32, source_span: Span, close: RegionClose) {
        self.push(pos, source_span, NodeRef::BlockClose(close));
    }

    fn record_classified_source(&mut self, span: &ClassifiedSpan) {
        if let Some(fact) = classified_source_fact(span) {
            self.classified_source_facts.push(fact);
        }
    }
}

fn classified_source_fact(span: &ClassifiedSpan) -> Option<ClassifiedSourceFact> {
    let (construct_id, source_role, disposition, evidence_class) = match &span.kind {
        SpanKind::Plain(plain) => match plain.provenance {
            PlainProvenance::Text => (
                ConstructId::PlainText,
                Role::VisibleText,
                Disposition::EmittedSemanticValue,
                EvidenceClass::AcceptedText,
            ),
            PlainProvenance::RecoveredVerbatim => (
                ConstructId::RecoveredVerbatim,
                Role::UnrecognizedSourceForm,
                Disposition::PreservedOpaque,
                EvidenceClass::RecoveredVerbatim,
            ),
        },
        SpanKind::Newline => (
            ConstructId::Newline,
            Role::StructuralNewline,
            Disposition::StructuralControl,
            EvidenceClass::StructuralToken,
        ),
        SpanKind::Aozora(node) => node_policy(*node)?,
        SpanKind::BlockOpen(_) => (
            ConstructId::ContainerOpen,
            Role::ContainerSyntax,
            Disposition::StructuralControl,
            EvidenceClass::TypedContainer,
        ),
        SpanKind::BlockClose(_) | SpanKind::BlockCloses { .. } => (
            ConstructId::ContainerClose,
            Role::ContainerSyntax,
            Disposition::StructuralControl,
            EvidenceClass::TypedContainer,
        ),
    };
    Some(ClassifiedSourceFact {
        source_span: span.source_span,
        construct_id,
        source_role,
        disposition,
        evidence_class,
    })
}

#[allow(
    clippy::too_many_lines,
    reason = "one exhaustive projection keeps the closed policy mapping auditable at its fold seam"
)]
fn node_policy(node: Node) -> Option<(ConstructId, Role, Disposition, EvidenceClass)> {
    let semantic = Disposition::EmittedSemanticValue;
    let typed = EvidenceClass::TypedNode;
    Some(match node {
        Node::Ruby(_) => (ConstructId::Ruby, Role::Ruby, semantic, typed),
        Node::Format(format) => match format.attrs.single() {
            Some(attr) => forward_attr_policy(attr)?,
            None => (ConstructId::Emphasis, Role::Typography, semantic, typed),
        },
        Node::Gaiji(_) => (ConstructId::Gaiji, Role::Gaiji, semantic, typed),
        Node::Line(line) => match line {
            LineFormat::Indent { .. } => (ConstructId::Indent, Role::Layout, semantic, typed),
            LineFormat::AlignEnd { .. } => (ConstructId::AlignEnd, Role::Layout, semantic, typed),
            LineFormat::Center { .. } => (ConstructId::Center, Role::Layout, semantic, typed),
            LineFormat::Framed(_) => (
                ConstructId::FramedOpen,
                Role::ContainerSyntax,
                Disposition::StructuralControl,
                EvidenceClass::TypedContainer,
            ),
            LineFormat::Gothic => (ConstructId::LineGothic, Role::Typography, semantic, typed),
            LineFormat::FontSizeAbsolute { .. } => {
                (ConstructId::LineFontSize, Role::Typography, semantic, typed)
            }
            _ => return None,
        },
        Node::PageBreak => (
            ConstructId::PageBreak,
            Role::Break,
            Disposition::StructuralControl,
            EvidenceClass::StructuralToken,
        ),
        Node::SectionBreak(_) => (
            ConstructId::SectionBreak,
            Role::Break,
            Disposition::StructuralControl,
            EvidenceClass::StructuralToken,
        ),
        Node::BodyEnd => (
            ConstructId::BodyEnd,
            Role::TerminalProvenance,
            Disposition::StructuralControl,
            EvidenceClass::StructuralToken,
        ),
        Node::ForcedBreak => (
            ConstructId::ForcedBreak,
            Role::Break,
            Disposition::StructuralControl,
            EvidenceClass::StructuralToken,
        ),
        Node::Heading(_) => (ConstructId::Heading, Role::Heading, semantic, typed),
        Node::HeadingHint(_) => (ConstructId::HeadingHint, Role::Heading, semantic, typed),
        Node::Illustration(_) => (
            ConstructId::Illustration,
            Role::Illustration,
            semantic,
            typed,
        ),
        Node::Kunten(_) => (ConstructId::Kunten, Role::Kunten, semantic, typed),
        Node::Directive(directive) => match directive.kind {
            DirectiveKind::Unknown => (
                ConstructId::UnknownDirective,
                Role::UnrecognizedSourceForm,
                Disposition::PreservedOpaque,
                EvidenceClass::UnknownDirective,
            ),
            DirectiveKind::WarichuOpen => (
                ConstructId::WarichuOpen,
                Role::SourceAnnotation,
                Disposition::StructuralControl,
                EvidenceClass::StructuralToken,
            ),
            DirectiveKind::WarichuClose => (
                ConstructId::WarichuClose,
                Role::SourceAnnotation,
                Disposition::StructuralControl,
                EvidenceClass::StructuralToken,
            ),
            // The typed editorial annotations render as visible markers (or,
            // for sic/variant/empty, as raw text carried in the output), so
            // their recognized role is source annotation and their source
            // contributes to the emitted value.
            DirectiveKind::Sic => (ConstructId::Sic, Role::SourceAnnotation, semantic, typed),
            DirectiveKind::BaseTextVariant => (
                ConstructId::BaseTextVariant,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::Empty => (
                ConstructId::EmptyDirective,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::EditorNote => (
                ConstructId::EditorNote,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::TranscriptionNote => (
                ConstructId::TranscriptionNote,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::OmissionNote => (
                ConstructId::OmissionNote,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::IncompletenessNote => (
                ConstructId::IncompletenessNote,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::RubyAttached => (
                ConstructId::RubyAttached,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::RubyRetarget => (
                ConstructId::RubyRetarget,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::RubyPairOpen => (
                ConstructId::RubyPairOpen,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::RubyPairClose => (
                ConstructId::RubyPairClose,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::MarginNotePairOpen => (
                ConstructId::MarginNotePairOpen,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            DirectiveKind::MarginNotePairClose => (
                ConstructId::MarginNotePairClose,
                Role::SourceAnnotation,
                semantic,
                typed,
            ),
            // A ruby span the parser recognised as ruby-shaped but could not
            // parse: no approved semantic role, so the bytes stay opaque like
            // the other recovery forms, witnessed by their typed node.
            DirectiveKind::InvalidRubySpan => (
                ConstructId::InvalidRubySpan,
                Role::UnrecognizedSourceForm,
                Disposition::PreservedOpaque,
                typed,
            ),
            // `DirectiveKind` is non_exhaustive across the crate boundary; a
            // future parser vocabulary addition produces no fact until the
            // policy admits it explicitly.
            _ => return None,
        },
        Node::AngleQuote(_) => (
            ConstructId::AngleQuote,
            Role::SourceAnnotation,
            semantic,
            typed,
        ),
        Node::MarginNote(_) => (
            ConstructId::MarginNote,
            Role::SourceAnnotation,
            semantic,
            typed,
        ),
        // No fact is invented for parser variants absent from the approved
        // policy. Policy evolution must add an explicit arm.
        _ => return None,
    })
}

/// Project only forward attributes explicitly admitted by the current ABC
/// policy. `ForwardAttr` is non-exhaustive across this crate boundary, so the
/// residual arm must fail closed when the parser vocabulary grows.
const fn forward_attr_policy(
    attr: ForwardAttr,
) -> Option<(ConstructId, Role, Disposition, EvidenceClass)> {
    let semantic = Disposition::EmittedSemanticValue;
    let typed = EvidenceClass::TypedNode;
    Some(match attr {
        ForwardAttr::Bouten { .. } => (ConstructId::Bouten, Role::Typography, semantic, typed),
        ForwardAttr::CombineUpright => (
            ConstructId::CombineUpright,
            Role::Typography,
            semantic,
            typed,
        ),
        ForwardAttr::Bold
        | ForwardAttr::Gothic
        | ForwardAttr::Italic
        | ForwardAttr::SuperScript
        | ForwardAttr::SubScript
        | ForwardAttr::SmallScript(_)
        | ForwardAttr::Framed(_)
        | ForwardAttr::Horizontal
        | ForwardAttr::Caption
        | ForwardAttr::FontSize(_)
        | ForwardAttr::FontSizeAbsolute(_)
        | ForwardAttr::Fraction
        | ForwardAttr::AccentDot
        | ForwardAttr::Accent(_)
        | ForwardAttr::AlignEnd { .. } => {
            (ConstructId::Emphasis, Role::Typography, semantic, typed)
        }
        _ => return None,
    })
}

/// Single-pass owned normalizer.
///
/// Streams the PUA-rewritten text into `out` and records each emitted
/// sentinel's node through `recorder`. The classifier emits spans in source
/// order, so every sentinel position is strictly greater than the previous and
/// the registry consumes the entries via `from_sorted_slice` without
/// re-sorting. The owned nodes are built upstream by the
/// [`Allocator`](ab_aozora_syntax::alloc::Allocator) during the
/// classify stage; this walker is the PUA-rewriter + position-recorder and does
/// zero AST allocation of its own.
#[derive(Debug)]
pub(crate) struct Normalizer<'src> {
    pub(crate) out: String,
    source: &'src str,
    pub(crate) recorder: Recorder,
    /// Stack of in-flight container opens awaiting their matching close. Each
    /// entry retains its normalized position, family and original marker span.
    /// A mismatched close recovers stack depth but does not establish a pair.
    open_stack: Vec<(NormalizedOffset, RegionFormat, Span)>,
    /// Resolved container open/close pairs in close order.
    pub(crate) container_pairs: Vec<ContainerPair>,
    /// Diagnostics observed during the fold (post-classify).
    pub(crate) diagnostics: Vec<Diagnostic>,
    /// Family tag of the most recent single-line layout directive on the
    /// current source line, if any.
    pending_single_line: Option<&'static str>,
    /// Nesting depth of open `［＃割り注］` … `［＃割り注終わり］` ranges.
    warichu_depth: u32,
}

impl<'src> Normalizer<'src> {
    pub(crate) fn new(source: &'src str, span_capacity_hint: usize) -> Self {
        Self {
            out: String::with_capacity(source.len()),
            source,
            recorder: Recorder::with_capacity(span_capacity_hint),
            open_stack: Vec::with_capacity(span_capacity_hint / 40),
            container_pairs: Vec::with_capacity(span_capacity_hint / 40),
            diagnostics: Vec::new(),
            pending_single_line: None,
            warichu_depth: 0,
        }
    }

    fn current_pos(&self) -> u32 {
        u32::try_from(self.out.len()).expect("normalized fits u32 per sanitize-stage cap")
    }

    pub(crate) fn emit(&mut self, span: &ClassifiedSpan) {
        let located = matches!(
            span.kind,
            SpanKind::BlockOpen(RegionFormat::RelativePlacement(
                ab_aozora_syntax::RelativePlacement::BelowHorizontal { anchor: None },
            ))
        )
        .then(|| ClassifiedSpan {
            source_span: span.source_span,
            kind: SpanKind::BlockOpen(RegionFormat::RelativePlacement(
                ab_aozora_syntax::RelativePlacement::BelowHorizontal {
                    anchor: self.replace_horizontal_for_label(span.source_span),
                },
            )),
        });
        let span = located.as_ref().unwrap_or(span);
        if let SpanKind::BlockCloses { targets, fallback } = &span.kind {
            self.emit_shared_close(span.source_span, *targets, *fallback);
            return;
        }
        self.recorder.record_classified_source(span);
        match &span.kind {
            SpanKind::Plain(_) => {
                self.out.push_str(span.source_span.slice(self.source));
            }
            SpanKind::Newline => {
                self.out.push('\n');
                self.pending_single_line = None;
            }
            SpanKind::Aozora(node) => {
                self.track_single_line_break(*node, span.source_span);
                if is_standalone_block_for_render(*node) {
                    self.out.push_str("\n\n");
                    let pos = self.current_pos();
                    self.out.push(BLOCK_LEAF_SENTINEL);
                    self.out.push_str("\n\n");
                    self.recorder
                        .record_block_leaf(pos, span.source_span, *node);
                } else {
                    let pos = self.current_pos();
                    self.out.push(INLINE_SENTINEL);
                    self.recorder.record_inline(pos, span.source_span, *node);
                }
            }
            SpanKind::BlockOpen(container) => {
                if matches!(container, RegionFormat::Indent(_))
                    && matches!(
                        self.open_stack.last(),
                        Some((_, RegionFormat::Indent(_), _))
                    )
                {
                    let (open, kind, source_open) =
                        self.open_stack.pop().expect("indent region is active");
                    self.out.push_str("\n\n");
                    let close = self.current_pos();
                    self.out.push(BLOCK_CLOSE_SENTINEL);
                    self.out.push_str("\n\n");
                    // The normalized tree needs a close; source ownership belongs
                    // only to the following opener, so no source node is invented.
                    self.recorder
                        .entries
                        .push((close, NodeRef::BlockClose(RegionClose::of(kind))));
                    self.container_pairs.push(ContainerPair {
                        kind,
                        open,
                        close: NormalizedOffset::new(close),
                        source_open,
                        source_end: ContainerEnd::SourceReplacement(span.source_span),
                    });
                }
                let inline = container.is_inline();
                if !inline {
                    self.out.push_str("\n\n");
                }
                let pos = self.current_pos();
                self.out.push(BLOCK_OPEN_SENTINEL);
                if !inline {
                    self.out.push_str("\n\n");
                }
                self.recorder
                    .record_block_open(pos, span.source_span, *container);
                self.open_stack
                    .push((NormalizedOffset::new(pos), *container, span.source_span));
            }
            SpanKind::BlockCloses { .. } => unreachable!("shared source boundary handled above"),
            SpanKind::BlockClose(close) => self.emit_block_close(span.source_span, *close),
        }
    }

    fn replace_horizontal_for_label(&mut self, marker: Span) -> Option<Span> {
        let (open, kind, source_open) = self.open_stack.last_mut()?;
        let presentation = match *kind {
            RegionFormat::Horizontal(presentation) => Some(presentation),
            RegionFormat::Indent(block) => block.styles.horizontal,
            RegionFormat::Columns(block) => block.styles.horizontal,
            _ => None,
        }?;
        let source = &self.source[source_open.end as usize..marker.start as usize];
        let content = source.trim();
        if content.is_empty() {
            return None;
        }
        let leading = source.len() - source.trim_start().len();
        let start = source_open.end + u32::try_from(leading).expect("native source fits u32");
        let anchor = Span::new(
            start,
            start + u32::try_from(content.len()).expect("native source fits u32"),
        );
        let (open, source_open) = (*open, *source_open);
        if matches!(kind, RegionFormat::Horizontal(_)) {
            self.open_stack.pop();
        } else {
            take_compound_presentation(kind, RegionClose::Horizontal);
        }
        self.out.push_str("\n\n");
        let close = self.current_pos();
        self.out.push(BLOCK_CLOSE_SENTINEL);
        self.out.push_str("\n\n");
        self.recorder
            .entries
            .push((close, NodeRef::BlockClose(RegionClose::Horizontal)));
        self.container_pairs.push(ContainerPair {
            kind: RegionFormat::Horizontal(presentation),
            open,
            close: NormalizedOffset::new(close),
            source_open,
            source_end: ContainerEnd::SourceReplacement(marker),
        });
        Some(anchor)
    }

    fn emit_block_close(&mut self, span: Span, close: RegionClose) {
        if let RegionClose::Indent {
            kumi_width,
            mut styles,
        } = close
            && styles.horizontal.take().is_some()
            && matches!(
                self.open_stack.last(),
                Some((
                    _,
                    RegionFormat::RelativePlacement(
                        ab_aozora_syntax::RelativePlacement::BelowHorizontal { anchor: Some(_) }
                    ),
                    _
                ))
            )
            && self.open_stack.len() >= 2
        {
            let indent = RegionClose::Indent { kumi_width, styles };
            if scope_closer_matches(self.open_stack[self.open_stack.len() - 2].1, indent) {
                self.emit_verified_closes(span, [RegionClose::Horizontal, indent]);
                return;
            }
        }
        let inline = close.is_inline();
        if !inline {
            self.out.push_str("\n\n");
        }
        let pos = self.current_pos();
        self.out.push(BLOCK_CLOSE_SENTINEL);
        if !inline {
            self.out.push_str("\n\n");
        }
        self.recorder.record_block_close(pos, span, close);
        if let Some((open, kind, source_open)) = self.open_stack.last_mut()
            && let Some(presentation) = take_compound_presentation(kind, close)
        {
            self.container_pairs.push(ContainerPair {
                kind: presentation,
                open: *open,
                close: NormalizedOffset::new(pos),
                source_open: *source_open,
                source_end: ContainerEnd::ClosingMarker(span),
            });
            return;
        }
        if let Some((open_pos, open_kind, source_open)) = self.open_stack.pop() {
            if self.container_matches(open_kind, close, span) {
                self.container_pairs.push(ContainerPair {
                    kind: open_kind,
                    open: open_pos,
                    close: NormalizedOffset::new(pos),
                    source_open,
                    source_end: ContainerEnd::ClosingMarker(span),
                });
            }
        } else {
            self.diagnostics.push(Diagnostic::unmatched_container_close(
                span,
                close.kind_str(),
            ));
        }
    }

    /// Validate the complete named stack suffix before establishing any endpoint.
    fn emit_shared_close(&mut self, span: Span, targets: [RegionClose; 2], fallback: Node) {
        let matches = self.open_stack.len() >= targets.len()
            && self
                .open_stack
                .iter()
                .rev()
                .zip(targets)
                .all(|((_, open, _), close)| scope_closer_matches(*open, close));
        if !matches {
            self.emit(&ClassifiedSpan {
                source_span: span,
                kind: SpanKind::Aozora(fallback),
            });
            return;
        }
        self.emit_verified_closes(span, targets);
    }

    fn emit_verified_closes(&mut self, span: Span, targets: [RegionClose; 2]) {
        self.recorder.record_classified_source(&ClassifiedSpan {
            source_span: span,
            kind: SpanKind::BlockClose(targets[0]),
        });
        self.out.push_str("\n\n");
        for (index, close) in targets.iter().enumerate() {
            let pos = self.current_pos();
            self.out.push(BLOCK_CLOSE_SENTINEL);
            if index == 0 {
                self.recorder.record_block_close(pos, span, *close);
            } else {
                self.recorder
                    .entries
                    .push((pos, NodeRef::BlockClose(*close)));
            }
            let (open, kind, source_open) = self.open_stack.pop().expect("complete suffix checked");
            self.container_pairs.push(ContainerPair {
                kind,
                open,
                close: NormalizedOffset::new(pos),
                source_open,
                source_end: ContainerEnd::ClosingMarker(span),
            });
        }
        self.out.push_str("\n\n");
    }

    /// Report every remaining opening marker before discarding recovery state.
    pub(crate) fn finish(&mut self) {
        for (_, kind, span) in self.open_stack.drain(..) {
            self.diagnostics
                .push(Diagnostic::unclosed_container(span, kind.kind_str()));
        }
    }

    /// Establish matching families or report the recovered mismatch.
    fn container_matches(&mut self, open: RegionFormat, close: RegionClose, span: Span) -> bool {
        let expected = RegionClose::of(open);
        let attributes_match = scope_closer_matches(open, close);
        if discriminant(&expected) != discriminant(&close) {
            self.diagnostics
                .push(Diagnostic::mismatched_container_close(
                    span,
                    open.kind_str(),
                    close.kind_str(),
                ));
            false
        } else if let (
            RegionClose::Bouten {
                kind: open_kind, ..
            },
            RegionClose::Bouten {
                kind: close_kind, ..
            },
        ) = (expected, close)
            && open_kind.is_line() != close_kind.is_line()
        {
            self.diagnostics
                .push(Diagnostic::mismatched_bouten_container(
                    span,
                    open_kind.family_str(),
                    close_kind.family_str(),
                ));
            false
        } else if !attributes_match {
            self.diagnostics
                .push(Diagnostic::mismatched_container_close(
                    span,
                    open.kind_str(),
                    close.kind_str(),
                ));
            false
        } else {
            true
        }
    }

    /// Single-line-container break tracker for one classified `Aozora` node.
    fn track_single_line_break(&mut self, node: Node, break_span: Span) {
        match node {
            Node::Line(LineFormat::Indent { .. }) => {
                self.pending_single_line = Some("indent");
            }
            Node::Line(LineFormat::AlignEnd { .. }) => {
                self.pending_single_line = Some("align-end");
            }
            Node::Line(LineFormat::Center { .. }) => {
                self.pending_single_line = Some("center");
            }
            Node::Line(LineFormat::Gothic) => {
                self.pending_single_line = Some("line-gothic");
            }
            Node::Directive(ann) => match ann.kind {
                DirectiveKind::WarichuOpen => self.warichu_depth += 1,
                DirectiveKind::WarichuClose => {
                    self.warichu_depth = self.warichu_depth.saturating_sub(1);
                }
                _ => {}
            },
            Node::PageBreak | Node::SectionBreak(_) => {
                if let Some(container) = self.pending_single_line.take() {
                    self.diagnostics
                        .push(Diagnostic::break_in_single_line_container(
                            break_span, container,
                        ));
                } else if self.warichu_depth > 0 {
                    self.diagnostics
                        .push(Diagnostic::break_in_single_line_container(
                            break_span, "warichu",
                        ));
                }
            }
            _ => {}
        }
    }
}

fn take_compound_presentation(open: &mut RegionFormat, close: RegionClose) -> Option<RegionFormat> {
    let styles = match open {
        RegionFormat::Indent(block) => &mut block.styles,
        RegionFormat::Columns(block) => &mut block.styles,
        _ => return None,
    };
    match close {
        RegionClose::Horizontal => styles.horizontal.take().map(RegionFormat::Horizontal),
        RegionClose::Framed(_) => {
            let frame = styles.frame?;
            if !scope_closer_matches(RegionFormat::Framed(frame), close) {
                return None;
            }
            styles.frame = None;
            Some(RegionFormat::Framed(frame))
        }
        _ => None,
    }
}

/// Match supplied closing attributes, including source-defined omitted payloads.
pub(crate) fn scope_closer_matches(open: RegionFormat, close: RegionClose) -> bool {
    let expected = RegionClose::of(open);
    if let (RegionFormat::Indent(block), RegionClose::Indent { kumi_width, styles }) = (open, close)
    {
        let width_matches = kumi_width.is_none()
            || matches!(expected,
            RegionClose::Indent { kumi_width: expected_width, .. } if kumi_width == expected_width);
        return width_matches
            && styles.iter_formats().all(|supplied| {
                block.styles.iter_formats().any(|established| {
                    matches!(
                        (supplied, established),
                        (
                            Format::Framed(EnclosureKind::Unspecified),
                            Format::Framed(_)
                        )
                    ) || matches!((supplied, established),
                        (Format::Horizontal(requested), Format::Horizontal(_)) if requested.align.is_none())
                        || supplied == established
                })
            });
    }
    match (expected, close) {
        (
            RegionClose::Heading {
                level: expected_level,
                style: expected_style,
                ..
            },
            RegionClose::Heading { level, style, .. },
        ) => level.is_none() || (level == expected_level && style == expected_style),
        (
            RegionClose::FontSize {
                larger: expected,
                magnitude: expected_magnitude,
            },
            RegionClose::FontSize { larger, magnitude },
        ) => larger == expected && (magnitude.is_none() || magnitude == expected_magnitude),
        (RegionClose::Columns(expected), RegionClose::Columns(actual)) => {
            actual.is_none() || expected == actual
        }
        (RegionClose::Bold { .. }, RegionClose::Bold { .. })
        | (RegionClose::Gothic { .. }, RegionClose::Gothic { .. })
        | (RegionClose::Italic { .. }, RegionClose::Italic { .. })
        | (RegionClose::Caption { .. }, RegionClose::Caption { .. }) => true,
        _ => expected == close,
    }
}

/// Whether an owned AST node is a standalone block (renders on its own line, no
/// surrounding plain-text context required). Pinned by variant kind so adding a
/// new standalone-block variant only needs updating here.
fn is_standalone_block_for_render(node: Node) -> bool {
    matches!(
        node,
        Node::PageBreak
            | Node::SectionBreak(_)
            | Node::BodyEnd
            | Node::Heading(_)
            | Node::Illustration(_)
    )
}

// Container registries: pure copy of (u32, RegionFormat) / RegionClose — all
// `Copy`. A static assertion pins the no-conversion expectation.
const _: fn() = || {
    fn assert_copy<T: Copy>() {}
    assert_copy::<(u32, RegionFormat)>();
    assert_copy::<RegionClose>();
};

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::PlainSpan;
    use ab_aozora_spec::{NormalizedOffset, Sentinel};
    use ab_aozora_syntax::ast::Content;
    use ab_aozora_syntax::{
        AbsoluteSize, AccentMark, BoutenKind, BoutenPosition, EnclosureKind, FontShift, IndentBlock,
    };
    use core::num::NonZeroI8;

    #[test]
    fn every_current_forward_attribute_has_an_explicit_policy_mapping() {
        let emphasis = [
            ForwardAttr::Bold,
            ForwardAttr::Gothic,
            ForwardAttr::Italic,
            ForwardAttr::SuperScript,
            ForwardAttr::SubScript,
            ForwardAttr::SmallScript(BoutenPosition::Right),
            ForwardAttr::Framed(EnclosureKind::Rule),
            ForwardAttr::Horizontal,
            ForwardAttr::Caption,
            ForwardAttr::FontSize(FontShift(NonZeroI8::new(1).unwrap())),
            ForwardAttr::FontSizeAbsolute(AbsoluteSize::Large),
            ForwardAttr::Fraction,
            ForwardAttr::AccentDot,
            ForwardAttr::Accent(AccentMark::Acute),
            ForwardAttr::AlignEnd { offset: 1 },
        ];
        for attr in emphasis {
            assert_eq!(
                forward_attr_policy(attr),
                Some((
                    ConstructId::Emphasis,
                    Role::Typography,
                    Disposition::EmittedSemanticValue,
                    EvidenceClass::TypedNode,
                )),
                "missing explicit mapping for {attr:?}"
            );
        }
        assert_eq!(
            forward_attr_policy(ForwardAttr::Bouten {
                kind: BoutenKind::Goma,
                position: BoutenPosition::Right,
            })
            .unwrap()
            .0,
            ConstructId::Bouten
        );
        assert_eq!(
            forward_attr_policy(ForwardAttr::CombineUpright).unwrap().0,
            ConstructId::CombineUpright
        );
        // `ForwardAttr` is non-exhaustive, so downstream code cannot construct
        // a future variant here. The residual arm in `forward_attr_policy`
        // returning `None` is the executable fail-closed boundary.
    }

    #[test]
    fn lex_materialises_ruby_resolving_back_to_source_text() {
        let src = "｜青梅《おうめ》";
        let owned = lex(src);

        // The single inline entry resolves back to the ruby base / reading.
        let Some((pos, _)) = owned.registry.iter_kind(Sentinel::Inline).next() else {
            panic!("expected one inline entry");
        };
        let Some(hit) = owned.registry.node_at(NormalizedOffset::new(pos)) else {
            panic!("expected an owned registry hit");
        };
        let NodeRef::Inline(Node::Ruby(r)) = hit else {
            panic!("expected an owned inline ruby, got {hit:?}");
        };
        let base = owned.store.resolve_content_range(r.base);
        let reading = owned.store.resolve_content_range(r.reading);
        let Content::Plain(base_id) = base[0] else {
            panic!("expected a plain ruby base");
        };
        let Content::Plain(reading_id) = reading[0] else {
            panic!("expected a plain ruby reading");
        };
        assert_eq!(owned.store.resolve_str(base_id), "青梅");
        assert_eq!(owned.store.resolve_str(reading_id), "おうめ");
    }

    #[test]
    fn empty_source_round_trips() {
        let out = lex("");
        assert!(out.normalized.is_empty());
        assert!(out.registry.is_empty());
        assert!(out.diagnostics.is_empty());
        assert_eq!(out.sanitized_len, 0);
    }

    #[test]
    fn plain_text_passes_through_unchanged() {
        let out = lex("hello, world");
        assert_eq!(out.normalized, "hello, world");
        assert!(out.registry.is_empty());
        assert!(out.diagnostics.is_empty());
    }

    #[test]
    fn explicit_ruby_lands_in_inline_registry() {
        let out = lex("｜青梅《おうめ》");
        assert_eq!(out.registry.count_kind(Sentinel::Inline), 1);
        let (pos, nr) = out
            .registry
            .iter_kind(Sentinel::Inline)
            .next()
            .expect("one entry");
        assert!(out.normalized.as_bytes()[pos as usize..].starts_with(&[0xEE, 0x80, 0x81]));
        let NodeRef::Inline(node) = nr else {
            panic!("expected NodeRef::Inline, got {nr:?}");
        };
        assert!(matches!(node, Node::Ruby(_)));
    }

    #[test]
    fn page_break_lands_in_block_leaf_registry() {
        let out = lex("text［＃改ページ］more");
        assert_eq!(out.registry.count_kind(Sentinel::BlockLeaf), 1);
        let (_pos, nr) = out
            .registry
            .iter_kind(Sentinel::BlockLeaf)
            .next()
            .expect("one entry");
        let NodeRef::BlockLeaf(node) = nr else {
            panic!("expected NodeRef::BlockLeaf, got {nr:?}");
        };
        assert!(matches!(node, Node::PageBreak));
    }

    #[test]
    fn paired_container_lands_in_open_close_registries() {
        let out = lex("［＃ここから2字下げ］\nbody\n［＃ここで字下げ終わり］");
        assert_eq!(out.registry.count_kind(Sentinel::BlockOpen), 1);
        assert_eq!(out.registry.count_kind(Sentinel::BlockClose), 1);
        let (_, nr) = out.registry.iter_kind(Sentinel::BlockOpen).next().unwrap();
        let NodeRef::BlockOpen(kind) = nr else {
            panic!("expected NodeRef::BlockOpen, got {nr:?}");
        };
        assert!(matches!(
            kind,
            RegionFormat::Indent(IndentBlock { amount: 2, .. })
        ));
    }

    #[test]
    fn diagnostics_carry_through_to_output() {
        let out = lex("source has 〔cafe'〕 accent notation");
        assert!(
            out.diagnostics
                .iter()
                .any(|d| matches!(d, Diagnostic::AccentDecompositionApplied { .. })),
            "expected AccentDecompositionApplied, got {:?}",
            out.diagnostics
        );
    }

    #[test]
    fn sanitized_len_equals_input_for_plain_text() {
        let input = "plain text\nwith newline";
        let out = lex(input);
        assert_eq!(usize::try_from(out.sanitized_len), Ok(input.len()));
    }

    #[test]
    fn container_kind_indent_amount_preserved() {
        let out = lex("［＃ここから3字下げ］\ntext\n［＃ここで字下げ終わり］");
        let (_, nr) = out.registry.iter_kind(Sentinel::BlockOpen).next().unwrap();
        let NodeRef::BlockOpen(kind) = nr else {
            panic!("expected NodeRef::BlockOpen, got {nr:?}");
        };
        match kind {
            RegionFormat::Indent(IndentBlock { amount, .. }) => assert_eq!(amount, 3),
            other => panic!("expected Indent {{ amount: 3 }}, got {other:?}"),
        }
    }

    #[test]
    fn dense_corpus_paragraph_lands_expected_pieces() {
        let src = "明治の頃｜青梅《おうめ》街道沿いに、※［＃「木＋吶のつくり」、第3水準1-85-54］\n\
                   なる珍しき木が立つ。［＃ここから2字下げ］\n\
                   その下で人々は語らひ、［＃「青空」に傍点］\n\
                   ［＃ここで字下げ終わり］";
        let out = lex(src);
        assert_eq!(out.registry.count_kind(Sentinel::Inline), 3);
        assert_eq!(out.registry.count_kind(Sentinel::BlockLeaf), 0);
        assert_eq!(out.registry.count_kind(Sentinel::BlockOpen), 1);
        assert_eq!(out.registry.count_kind(Sentinel::BlockClose), 1);
        for (pos, _) in out.registry.iter_kind(Sentinel::Inline) {
            assert!(out.registry.node_at(NormalizedOffset::new(pos)).is_some());
        }
    }

    #[test]
    fn block_open_close_padding_is_blank_line_sentinel_blank_line() {
        let src = "［＃ここから2字下げ］\nbody\n［＃ここで字下げ終わり］";
        let out = lex(src);

        let (open_pos, _) = out
            .registry
            .iter_kind(Sentinel::BlockOpen)
            .next()
            .expect("one open entry");
        let (close_pos, _) = out
            .registry
            .iter_kind(Sentinel::BlockClose)
            .next()
            .expect("one close entry");

        let bytes = out.normalized.as_bytes();
        let open_sentinel_bytes = "\u{E003}".as_bytes();
        let close_sentinel_bytes = "\u{E004}".as_bytes();

        assert!(open_pos as usize >= 2);
        assert_eq!(&bytes[(open_pos as usize - 2)..open_pos as usize], b"\n\n");
        let open_after = open_pos as usize + open_sentinel_bytes.len();
        assert_eq!(&bytes[open_pos as usize..open_after], open_sentinel_bytes);
        assert!(open_after + 2 <= bytes.len());
        assert_eq!(&bytes[open_after..open_after + 2], b"\n\n");

        assert!(close_pos as usize >= 2);
        assert_eq!(
            &bytes[(close_pos as usize - 2)..close_pos as usize],
            b"\n\n"
        );
        let close_after = close_pos as usize + close_sentinel_bytes.len();
        assert_eq!(
            &bytes[close_pos as usize..close_after],
            close_sentinel_bytes
        );
        assert!(close_after + 2 <= bytes.len());
        assert_eq!(&bytes[close_after..close_after + 2], b"\n\n");
    }

    #[test]
    fn independently_emitted_identical_claim_candidates_preserve_multiplicity() {
        let source = "｜";
        let span = ClassifiedSpan {
            kind: SpanKind::Plain(PlainSpan {
                provenance: PlainProvenance::RecoveredVerbatim,
            }),
            source_span: Span::new(0, 3),
        };
        let mut normalizer = Normalizer::new(source, 2);

        normalizer.emit(&span);
        normalizer.emit(&span);

        assert_eq!(normalizer.recorder.classified_source_facts.len(), 2);
    }
}
