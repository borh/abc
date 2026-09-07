//! No-lifetime AST payload structs and the `Content` / `Segment` two-tier
//! content model.
//!
//! Every text slice is a [`StrId`]; a non-empty content run is a
//! [`ContentRange`]; a segment run is a [`SegRange`]; a bare `Content` field
//! is an inline [`Content`]. Large illustration payloads use a store handle
//! so ordinary nodes retain their compact `Copy` representation.

use core::fmt;
use core::num::NonZeroU32;

use ab_aozora_encoding::gaiji::{GaijiCanonical, MenKuTen, Resolved};

use crate::format::{ForwardAttr, ForwardOrigin, LineFormat};
use crate::{
    Container, DirectiveKind, HeadingKind, HeadingStyle, MarginNoteKind, MarginNotePosition,
    RubySide, SectionKind,
};

use super::intern::StrId;
use super::store::{ContentRange, ForwardAttrs, IllustrationId, NodeStore, SegRange};

/// Body content that may carry nested Aozora constructs. Two-tier: a single
/// plain run or a mixed sequence of segments.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Content {
    /// Plain text.
    Plain(StrId),
    /// Mixed text + nested constructs.
    Segments(SegRange),
}

/// One element of a [`Content::Segments`] run.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Segment {
    /// Plain-text run between nested constructs.
    Text(StrId),
    /// Supplied iteration notation at its sanitized source extent.
    IterationMark {
        /// Whole unvoiced or voiced repeat mark.
        value: IterationMark,
        /// Original notation extent in sanitized UTF-8 bytes.
        source_span: crate::Span,
    },
    /// Nested 外字 reference.
    Gaiji(Gaiji),
    /// Nested generic annotation.
    Directive {
        /// Native interpretation of the annotation.
        value: Directive,
        /// Annotation extent in sanitized UTF-8 source bytes.
        source_span: crate::Span,
    },
    /// Supplied formatting over its established target within rich content.
    Format {
        /// Native formatting attributes and target content.
        value: ForwardFormat,
        /// Target and directive extent in sanitized UTF-8 source bytes.
        source_span: crate::Span,
    },
    /// Supplied kunten at its native sanitized UTF-8 source coordinates.
    Kunten {
        /// Supplied mark or okurigana, without a derived reading order.
        value: Kunten,
        /// Annotation extent in sanitized UTF-8 source bytes.
        source_span: crate::Span,
    },
}

/// Aozora's two spellings for the whole double-height kana repeat mark.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IterationMark {
    /// `／＼`, transcribed as U+3031.
    Unvoiced,
    /// `／″＼`, transcribed as U+3032.
    Voiced,
}

impl IterationMark {
    /// Exact source spelling, without expanding the repeated text.
    #[must_use]
    pub const fn source(self) -> &'static str {
        match self {
            Self::Unvoiced => "／＼",
            Self::Voiced => "／″＼",
        }
    }

    /// Whole Unicode mark; top and bottom halves are separate characters.
    #[must_use]
    pub const fn character(self) -> char {
        match self {
            Self::Unvoiced => '〱',
            Self::Voiced => '〲',
        }
    }

    /// Recognize only a complete supplied notation at the start of a text run.
    #[must_use]
    pub fn at_start(text: &str) -> Option<Self> {
        if text.starts_with("／＼") {
            Some(Self::Unvoiced)
        } else if text.starts_with("／″＼") {
            Some(Self::Voiced)
        } else {
            None
        }
    }
}

impl GaijiCanonicalOwned {
    /// Reconstruct the [`GaijiCanonical`], resolving the `Unresolved` mencode
    /// `StrId` against `store`. The single bridge to the `aozora-encoding`
    /// canonical authority (`resolve` / `write_mencode`), kept private so the
    /// owned API surface stays owned.
    ///
    /// # Panics
    ///
    /// Panics if an `Unresolved` mencode `StrId` was not produced by `store`.
    fn to_canonical(self, store: &NodeStore) -> GaijiCanonical<'_> {
        match self {
            Self::MenKuTen(m) => GaijiCanonical::MenKuTen(m),
            Self::Unicode(c) => GaijiCanonical::Unicode(c),
            Self::Unresolved { mencode } => GaijiCanonical::Unresolved {
                mencode: mencode.map(|id| store.resolve_str(id)),
            },
        }
    }

    /// `true` when the source carried a mencode tail. Owned counterpart of
    /// [`GaijiCanonical::has_mencode`] — store-free (only the variant matters).
    #[must_use]
    pub fn has_mencode(self) -> bool {
        !matches!(self, Self::Unresolved { mencode: None })
    }

    /// Write the canonical mencode token (without the leading `、`). Owned
    /// counterpart of [`GaijiCanonical::write_mencode`]; delegates to the single
    /// encoding authority via the private `to_canonical` bridge.
    ///
    /// # Errors
    ///
    /// Propagates the writer's own errors.
    ///
    /// # Panics
    ///
    /// Panics if an `Unresolved` mencode `StrId` was not produced by `store`.
    pub fn write_mencode<W: fmt::Write>(self, store: &NodeStore, w: &mut W) -> fmt::Result {
        self.to_canonical(store).write_mencode(w)
    }
}

impl Gaiji {
    /// Resolve to a concrete glyph via the canonical value: rebuilds the
    /// lifetime-free `GaijiCanonical` against `store` and delegates to the
    /// single `GaijiCanonical::resolve` authority, passing [`Self::hint`] as
    /// the resolver's description fallback. The `Resolved` result is owned
    /// (no borrow of `store`).
    ///
    /// # Panics
    ///
    /// Panics if `self.hint` / the `Unresolved` mencode `StrId` were not
    /// produced by `store`'s interner.
    #[must_use]
    pub fn resolve(&self, store: &NodeStore) -> Option<Resolved> {
        self.canonical
            .to_canonical(store)
            .resolve(store.resolve_str(self.hint))
    }
}

/// Ruby (furigana) annotation.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Ruby {
    /// Base text the reading annotates.
    pub base: ContentRange,
    /// Furigana reading.
    pub reading: ContentRange,
    /// Which side the reading sits on.
    pub side: RubySide,
    /// Render-only forward emphasis applied to the base. Set by the
    /// lowering pass when a declined forward directive `［＃「X」に傍点/罫囲み/
    /// 行右小書き/…］` (a [`ForwardOrigin::Referenced`](crate::ForwardOrigin)
    /// leaf) names this ruby's base as its *unique* preceding referent — the
    /// classic `｜X《y》…［＃「X」は罫囲み］` where the target is a ruby base and
    /// so cannot be pulled into a plain forward leaf (bouten-over-ruby is not
    /// representable). The renderer wraps the base in the attribute's emphasis
    /// element; the directive leaf stays `Referenced` (serializes the bracket
    /// verbatim, renders nothing), so `base_emphasis` is never read by
    /// `to_source` — it is a render decoration, not a serialized field. As a
    /// `Copy` `Option<ForwardAttr>` it keeps `Ruby` `Copy` and inline.
    pub base_emphasis: Option<ForwardAttr>,
}

/// Nonempty source extent. The nonzero end keeps optional extents compact.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct NonEmptySpan {
    start: u32,
    end: NonZeroU32,
}

impl NonEmptySpan {
    /// Accept only forward, nonempty source ranges.
    #[must_use]
    pub fn new(span: crate::Span) -> Option<Self> {
        (span.start < span.end).then_some(Self {
            start: span.start,
            end: NonZeroU32::new(span.end)?,
        })
    }

    /// Recover the native source coordinates.
    #[must_use]
    pub const fn span(self) -> crate::Span {
        crate::Span::new(self.start, self.end.get())
    }
}

/// Margin note (注記 / 傍記).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct MarginNote {
    /// 注記 vs 傍記.
    pub kind: MarginNoteKind,
    /// Physical side explicitly supplied by the source; bare に supplies none.
    pub position: Option<MarginNotePosition>,
    /// Exact annotation-text extent in native source coordinates, when sourced.
    pub note_span: Option<NonEmptySpan>,
    /// Exact literal target extent when the native resolver established one.
    pub target_span: Option<NonEmptySpan>,
    /// Whether this node owns the principal target or only references it.
    pub origin: ForwardOrigin,
    /// Preceding run the note attaches to.
    pub base: ContentRange,
    /// Gloss / redaction text.
    pub note: ContentRange,
}

/// Forward-reference emphasis.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForwardFormat {
    /// Which forward-scope attribute decorates the run.
    pub attrs: ForwardAttrs,
    /// The decorated run.
    pub target: ContentRange,
    /// Target-text provenance.
    pub origin: ForwardOrigin,
    /// Exact directive body when attributes alone omit source semantics, such as
    /// accent-dot composition or an accompanying edition assertion. The source
    /// serializer retains it; the accent renderer reads it only for `AccentDot`.
    pub annotation_body: Option<StrId>,
}

/// Owned, lifetime-free counterpart of
/// [`ab_aozora_encoding::gaiji::GaijiCanonical`], whose `Unresolved` variant
/// carries a `&'src str`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum GaijiCanonicalOwned {
    /// Structured `第N水準P-K-T`. Reused lifetime-free `MenKuTen`.
    MenKuTen(MenKuTen),
    /// Explicit `U+XXXX` codepoint.
    Unicode(char),
    /// Verbatim tail.
    Unresolved {
        /// The raw mencode tail, interned. `None` when absent.
        mencode: Option<StrId>,
    },
}

/// Out-of-range glyph (外字).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Gaiji {
    /// Free-form source description / resolver fallback key.
    pub hint: StrId,
    /// Typed canonical value.
    pub canonical: GaijiCanonicalOwned,
    /// `true` for the no-`※` standalone form.
    pub standalone: bool,
}

/// Split annotation (割注).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Warichu {
    /// First (upper / right) half-size line.
    pub upper: Content,
    /// Second (lower / left) half-size line.
    pub lower: Content,
}

/// Heading (見出し).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Heading {
    /// 大 / 中 / 小 outline level.
    pub kind: HeadingKind,
    /// Standard / 同行 / 窓 style.
    pub style: HeadingStyle,
    /// Heading label.
    pub text: ContentRange,
}

/// Heading hint (見出し指定).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeadingHint {
    /// Intended outline level.
    pub level: HeadingKind,
    /// Standard / 同行 / 窓 style.
    pub style: HeadingStyle,
    /// Quoted target run.
    pub target: StrId,
    /// Whether the quoted target is absent from the preceding source — a
    /// no-referent forward heading whose target run is itself the heading text
    /// (the [`ForwardOrigin::SelfContained`](crate::ForwardOrigin) analogue for
    /// headings). When set, the hint renders the target visibly instead of as a
    /// hidden marker; it still serializes bracket-only (no fabricated referent
    /// line), keeping the round-trip a fixed point. A hint whose target *is*
    /// preceded leaves this `false`.
    pub self_contained: bool,
}

/// Illustration (挿絵).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Illustration {
    /// Complete source body when image insertion carries documentary clauses.
    pub annotation_body: Option<StrId>,
    /// Image path / filename.
    pub file: StrId,
    /// Optional figure number (raw digits).
    pub number: Option<StrId>,
    /// Optional verbatim `横W×縦H` size note.
    pub dimensions: Option<StrId>,
    /// Quoted caption reference inside the image annotation, not body text.
    pub caption: Option<Content>,
    /// Exact quoted reference fragment when retained by the source classifier.
    pub caption_span: Option<crate::Span>,
    /// Optional alt description.
    pub description: Option<StrId>,
    /// Exact description fragment when supplied by the source classifier.
    pub description_span: Option<crate::Span>,
}

/// Generic annotation (注記).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Directive {
    /// Raw bytes between `［＃` and `］`.
    pub raw: StrId,
    /// Classification.
    pub kind: DirectiveKind,
}

/// Source-supplied annotation category; placement follows this distinction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum KuntenKind {
    /// Return mark printed below the principal line.
    ReturnMark,
    /// Supplied okurigana printed above the principal line.
    Okurigana,
}

/// Supplied kanbun annotation, separate from principal body text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Kunten {
    /// Source-established annotation category.
    pub kind: KuntenKind,
    /// Supplied text without notation delimiters.
    pub text: StrId,
}

/// Angle quote (`≪…≫` -> `《…》`).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct AngleQuote {
    /// Quoted run.
    pub content: ContentRange,
}

/// A single, no-lifetime AST node. Every scalar payload is held INLINE
/// (no `Box`/`Id`); `Copy` scalar-enum variants carry their
/// value directly.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[non_exhaustive]
pub enum Node {
    /// Supplied double-height kana iteration mark.
    IterationMark(IterationMark),
    /// Ruby (furigana) annotation.
    Ruby(Ruby),
    /// Forward-reference emphasis.
    Format(ForwardFormat),
    /// Out-of-range glyph (外字).
    Gaiji(Gaiji),
    /// Line-level format — `Copy` enum.
    Line(LineFormat),
    /// Split annotation (割注).
    Warichu(Warichu),
    /// Page break — unit.
    PageBreak,
    /// Section break — `Copy` enum.
    SectionBreak(SectionKind),
    /// End of body — unit.
    BodyEnd,
    /// Forced line break — unit.
    ForcedBreak,
    /// Heading (見出し).
    Heading(Heading),
    /// Heading hint (見出し指定).
    HeadingHint(HeadingHint),
    /// Illustration (挿絵).
    Illustration(IllustrationId),
    /// Kanbun reading-order mark (返り点).
    Kunten(Kunten),
    /// Generic annotation (注記).
    Directive(Directive),
    /// Angle quote (`≪…≫` -> `《…》`).
    AngleQuote(AngleQuote),
    /// Margin note (注記 / 傍記).
    MarginNote(MarginNote),
    /// Container — `Copy` enum.
    Container(Container),
}

impl Node {
    /// Cross-cutting [`crate::NodeKind`] tag for this node.
    #[must_use]
    pub const fn kind(self) -> crate::NodeKind {
        use crate::NodeKind;
        match self {
            Self::IterationMark(_) => NodeKind::IterationMark,
            Self::Ruby(_) => NodeKind::Ruby,
            Self::Format(f) => match f.attrs.single() {
                Some(ForwardAttr::Bouten { .. }) => NodeKind::Bouten,
                Some(ForwardAttr::CombineUpright) => NodeKind::CombineUpright,
                _ => NodeKind::Emphasis,
            },
            Self::Gaiji(_) => NodeKind::Gaiji,
            Self::Line(l) => match l {
                LineFormat::Indent { .. } => NodeKind::Indent,
                LineFormat::AlignEnd { .. } => NodeKind::AlignEnd,
                LineFormat::Center { .. } => NodeKind::Center,
                LineFormat::Framed(_) => NodeKind::Framed,
                LineFormat::Gothic => NodeKind::LineGothic,
                LineFormat::FontSizeAbsolute { .. } => NodeKind::LineFontSize,
            },
            Self::Warichu(_) => NodeKind::Warichu,
            Self::PageBreak => NodeKind::PageBreak,
            Self::SectionBreak(_) => NodeKind::SectionBreak,
            Self::BodyEnd => NodeKind::BodyEnd,
            Self::ForcedBreak => NodeKind::ForcedBreak,
            Self::Heading(_) => NodeKind::Heading,
            Self::HeadingHint(_) => NodeKind::HeadingHint,
            Self::Illustration(_) => NodeKind::Illustration,
            Self::Kunten(_) => NodeKind::Kunten,
            Self::Directive(_) => NodeKind::Directive,
            Self::AngleQuote(_) => NodeKind::AngleQuote,
            Self::MarginNote(_) => NodeKind::MarginNote,
            Self::Container(_) => NodeKind::Container,
        }
    }

    /// Stable XML/element-style node name, feeding the serializer's fallback
    /// placeholder (`<!-- unsupported-aozora: … -->`).
    #[must_use]
    pub const fn xml_node_name(self) -> &'static str {
        match self {
            Self::Ruby(_) => "aozora_ruby",
            Self::Format(f) => match f.attrs.single() {
                Some(ForwardAttr::Bouten { .. }) => "aozora_bouten",
                Some(ForwardAttr::CombineUpright) => "aozora_tcy",
                _ => "aozora_emphasis",
            },
            Self::Gaiji(_) => "aozora_gaiji",
            Self::Line(l) => match l {
                LineFormat::Indent { .. } => "aozora_indent",
                LineFormat::AlignEnd { .. } => "aozora_align_end",
                LineFormat::Center { .. } => "aozora_center",
                LineFormat::Framed(_) => "aozora_keigakomi",
                LineFormat::Gothic => "aozora_line_goshikku",
                LineFormat::FontSizeAbsolute { .. } => "aozora_line_font_size",
            },
            Self::Warichu(_) => "aozora_warichu",
            Self::PageBreak => "aozora_page_break",
            Self::SectionBreak(_) => "aozora_section_break",
            Self::BodyEnd => "aozora_body_end",
            Self::ForcedBreak => "aozora_forced_break",
            Self::Heading(_) => "aozora_heading",
            Self::HeadingHint(_) => "aozora_heading_hint",
            Self::Illustration(_) => "aozora_sashie",
            Self::Kunten(_) => "aozora_kaeriten",
            Self::IterationMark(_) => "aozora_iteration_mark",
            Self::Directive(_) => "aozora_annotation",
            Self::AngleQuote(_) => "aozora_angle_quote",
            Self::MarginNote(_) => "aozora_side_note",
            Self::Container(_) => "aozora_container",
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn optional_source_extents_are_compact_and_nonempty() {
        assert_eq!(size_of::<Option<NonEmptySpan>>(), 8);
        for (start, end) in [(0, 0), (1, 1), (2, 1)] {
            assert!(NonEmptySpan::new(crate::Span::new(start, end)).is_none());
        }
        for (start, end) in [(0, 1), (4, 20), (u32::MAX - 1, u32::MAX)] {
            let span = crate::Span::new(start, end);
            assert_eq!(NonEmptySpan::new(span).unwrap().span(), span);
        }
    }

    #[test]
    fn payloads_are_copy() {
        // Pin the Copy chain — every owned payload must stay Copy. If a future
        // field breaks Copy, this fails to compile.
        const fn assert_copy<T: Copy>() {}
        assert_copy::<Content>();
        assert_copy::<Segment>();
        assert_copy::<Node>();
        assert_copy::<Ruby>();
        assert_copy::<Gaiji>();
    }

    #[test]
    fn node_kind_tag_matches_variant() {
        assert_eq!(Node::PageBreak.kind(), crate::NodeKind::PageBreak);
        assert_eq!(
            Node::SectionBreak(SectionKind::Kaicho).kind(),
            crate::NodeKind::SectionBreak
        );
    }

    #[test]
    fn gaiji_resolve_matches_canonical_authority() {
        // Build an owned `Gaiji` per canonical arm and confirm the on-demand
        // glyph resolution is byte-identical to the `aozora-encoding`
        // `GaijiCanonical::resolve` authority. Uses `GaijiCanonical::from_mencode`
        // so the canonical values are realistic (structured Unicode / 面区点 /
        // verbatim / absent) without hand-built `MenKuTen` internals.
        let mut store = NodeStore::new();
        for (hint, mencode) in [
            ("竜", Some("U+9F8D")),         // → Unicode
            ("熙", Some("第3水準1-14-29")), // → MenKuTen (面区点)
            ("謎の字", Some("未知の注記")), // → Unresolved { Some }
            ("謎", None),                   // → Unresolved { None }
        ] {
            let canonical = GaijiCanonical::from_mencode(mencode);
            let hint_id = store.intern(hint);
            let owned_canonical = match canonical {
                GaijiCanonical::MenKuTen(m) => GaijiCanonicalOwned::MenKuTen(m),
                GaijiCanonical::Unicode(c) => GaijiCanonicalOwned::Unicode(c),
                GaijiCanonical::Unresolved { mencode } => GaijiCanonicalOwned::Unresolved {
                    mencode: mencode.map(|m| store.intern(m)),
                },
            };
            let owned = Gaiji {
                hint: hint_id,
                canonical: owned_canonical,
                standalone: false,
            };
            assert_eq!(
                owned.resolve(&store),
                canonical.resolve(hint),
                "owned gaiji resolve diverged for hint={hint:?} mencode={mencode:?}",
            );
        }
    }
}
