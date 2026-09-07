//! Immutable parser facts projected at the classified-source fold boundary.

#![allow(
    missing_docs,
    reason = "closed vocabulary variants preserve the identically named ABC policy tokens"
)]

use ab_aozora_spec::Span;

/// Research-owned source-role vocabulary implemented by the parser adapter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ClassifiedSourceRole {
    VisibleText,
    StructuralNewline,
    Ruby,
    Typography,
    Gaiji,
    Layout,
    Break,
    Heading,
    Illustration,
    Kunten,
    SourceAnnotation,
    ContainerSyntax,
    TerminalProvenance,
    PublicationMetadata,
    UnrecognizedSourceForm,
}

/// Research-owned disposition vocabulary implemented by the parser adapter.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ClassifiedSourceDisposition {
    EmittedSemanticValue,
    PreservedSidecarValue,
    StructuralControl,
    LosslessNormalization,
    PreservedOpaque,
}

/// Closed evidence classes accepted by the classified-source policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ClassifiedSourceEvidenceClass {
    AcceptedText,
    RecoveredVerbatim,
    TypedNode,
    StructuralToken,
    TypedContainer,
    UnknownDirective,
}

/// Construct identifiers named by the approved ab-aozora v1 policy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ConstructId {
    PlainText,
    RecoveredVerbatim,
    Newline,
    UnknownDirective,
    Ruby,
    Bouten,
    CombineUpright,
    Gaiji,
    Indent,
    AlignEnd,
    Center,
    WarichuOpen,
    FramedOpen,
    LineGothic,
    LineFontSize,
    PageBreak,
    SectionBreak,
    BodyEnd,
    ForcedBreak,
    Heading,
    HeadingHint,
    Illustration,
    Kunten,
    AngleQuote,
    Emphasis,
    MarginNote,
    ContainerOpen,
    ContainerClose,
    Sic,
    BaseTextVariant,
    InvalidRubySpan,
    WarichuClose,
    EmptyDirective,
    EditorNote,
    TranscriptionNote,
    OmissionNote,
    IncompletenessNote,
    ExplanationNote,
    ExternalTableReference,
    RubyAttached,
    RubyRetarget,
    RubyPairOpen,
    RubyPairClose,
    MarginNotePairOpen,
    MarginNotePairClose,
}

/// One source-local policy fact, before decoded-coordinate rebasing and target
/// authentication. Every value is copied from one classified span.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ClassifiedSourceFact {
    /// Half-open interval in the parser's sanitized UTF-8 coordinate space.
    pub source_span: Span,
    pub construct_id: ConstructId,
    pub source_role: ClassifiedSourceRole,
    pub disposition: ClassifiedSourceDisposition,
    pub evidence_class: ClassifiedSourceEvidenceClass,
}

impl ClassifiedSourceFact {
    /// Whether this fact recognizes the named source role semantically.
    #[must_use]
    pub const fn is_semantic(self) -> bool {
        !matches!(
            self.disposition,
            ClassifiedSourceDisposition::PreservedOpaque
        )
    }

    /// Whether this fact accounts for its interval, semantically or opaquely.
    #[must_use]
    pub const fn is_accounted(self) -> bool {
        true
    }
}

/// Canonical complete-value order for fact publication.
#[must_use]
pub fn canonicalize_classified_source_facts(
    mut facts: Vec<ClassifiedSourceFact>,
) -> Vec<ClassifiedSourceFact> {
    facts.sort_unstable_by_key(|fact| {
        (
            fact.source_span.start,
            fact.source_span.end,
            fact.construct_id,
            fact.source_role,
            fact.disposition,
            fact.evidence_class,
        )
    });
    facts
}
