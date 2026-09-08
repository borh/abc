//! Attribute-major formatting model: one `Format` attribute, typed
//! per-scope sums.
//!
//! Aozora's typographic notations (太字, 傍点, 字下げ, …) each apply at one
//! or more *scopes*: forward-reference (`「X」は太字`), single line
//! (`［＃地付き］`), or a paired range / block (`［＃ここから太字］ …
//! ［＃ここで太字終わり］`).
//!
//! This module separates the two axes:
//!
//! - [`Format`] names the **attribute** once, scope-independent: the source
//!   of the canonical keyword and the attribute-identity tag.
//! - [`ForwardAttr`], [`LineFormat`], [`RegionFormat`] each enumerate **only
//!   the attributes legal at that scope**, so an illegal pair is unrepresentable.
//!   Every scope sum projects back via `fn format() -> Format`.
//! - [`RegionClose`] is the close-marker discriminant; the open
//!   [`RegionFormat`] payload stays authoritative (see [`RegionClose::of`]).
//! - The scalar parameters ([`FontShift`], [`ColumnCount`], [`LineWidth`],
//!   [`Kumi`]) reject zero magnitudes. Close markers carry no fabricated
//!   open-marker parameters.

use core::num::{NonZeroI8, NonZeroU8};

use crate::ast::{PartialLayoutId, StrId};
use crate::{BoutenKind, BoutenPosition, HeadingKind, HeadingStyle};

// ----------------------------------------------------------------------
// Scalar parameters: NonZero so placeholders are unconstructable
// ----------------------------------------------------------------------

/// Signed relative font-size shift.
///
/// Positive = 大きな (larger), negative = 小さな (smaller). `NonZero` because
/// a zero-stage shift is not a font-size change.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct FontShift(pub NonZeroI8);

impl FontShift {
    /// `true` when the shift enlarges (大きな); `false` when it shrinks (小さな).
    #[must_use]
    pub const fn larger(self) -> bool {
        self.0.get() > 0
    }

    /// The unsigned stage count.
    #[must_use]
    pub const fn magnitude(self) -> u8 {
        self.0.get().unsigned_abs()
    }
}

/// A supplied relative font size without a numbered stage count.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum QualitativeFontSize {
    /// 小さい活字: smaller type, with no supplied degree.
    Smaller,
    /// やや小さく: slightly smaller type.
    SlightlySmaller,
    /// ひとまわり大きい: larger type with the supplied qualitative comparison.
    HitomawariLarger,
}

impl QualitativeFontSize {
    /// Whether the supplied comparison enlarges the surrounding type.
    #[must_use]
    pub const fn larger(self) -> bool {
        matches!(self, Self::HitomawariLarger)
    }

    /// The source's degree qualifier; no variant denotes a numbered stage.
    #[must_use]
    pub const fn qualifier(self) -> Option<&'static str> {
        match self {
            Self::Smaller => None,
            Self::SlightlySmaller => Some("やや"),
            Self::HitomawariLarger => Some("ひとまわり"),
        }
    }
}

/// An **absolute** font size keyword (`特大文字` / `大文字` / `中文字` /
/// `小文字`), as distinct from the relative `N段階…文字` shift carried by
/// [`FontShift`].
///
/// The 青空文庫 corpus attests this scale in works that name a fixed size
/// rather than a step count (e.g. 暗黒公使's 特大 > 大 > 中 > 本文 > 小
/// headline scheme, and the forward `「X」は小文字`). The variants are ordered
/// largest-to-smallest; `Medium` is one step *below* the surrounding body, so
/// it has no representable [`FontShift`] (`NonZero`), which is why this is its
/// own type rather than a magnitude.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum AbsoluteSize {
    /// 特大文字: the largest size.
    ExtraLarge,
    /// 大文字: large.
    Large,
    /// 中文字: medium (below body).
    Medium,
    /// 小文字: small.
    Small,
}

impl AbsoluteSize {
    /// The canonical 青空文庫 keyword for this size.
    #[must_use]
    pub const fn keyword(self) -> &'static str {
        match self {
            Self::ExtraLarge => "特大文字",
            Self::Large => "大文字",
            Self::Medium => "中文字",
            Self::Small => "小文字",
        }
    }
}

/// The supplied shape and rule style of an enclosure.
/// All scopes reuse this vocabulary without inferring an independent text role.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum EnclosureKind {
    /// 枠囲み: a frame whose rule pattern is not specified.
    Unspecified,
    /// 罫囲み: a ruled rectangular frame.
    Rule,
    /// 破線枠囲み: a rectangular frame with dashed rules.
    DashedRule,
    /// 「□」囲み: a box-glyph enclosure. The glyph is □ (U+25A1); the box is
    /// drawn by the stylesheet, so the glyph is re-emitted only on serialize.
    Box,
    /// ○付き文字: an encircled character (`「X」は○付き文字`). The circle is
    /// drawn by the stylesheet around the target run.
    Circle,
    /// 点線丸囲み: a dotted circular enclosure (`「X」は点線丸囲み`).
    CircleDotted,
    /// 二重罫囲み: a double-ruled rectangular frame (`「X」は二重罫囲み`).
    DoubleRule,
}

/// The diacritical mark of a forward accent directive
/// (`「X」はアクサン（´）付き` / `…ウムラウト（¨）付き`), specifying which
/// precomposed accented glyph the single quoted Latin letter maps to.
///
/// The mark *word* (アクサン) does not distinguish acute from grave; the
/// bracketed *symbol* does (´ = U+00B4 vs ｀ = U+FF40); ウムラウト（¨） (¨ =
/// U+00A8) names the umlaut. Composition reuses the `〔…〕` accent digraph table
/// via [`crate::accent::compose_accent`], the single authority for the
/// "Latin letter plus diacritic yields a precomposed glyph" mapping;
/// [`Format::AccentDot`] is the sibling combining-dot facility.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum AccentMark {
    /// アクサン（´）: acute accent (`e` → é). Accent-table marker byte `'`.
    Acute,
    /// ウムラウト（¨）: umlaut / diaeresis (`o` → ö). Marker byte `:`.
    Umlaut,
    /// アクサン（｀）: grave accent (`e` → è). Marker byte `` ` ``. Corpus-absent
    /// but supported for completeness.
    Grave,
}

/// Positive number of supplied columns in a 段組 region.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ColumnCount(pub NonZeroU8);

/// Full-width characters per line (字詰め / 字組み width). `NonZero` because a
/// zero-width line is meaningless.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct LineWidth(pub NonZeroU8);

/// The `L行W字組み` clause: `lines` lines of `width` full-width characters.
///
/// Both dimensions must be positive. The opening marker supplies the dimensions
/// when a closing marker is rendered.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Kumi {
    /// Line count `L` from the `L行` clause.
    pub lines: NonZeroU8,
    /// Full-width characters per line `W` from the `W字組み` clause.
    pub width: NonZeroU8,
}

/// Secondary line-layout clause of an indent block (`、N字詰め` / `、L行W字組みで`).
// Exhaustive by design: serialize / render must handle every
// arm explicitly so a future layout is compiler-flagged at every site rather
// than silently dropped by a `_` fallback.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum IndentLayout {
    /// A plain `［＃ここから N字下げ］` block: no secondary layout.
    None,
    /// `［＃ここから N字下げ、M字詰め］`: sets `M` full-width chars per line.
    LineWidth(LineWidth),
    /// `［＃ここから N字下げ、L行W字組みで］`: sets `L` lines of `W` chars.
    Kumi(Kumi),
}

/// Semantic purpose explicitly supplied for an indented source scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum BlockPurpose {
    /// The source designates the enclosed text as a formula.
    Formula,
}

/// Independent presentation attributes supplied on a block-layout opener.
/// Fields remain `Copy` and project through the shared formatting identities.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Default)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct BlockStyles {
    /// `ゴシック体`: co-applied gothic typeface (`Format::Gothic`).
    pub gothic: bool,
    /// Explicit co-applied bold weight.
    pub bold: bool,
    /// `横書き` / `横組み`: horizontal writing (`Format::Horizontal`).
    pub horizontal: Option<HorizontalPresentation>,
    /// The supplied enclosing shape and rule style (`Format::Framed`).
    pub frame: Option<EnclosureKind>,
    /// Supplied qualitative font comparison.
    pub font: Option<QualitativeFontSize>,
}

impl BlockStyles {
    /// No co-applied presentation attributes.
    pub const EMPTY: Self = Self {
        gothic: false,
        bold: false,
        horizontal: None,
        frame: None,
        font: None,
    };

    /// Whether no presentation attribute is set.
    #[must_use]
    pub fn is_empty(self) -> bool {
        self == Self::EMPTY
    }

    /// Project attributes in canonical typeface, weight, direction, frame and font order.
    pub fn iter_formats(self) -> impl Iterator<Item = Format> {
        let Self {
            gothic,
            bold,
            horizontal,
            frame,
            font,
        } = self;
        [
            gothic.then_some(Format::Gothic),
            bold.then_some(Format::Bold),
            horizontal.map(Format::Horizontal),
            frame.map(Format::Framed),
            font.map(Format::QualitativeFontSize),
        ]
        .into_iter()
        .flatten()
    }
}

/// Supplied text alignment within a line, independent of page placement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum LineAlignment {
    /// Centre the text within its line.
    Center,
    /// Align horizontal text with the right edge of its line.
    Right,
}

/// Horizontal writing and alignment supplied as part of that presentation clause.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct HorizontalPresentation {
    /// Absent when the source specifies direction without alignment.
    pub align: Option<LineAlignment>,
}

/// Supplied placement relative to visible text or an established horizontal block.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum RelativePlacement {
    /// A horizontal, right-aligned block starts below the named character.
    BelowText {
        /// Exact source quotation naming the anchor.
        reference: StrId,
        /// Fullwidth-character distance supplied from the anchor's lower edge.
        offset_chars: u8,
        /// Unique visible source anchor, absent until native ownership is established.
        anchor: Option<crate::Span>,
    },
    /// A vertically written label is horizontally centered below the horizontal block.
    BelowHorizontal {
        /// Source extent of the established horizontal content.
        anchor: Option<crate::Span>,
    },
}

/// Clauses not interpreted as layout, retained individually for apparatus interpretation.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PartialLayout {
    /// Original annotation body in the owning node store.
    pub body: StrId,
    /// Individual unmatched or conflicting clauses in sanitized-source coordinates.
    pub clauses: Vec<crate::Span>,
}

/// A supplied column scope with independent presentation and retained source clauses.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct ColumnBlock {
    /// The source supplies a rule between columns; no stroke dimensions are implied.
    pub column_rule: bool,
    /// Number of columns supplied by the source.
    pub count: ColumnCount,
    /// Co-applied presentation, independent of column count.
    pub styles: BlockStyles,
    /// Source clauses retained outside the column geometry.
    pub partial: Option<PartialLayoutId>,
}

/// The block-only payload of an indent region.
///
/// `wrap` / `layout` / `styles` live here rather than on the single-line
/// `Indent` so the block-only clauses cannot leak into the line scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct IndentBlock {
    /// Supplied semantic purpose, independent of indentation and typography.
    pub purpose: Option<BlockPurpose>,
    /// Original source and clauses interpreted separately from geometry.
    pub partial: Option<PartialLayoutId>,
    /// Supplied column count, independent of character indentation.
    pub column_count: Option<ColumnCount>,
    /// Full-width characters the block is indented by.
    pub amount: u8,
    /// Hanging-indent continuation width: `Some(M)` for `折り返して M字下げ`.
    pub wrap: Option<u8>,
    /// Centre the supplied block between the physical left and right page edges.
    pub page_horizontal_center: bool,
    /// Supplied text alignment within the block's lines.
    pub align: Option<LineAlignment>,
    /// Full-width characters left empty at the line's end margin.
    pub end_offset: Option<u8>,
    /// Secondary line-layout clause; see [`IndentLayout`].
    pub layout: IndentLayout,
    /// Co-applied decorative styles (`ゴシック体` / `横書き` / `罫囲み` /
    /// `小さい活字`); see [`BlockStyles`].
    pub styles: BlockStyles,
}

// ----------------------------------------------------------------------
// Format: the attribute identity (scope-independent)
// ----------------------------------------------------------------------

/// The typographic attribute, independent of the scope it applies at.
///
/// The single source of the attribute-identity tag ([`Self::as_json_tag`]).
/// Each scope sum ([`ForwardAttr`] / [`LineFormat`] / [`RegionFormat`])
/// projects to this via its `format()` method, so cross-scope grouping
/// (e.g. "is this the same attribute as that one?") keys on one enum.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum Format {
    /// 太字 (bold).
    Bold,
    /// ゴシック体 (gothic / sans-serif). A distinct typeface family, **not** a
    /// weight of 太字: the corpus uses ゴシック体 and 太字 in disjoint works and
    /// print sets them differently, so the parser keeps them separate.
    Gothic,
    /// 教科書体: the supplied textbook typeface designation.
    Textbook,
    /// 斜体 (italic).
    Italic,
    /// 傍点 / 傍線 (emphasis dots / sidelines).
    Bouten(BoutenKind),
    /// 罫囲み (ruled box) / 「□」囲み (box glyph): an enclosure of some
    /// [`EnclosureKind`].
    Framed(EnclosureKind),
    /// Horizontal writing with its supplied line alignment.
    Horizontal(HorizontalPresentation),
    /// Source-relative placement, without absolute page coordinates.
    RelativePlacement(RelativePlacement),
    /// N段階大きな / 小さな文字 (relative font size).
    FontSize(FontShift),
    /// Relative size with a supplied qualitative degree, never numbered stages.
    QualitativeFontSize(QualitativeFontSize),
    /// 特大 / 大 / 中 / 小文字 (absolute font size).
    FontSizeAbsolute(AbsoluteSize),
    /// キャプション (caption).
    Caption,
    /// 上付き小文字 (superscript).
    SuperScript,
    /// 下付き小文字 (subscript).
    SubScript,
    /// 下付き: lowered baseline without a supplied smaller size.
    Lowered,
    /// 指数: supplied mathematical exponent role, without a base association.
    Exponent,
    /// 行右 / 行左小書き (small side-script).
    SmallScript(BoutenPosition),
    /// 縦中横 (tate-chu-yoko).
    CombineUpright,
    /// 分数 (fraction, `「a/b」は分数`).
    Fraction,
    /// ドット付き (dotted-letter composition, `mは上ドット付き` → ṁ).
    AccentDot,
    /// アクサン / ウムラウト (forward accent mark, `「e」はアクサン（´）付き` → é).
    /// The [`AccentMark`] rides the scope sum ([`ForwardAttr::Accent`]); the
    /// attribute identity discards it, like [`Self::AccentDot`].
    Accent,
    /// 字下げ (indent).
    Indent,
    /// 地付き / 地から N 字上げ (end alignment).
    AlignEnd,
    /// 中央 (centring).
    Center,
    /// 字詰め (line width).
    LineWidth,
    /// 表 (table).
    Table,
    /// A supplied translation of banknote lettering.
    BanknoteTranslation,
    /// Text explicitly designated as a formula.
    Formula,
    /// 段組 (multi-column).
    Columns(ColumnCount),
    /// 割り注 (split annotation).
    Warichu,
    /// 見出し (heading).
    Heading {
        /// The 大 / 中 / 小 outline level.
        level: HeadingKind,
        /// Standard / 同行 / 窓 style.
        style: HeadingStyle,
    },
}

impl Format {
    /// Stable camelCase attribute-identity tag (exhaustive, no `_` fallback,
    /// so a new attribute fails to build until it is given a tag).
    #[must_use]
    pub const fn as_json_tag(self) -> &'static str {
        match self {
            Self::Bold => "bold",
            Self::Gothic => "gothic",
            Self::Textbook => "textbook",
            Self::Italic => "italic",
            Self::Bouten(_) => "bouten",
            Self::Framed(_) => "framed",
            Self::Horizontal(_) => "horizontal",
            Self::RelativePlacement(_) => "relative-placement",
            Self::FontSize(_) => "fontSize",
            Self::QualitativeFontSize(_) => "qualitativeFontSize",
            Self::FontSizeAbsolute(_) => "fontSizeAbsolute",
            Self::Caption => "caption",
            Self::SuperScript => "superScript",
            Self::SubScript => "subScript",
            Self::Lowered => "lowered",
            Self::Exponent => "exponent",
            Self::SmallScript(_) => "smallScript",
            Self::CombineUpright => "combineUpright",
            Self::Fraction => "fraction",
            Self::AccentDot => "accentDot",
            Self::Accent => "accent",
            Self::Indent => "indent",
            Self::AlignEnd => "alignEnd",
            Self::Center => "center",
            Self::LineWidth => "lineWidth",
            Self::Table => "table",
            Self::BanknoteTranslation => "banknote-translation",
            Self::Formula => "formula",
            Self::Columns(_) => "columns",
            Self::Warichu => "warichu",
            Self::Heading { .. } => "heading",
        }
    }
}

// ----------------------------------------------------------------------
// Forward scope: `「X」は…` reference-attached emphasis
// ----------------------------------------------------------------------

/// The attributes legal at the forward-reference scope (`「X」は太字` etc.).
///
/// The content-carrying leaf that pairs an attribute with its target run is
/// the forward-reference leaf (`ForwardFormat`); this enum is the
/// attribute alone.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum ForwardAttr {
    /// 太字.
    Bold,
    /// ゴシック体: gothic typeface (distinct from 太字, see [`Format::Gothic`]).
    Gothic,
    /// 斜体.
    Italic,
    /// 上付き小文字.
    SuperScript,
    /// 下付き小文字.
    SubScript,
    /// 下付き: lowered baseline without a supplied smaller size.
    Lowered,
    /// 指数: supplied mathematical exponent role, without a base association.
    Exponent,
    /// 行右 / 行左小書き.
    SmallScript(BoutenPosition),
    /// 罫囲み / 「□」囲み: an enclosure of some [`EnclosureKind`].
    Framed(EnclosureKind),
    /// 横組み.
    Horizontal,
    /// キャプション.
    Caption,
    /// N段階大きな / 小さな文字.
    FontSize(FontShift),
    /// 特大 / 大 / 中 / 小文字: absolute font size (`「X」は小文字`).
    FontSizeAbsolute(AbsoluteSize),
    /// 傍点 / 傍線. `position` records a `左に` left-side modifier.
    Bouten {
        /// The 傍点 / 傍線 mark.
        kind: BoutenKind,
        /// `Left` for the `左に` modifier, else `Right`.
        position: BoutenPosition,
    },
    /// 縦中横.
    CombineUpright,
    /// 分数 (`「a/b」は分数`). The render arm splits the target on `/` (ASCII)
    /// or `／` (fullwidth) into a `<sup>`/`<sub>` fraction.
    Fraction,
    /// ドット付き. Composes a combining dot onto an addressed Latin letter
    /// in the reclaimed run (`Sam` + `mは上ドット付き` → `Saṁ`). The selector
    /// grammar lives in the raw directive body, interned on the owned leaf's
    /// `annotation_body` (this attribute stays a `Copy` unit, arena-free).
    AccentDot,
    /// アクサン / ウムラウト: map a single quoted Latin letter to its precomposed
    /// accented glyph (`「e」はアクサン（´）付き` → é, `「o」はウムラウト（¨）付き` → ö).
    /// The letter rides on the owned leaf's `target`; only the [`AccentMark`] is
    /// carried here (no interned body, unlike [`Self::AccentDot`]). Serialized
    /// separately (the suffix carries the bracketed mark symbol, not a bare
    /// keyword), so [`Self::keyword`] falls through to its 太字 default.
    Accent(AccentMark),
    /// End-relative alignment (`「X」は文末より N字上げ揃え` / `「X」は地付き`):
    /// aligns the target run to the text-end edge. `offset` 0 is flush-to-end
    /// (`地付き`); N ≥ 1 lifts the run N full-width chars off the edge. The
    /// forward-scope analogue of [`LineFormat::AlignEnd`]; like it, the input
    /// anchor (文末 / 行末 / 地より / 地から) is not distinguished, only the offset.
    /// Serialized separately (it carries a magnitude), so [`Self::keyword`]
    /// falls through to its 太字 default.
    AlignEnd {
        /// Chars lifted off the text-end edge; 0 = flush (`地付き`), N ≥ 1 = lift.
        offset: u8,
    },
}

/// A forward emphasis node's target-text provenance: whether `serialize`
/// must re-emit the leading literal to reconstruct the source.
///
/// Records whether principal text is owned by this formatting node or retained
/// elsewhere. A quoted annotation operand is never itself principal text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum ForwardOrigin {
    /// The classifier pulled the literal out of the immediately-preceding
    /// source (`青空［＃「青空」に傍点］`): the surrounding plain run was truncated,
    /// so the decorated run is the *sole* visible copy and the serializer
    /// re-emits the literal before the bracket.
    Reclaimed,
    /// The annotation retains a reference operand without owning principal
    /// text. Its target may be established elsewhere or remain unresolved; the
    /// serializer emits the bracket alone.
    Referenced,
    /// The styled-literal half of a **non-adjacent** forward-reference split
    /// pair (`太字の［＃「太字」は太字］`). The classifier located the target as an
    /// *interior* occurrence of the current plain run (present, but not
    /// byte-adjacent to the bracket) and pulled that occurrence out of the
    /// surrounding plain run into this decoration leaf, so it renders **once**,
    /// styled. The directive bracket stays a separate
    /// [`Referenced`](Self::Referenced) node (renders nothing, serializes the
    /// `［＃…］` alone). This is the provenance dual of `Referenced`: the
    /// renderer styles the target (**not** a no-op), and the serializer emits
    /// the literal **alone** (no bracket, because the bracket is the separate
    /// `Referenced` node's job (unlike [`Reclaimed`](Self::Reclaimed), which
    /// re-emits literal *and* bracket from one node). Cannot duplicate the rendered text:
    /// the literal was removed from the plain run, so exactly one copy exists.
    /// Produced only by the interior-referent resolver path, never via
    /// [`from_consume`](Self::from_consume).
    Detached,
}

impl ForwardOrigin {
    /// Derive the provenance from the classifier's consume window:
    /// [`Reclaimed`](Self::Reclaimed) iff `consume_start` was pulled back
    /// before the directive's `［` at `bracket_start`, otherwise
    /// [`Referenced`](Self::Referenced).
    #[must_use]
    pub const fn from_consume(consume_start: u32, bracket_start: u32) -> Self {
        if consume_start < bracket_start {
            Self::Reclaimed
        } else {
            Self::Referenced
        }
    }
}

impl ForwardAttr {
    /// Project to the scope-independent [`Format`] attribute.
    #[must_use]
    pub const fn format(self) -> Format {
        match self {
            Self::Bold => Format::Bold,
            Self::Gothic => Format::Gothic,
            Self::Italic => Format::Italic,
            Self::SuperScript => Format::SuperScript,
            Self::SubScript => Format::SubScript,
            Self::Lowered => Format::Lowered,
            Self::Exponent => Format::Exponent,
            Self::SmallScript(p) => Format::SmallScript(p),
            Self::Framed(k) => Format::Framed(k),
            Self::Horizontal => Format::Horizontal(HorizontalPresentation { align: None }),
            Self::Caption => Format::Caption,
            Self::FontSize(f) => Format::FontSize(f),
            Self::FontSizeAbsolute(s) => Format::FontSizeAbsolute(s),
            Self::Bouten { kind, .. } => Format::Bouten(kind),
            Self::CombineUpright => Format::CombineUpright,
            Self::Fraction => Format::Fraction,
            Self::AccentDot => Format::AccentDot,
            Self::Accent(_) => Format::Accent,
            Self::AlignEnd { .. } => Format::AlignEnd,
        }
    }

    /// Canonical 青空文庫 keyword for the treatment (the body of
    /// `「X」は〈keyword〉` / `「X」に〈keyword〉`).
    ///
    /// [`Self::FontSize`] carries a magnitude and is serialized separately, so
    /// it falls through to the 太字 default here (the serializer never calls
    /// this for it).
    #[must_use]
    pub const fn keyword(self) -> &'static str {
        match self {
            Self::Gothic => "ゴシック体",
            Self::Italic => "斜体",
            Self::SuperScript => "上付き小文字",
            Self::SubScript => "下付き小文字",
            Self::Lowered => "下付き",
            Self::Exponent => "指数",
            Self::SmallScript(BoutenPosition::Right) => "行右小書き",
            Self::SmallScript(BoutenPosition::Left) => "行左小書き",
            Self::Framed(EnclosureKind::Rule) => "罫囲み",
            Self::Framed(EnclosureKind::Unspecified) => "枠囲み",
            Self::Framed(EnclosureKind::DashedRule) => "破線枠囲み",
            // 「□」囲み: the source keyword embeds the quoted glyph, so serialize
            // reconstructs it in a dedicated arm; this bare base word only feeds
            // the keyword round-trip table.
            Self::Framed(EnclosureKind::Box) => "囲み",
            // These three carry no embedded glyph, so the keyword *is* the whole
            // `は…` suffix and serialize round-trips through the default path.
            Self::Framed(EnclosureKind::Circle) => "○付き文字",
            Self::Framed(EnclosureKind::CircleDotted) => "点線丸囲み",
            Self::Framed(EnclosureKind::DoubleRule) => "二重罫囲み",
            Self::Horizontal => "横組み",
            Self::Caption => "キャプション",
            Self::CombineUpright => "縦中横",
            Self::Fraction => "分数",
            Self::FontSizeAbsolute(s) => s.keyword(),
            Self::Bouten { kind, .. } => kind.keyword(),
            // Bold, FontSize, and any future weight default to 太字.
            _ => "太字",
        }
    }
}

// ----------------------------------------------------------------------
// Line scope: single-line layout directives
// ----------------------------------------------------------------------

/// The attributes legal at the single-line scope (`［＃地付き］` etc.).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum LineFormat {
    /// `［＃天から N字下げ］`: indent the line by `amount` full-width chars.
    ///
    /// `end_offset` carries the *both-margin* compound
    /// (`［＃N字下げ、地よりM字上げで］`): a single line set with both a head
    /// indent (`amount`) and a foot-edge lift of `M` full-width chars.
    /// `Some(M)` has the same foot-edge semantics as an
    /// [`AlignEnd`](Self::AlignEnd) with `offset: M`. `None` is the plain
    /// head-only indent.
    Indent {
        /// Full-width characters to indent by.
        amount: u8,
        /// `Some(M)` lifts the line `M` full-width chars off the foot edge (the
        /// `、地よりM字上げで` clause of the both-margin compound); `None` = a
        /// plain head-only indent.
        end_offset: Option<u8>,
    },
    /// `［＃地付き］` / `［＃地から N字上げ］`: end alignment.
    AlignEnd {
        /// Chars lifted off the foot edge. `0` = 地付き, `n` = 地から n 字上げ.
        offset: u8,
        /// A supplied Gothic typeface co-applied to the same line target.
        gothic: bool,
    },
    /// `中央揃え` / `ページの左右中央`: centring.
    Center {
        /// `true` for `ページの左右中央` (page centre), `false` for `中央揃え`.
        page: bool,
    },
    /// `［＃罫囲み］`: enclose the single line it sits on ([`EnclosureKind`]).
    Framed(EnclosureKind),
    /// `［＃この行はゴシック体］`: set the single line it sits on in gothic
    /// ([`Format::Gothic`], distinct from 太字).
    Gothic,
    /// `［＃大文字］` … `［＃特大文字、太字］`: an absolute font size applied to
    /// the whole line (the postfix headline form). `bold` records a co-applied
    /// `、太字` (the `、ゴシック体` postfix is not recognised on this line form;
    /// see `parse_line_font_size`).
    FontSizeAbsolute {
        /// The absolute size.
        size: AbsoluteSize,
        /// `true` for the `、太字` compound.
        bold: bool,
    },
}

impl LineFormat {
    /// Project to the scope-independent [`Format`] attribute.
    #[must_use]
    pub const fn format(self) -> Format {
        match self {
            Self::Indent { .. } => Format::Indent,
            Self::AlignEnd { .. } => Format::AlignEnd,
            Self::Center { .. } => Format::Center,
            Self::Framed(k) => Format::Framed(k),
            Self::Gothic => Format::Gothic,
            Self::FontSizeAbsolute { size, .. } => Format::FontSizeAbsolute(size),
        }
    }
}

// ----------------------------------------------------------------------
// Region scope: paired range / block containers
// ----------------------------------------------------------------------

/// Supplied caption scope forms; figure explanations retain their below-figure placement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum CaptionScope {
    /// Bare caption delimiters within a source line.
    Inline,
    /// Explicit caption block delimiters.
    Block,
    /// A block explicitly described as explanation below a figure or table.
    FigureExplanationBelow,
}

/// The attributes legal at the paired range / block scope.
///
/// The open marker of a `［＃ここから…］ … ［＃ここで…終わり］` (or bare
/// `［＃…］ … ［＃…終わり］`) pair. The matching close is a [`RegionClose`];
/// the open payload here stays authoritative when the pair round-trips.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum RegionFormat {
    /// Explicitly scoped 教科書体, without a selected font asset.
    Textbook,
    /// 太字 range / block. `padded` = block-level (`<div>`, `\n\n` padded);
    /// `!padded` = inline bare range (`<b>`).
    Bold {
        /// `true` = `ここから` block; `false` = inline bare range.
        padded: bool,
    },
    /// ゴシック体 range / block (`［＃ここからゴシック体］ …`). The gothic
    /// counterpart of [`Self::Bold`] (distinct typeface, [`Format::Gothic`]).
    Gothic {
        /// `true` = `ここから` block; `false` = inline bare range.
        padded: bool,
    },
    /// 斜体 range / block: the slant counterpart of [`Self::Bold`].
    Italic {
        /// `true` = `ここから` block; `false` = inline bare range.
        padded: bool,
    },
    /// Caption or figure-explanation source scope.
    Caption(CaptionScope),
    /// 見出し (delimited heading) range / block.
    Heading {
        /// The 大 / 中 / 小 outline level.
        level: HeadingKind,
        /// Standard / 同行 / 窓 style.
        style: HeadingStyle,
        /// `true` = `ここから` block; `false` = paired `窓中見出し…終わり`.
        padded: bool,
    },
    /// 傍点 / 傍線 range (`［＃傍点］ … ［＃傍点終わり］`, `［＃左に傍線］ …`).
    Bouten {
        /// The 傍点 / 傍線 mark; its 点/線 family drives the mismatch check.
        kind: BoutenKind,
        /// `Left` for the `左に` modifier, else `Right`.
        position: BoutenPosition,
    },
    /// 行右 / 行左小書き range.
    SmallScript(BoutenPosition),
    /// Inline horizontal text within vertical composition.
    CombineUpright,
    /// 字下げ block (`［＃ここから N字下げ］ …`).
    Indent(IndentBlock),
    /// 地付き / 地から N 字上げ block.
    AlignEnd {
        /// Chars lifted off the foot edge. `0` = 地付き, `n` = 地から n 字上げ.
        offset: u8,
    },
    /// 字詰め block (`［＃ここから N字詰め］ …`).
    LineWidth(LineWidth),
    /// 表 block.
    Table,
    /// A supplied translation of banknote lettering.
    BanknoteTranslation,
    /// Text explicitly designated as a formula.
    Formula,
    /// 段組 block.
    Columns(ColumnBlock),
    /// 横組み block.
    Horizontal(HorizontalPresentation),
    /// Source-relative placement, without absolute page coordinates.
    RelativePlacement(RelativePlacement),
    /// N段階大きな / 小さな文字 block.
    FontSize(FontShift),
    /// 罫囲み block / range ([`EnclosureKind`]).
    Framed(EnclosureKind),
    /// 割り注 block (multi-line `［＃割り注］ … ［＃割り注終わり］`).
    Warichu,
}

impl RegionFormat {
    /// Project to the scope-independent [`Format`] attribute.
    #[must_use]
    pub const fn format(self) -> Format {
        match self {
            Self::Textbook => Format::Textbook,
            Self::Bold { .. } => Format::Bold,
            Self::Gothic { .. } => Format::Gothic,
            Self::Italic { .. } => Format::Italic,
            Self::Caption(_) => Format::Caption,
            Self::Heading { level, style, .. } => Format::Heading { level, style },
            Self::Bouten { kind, .. } => Format::Bouten(kind),
            Self::SmallScript(p) => Format::SmallScript(p),
            Self::CombineUpright => Format::CombineUpright,
            Self::Indent(_) => Format::Indent,
            Self::AlignEnd { .. } => Format::AlignEnd,
            Self::LineWidth(_) => Format::LineWidth,
            Self::Table => Format::Table,
            Self::BanknoteTranslation => Format::BanknoteTranslation,
            Self::Formula => Format::Formula,
            Self::Columns(block) => Format::Columns(block.count),
            Self::Horizontal(presentation) => Format::Horizontal(presentation),
            Self::RelativePlacement(placement) => Format::RelativePlacement(placement),
            Self::FontSize(f) => Format::FontSize(f),
            Self::Framed(k) => Format::Framed(k),
            Self::Warichu => Format::Warichu,
        }
    }

    /// Stable camelCase wire tag: the machine-contract counterpart used by the
    /// `container_pairs` driver endpoint. Scope-specific (`boutenRange`,
    /// `combineUprightRange`), distinct from the attribute-level
    /// [`Format::as_json_tag`]; exhaustive so a new variant cannot fall through
    /// to a silent `"unknown"`.
    #[must_use]
    pub const fn as_json_tag(self) -> &'static str {
        match self {
            Self::Indent(_) => "indent",
            Self::Warichu => "warichu",
            Self::Framed(_) => "framed",
            Self::AlignEnd { .. } => "alignEnd",
            Self::LineWidth(_) => "lineWidth",
            Self::Bouten { .. } => "boutenRange",
            Self::Bold { .. } => "bold",
            Self::Gothic { .. } => "gothic",
            Self::Textbook => "textbook",
            Self::Italic { .. } => "italic",
            Self::Heading { .. } => "heading",
            Self::Columns(_) => "columns",
            Self::Table => "table",
            Self::BanknoteTranslation => "banknote-translation",
            Self::Formula => "formula",
            Self::Horizontal(_) => "horizontal",
            Self::RelativePlacement(_) => "relative-placement",
            Self::FontSize(_) => "fontSize",
            Self::SmallScript(_) => "smallScript",
            Self::CombineUpright => "combineUprightRange",
            Self::Caption(_) => "caption",
        }
    }

    /// Stable lowercase kebab family tag for human-facing diagnostics (the
    /// mismatched open/close pair names). Payload-independent.
    #[must_use]
    pub const fn kind_str(self) -> &'static str {
        match self {
            Self::Indent(_) => "indent",
            Self::Warichu => "warichu",
            Self::Framed(_) => "framed",
            Self::AlignEnd { .. } => "align-end",
            Self::LineWidth(_) => "line-width",
            Self::Bouten { .. } => "bouten-range",
            Self::Bold { .. } => "bold",
            Self::Gothic { .. } => "gothic",
            Self::Textbook => "textbook",
            Self::Italic { .. } => "italic",
            Self::Heading { .. } => "heading",
            Self::Columns(_) => "columns",
            Self::Table => "table",
            Self::BanknoteTranslation => "banknote-translation",
            Self::Formula => "formula",
            Self::Horizontal(_) => "horizontal",
            Self::RelativePlacement(_) => "relative-placement",
            Self::FontSize(_) => "font-size",
            Self::SmallScript(_) => "small-script",
            Self::CombineUpright => "combine-upright",
            Self::Caption(_) => "caption",
        }
    }

    /// Whether this region renders *inline* (within the current paragraph)
    /// rather than as a block wrapper.
    ///
    /// The 傍点 / 傍線 range, the bare-range 太字 / 斜体 / キャプション forms
    /// (`!padded`), the 小書き range, and the 縦中横 range sit within a line;
    /// every other region is block-level (gets `\n\n` padding + a `<div>`).
    #[must_use]
    pub const fn is_inline(self) -> bool {
        matches!(
            self,
            Self::Bouten { .. }
                | Self::Bold { padded: false }
                | Self::Gothic { padded: false }
                | Self::Italic { padded: false }
                | Self::SmallScript(_)
                | Self::CombineUpright
                | Self::Caption(CaptionScope::Inline)
        )
    }

    /// Whether this region's content is *phrasing* (rendered directly inside
    /// the block element rather than wrapped in `<p>` paragraphs).
    ///
    /// Only [`Self::Heading`] is phrasing: a heading element holds its title
    /// directly, so `<h1><p>…</p></h1>` would be invalid.
    #[must_use]
    pub const fn content_is_phrasing(self) -> bool {
        matches!(self, Self::Heading { .. })
    }

    /// Every variant, one representative instance per data-carrying variant;
    /// the payload is irrelevant to the discriminant-only tag projections. Lets
    /// the wire-tag exhaustiveness test and the codegen enumerate the family
    /// list without a hand-maintained parallel.
    pub const ALL: [Self; 21] = [
        Self::Indent(IndentBlock {
            purpose: None,
            partial: None,
            column_count: None,
            amount: 0,
            wrap: None,
            page_horizontal_center: false,
            align: None,
            end_offset: None,
            layout: IndentLayout::None,
            styles: BlockStyles::EMPTY,
        }),
        Self::Textbook,
        Self::Warichu,
        Self::Framed(EnclosureKind::Rule),
        Self::AlignEnd { offset: 0 },
        Self::LineWidth(LineWidth(NonZeroU8::MIN)),
        Self::Bouten {
            kind: BoutenKind::Goma,
            position: BoutenPosition::Right,
        },
        Self::Bold { padded: false },
        Self::Gothic { padded: false },
        Self::Italic { padded: false },
        Self::Heading {
            level: HeadingKind::Large,
            style: HeadingStyle::Standard,
            padded: false,
        },
        Self::Columns(ColumnBlock {
            column_rule: false,
            count: ColumnCount(NonZeroU8::MIN),
            styles: BlockStyles::EMPTY,
            partial: None,
        }),
        Self::Table,
        Self::BanknoteTranslation,
        Self::Formula,
        Self::Horizontal(HorizontalPresentation { align: None }),
        Self::RelativePlacement(RelativePlacement::BelowHorizontal { anchor: None }),
        Self::FontSize(FontShift(NonZeroI8::MIN)),
        Self::SmallScript(BoutenPosition::Right),
        Self::CombineUpright,
        Self::Caption(CaptionScope::Inline),
    ];
}

// ----------------------------------------------------------------------
// Region close: self-sufficient; carries the close marker's own data
// ----------------------------------------------------------------------

/// The close marker of a paired region.
///
/// Carries exactly what the close marker (and HTML close tag) reproduce from
/// the **close** source text, never a placeholder. A close can appear without
/// a matching open (a stray `［＃…終わり］`) and a mismatched close keeps its
/// own family (`［＃傍線終わり］` closing a `［＃傍点］`), so the close must be
/// self-sufficient; it is *not* reconstructed from the open. `Option` /
/// `NonZero` / `bool` make the former `width: 0` / `steps: ±1` / `lines: 0`
/// placeholder states unrepresentable, which was Pillar 1's actual goal.
///
/// It stays meaningfully smaller than [`RegionFormat`] (the indent, line-width,
/// columns, align-end, and font-size closes all shed payload), so the separate
/// type still earns its keep.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
#[non_exhaustive]
pub enum RegionClose {
    /// Close the explicitly supplied textbook typeface scope.
    Textbook,
    /// `字下げ終わり`, or the `字下げ、{W}字組み終わり` compound (the close
    /// carries `W`, so the marker round-trips byte-exact).
    Indent {
        /// Semantic purpose explicitly named by the closing marker.
        purpose: Option<BlockPurpose>,
        /// Explicit indentation magnitude on the closing marker.
        amount: Option<u8>,
        /// The `W` of a `字組み終わり` compound; `None` for the generic
        /// `字下げ終わり` (plain / 字詰め / 折り返して / 中央 indents).
        kumi_width: Option<LineWidth>,
        /// Presentation explicitly named by the closer; omitted axes do not
        /// constrain the opening marker.
        styles: BlockStyles,
    },
    /// `割り注終わり`.
    Warichu,
    /// `罫囲み終わり` ([`EnclosureKind`]).
    Framed(EnclosureKind),
    /// `字上げ終わり` / 地付き close (no offset; the close marker carries none).
    AlignEnd,
    /// `字詰め終わり`.
    LineWidth,
    /// `傍点終わり` / `傍線終わり` / `波線終わり` …: the close's own mark and
    /// `左に` side. The 点/線 family (`kind.is_line()`) drives the
    /// `mismatched_bouten_container` diagnostic.
    Bouten {
        /// The close marker's own 傍点 / 傍線 mark.
        kind: BoutenKind,
        /// `Left` for the close's `左に` modifier, else `Right`.
        position: BoutenPosition,
    },
    /// `太字終わり` (`!padded`) / `ここで太字終わり` (`padded`).
    Bold {
        /// `true` = `ここで` block close; `false` = inline bare-range close.
        padded: bool,
    },
    /// `ゴシック体終わり` (`!padded`) / `ここでゴシック体終わり` (`padded`).
    Gothic {
        /// `true` = `ここで` block close; `false` = inline bare-range close.
        padded: bool,
    },
    /// `斜体終わり` / `ここで斜体終わり`.
    Italic {
        /// `true` = `ここで` block close; `false` = inline bare-range close.
        padded: bool,
    },
    /// `<見出し>終わり` / `ここで<見出し>終わり`.
    Heading {
        /// The 大 / 中 / 小 outline level. `None` for the level-less bare
        /// `ここで見出し終わり` close (the open payload stays authoritative for
        /// pairing/render; this only round-trips the close marker's own
        /// spelling); `Some(k)` for a leveled close.
        level: Option<HeadingKind>,
        /// Standard / 同行 / 窓 style.
        style: HeadingStyle,
        /// `true` = `ここで` block close; `false` = paired `…終わり` close.
        padded: bool,
    },
    /// `段組終わり`, optionally restating the supplied column count.
    Columns(Option<ColumnCount>),
    /// `表終わり`.
    Table,
    /// A supplied translation of banknote lettering.
    BanknoteTranslation,
    /// Text explicitly designated as a formula.
    Formula,
    /// `横組み終わり`.
    Horizontal,
    /// `大きな文字終わり` (`larger`) / `小さな文字終わり`.
    FontSize {
        /// `true` = 大きな (larger); `false` = 小さな (smaller).
        larger: bool,
        /// Supplied relative step count; omitted closers impose only direction.
        magnitude: Option<NonZeroU8>,
    },
    /// `行右 / 行左小書き終わり`.
    SmallScript(BoutenPosition),
    /// `縦中横終わり`.
    CombineUpright,
    /// Closing marker for the supplied caption scope form.
    Caption(CaptionScope),
}

impl RegionClose {
    /// Canonical closing form for a region.
    ///
    /// A parsed source closer may supply additional magnitude constraints;
    /// the scope matcher checks those against the actual opening payload.
    #[must_use]
    pub const fn of(region: RegionFormat) -> Self {
        match region {
            RegionFormat::Indent(block) => Self::Indent {
                purpose: block.purpose,
                amount: None,
                styles: BlockStyles::EMPTY,
                kumi_width: match block.layout {
                    IndentLayout::Kumi(kumi) => Some(LineWidth(kumi.width)),
                    IndentLayout::LineWidth(_) | IndentLayout::None => None,
                },
            },
            RegionFormat::Warichu => Self::Warichu,
            RegionFormat::Framed(k) => Self::Framed(k),
            RegionFormat::AlignEnd { .. } => Self::AlignEnd,
            RegionFormat::LineWidth(_) => Self::LineWidth,
            RegionFormat::Bouten { kind, position } => Self::Bouten { kind, position },
            RegionFormat::Bold { padded } => Self::Bold { padded },
            RegionFormat::Gothic { padded } => Self::Gothic { padded },
            RegionFormat::Textbook => Self::Textbook,
            RegionFormat::Italic { padded } => Self::Italic { padded },
            RegionFormat::Heading {
                level,
                style,
                padded,
            } => Self::Heading {
                // `of` always yields a leveled close (a `None` close originates
                // only from the classifier parsing a bare `見出し終わり` marker).
                level: Some(level),
                style,
                padded,
            },
            RegionFormat::Columns(block) => Self::Columns(Some(block.count)),
            RegionFormat::Table => Self::Table,
            RegionFormat::BanknoteTranslation => Self::BanknoteTranslation,
            RegionFormat::Formula => Self::Formula,
            RegionFormat::Horizontal(_) | RegionFormat::RelativePlacement(_) => Self::Horizontal,
            RegionFormat::FontSize(shift) => Self::FontSize {
                larger: shift.larger(),
                magnitude: NonZeroU8::new(shift.magnitude()),
            },
            RegionFormat::SmallScript(side) => Self::SmallScript(side),
            RegionFormat::CombineUpright => Self::CombineUpright,
            RegionFormat::Caption(scope) => Self::Caption(scope),
        }
    }

    /// Stable lowercase kebab family tag for human-facing diagnostics (mirrors
    /// [`RegionFormat::kind_str`] so an open/close mismatch names both sides
    /// consistently).
    #[must_use]
    pub const fn kind_str(self) -> &'static str {
        match self {
            Self::Indent { .. } => "indent",
            Self::Warichu => "warichu",
            Self::Framed(_) => "framed",
            Self::AlignEnd => "align-end",
            Self::LineWidth => "line-width",
            Self::Bouten { .. } => "bouten-range",
            Self::Bold { .. } => "bold",
            Self::Gothic { .. } => "gothic",
            Self::Textbook => "textbook",
            Self::Italic { .. } => "italic",
            Self::Heading { .. } => "heading",
            Self::Columns(_) => "columns",
            Self::Table => "table",
            Self::BanknoteTranslation => "banknote-translation",
            Self::Formula => "formula",
            Self::Horizontal => "horizontal",
            Self::FontSize { .. } => "font-size",
            Self::SmallScript(_) => "small-script",
            Self::CombineUpright => "combine-upright",
            Self::Caption(_) => "caption",
        }
    }

    /// Whether the close renders inline (mirrors [`RegionFormat::is_inline`] so
    /// the close marker's `\n\n` padding / `<p>` handling matches the open).
    #[must_use]
    pub const fn is_inline(self) -> bool {
        matches!(
            self,
            Self::Bouten { .. }
                | Self::Bold { padded: false }
                | Self::Gothic { padded: false }
                | Self::Italic { padded: false }
                | Self::SmallScript(_)
                | Self::CombineUpright
                | Self::Caption(CaptionScope::Inline)
        )
    }

    /// Whether the close's content was phrasing: only [`Self::Heading`].
    #[must_use]
    pub const fn content_is_phrasing(self) -> bool {
        matches!(self, Self::Heading { .. })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Illustration, MarginNote, Node, NodeRef};

    /// Formatting stays `Copy`; size limits protect actual node and registry
    /// entries, rather than smaller payloads sharing their enum storage.
    #[test]
    fn formatting_preserves_copy_and_node_size_budgets() {
        const fn assert_copy<T: Copy>() {}
        assert_copy::<RegionFormat>();
        assert_copy::<Format>();
        assert_copy::<ForwardAttr>();
        assert_copy::<LineFormat>();
        assert_copy::<RegionClose>();
        assert!(
            size_of::<Node>() <= 40,
            "Node={}, MarginNote={}, Illustration={}",
            size_of::<Node>(),
            size_of::<MarginNote>(),
            size_of::<Illustration>()
        );
        assert!(size_of::<NodeRef>() <= 44);
    }

    /// The container-pairs wire tags distinguish region scopes from attributes.
    #[test]
    fn region_format_wire_tags_are_stable() {
        assert_eq!(
            RegionFormat::Bouten {
                kind: BoutenKind::Goma,
                position: BoutenPosition::Right,
            }
            .as_json_tag(),
            "boutenRange"
        );
        assert_eq!(
            RegionFormat::LineWidth(LineWidth(NonZeroU8::MIN)).as_json_tag(),
            "lineWidth"
        );
        assert_eq!(RegionFormat::Bold { padded: true }.as_json_tag(), "bold");
        assert_eq!(RegionFormat::Bold { padded: false }.as_json_tag(), "bold");
        assert_eq!(
            RegionFormat::Gothic { padded: true }.as_json_tag(),
            "gothic"
        );
    }

    /// A relative placement uses the supplied horizontal closing family.
    #[test]
    fn region_close_of_round_trips_kind_str() {
        for open in RegionFormat::ALL {
            assert_eq!(
                RegionClose::of(open).kind_str(),
                match open {
                    RegionFormat::RelativePlacement(_) => "horizontal",
                    _ => open.kind_str(),
                },
                "close family must mirror open family for {open:?}"
            );
        }
    }

    /// The 傍点/傍線 kind + position survive the open → close projection (so a
    /// mismatched close keeps its own family for round-trip and diagnostics).
    #[test]
    fn region_close_preserves_bouten_kind() {
        let line = RegionFormat::Bouten {
            kind: BoutenKind::UnderLine,
            position: BoutenPosition::Left,
        };
        assert_eq!(
            RegionClose::of(line),
            RegionClose::Bouten {
                kind: BoutenKind::UnderLine,
                position: BoutenPosition::Left,
            }
        );
    }

    /// The 字組み close keeps its own width; every other indent close is generic.
    #[test]
    fn region_close_indent_keeps_kumi_width() {
        let kumi = RegionFormat::Indent(IndentBlock {
            purpose: None,
            partial: None,
            column_count: None,
            amount: 2,
            wrap: None,
            page_horizontal_center: false,
            align: None,
            end_offset: None,
            layout: IndentLayout::Kumi(Kumi {
                lines: NonZeroU8::MIN,
                width: NonZeroU8::new(20).unwrap(),
            }),
            styles: BlockStyles::EMPTY,
        });
        assert_eq!(
            RegionClose::of(kumi),
            RegionClose::Indent {
                purpose: None,
                amount: None,
                kumi_width: Some(LineWidth(NonZeroU8::new(20).unwrap())),
                styles: BlockStyles::EMPTY,
            }
        );
        let plain = RegionFormat::Indent(IndentBlock {
            purpose: None,
            partial: None,
            column_count: None,
            amount: 2,
            wrap: None,
            page_horizontal_center: false,
            align: None,
            end_offset: None,
            layout: IndentLayout::None,
            styles: BlockStyles::EMPTY,
        });
        assert_eq!(
            RegionClose::of(plain),
            RegionClose::Indent {
                purpose: None,
                amount: None,
                kumi_width: None,
                styles: BlockStyles::EMPTY
            }
        );
    }

    /// Attribute-level tags are exhaustive and distinct from the scope wire
    /// tags (e.g. `bouten` vs `boutenRange`).
    #[test]
    fn format_attribute_tags() {
        assert_eq!(Format::Bouten(BoutenKind::Goma).as_json_tag(), "bouten");
        assert_eq!(Format::CombineUpright.as_json_tag(), "combineUpright");
        assert_eq!(Format::Warichu.as_json_tag(), "warichu");
        assert_eq!(Format::Center.as_json_tag(), "center");
    }

    /// Each scope sum projects onto a `Format` attribute (no panics / total).
    #[test]
    fn scope_projections_are_total() {
        assert_eq!(ForwardAttr::Bold.format(), Format::Bold);
        assert_eq!(
            LineFormat::Framed(EnclosureKind::Rule).format(),
            Format::Framed(EnclosureKind::Rule)
        );
        assert_eq!(RegionFormat::Warichu.format(), Format::Warichu);
        assert_eq!(
            RegionFormat::Bold { padded: true }.format(),
            Format::Bold,
            "range and forward Bold share the attribute identity"
        );
    }
}
