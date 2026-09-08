//! Body-keyword directive classifier.
//!
//! The `［＃<keyword>］` body dispatcher: the `BODY_PATTERNS` table and
//! its Aho-Corasick DFA, `classify_annotation_body`, and the per-family
//! body parsers. Operates purely on the trimmed body string (no event
//! context); the forward-reference recognisers that need event context
//! live in the parent module. Extracted verbatim from the classify-stage
//! classifier.

#[cfg(feature = "classify-instrument")]
use super::super::instrumentation::{Subsystem, SubsystemGuard};
use crate::text_variant::image_edition_note;
use ab_aozora_syntax::ast::KuntenKind;
use ab_aozora_syntax::ast::{Content, Node};
use ab_aozora_syntax::external_table_reference::external_table_filename;

use std::sync::OnceLock;

use aho_corasick::{AhoCorasick, AhoCorasickBuilder, Anchored, Input, MatchKind, StartKind};
use core::num::{NonZeroI8, NonZeroU8};

use ab_aozora_syntax::alloc::Allocator;
use ab_aozora_syntax::ast::Directive;
use ab_aozora_syntax::{
    AbsoluteSize, BOUTEN_KINDS, BlockStyles, BoutenKind, BoutenPosition, CaptionScope, ColumnBlock,
    ColumnCount, DirectiveKind, EnclosureKind, FontShift, HeadingKind, HeadingStyle,
    HorizontalPresentation, IndentBlock, IndentLayout, Kumi, LineFormat, LineWidth,
    QualitativeFontSize, RegionClose, RegionFormat, SectionKind, Span,
};

use super::super::pair::PairEvent;
use super::{BodyView, EmitKind};

/// Annotation text and the native delimiter ownership enclosing its clauses.
pub(super) struct AnnotationBody<'a> {
    pub text: &'a str,
    pub start: u32,
    pub view: BodyView<'a>,
}

impl<'a> AnnotationBody<'a> {
    fn clauses(&self, after: &'a str) -> Vec<(&'a str, Span)> {
        let body_end =
            self.start + u32::try_from(self.text.len()).expect("source fits native span");
        let mut nested = Vec::new();
        for (index, event) in self.view.events.iter().enumerate() {
            let range = match *event {
                PairEvent::PairOpen { span, .. } if span.start >= self.start => {
                    let end = self
                        .view
                        .links
                        .get(index)
                        .and_then(|index| self.view.events.get(*index as usize))
                        .and_then(|event| match event {
                            PairEvent::PairClose { span, .. } => Some(span.end),
                            _ => None,
                        })
                        .unwrap_or(body_end);
                    Some(Span::new(span.start, end.min(body_end)))
                }
                PairEvent::Unclosed { span, .. } | PairEvent::Unmatched { span, .. }
                    if span.start >= self.start =>
                {
                    Some(Span::new(span.start, body_end))
                }
                _ => None,
            };
            if let Some(range) = range.filter(|range| range.start < body_end) {
                nested.push(range);
            }
        }
        nested.sort_unstable_by_key(|range| range.start);
        let after_start = self.start
            + u32::try_from(self.text.len() - after.len()).expect("source fits native span");
        let mut next_nested = 0;
        let mut covered_until = 0;
        let mut start = 0;
        let mut clauses = Vec::new();
        for (offset, _) in after.match_indices('、') {
            let position = after_start + u32::try_from(offset).expect("source fits native span");
            while next_nested < nested.len() && nested[next_nested].start <= position {
                covered_until = covered_until.max(nested[next_nested].end);
                next_nested += 1;
            }
            if position < covered_until {
                continue;
            }
            clauses.push((
                &after[start..offset],
                Span::new(
                    after_start + u32::try_from(start).expect("source fits native span"),
                    position,
                ),
            ));
            start = offset + '、'.len_utf8();
        }
        clauses.push((
            &after[start..],
            Span::new(
                after_start + u32::try_from(start).expect("source fits native span"),
                body_end,
            ),
        ));
        clauses
    }
}

/// One row of [`BODY_PATTERNS`]: the byte sequence the DFA matches at
/// `body[0..match_end]`, and the family that decides what to emit.
#[derive(Clone, Copy)]
struct BodyPattern {
    needle: &'static str,
    family: BodyFamily,
}

/// Outcome category for an anchored AC match against the annotation
/// body. Each variant carries enough information to either emit a
/// constant `EmitKind` directly (when the family is exact-match) or to
/// dispatch to a small per-family parser for the body remainder.
#[derive(Clone, Copy)]
enum BodyFamily {
    // === Exact-match (body must equal needle) ===
    PageBreak,
    BodyEnd,     // 本文終わり
    ForcedBreak, // 改行
    SectionKaicho,
    SectionKaidan,
    SectionKaimihiraki,
    AlignEnd0,           // 地付き
    CenterMarker,        // ページの左右中央 / 中央揃え
    LineGothic,          // この行はゴシック体
    KeigakomiOpen,       // 罫囲み
    KeigakomiClose,      // 罫囲み終わり
    IndentBlock1,        // ここから字下げ → Indent { amount: 1 }
    PageCenterBlockOpen, // ここからページの左右中央
    AlignEndBlock0,      // ここから地付き → AlignEnd { offset: 0 }
    IndentBlockEnd,      // ここで字下げ終わり
    AlignEndBlockEnd,    // ここで地付き終わり
    LineWidthBlockEnd,   // ここで字詰め終わり
    FormulaBlockOpen,
    FormulaBlockEnd,
    TableBlockOpen,      // ここから表
    TableBlockEnd,       // ここで表終わり
    HorizontalBlockOpen, // ここから横組み
    HorizontalBlockEnd,  // ここで横組み終わり
    FontSizeBlockEnd,    // ここで大きな/小さな文字終わり
    ParamBlockEnd,       // ここで[N]段組(み) / N段階…文字終わり
    WarichuOpen,         // 割り注
    WarichuClose,        // 割り注終わり
    KaeritenSingle,      // body must equal one of 12 single-char marks
    KaeritenCompound,    // body must equal one of 8 compound marks

    // === Prefix-with-parameter (parse body[match_end..]) ===
    AlignEndParamPrefix, // 地から → 地から{N}字上げ
    AlignEndSpacingPrefix,
    GothicAlignEndPrefix,
    SashiePrefix,             // 挿絵（ → 挿絵（X）入る
    IndentBlockParamPrefix,   // ここから → ここから{N}字下げ
    AlignEndBlockParamPrefix, // ここから地から → ここから地から{N}字上げ
    IndentCompoundBlockEnd,   // ここで字下げ、 + supplied width or presentation
    OkuriganaPrefix,          // （ → kaeriten okurigana （X）

    // === Body-equals-pattern then parse from body[0] ===
    IndentParamPrefix, // {digit} → {N}字下げ (re-parse from body[0])

    /// 傍点 / 傍線 range form (`傍点` / `白丸傍点` / `二重傍線` / `左に傍線`
    /// …, with optional `終わり` close suffix). The needle matches the
    /// variant (or the `左に` prefix); `parse_bouten_range_body` reads the
    /// full body for the kind, the `左に` position, and the `終わり` close.
    BoutenRange,

    /// 太字 / 斜体 range / block form (`太字` / `斜体` inline,
    /// `ここから太字` / `ここで斜体終わり` block, with `終わり` close). The
    /// needle anchors the body; `parse_emphasis_body` reads the full body
    /// for the kind, the block vs inline form, and open vs close.
    Emphasis,

    /// 小書き range form (`行右小書き` / `行左小書き`, with optional `終わり`
    /// close). The bare-range sibling of the forward `「X」は行右小書き`
    /// emphasis leaf; `parse_small_script_range_body` reads the full body
    /// for the 右/左 side and open vs close.
    SmallScriptRange,
    /// Exact scoped TCY opener.
    CombineUprightOpen,
    /// Exact scoped TCY closer.
    CombineUprightClose,

    /// キャプション range / block (`キャプション` / `キャプション終わり` inline,
    /// `ここからキャプション` / `ここでキャプション終わり` block).
    /// `parse_caption_body` reads the full body for the block-vs-inline form
    /// and open vs close.
    CaptionRange,
    /// Complete supplied banknote translation opener.
    BanknoteTranslationOpen,
    /// Complete supplied translation closer.
    TranslationClose,

    /// `ここから割り注`: block 割り注 opener (the multi-line region form;
    /// the inline `［＃割り注］` is [`Self::WarichuOpen`]). → `Container(Warichu)`.
    WarichuBlockOpen,
    /// `ここで割り注終わり`: block 割り注 closer.
    WarichuBlockEnd,
    /// `天から` / `天より` → `天X{N}字下げ`: a single-line indent measured
    /// from the top margin; identical to a plain `{N}字下げ`, so it emits an
    /// `Indent` leaf. Also carries the both-margin compound
    /// (`天X{N}字下げ、地より{M}字上げで`), see [`parse_both_margin_tail`].
    TopIndentPrefix,
    /// `改行天付き` → `改行天付き、折り返して{N}字下げ`: the ここから-less
    /// bare sibling of the top-flush hanging indent (amount 0 + wrap N).
    KaigyouTentsukiPrefix,

    /// Absolute font-size line directive (`大文字` … `特大文字、太字`). The
    /// needle anchors on the size keyword; `parse_line_font_size` re-reads the
    /// full body for the size and an optional `、太字` / `、ゴシック体` compound.
    LineFontSize,
}

/// How a [`BodyFamily`] consumes its DFA match: an `Exact` family must
/// equal the whole body, a `Prefix` family parses `body[match_end..]`,
/// and a `Reparse` family re-reads the full body from `body[0]`. Derived
/// 1:1 from the family so the exact-vs-not contract lives in one place
/// instead of being split across the per-arm `if exact` guards and a
/// parallel catch-all `None`.
#[derive(Clone, Copy, PartialEq, Eq)]
enum MatchMode {
    Exact,
    Prefix,
    Reparse,
}

/// The [`MatchMode`] of a [`BodyFamily`] (see [`MatchMode`]).
const fn body_family_mode(family: BodyFamily) -> MatchMode {
    match family {
        BodyFamily::PageBreak
        | BodyFamily::BodyEnd
        | BodyFamily::ForcedBreak
        | BodyFamily::SectionKaicho
        | BodyFamily::SectionKaidan
        | BodyFamily::SectionKaimihiraki
        | BodyFamily::AlignEnd0
        | BodyFamily::CenterMarker
        | BodyFamily::LineGothic
        | BodyFamily::KeigakomiOpen
        | BodyFamily::KeigakomiClose
        | BodyFamily::WarichuBlockOpen
        | BodyFamily::WarichuBlockEnd
        | BodyFamily::IndentBlock1
        | BodyFamily::PageCenterBlockOpen
        | BodyFamily::AlignEndBlock0
        | BodyFamily::AlignEndBlockEnd
        | BodyFamily::LineWidthBlockEnd
        | BodyFamily::FormulaBlockOpen
        | BodyFamily::FormulaBlockEnd
        | BodyFamily::TableBlockOpen
        | BodyFamily::TableBlockEnd
        | BodyFamily::HorizontalBlockOpen
        | BodyFamily::HorizontalBlockEnd
        | BodyFamily::BanknoteTranslationOpen
        | BodyFamily::TranslationClose
        | BodyFamily::CombineUprightOpen
        | BodyFamily::CombineUprightClose
        | BodyFamily::WarichuOpen
        | BodyFamily::WarichuClose
        | BodyFamily::KaeritenSingle
        | BodyFamily::KaeritenCompound => MatchMode::Exact,
        BodyFamily::AlignEndParamPrefix
        | BodyFamily::AlignEndSpacingPrefix
        | BodyFamily::GothicAlignEndPrefix
        | BodyFamily::SashiePrefix
        | BodyFamily::IndentBlockParamPrefix
        | BodyFamily::AlignEndBlockParamPrefix
        | BodyFamily::IndentBlockEnd
        | BodyFamily::IndentCompoundBlockEnd
        | BodyFamily::ParamBlockEnd
        | BodyFamily::OkuriganaPrefix
        | BodyFamily::TopIndentPrefix
        | BodyFamily::KaigyouTentsukiPrefix => MatchMode::Prefix,
        BodyFamily::FontSizeBlockEnd
        | BodyFamily::IndentParamPrefix
        | BodyFamily::BoutenRange
        | BodyFamily::Emphasis
        | BodyFamily::SmallScriptRange
        | BodyFamily::CaptionRange
        | BodyFamily::LineFontSize => MatchMode::Reparse,
    }
}

/// Static pattern table. Order is irrelevant for behavior because the
/// DFA is built with [`MatchKind::LeftmostLongest`]: the longer needle
/// always wins (so `罫囲み終わり` beats `罫囲み`, `ここから字下げ` beats
/// `ここから`, `一レ` beats `一`, etc.). Keeping families together for
/// readability instead of sorting by length.
static BODY_PATTERNS: &[BodyPattern] = &[
    // Block container with full-keyword bodies.
    BodyPattern {
        needle: "ここから字下げ",
        family: BodyFamily::IndentBlock1,
    },
    BodyPattern {
        needle: "ここからページの左右中央",
        family: BodyFamily::PageCenterBlockOpen,
    },
    BodyPattern {
        needle: "ここから地付き",
        family: BodyFamily::AlignEndBlock0,
    },
    BodyPattern {
        needle: "ここから地から",
        family: BodyFamily::AlignEndBlockParamPrefix,
    },
    BodyPattern {
        needle: "ここより",
        family: BodyFamily::IndentBlockParamPrefix,
    },
    BodyPattern {
        needle: "ここから",
        family: BodyFamily::IndentBlockParamPrefix,
    },
    BodyPattern {
        needle: "ここで字下げ終り",
        family: BodyFamily::IndentBlockEnd,
    },
    BodyPattern {
        needle: "ここで文字下げ終わり",
        family: BodyFamily::IndentBlockEnd,
    },
    BodyPattern {
        needle: "ここで字下げ終わり",
        family: BodyFamily::IndentBlockEnd,
    },
    BodyPattern {
        needle: "ここで字下げおわり",
        family: BodyFamily::IndentBlockEnd,
    },
    BodyPattern {
        needle: "字下げ終わり",
        family: BodyFamily::IndentBlockEnd,
    },
    // The 字組み compound closer carries the width (`ここで字下げ、20字組み終わり`). Distinct from the generic `ここで字下げ終わり` above: the char after
    // `ここで字下げ` is `、` vs `終`, so the two needles never overlap.
    BodyPattern {
        needle: "ここで字下げ、",
        family: BodyFamily::IndentCompoundBlockEnd,
    },
    BodyPattern {
        needle: "ここで地付き終わり",
        family: BodyFamily::AlignEndBlockEnd,
    },
    // The 字上げ block (［＃ここから地から N 字上げ］) is closed by either
    // ［＃ここで字上げ終わり］ or ［＃ここで地付き終わり］; both end the same
    // AlignEnd container. The open-side offset is authoritative when
    // pairing, so this closer reuses AlignEndBlockEnd.
    BodyPattern {
        needle: "ここで字上げ終わり",
        family: BodyFamily::AlignEndBlockEnd,
    },
    BodyPattern {
        needle: "ここで字詰め終わり",
        family: BodyFamily::LineWidthBlockEnd,
    },
    BodyPattern {
        needle: "ここから数式",
        family: BodyFamily::FormulaBlockOpen,
    },
    BodyPattern {
        needle: "ここで数式終わり",
        family: BodyFamily::FormulaBlockEnd,
    },
    BodyPattern {
        needle: "ここから表",
        family: BodyFamily::TableBlockOpen,
    },
    BodyPattern {
        needle: "ここから表組",
        family: BodyFamily::TableBlockOpen,
    },
    BodyPattern {
        needle: "ここで表終わり",
        family: BodyFamily::TableBlockEnd,
    },
    BodyPattern {
        needle: "ここで表組終わり",
        family: BodyFamily::TableBlockEnd,
    },
    BodyPattern {
        needle: "ここから横組み",
        family: BodyFamily::HorizontalBlockOpen,
    },
    BodyPattern {
        needle: "ここから横組み右揃えで",
        family: BodyFamily::HorizontalBlockOpen,
    },
    BodyPattern {
        needle: "ここから横組み中央揃えで",
        family: BodyFamily::HorizontalBlockOpen,
    },
    BodyPattern {
        needle: "ここで横組み終わり",
        family: BodyFamily::HorizontalBlockEnd,
    },
    BodyPattern {
        needle: "ここから横書き",
        family: BodyFamily::HorizontalBlockOpen,
    },
    BodyPattern {
        needle: "ここで横書き終わり",
        family: BodyFamily::HorizontalBlockEnd,
    },
    BodyPattern {
        needle: "ここで大きな文字終わり",
        family: BodyFamily::FontSizeBlockEnd,
    },
    BodyPattern {
        needle: "ここで小さな文字終わり",
        family: BodyFamily::FontSizeBlockEnd,
    },
    // Bare-range font-size close (ここ-less): ［＃大きな/小さな文字終わり］,
    // the sibling of ここで…終わり. Reuses FontSizeBlockEnd; the open side
    // (［＃{N}段階…文字］) routes through IndentParamPrefix (leading digit).
    // LeftmostLongest keeps ここで… winning over the bare needle.
    BodyPattern {
        needle: "大きな文字終わり",
        family: BodyFamily::FontSizeBlockEnd,
    },
    BodyPattern {
        needle: "小さな文字終わり",
        family: BodyFamily::FontSizeBlockEnd,
    },
    // Bare-range horizontal (ここ-less): ［＃横組み］ … ［＃横組み終わり］,
    // the sibling of ここから横組み / ここで横組み終わり. Same Horizontal
    // container; LeftmostLongest keeps 横組み終わり winning over 横組み, and
    // the exact-match guard rejects compounds like 横組みで、… (→ Unknown).
    BodyPattern {
        needle: "横組み",
        family: BodyFamily::HorizontalBlockOpen,
    },
    BodyPattern {
        needle: "横組み終わり",
        family: BodyFamily::HorizontalBlockEnd,
    },
    BodyPattern {
        needle: "縦中横",
        family: BodyFamily::CombineUprightOpen,
    },
    BodyPattern {
        needle: "縦中横終わり",
        family: BodyFamily::CombineUprightClose,
    },
    // 小書き range: ［＃行右小書き］ … ［＃行右小書き終わり］ (and 行左).
    // LeftmostLongest keeps 行右小書き終わり winning over 行右小書き.
    BodyPattern {
        needle: "行右小書き",
        family: BodyFamily::SmallScriptRange,
    },
    BodyPattern {
        needle: "行右小書き終わり",
        family: BodyFamily::SmallScriptRange,
    },
    BodyPattern {
        needle: "行左小書き",
        family: BodyFamily::SmallScriptRange,
    },
    BodyPattern {
        needle: "行左小書き終わり",
        family: BodyFamily::SmallScriptRange,
    },
    // キャプション range / block. LeftmostLongest keeps キャプション終わり
    // over キャプション, and ここからキャプション over ここから.
    BodyPattern {
        needle: "キャプション",
        family: BodyFamily::CaptionRange,
    },
    BodyPattern {
        needle: "キャプション終わり",
        family: BodyFamily::CaptionRange,
    },
    BodyPattern {
        needle: "ここからキャプション",
        family: BodyFamily::CaptionRange,
    },
    BodyPattern {
        needle: "ここでキャプション終わり",
        family: BodyFamily::CaptionRange,
    },
    BodyPattern {
        needle: "ここから紙幣の文字の訳文",
        family: BodyFamily::BanknoteTranslationOpen,
    },
    BodyPattern {
        needle: "ここで訳文終わり",
        family: BodyFamily::TranslationClose,
    },
    BodyPattern {
        needle: "ここから図表下部解説文",
        family: BodyFamily::CaptionRange,
    },
    BodyPattern {
        needle: "ここで図表下部解説文終わり",
        family: BodyFamily::CaptionRange,
    },
    // 縦中横 has no paired-range form (spec §6.3 defines only the forward-
    // reference `「X」は縦中横` leaf). A bare `［＃縦中横］…［＃縦中横終わり］` is a
    // non-canonical corpus convention that used to open a styling range,
    // contradicting the handbook's own tcy page; it now stays verbatim
    // `Directive{Unknown}` and never opens a block.
    // Block 罫囲み (ここから form; the bare 罫囲み is also KeigakomiOpen).
    // LeftmostLongest keeps ここから罫囲み over the ここから indent prefix.
    BodyPattern {
        needle: "ここから罫囲み",
        family: BodyFamily::KeigakomiOpen,
    },
    BodyPattern {
        needle: "ここで罫囲み終わり",
        family: BodyFamily::KeigakomiClose,
    },
    // 表罫囲み / ミシン罫囲み (specific rule styles) are non-canonical and
    // corpus-vanishing (2 / 1 works); they are *not* folded onto Rule (which
    // would erase the rule-style spelling); they decline to Directive{Unknown}
    // (lossless verbatim), the core recognising only the canonical 罫囲み.
    // Block 割り注 (multi-line region; inline ［＃割り注］ stays WarichuOpen).
    BodyPattern {
        needle: "ここから割り注",
        family: BodyFamily::WarichuBlockOpen,
    },
    BodyPattern {
        needle: "ここで割り注終わり",
        family: BodyFamily::WarichuBlockEnd,
    },
    // 天から{N}字下げ (single-line indent from the top) and the bare
    // 改行天付き、折り返して{N}字下げ hanging indent.
    BodyPattern {
        needle: "天から",
        family: BodyFamily::TopIndentPrefix,
    },
    // 天より: the alternate wording of 天から (both "measured from the top
    // margin"); attested only in the both-margin compound
    // (`天より{N}字下げ、地より{M}字上げで`). Routes through the same
    // TopIndentPrefix arm, which canonicalises to a plain `{N}字下げ` head.
    BodyPattern {
        needle: "天より",
        family: BodyFamily::TopIndentPrefix,
    },
    BodyPattern {
        needle: "改行天付き",
        family: BodyFamily::KaigyouTentsukiPrefix,
    },
    // Parameterized closes share ここで; longer fixed closers win. The
    // handler validates the complete column-count or font-size suffix.
    BodyPattern {
        needle: "ここで",
        family: BodyFamily::ParamBlockEnd,
    },
    // Section / page break (exact).
    BodyPattern {
        needle: "改ページ",
        family: BodyFamily::PageBreak,
    },
    // 改頁: the kanji spelling of 改ページ (annotation/layout_1.html);
    // canonicalises to 改ページ on serialize.
    BodyPattern {
        needle: "改頁",
        family: BodyFamily::PageBreak,
    },
    BodyPattern {
        needle: "改丁",
        family: BodyFamily::SectionKaicho,
    },
    BodyPattern {
        needle: "改段",
        family: BodyFamily::SectionKaidan,
    },
    BodyPattern {
        needle: "改見開き",
        family: BodyFamily::SectionKaimihiraki,
    },
    // Structural markers. `改行` is an exact match: a bare `［＃改行］`
    // forced line break. The longer `改行天付き` needle wins under
    // LeftmostLongest for the hanging-indent form, and the Exact mode rejects
    // any `改行X` tail, so only the bare body reaches `ForcedBreak`.
    BodyPattern {
        needle: "本文終わり",
        family: BodyFamily::BodyEnd,
    },
    BodyPattern {
        needle: "改行",
        family: BodyFamily::ForcedBreak,
    },
    // Geographic alignment.
    BodyPattern {
        needle: "地から",
        family: BodyFamily::AlignEndParamPrefix,
    },
    // 地より: the alternate wording of 地から (both "measured from the
    // bottom margin"); `地よりN字上げ` parses identically and canonicalises
    // to 地から on serialize. LeftmostLongest keeps ここから地より winning.
    BodyPattern {
        needle: "地より",
        family: BodyFamily::AlignEndParamPrefix,
    },
    BodyPattern {
        needle: "ここから地より",
        family: BodyFamily::AlignEndBlockParamPrefix,
    },
    // 文末より / 行末より: raised alignment measured from the end of the
    // text (vs 地から = bottom margin). Same zero-width AlignEnd hook.
    // `この行は行末より…` needs its own anchored needle.
    BodyPattern {
        needle: "文末より",
        family: BodyFamily::AlignEndParamPrefix,
    },
    BodyPattern {
        needle: "行末より",
        family: BodyFamily::AlignEndParamPrefix,
    },
    BodyPattern {
        needle: "この行は行末より",
        family: BodyFamily::AlignEndParamPrefix,
    },
    BodyPattern {
        needle: "地付き、地より",
        family: BodyFamily::AlignEndSpacingPrefix,
    },
    BodyPattern {
        needle: "ゴシック体、地付き",
        family: BodyFamily::GothicAlignEndPrefix,
    },
    BodyPattern {
        needle: "地付きで",
        family: BodyFamily::AlignEnd0,
    },
    BodyPattern {
        needle: "地付き",
        family: BodyFamily::AlignEnd0,
    },
    // 右寄せ / 地寄せ: wording variants of 地付き (inline-end alignment; in
    // horizontal render inline-end == the right edge). Canonicalize to 地付き.
    BodyPattern {
        needle: "右寄せ",
        family: BodyFamily::AlignEnd0,
    },
    BodyPattern {
        needle: "地寄せ",
        family: BodyFamily::AlignEnd0,
    },
    BodyPattern {
        needle: "ページの左右中央",
        family: BodyFamily::CenterMarker,
    },
    BodyPattern {
        needle: "中央揃え",
        family: BodyFamily::CenterMarker,
    },
    BodyPattern {
        needle: "この行はゴシック体",
        family: BodyFamily::LineGothic,
    },
    // Absolute font-size line directives (`大文字` … `特大文字、太字`). All four
    // size keywords anchor `LineFontSize`; `parse_line_font_size` re-reads the
    // body for the size and the optional `、太字` compound. `特大文字` is the
    // longest, so LeftmostLongest prefers it over `大文字`.
    BodyPattern {
        needle: "特大文字",
        family: BodyFamily::LineFontSize,
    },
    BodyPattern {
        needle: "大文字",
        family: BodyFamily::LineFontSize,
    },
    BodyPattern {
        needle: "中文字",
        family: BodyFamily::LineFontSize,
    },
    BodyPattern {
        needle: "小文字",
        family: BodyFamily::LineFontSize,
    },
    // Other inline / block. Needle is bare 挿絵 (not 挿絵（) so the numbered
    // form 挿絵{N}（…） also reaches classify_sashie_body, which re-validates.
    BodyPattern {
        needle: "挿絵",
        family: BodyFamily::SashiePrefix,
    },
    BodyPattern {
        needle: "罫囲み終わり",
        family: BodyFamily::KeigakomiClose,
    },
    BodyPattern {
        needle: "罫囲み",
        family: BodyFamily::KeigakomiOpen,
    },
    BodyPattern {
        needle: "割り注終わり",
        family: BodyFamily::WarichuClose,
    },
    BodyPattern {
        needle: "割り注",
        family: BodyFamily::WarichuOpen,
    },
    // 傍点 / 傍線 range form openers (`［＃傍点］ … ［＃傍点終わり］`). One
    // needle per emphasis variant `bouten_kind_from_suffix` recognises,
    // plus the `左に` left-side prefix. LeftmostLongest disambiguates
    // overlaps (`二重丸傍点` vs `丸傍点`, `白丸傍点` vs `丸傍点`); the close
    // form (`…終わり`) matches the same variant needle as a prefix and is
    // re-parsed in full by `parse_bouten_range_body`.
    BodyPattern {
        needle: "左に",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "白ゴマ傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "白丸傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "黒丸傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "二重丸傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "蛇の目傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "ばつ傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "白三角傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "黒三角傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "丸傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "傍点",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "二重傍線",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "鎖線",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "破線",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "波線",
        family: BodyFamily::BoutenRange,
    },
    BodyPattern {
        needle: "傍線",
        family: BodyFamily::BoutenRange,
    },
    // 太字 / 斜体 emphasis. The inline-range openers (`太字` / `斜体`)
    // also anchor their `…終わり` closers (re-parsed in full by
    // `parse_emphasis_body`); the block forms need their own anchors
    // (`ここから太字` beats the generic `ここから` via LeftmostLongest;
    // `ここで太字終わり` has no shorter generic anchor).
    BodyPattern {
        needle: "太字",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "斜体",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここから太字",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここから斜体",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここで太字終わり",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここで斜体終わり",
        family: BodyFamily::Emphasis,
    },
    // ゴシック体: a first-class gothic typeface, distinct from 太字:
    // the corpus uses ゴシック体 and 太字 in disjoint works and print sets a
    // gothic family apart from a bold weight, so the parser keeps its own
    // spelling and never folds it to 太字. The bare openers also anchor their
    // `…終わり` closers and the forward-reference `「X」はゴシック体` leaf.
    // ゴチック (1 corpus work) is *not* recognised: it declines to
    // Directive{Unknown} and a Tier1 lint suggests ゴシック体.
    BodyPattern {
        needle: "ゴシック体",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここからゴシック体",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここでゴシック体終わり",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここから教科書体",
        family: BodyFamily::Emphasis,
    },
    BodyPattern {
        needle: "ここで教科書体終わり",
        family: BodyFamily::Emphasis,
    },
    // Kunten okurigana opener (full-width left paren U+FF08).
    BodyPattern {
        needle: "（",
        family: BodyFamily::OkuriganaPrefix,
    },
    // Kunten compound marks (6): must precede the single forms in
    // the table only for documentation; LeftmostLongest does the
    // actual disambiguation (`一レ` 6 bytes > `一` 3 bytes).
    BodyPattern {
        needle: "一レ",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "上レ",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "下レ",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "中レ",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "二レ",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "三レ",
        family: BodyFamily::KaeritenCompound,
    },
    // Group + level combinations (上二 / 下二): the outer 上中下 mark paired
    // with an inner order number, attested in kanbun corpus text.
    BodyPattern {
        needle: "上二",
        family: BodyFamily::KaeritenCompound,
    },
    BodyPattern {
        needle: "下二",
        family: BodyFamily::KaeritenCompound,
    },
    // Kunten single marks (12).
    BodyPattern {
        needle: "一",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "丁",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "三",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "上",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "下",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "中",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "丙",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "乙",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "二",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "四",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "甲",
        family: BodyFamily::KaeritenSingle,
    },
    BodyPattern {
        needle: "レ",
        family: BodyFamily::KaeritenSingle,
    },
    // {N}字下げ: anchored on each digit (ASCII + full-width).
    BodyPattern {
        needle: "0",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "1",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "2",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "3",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "4",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "5",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "6",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "7",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "8",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "9",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "０",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "１",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "２",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "３",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "４",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "５",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "６",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "７",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "８",
        family: BodyFamily::IndentParamPrefix,
    },
    BodyPattern {
        needle: "９",
        family: BodyFamily::IndentParamPrefix,
    },
];

/// Build the annotation-body Aho-Corasick automaton from `BODY_PATTERNS`.
///
/// This DFA build is the bulk of parser boot cost (~150 microseconds, as
/// the `boot` bench measures). It is exposed under `#[doc(hidden)]` so
/// that bench can build it in isolation without making `BODY_PATTERNS`
/// public; the same pattern as `aozora-scan`'s hidden `NaiveScanner`
/// export. The process-lifetime cache lives in `body_dispatcher`;
/// `prewarm` warms it.
#[doc(hidden)]
#[must_use]
pub fn build_body_dispatcher() -> AhoCorasick {
    AhoCorasickBuilder::new()
        .match_kind(MatchKind::LeftmostLongest)
        .start_kind(StartKind::Anchored)
        .build(BODY_PATTERNS.iter().map(|p| p.needle))
        .expect("BODY_PATTERNS is a static, non-empty, valid set")
}

/// One-time DFA build, amortised across the entire process lifetime.
/// Lookup cost is a few ns per call so the build pays back in under a
/// thousand annotations.
fn body_dispatcher() -> &'static AhoCorasick {
    static DFA: OnceLock<AhoCorasick> = OnceLock::new();
    DFA.get_or_init(build_body_dispatcher)
}

/// Force the one-time Aho-Corasick DFA build now.
///
/// This is the bulk of parser boot cost. Idempotent: the `OnceLock` is
/// set at most once per process. `body_dispatcher` stays private; this
/// only triggers its init.
pub(crate) fn prewarm() {
    let _ = body_dispatcher();
}

/// Classify an input-editor note body into its [`DirectiveKind`], or
/// `None` if the body is not a recognised editorial note.
///
/// These are the corpus's two dominant editorial families:
/// - `ママ` / `「X」はママ` (and `ルビの「X」はママ`): *sic*: X is reproduced
///   as it stands in the source. `底本のまま` ("as in the base text") is the
///   same kept-irregularity note. → [`DirectiveKind::Sic`].
/// - `…底本では…` (`「X」は底本では「Y」`, `「X」は底本では脱落`, …): a
///   source-text divergence note. `…初出では…` ("in the first appearance …")
///   is the same shape against the first publication. →
///   [`DirectiveKind::BaseTextVariant`].
///
/// Called only at the tail of `RecogniseCtx::recognize_annotation`, after
/// every styling recogniser has declined, so a target-bearing form like
/// `「ママ」に傍点` has already been claimed as a Bouten and never reaches
/// here. The note does not restyle its target, so the caller leaves X in
/// the text and consumes only the bracket.
pub(super) fn editorial_note_kind(body: &str) -> Option<DirectiveKind> {
    if external_table_filename(body).is_some() {
        return Some(DirectiveKind::ExternalTableReference);
    }
    if body
        .strip_prefix("現代語訳「")
        .and_then(|text| text.strip_suffix('」'))
        .is_some_and(|text| {
            !text.is_empty() && !text.contains(['「', '」', '［', '］', '\r', '\n'])
        })
    {
        Some(DirectiveKind::ExplanationNote)
    } else if body == "ママ" || body.ends_with("はママ") || body == "底本のまま" {
        Some(DirectiveKind::Sic)
    } else if body.contains("底本では") || body.contains("初出では") {
        // Checked before EditorNote so a 底本 correction that happens to cite a
        // numbered note (`底本では…誤記。入力者注(6)`) stays a BaseTextVariant.
        Some(DirectiveKind::BaseTextVariant)
    } else if is_editor_note_body(body) {
        Some(DirectiveKind::EditorNote)
    } else if is_omission_note_body(body) {
        Some(DirectiveKind::OmissionNote)
    } else if body == "未完" {
        Some(DirectiveKind::IncompletenessNote)
    } else if matches!(
        body,
        "この作品は表題と副題のみで、本文はありません。"
            | "「雲隠れ」の帖は冒頭の晶子詞のみで本文はありません。"
            | "「Ｏ」は覆面の英字です。"
            | "劇場名"
            | "ホテル名"
            | "お手伝いさん"
            | "夫人"
            | "スカーフ"
            | "長男"
            | "三男"
            | "次男"
            | "小説家"
            | "長女"
            | "父"
            | "母"
            | "甥"
            | "次女"
    ) {
        Some(DirectiveKind::ExplanationNote)
    } else if body == "改行を挿入" {
        Some(DirectiveKind::TranscriptionNote)
    } else if is_ruby_attached_body(body) {
        Some(DirectiveKind::RubyAttached)
    } else if is_ruby_retarget_body(body) {
        Some(DirectiveKind::RubyRetarget)
    } else if body == "左にルビ付き" {
        Some(DirectiveKind::RubyPairOpen)
    } else if is_ruby_pair_close_body(body) {
        Some(DirectiveKind::RubyPairClose)
    } else if body == "注記付き" || body == "左に注記付き" {
        Some(DirectiveKind::MarginNotePairOpen)
    } else if is_margin_note_pair_close_body(body) {
        Some(DirectiveKind::MarginNotePairClose)
    } else {
        None
    }
}

fn is_omission_note_body(body: &str) -> bool {
    matches!(
        body,
        "「註」略"
            | "省略"
            | "図省略"
            | "図は省略"
            | "Ａ、Ｂ、Ｃの図省略"
            | "紙片の図、図省略"
            | "王家の紙幣の図、図省略"
            | "この後、改ページに続いて「VI.　文例」の章があるが、著作権の状態が不明なため、省略する。"
            | "目次のページ数および「解題（大内兵衛）」「追記」は省略しました"
    ) || body
        .strip_prefix('「')
        .and_then(|body| body.strip_suffix("」省略"))
        .is_some_and(|label| !label.is_empty() && !label.contains(['「', '」']))
        || body
            .strip_prefix("図が入るが省略。底本")
            .and_then(|body| body.strip_suffix("ページ"))
            .is_some_and(|page| !page.is_empty() && page.bytes().all(|b| b.is_ascii_digit()))
}

/// Whether `body` is exactly a ruby-presence note `「X」にルビ` (whole body, `X`
/// non-empty). A proofreading marker that the run `X` carries a ruby gloss.
fn is_ruby_attached_body(body: &str) -> bool {
    body.strip_prefix("「")
        .and_then(|r| r.strip_suffix("」にルビ"))
        .is_some_and(|x| !x.is_empty())
}

/// Whether `body` is exactly a ruby-binding note `ルビは「X」にかかる` (whole body,
/// `X` non-empty). Records that a nearby ruby applies to the run `X`.
fn is_ruby_retarget_body(body: &str) -> bool {
    body.strip_prefix("ルビは「")
        .and_then(|r| r.strip_suffix("」にかかる"))
        .is_some_and(|x| !x.is_empty())
}

/// Whether `body` is a left-side-ruby span closer `左に「Y」のルビ付き終わり`
/// (whole body, reading `Y` non-empty). The matching opener is the fixed
/// `左にルビ付き` body.
fn is_ruby_pair_close_body(body: &str) -> bool {
    body.strip_prefix("左に「")
        .and_then(|r| r.strip_suffix("」のルビ付き終わり"))
        .is_some_and(|y| !y.is_empty())
}

/// Whether `body` is a margin-note span closer `「Y」の注記付き終わり` or
/// `左に「Y」の注記付き終わり` (whole body, note text `Y` non-empty). The matching
/// opener is the fixed `注記付き` / `左に注記付き` body. `Y` may contain a nested
/// `［＃…］` gaiji, which the bracket pairer keeps inside the outer directive.
fn is_margin_note_pair_close_body(body: &str) -> bool {
    body.strip_prefix("左に「")
        .or_else(|| body.strip_prefix("「"))
        .and_then(|r| r.strip_suffix("」の注記付き終わり"))
        .is_some_and(|y| !y.is_empty())
}

/// Whether `body` is exactly a numbered input-typist note `入力者注(N)` with an
/// ASCII-paren, ASCII-digit index (the corpus form). A compound note that
/// merely *contains* the phrase is excluded (the whole body must match).
fn is_editor_note_body(body: &str) -> bool {
    let Some(rest) = body.strip_prefix("入力者注(") else {
        return false;
    };
    let Some(digits) = rest.strip_suffix(')') else {
        return false;
    };
    !digits.is_empty() && digits.bytes().all(|b| b.is_ascii_digit())
}

pub(super) fn is_return_mark(body: &str) -> bool {
    body_dispatcher()
        .find(Input::new(body).anchored(Anchored::Yes))
        .is_some_and(|matched| {
            matched.end() == body.len()
                && matches!(
                    BODY_PATTERNS[matched.pattern().as_usize()].family,
                    BodyFamily::KaeritenSingle | BodyFamily::KaeritenCompound
                )
        })
}

/// Single-pass classification of `body` (the trimmed bytes between
/// `［＃` and `］`) into an `EmitKind` for body-only annotation
/// families. Returns `None` if the body matches no body-only family;
/// the caller then falls through to forward classifiers and finally
/// the `Directive{Unknown}` catch-all.
#[allow(clippy::too_many_lines, reason = "Single match arm per BodyFamily.")]
pub(super) fn classify_annotation_body(
    source: &AnnotationBody<'_>,
    alloc: &mut Allocator,
) -> Option<(EmitKind, Option<Directive>)> {
    let body = source.text;
    #[cfg(feature = "classify-instrument")]
    let _classify_guard = SubsystemGuard::new(Subsystem::BodyDispatcher);
    if body.is_empty() {
        return None;
    }
    if let Some(placement) = relative_placement(body, alloc) {
        return Some((
            EmitKind::BlockOpen(RegionFormat::RelativePlacement(placement)),
            None,
        ));
    }
    // Paired / block headings route through the container machinery as
    // `ContainerKind::Heading`. Tried before the body dispatcher: their
    // keywords overlap the `ここから…` / `…終わり` shapes but always carry a
    // `見出し` keyword, so a non-heading `ここから…` body falls through.
    if let Some(emit) = parse_heading_directive(body) {
        return Some((emit, None));
    }
    let dfa = body_dispatcher();
    let mat = dfa.find(Input::new(body).anchored(Anchored::Yes))?;
    let pat = BODY_PATTERNS[mat.pattern().as_usize()];
    let match_end = mat.end();
    let exact = match_end == body.len();
    // An exact-match family must consume the whole body; a prefix-only
    // DFA hit (`罫囲みfoo` matches the needle `罫囲み`) makes no claim.
    // Checking the mode once here lets every exact arm below drop its
    // `if exact` guard and replaces the parallel catch-all `None`.
    if body_family_mode(pat.family) == MatchMode::Exact && !exact {
        return None;
    }
    match pat.family {
        // ----- Exact-match families (must consume the entire body) -----
        BodyFamily::PageBreak => Some((EmitKind::Aozora(alloc.page_break()), None)),
        BodyFamily::BodyEnd => Some((EmitKind::Aozora(alloc.body_end()), None)),
        BodyFamily::ForcedBreak => Some((EmitKind::Aozora(alloc.forced_break()), None)),
        BodyFamily::SectionKaicho => Some((
            EmitKind::Aozora(alloc.section_break(SectionKind::Kaicho)),
            None,
        )),
        BodyFamily::SectionKaidan => Some((
            EmitKind::Aozora(alloc.section_break(SectionKind::Kaidan)),
            None,
        )),
        BodyFamily::SectionKaimihiraki => Some((
            EmitKind::Aozora(alloc.section_break(SectionKind::Kaimihiraki)),
            None,
        )),
        BodyFamily::AlignEnd0 => Some((
            EmitKind::Aozora(alloc.line(LineFormat::AlignEnd {
                offset: 0,
                gothic: false,
            })),
            None,
        )),
        BodyFamily::CenterMarker => {
            // ページの左右中央 (page centre) vs 中央揃え: a single-line
            // zero-width centring marker.
            let page = body == "ページの左右中央";
            Some((
                EmitKind::Aozora(alloc.line(LineFormat::Center { page })),
                None,
            ))
        }
        BodyFamily::LineGothic => Some((EmitKind::Aozora(alloc.line(LineFormat::Gothic)), None)),
        BodyFamily::LineFontSize => parse_line_font_size(body).map(|(size, bold)| {
            (
                EmitKind::Aozora(alloc.line(LineFormat::FontSizeAbsolute { size, bold })),
                None,
            )
        }),
        BodyFamily::KeigakomiOpen => Some((
            EmitKind::BlockOpen(RegionFormat::Framed(EnclosureKind::Rule)),
            None,
        )),
        BodyFamily::KeigakomiClose => Some((
            EmitKind::BlockClose(RegionClose::Framed(EnclosureKind::Rule)),
            None,
        )),
        BodyFamily::WarichuBlockOpen => Some((EmitKind::BlockOpen(RegionFormat::Warichu), None)),
        BodyFamily::WarichuBlockEnd => Some((EmitKind::BlockClose(RegionClose::Warichu), None)),
        BodyFamily::IndentBlock1 => Some((
            EmitKind::BlockOpen(RegionFormat::Indent(IndentBlock {
                purpose: None,
                partial: None,
                column_count: None,
                amount: 1,
                wrap: None,
                page_horizontal_center: false,
                align: None,
                end_offset: None,
                layout: IndentLayout::None,
                styles: BlockStyles::EMPTY,
            })),
            None,
        )),
        // Page positioning ends at a supplied page break, not an indent close.
        BodyFamily::PageCenterBlockOpen => Some((
            EmitKind::Aozora(alloc.line(LineFormat::Center { page: true })),
            None,
        )),
        BodyFamily::AlignEndBlock0 => Some((
            EmitKind::BlockOpen(RegionFormat::AlignEnd { offset: 0 }),
            None,
        )),
        BodyFamily::IndentBlockEnd => {
            // Formula has an independent endpoint, beyond matching the indentation family.
            let tail = &body[match_end..];
            let purpose =
                (tail == "、ここで数式終わり").then_some(ab_aozora_syntax::BlockPurpose::Formula);
            (tail.is_empty() || (tail.starts_with('、') && tail.ends_with("終わり"))).then_some((
                EmitKind::BlockClose(RegionClose::Indent {
                    purpose,
                    amount: None,
                    kumi_width: None,
                    styles: BlockStyles::EMPTY,
                }),
                None,
            ))
        }
        BodyFamily::AlignEndBlockEnd => Some((EmitKind::BlockClose(RegionClose::AlignEnd), None)),
        BodyFamily::LineWidthBlockEnd => {
            // The close marker carries no width; the open-side payload is
            // authoritative when pairing (mirrors the generic 字下げ終わり).
            Some((EmitKind::BlockClose(RegionClose::LineWidth), None))
        }
        BodyFamily::IndentCompoundBlockEnd => {
            let rest = &body[match_end..];
            let mut styles = BlockStyles::EMPTY;
            if rest.strip_suffix("終わり").is_some_and(|clauses| {
                clauses
                    .split('、')
                    .all(|clause| resolve_block_style(clause, &mut styles).is_some())
            }) {
                return Some((
                    EmitKind::BlockClose(RegionClose::Indent {
                        purpose: None,
                        amount: None,
                        kumi_width: None,
                        styles,
                    }),
                    None,
                ));
            }
            // ここで字下げ、{W}字組み終わり: the 字組み compound closer.
            // The close carries its own `W` so the marker round-trips byte-exact
            // (it pairs with the Indent open by family). Tolerate an optional
            // leading `{L}行`. Declines (→ Unknown) on any other shape.
            let rest = &body[match_end..];
            let rest = rest.split_once('行').map_or(rest, |(_lines, after)| after);
            let (width, tail) = parse_decimal_u8_prefix(rest)?;
            (tail == "字組み終わり")
                .then(|| NonZeroU8::new(width))
                .flatten()
                .map(|w| {
                    (
                        EmitKind::BlockClose(RegionClose::Indent {
                            purpose: None,
                            amount: None,
                            kumi_width: Some(LineWidth(w)),
                            styles: BlockStyles::EMPTY,
                        }),
                        None,
                    )
                })
        }
        BodyFamily::FormulaBlockOpen => Some((EmitKind::BlockOpen(RegionFormat::Formula), None)),
        BodyFamily::FormulaBlockEnd => Some((EmitKind::BlockClose(RegionClose::Formula), None)),
        BodyFamily::TableBlockOpen => Some((EmitKind::BlockOpen(RegionFormat::Table), None)),
        BodyFamily::TableBlockEnd => Some((EmitKind::BlockClose(RegionClose::Table), None)),
        BodyFamily::HorizontalBlockOpen => Some((
            EmitKind::BlockOpen(RegionFormat::Horizontal(HorizontalPresentation {
                align: if body.ends_with("右揃えで") {
                    Some(ab_aozora_syntax::LineAlignment::Right)
                } else if body.ends_with("中央揃えで") {
                    Some(ab_aozora_syntax::LineAlignment::Center)
                } else {
                    None
                },
            })),
            None,
        )),
        BodyFamily::HorizontalBlockEnd => {
            Some((EmitKind::BlockClose(RegionClose::Horizontal), None))
        }
        BodyFamily::FontSizeBlockEnd => {
            font_size_block_close(body).map(|close| (EmitKind::BlockClose(close), None))
        }
        BodyFamily::ParamBlockEnd => {
            if let Some(close) = font_size_block_close(body) {
                return Some((EmitKind::BlockClose(close), None));
            }
            let rest = &body[match_end..];
            if let Some((amount, tail)) = parse_layout_count_prefix(rest)
                && tail == "字下げ終わり"
            {
                return Some((
                    EmitKind::BlockClose(RegionClose::Indent {
                        purpose: None,
                        amount: Some(amount),
                        kumi_width: None,
                        styles: BlockStyles::EMPTY,
                    }),
                    None,
                ));
            }
            let (count, rest) = match parse_decimal_u8_prefix(rest) {
                Some((n, tail)) => (Some(ColumnCount(NonZeroU8::new(n)?)), tail),
                None => (None, rest),
            };
            if matches!(rest, "段組、罫囲み終わり" | "段組み、罫囲み終わり") {
                let directive =
                    alloc.make_directive(&format!("［＃{body}］"), DirectiveKind::Unknown);
                return Some((
                    EmitKind::BlockCloses {
                        targets: [
                            RegionClose::Columns(count),
                            RegionClose::Framed(EnclosureKind::Rule),
                        ],
                        fallback: alloc.annotation(directive),
                    },
                    None,
                ));
            }
            (rest == "段組終わり" || rest == "段組み終わり")
                .then_some((EmitKind::BlockClose(RegionClose::Columns(count)), None))
        }
        BodyFamily::WarichuOpen => {
            let p = alloc.make_directive("［＃割り注］", DirectiveKind::WarichuOpen);
            let node = alloc.annotation(p);
            // Re-build a payload for the segment-wrap case. The
            // Allocator interns by string content, so the second
            // call hits the dedup table; it pays at most a single
            // `Box<str>` clone, which is cheap relative
            // to the rare nested-Warichu shape this case targets.
            let p2 = alloc.make_directive("［＃割り注］", DirectiveKind::WarichuOpen);
            Some((EmitKind::Aozora(node), Some(p2)))
        }
        BodyFamily::WarichuClose => {
            let p = alloc.make_directive("［＃割り注終わり］", DirectiveKind::WarichuClose);
            let node = alloc.annotation(p);
            let p2 = alloc.make_directive("［＃割り注終わり］", DirectiveKind::WarichuClose);
            Some((EmitKind::Aozora(node), Some(p2)))
        }
        BodyFamily::KaeritenSingle | BodyFamily::KaeritenCompound => Some((
            EmitKind::Aozora(alloc.kunten(KuntenKind::ReturnMark, body)),
            None,
        )),

        // ----- Prefix-with-parameter families -----
        BodyFamily::AlignEndParamPrefix => {
            // body == 地から/文末より/行末より{N}字上げ; remainder = body[match_end..].
            // The verb is the intransitive 字上がり as well as 字上げ, with an
            // optional 揃え suffix (`文末よりN字上げ揃え`).
            let rest = &body[match_end..];
            let (n, tail) = parse_layout_count_prefix(rest)?;
            (matches!(tail, "字上げ" | "字上がり" | "字上げ揃え" | "字上がり揃え") && n >= 1).then(
                || {
                    (
                        EmitKind::Aozora(alloc.line(LineFormat::AlignEnd {
                            offset: n,
                            gothic: false,
                        })),
                        None,
                    )
                },
            )
        }
        BodyFamily::AlignEndSpacingPrefix => {
            let offset = parse_end_spacing(&body[match_end..])?;
            Some((
                EmitKind::Aozora(alloc.line(LineFormat::AlignEnd {
                    offset,
                    gothic: false,
                })),
                None,
            ))
        }
        BodyFamily::GothicAlignEndPrefix => {
            let rest = &body[match_end..];
            let offset = if rest.is_empty() {
                0
            } else {
                let (offset, tail) = parse_layout_count_prefix(rest.strip_prefix("、地より")?)?;
                if offset == 0 || !matches!(tail, "字あげ" | "字上げ") {
                    return None;
                }
                offset
            };
            Some((
                EmitKind::Aozora(alloc.line(LineFormat::AlignEnd {
                    offset,
                    gothic: true,
                })),
                None,
            ))
        }
        BodyFamily::TopIndentPrefix => {
            // body == 天から/天より{N}字下げ[、地より{M}字…]: single-line indent
            // from the top margin. The plain form is identical to a plain
            // {N}字下げ (Indent leaf); the both-margin compound also lifts M
            // chars off the foot edge.
            let rest = &body[match_end..];
            let (n, tail) = parse_decimal_u8_prefix(rest)?;
            if tail == "字下げ" && n >= 1 {
                Some((
                    EmitKind::Aozora(alloc.line(LineFormat::Indent {
                        amount: n,
                        end_offset: None,
                    })),
                    None,
                ))
            } else {
                parse_both_margin_tail(n, tail).map(|lf| (EmitKind::Aozora(alloc.line(lf)), None))
            }
        }
        BodyFamily::KaigyouTentsukiPrefix => {
            // body == 改行天付き、折り返して{N}字下げ: the ここから-less bare
            // top-flush hanging indent (amount 0 + wrap N), closed by the
            // shared 字下げ終わり.
            let rest = &body[match_end..];
            let after = rest.strip_prefix("、折り返して")?;
            let (m, tail) = parse_decimal_u8_prefix(after)?;
            (tail == "字下げ").then_some((
                EmitKind::BlockOpen(RegionFormat::Indent(IndentBlock {
                    purpose: None,
                    partial: None,
                    column_count: None,
                    amount: 0,
                    wrap: Some(m),
                    page_horizontal_center: false,
                    align: None,
                    end_offset: None,
                    layout: IndentLayout::None,
                    styles: BlockStyles::EMPTY,
                })),
                None,
            ))
        }
        BodyFamily::SashiePrefix => classify_sashie_body(source, alloc).map(|e| (e, None)),
        BodyFamily::IndentBlockParamPrefix => {
            // body == ここから{N}字下げ; remainder = body[match_end..]
            let rest = &body[match_end..];
            let rest = rest.strip_prefix("改行").unwrap_or(rest);
            // ここから[改行]天付き、折り返して{M}字下げ: top-flush hanging
            // indent: the first line sits at the top margin (天付き = no
            // indent), wrapped continuation lines indent M. Models as the same
            // Indent container with amount 0 + wrap M, so it closes with the
            // shared 字下げ終わり (pairing is by family). Both the `改行天付き`
            // (corpus's most common top form) and the bare `天付き` spellings
            // appear; accept either before the leading-digit parse below.
            if let Some(after) = rest
                .strip_prefix("天付き、折り返して")
                .or_else(|| rest.strip_prefix("天付き折り返して"))
            {
                let (m, tail2) = parse_layout_count_prefix(after)?;
                return (tail2 == "字下げ").then_some((
                    EmitKind::BlockOpen(RegionFormat::Indent(IndentBlock {
                        purpose: None,
                        partial: None,
                        column_count: None,
                        amount: 0,
                        wrap: Some(m),
                        page_horizontal_center: false,
                        align: None,
                        end_offset: None,
                        layout: IndentLayout::None,
                        styles: BlockStyles::EMPTY,
                    })),
                    None,
                ));
            }
            let (n, tail) = parse_layout_count_prefix(rest)?;
            let tail = tail.trim_start_matches([' ', '　']);
            if tail == "字下げ" {
                Some((
                    EmitKind::BlockOpen(RegionFormat::Indent(IndentBlock {
                        purpose: None,
                        partial: None,
                        column_count: None,
                        amount: n,
                        wrap: None,
                        page_horizontal_center: false,
                        align: None,
                        end_offset: None,
                        layout: IndentLayout::None,
                        styles: BlockStyles::EMPTY,
                    })),
                    None,
                ))
            } else if let Some(after) = tail.strip_prefix("字下げ、").or_else(|| {
                tail.strip_prefix("字下げ")
                    .filter(|rest| rest.starts_with("折り返して"))
            }) {
                // Independent clauses share the supplied indentation scope;
                // unresolved clauses retain their source rather than erasing it.
                parse_indent_compound(n, after, source, alloc)
                    .map(|block| (EmitKind::BlockOpen(RegionFormat::Indent(block)), None))
            } else if tail == "字詰め" {
                // ここから{N}字詰め: line-width container (字詰め): N
                // full-width characters per line. Shares the `ここから`
                // opener prefix with 字下げ; block-only, closes with
                // `ここで字詰め終わり`. `NonZero` folds the `N >= 1` guard.
                NonZeroU8::new(n).map(|w| {
                    (
                        EmitKind::BlockOpen(RegionFormat::LineWidth(LineWidth(w))),
                        None,
                    )
                })
            } else if let Some(after) = tail
                .strip_prefix("段組み")
                .or_else(|| tail.strip_prefix("段組"))
            {
                let count = ColumnCount(NonZeroU8::new(n)?);
                let block = parse_column_compound(count, after, source, alloc)?;
                Some((EmitKind::BlockOpen(RegionFormat::Columns(block)), None))
            } else {
                // ここから{N}段階大きな/小さな文字: block font-size shift.
                // Shares the `ここから` prefix; closers supply direction and may
                // restate the relative step count.
                font_size_block_open_steps(tail, n)
                    .and_then(NonZeroI8::new)
                    .map(|s| {
                        (
                            EmitKind::BlockOpen(RegionFormat::FontSize(FontShift(s))),
                            None,
                        )
                    })
            }
        }
        BodyFamily::AlignEndBlockParamPrefix => {
            // body == ここから地から{N}字上げ; remainder = body[match_end..]
            let rest = &body[match_end..];
            let (n, tail) = parse_decimal_u8_prefix(rest)?;
            (tail == "字上げ").then_some((
                EmitKind::BlockOpen(RegionFormat::AlignEnd { offset: n }),
                None,
            ))
        }
        BodyFamily::OkuriganaPrefix => {
            // The DFA matched `（` at body[0..3]. Defer to the same
            // parens-recognising helper as the legacy code so the
            // length / character-class invariants stay in one place.
            is_okurigana_body(body).then(|| {
                (
                    EmitKind::Aozora(alloc.kunten(KuntenKind::Okurigana, &body[3..body.len() - 3])),
                    None,
                )
            })
        }
        BodyFamily::IndentParamPrefix => {
            // The DFA matched a single digit. Re-parse from body[0]
            // for full multi-digit support.
            let (n, tail) = parse_decimal_u8_prefix(body)?;
            if tail == "字下げ" && n >= 1 {
                Some((
                    EmitKind::Aozora(alloc.line(LineFormat::Indent {
                        amount: n,
                        end_offset: None,
                    })),
                    None,
                ))
            } else if let Some(lf) = parse_both_margin_tail(n, tail) {
                // Both-margin compound: ［＃{N}字下げ[て][、]地より{M}字(あき|上げ)[で|て]］
                // A head indent plus a foot-edge lift on one single line.
                Some((EmitKind::Aozora(alloc.line(lf)), None))
            } else {
                // Bare-range font-size open: ［＃{N}段階大きな/小さな文字］:
                // the ここから-less sibling of the block opener, closed by the
                // bare ［＃大きな/小さな文字終わり］. Reuses the FontSize
                // region so render / pairing / serialize already apply.
                font_size_block_open_steps(tail, n)
                    .and_then(NonZeroI8::new)
                    .map(|s| {
                        (
                            EmitKind::BlockOpen(RegionFormat::FontSize(FontShift(s))),
                            None,
                        )
                    })
            }
        }
        BodyFamily::BoutenRange => {
            // `傍点` / `白丸傍点` / `二重傍線` / `左に傍線` … with an optional
            // `終わり` close suffix. Re-parse the full body for the variant,
            // the `左に` position, and open vs close.
            let (kind, position, is_close) = parse_bouten_range_body(body)?;
            Some((
                open_or_close(RegionFormat::Bouten { kind, position }, is_close),
                None,
            ))
        }
        BodyFamily::Emphasis => {
            // `太字` / `斜体` / `ここから太字` / `ここで斜体終わり` …:
            // re-parse the full body for the kind, the block vs inline
            // form, and open vs close.
            let (region, is_close) = parse_emphasis_body(body)?;
            Some((open_or_close(region, is_close), None))
        }
        BodyFamily::SmallScriptRange => {
            // `行右小書き` / `行左小書き` with an optional `終わり` close.
            // Re-parse the full body so `行右小書きほげ` (needle prefix but
            // longer body) declines to Directive{Unknown}.
            let (side, is_close) = parse_small_script_range_body(body)?;
            Some((
                open_or_close(RegionFormat::SmallScript(side), is_close),
                None,
            ))
        }
        BodyFamily::CombineUprightOpen => {
            Some((EmitKind::BlockOpen(RegionFormat::CombineUpright), None))
        }
        BodyFamily::CombineUprightClose => {
            Some((EmitKind::BlockClose(RegionClose::CombineUpright), None))
        }
        BodyFamily::BanknoteTranslationOpen => {
            Some((EmitKind::BlockOpen(RegionFormat::BanknoteTranslation), None))
        }
        BodyFamily::TranslationClose => {
            Some((EmitKind::BlockClose(RegionClose::BanknoteTranslation), None))
        }
        BodyFamily::CaptionRange => {
            // `キャプション` (inline) / `ここからキャプション` (block) with an
            // optional `終わり` close; re-parse the full body.
            let (scope, is_close) = parse_caption_body(body)?;
            Some((open_or_close(RegionFormat::Caption(scope), is_close), None))
        }
    }
}

/// Wrap an open [`RegionFormat`] as the matching [`EmitKind`]: the open marker
/// carries the full payload; the close projects to its [`RegionClose`]
/// discriminant via [`RegionClose::of`] (the open side stays authoritative).
fn open_or_close(region: RegionFormat, is_close: bool) -> EmitKind {
    if is_close {
        EmitKind::BlockClose(RegionClose::of(region))
    } else {
        EmitKind::BlockOpen(region)
    }
}

/// Whether `body` is the okurigana shape `（X）` where X is a short
/// run of Japanese characters.
///
/// The length bound guards against accidentally claiming long
/// parenthesised glosses (which belong to the generic annotation
/// catch-all). 6 characters is the ~99th-percentile okurigana length
/// in Aozora corpora; anything longer is practically always editorial
/// prose rather than an inflection marker.
fn is_okurigana_body(body: &str) -> bool {
    let Some(inner) = body.strip_prefix('（').and_then(|s| s.strip_suffix('）')) else {
        return false;
    };
    // Byte-length prefilter: every accepted okurigana char is a CJK
    // glyph in {hiragana, katakana, half-width katakana, CJK unified}.
    // Hiragana/katakana/CJK are 3 bytes UTF-8; half-width katakana
    // is also 3 bytes (U+FF61..U+FF9F). So a 1..=6 char inner has
    // byte length in `3..=18`. Any inner outside that range cannot
    // satisfy `is_okurigana_char.all` and we skip the char decode.
    if !(3..=18).contains(&inner.len()) {
        return false;
    }
    // Single-pass fusion of `chars().count()` + `chars().all()`:
    // count and class-check in one walk, with early-out at >6 chars
    // or first non-conforming char. Replaces two iterations over
    // the same byte stream.
    let mut count = 0usize;
    for c in inner.chars() {
        count += 1;
        if count > 6 || !is_okurigana_char(c) {
            return false;
        }
    }
    count >= 1
}

/// Character class accepted inside okurigana parens: hiragana,
/// katakana (incl. half-width), CJK unified ideographs. Narrower
/// than "any non-whitespace" so editorial `（注）` or
/// punctuation-rich glosses fall through to the annotation path.
const fn is_okurigana_char(ch: char) -> bool {
    matches!(
        ch,
        '\u{3041}'..='\u{309F}'      // hiragana
        | '\u{30A0}'..='\u{30FF}'    // katakana
        | '\u{FF66}'..='\u{FF9F}'    // half-width katakana
        | '\u{4E00}'..='\u{9FFF}'    // CJK unified
        | '\u{3400}'..='\u{4DBF}'    // CJK ext A
        | '\u{F900}'..='\u{FAFF}'    // CJK compat
    )
}

pub(super) fn illustration_file_spec(spec: &str) -> Option<(&str, Option<&str>)> {
    let (file, dimensions) = spec
        .split_once('、')
        .map_or((spec, None), |(file, dimensions)| (file, Some(dimensions)));
    (!file.is_empty()).then_some((file, dimensions))
}

/// Classify a `［＃挿絵（file）入る］` sashie (illustration insert),
/// optionally bundling a caption: `［＃挿絵（file）「caption」入る］`.
///
/// Called from [`classify_annotation_body`]'s `SashiePrefix` arm:
/// the AC has already verified the `挿絵（` prefix at body[0..9]; this
/// function captures the filename between `（` and `）`, an optional
/// `「caption」` (per <https://www.aozora.gr.jp/annotation/graphics.html>),
/// and confirms the trailing `入る` keyword. Quoted caption text stays
/// image metadata; visible captions are separate source content.
fn relative_placement(
    body: &str,
    alloc: &mut Allocator,
) -> Option<ab_aozora_syntax::RelativePlacement> {
    if body == "横組みの下に、左右中央縦組みで" {
        return Some(ab_aozora_syntax::RelativePlacement::BelowHorizontal { anchor: None });
    }
    let body = body.strip_prefix('「')?;
    let (reference, rest) = body.split_once("」の文字の下から")?;
    if reference.is_empty() {
        return None;
    }
    let (offset, rest) = parse_decimal_u8_prefix(rest)?;
    (rest == "字下げ、横組み右揃えで").then(|| alloc.relative_text_placement(reference, offset))
}

fn classify_sashie_body(source: &AnnotationBody<'_>, alloc: &mut Allocator) -> Option<EmitKind> {
    let body = source.text;
    // `挿絵（file）入る` and the numbered `挿絵{N}（file）入る` (N a run of
    // half/full-width digits before the `（`). A description *before* 挿絵
    // (`女性と犬の挿絵（…）`, `「…」のキャプション付きの挿絵（…）`) is a separate,
    // unhandled form; it does not start with 挿絵, so the needle misses it.
    let after_kw = body.strip_prefix("挿絵")?;
    let paren = after_kw.find('（')?;
    let number = if paren == 0 {
        None
    } else {
        let num = &after_kw[..paren];
        if num
            .chars()
            .all(|c| c.is_ascii_digit() || ('０'..='９').contains(&c))
        {
            Some(num)
        } else {
            return None;
        }
    };
    let rest = &after_kw[paren + '（'.len_utf8()..];
    // `）` is a full-width right parenthesis (U+FF09). Find its first
    // occurrence; corpus rarely nests `（）` inside a filename.
    let close_off = rest.find('）')?;
    // The `（…）` body is either a bare `file` or `file、横W×縦H`: split off
    // the optional pixel-size note so `file` stays a clean `<img src>` path
    // and the dimensions render as `width`/`height` (see render_node).
    let inside = &rest[..close_off];
    let (file, dimensions) = illustration_file_spec(inside)?;
    let tail = &rest[close_off + '）'.len_utf8()..];
    // After `）` the tail is either the bare `入る` keyword or a bundled
    // `「caption」入る`. Any other shape declines (→ `Directive{Unknown}`).
    let caption = if tail == "入る" {
        None
    } else if let Some(inner) = tail
        .strip_prefix('「')
        .and_then(|t| t.strip_suffix("」入る"))
    {
        if inner.is_empty() {
            return None;
        }
        Some(alloc.content_plain(inner))
    } else {
        return None;
    };
    let result = alloc.sashie(file, number, dimensions, caption);
    if let Node::Illustration(id) = result
        && caption.is_some()
    {
        let image = alloc.illustration_mut(id);
        let start = source.start
            + u32::try_from(tail.as_ptr().addr() - body.as_ptr().addr() + '「'.len_utf8())
                .expect("source offset fits u32");
        let end = source.start
            + u32::try_from(body.len() - "」入る".len()).expect("source offset fits u32");
        image.caption_span = Some(Span::new(start, end));
    }
    Some(EmitKind::Aozora(result))
}

/// Classify the *general* image form `［＃<説明>（file［、横W×縦H］）入る］`
/// (図 / 地図 / 口絵 / 表紙 / コンドル博士の図 / 神代文字ア …) per
/// <https://www.aozora.gr.jp/annotation/graphics.html>: the leading text
/// before `（` is the image's alt-description (the guide lists 図 / 地図 /
/// 絵 / 挿絵 / 表 / 写真 as type words but the description is free text),
/// the parenthesised part is `file` (+ optional `、横W×縦H` pixel size),
/// and `入る` closes it.
///
/// The keyword `挿絵` form is claimed earlier by [`classify_sashie_body`]
/// via its anchored needle; this is the fallback for every other
/// description, tried just before the `Directive{Unknown}` catch-all (it
/// has no prefix needle because the description is arbitrary). Returns
/// `None` unless the insertion is complete; an explicitly attributed statement
/// about image lettering may follow it as documentary apparatus.
pub(super) fn classify_general_image_body(
    body: &str,
    source_start: u32,
    alloc: &mut Allocator,
) -> Option<EmitKind> {
    let mixed = image_edition_note(body);
    let original = body;
    let body = mixed.map_or(body, |(image, _)| image);
    let middle = body.strip_suffix("入る")?;
    // The file spec `（file、横W×縦H）` is always the LAST paren group before
    // `入る`; use `rfind` so a description that itself embeds `（…）` (e.g.
    // `…（1798）…の図（fig.png、…）入る`) splits at the file paren, not the
    // first inner one. For a single-paren body `rfind == find`, so every
    // already-recognized body is byte-identical.
    let paren = middle.rfind('（')?;
    let description = &middle[..paren];
    if description.is_empty() {
        return None;
    }
    let rest = &middle[paren + '（'.len_utf8()..];
    let close_off = rest.find('）')?;
    // Once `入る` is stripped, `）` must be the final byte; a trailing
    // `「caption」` or any other shape is not this form and declines.
    if close_off + '）'.len_utf8() != rest.len() {
        return None;
    }
    let inside = &rest[..close_off];
    let (file, dimensions) = illustration_file_spec(inside)?;
    let annotation_body = mixed.and_then(|_| match alloc.content_plain(original) {
        Content::Plain(id) => Some(id),
        _ => None,
    });
    let result = alloc.sashie_general(file, description, dimensions);
    if let Node::Illustration(id) = result {
        let image = alloc.illustration_mut(id);
        image.annotation_body = annotation_body;
        image.description_span = Some(Span::new(
            source_start,
            source_start + u32::try_from(description.len()).expect("source offset fits u32"),
        ));
    }
    Some(EmitKind::Aozora(result))
}

/// Parse a heading keyword into `(style, kind)`. An optional `同行`
/// (same-line) / `窓` (window) prefix selects the style; the remaining
/// `大 / 中 / 小見出し` selects the level. Shared by the forward-reference
/// hint (`「X」はSTYLEレベル見出し`) and the paired / block container forms
/// ([`parse_heading_directive`]).
///
/// `副見出し` is not a real annotation (it never occurs in the corpus), so
/// it matches nothing and the directive falls through to `Directive{Unknown}`.
/// The 同行 / 窓 styles cross with every level (`同行中見出し`, `窓小見出し`, …).
pub(super) fn parse_heading_keyword(s: &str) -> Option<(HeadingStyle, HeadingKind)> {
    let (style, rest) = strip_heading_style(s);
    let kind = match rest {
        "大見出し" => HeadingKind::Large,
        "中見出し" => HeadingKind::Medium,
        "小見出し" => HeadingKind::Small,
        _ => return None,
    };
    Some((style, kind))
}

/// Strip an optional 同行 / 窓 style prefix, returning the style and the
/// remaining `大/中/小見出(し)` stem. Shared by the strict open/hint parser
/// and the 送り仮名-tolerant close parser.
fn strip_heading_style(s: &str) -> (HeadingStyle, &str) {
    [
        ("同行", HeadingStyle::SameLine),
        ("窓", HeadingStyle::Window),
    ]
    .into_iter()
    .find_map(|(prefix, style)| s.strip_prefix(prefix).map(|rest| (style, rest)))
    .unwrap_or((HeadingStyle::Standard, s))
}

/// Closing headings accept the source spelling with or without the final し.
/// Openers remain explicit about heading level and presentation.
fn parse_heading_close_level(s: &str) -> Option<(HeadingStyle, Option<HeadingKind>)> {
    let (style, rest) = strip_heading_style(s);
    let level = match rest {
        "大見出し" | "大見出" => Some(HeadingKind::Large),
        "中見出し" | "中見出" => Some(HeadingKind::Medium),
        "小見出し" | "小見出" => Some(HeadingKind::Small),
        // Bare close: no level. Style-less only; `窓見出し` etc. never occur, and
        // a level-less serialize drops style, so keep those Unknown (lossless).
        "見出し" if matches!(style, HeadingStyle::Standard) => None,
        _ => return None,
    };
    Some((style, level))
}

/// Recognise a **paired** (`STYLEレベル見出し` / `…見出し終わり`) or **block**
/// (`ここからSTYLEレベル見出し` / `ここでSTYLEレベル見出し終わり`) heading
/// directive body, returning the [`EmitKind`] to emit. These delimit their
/// content and route through the container pairing machinery as
/// [`RegionFormat::Heading`] (the counterpart of the `は`-form leaf heading).
///
/// A close carries its own level via [`RegionClose::Heading`] so the bare,
/// level-less `ここで見出し終わり` close can round-trip as `level: None` rather
/// than backfilling a lossy level from the open.
///
/// The forward-reference `「X」は…見出し` hint starts with `「`, so it never
/// matches here; a `ここから…` / `…終わり` body that is not a heading keyword
/// (e.g. `ここから2字下げ`) fails `parse_heading_keyword` and falls through to
/// the body dispatcher.
fn parse_heading_directive(body: &str) -> Option<EmitKind> {
    if let Some(rest) = body.strip_prefix("ここから") {
        let (style, level) = parse_heading_keyword(rest)?;
        return Some(EmitKind::BlockOpen(RegionFormat::Heading {
            level,
            style,
            padded: true,
        }));
    }
    if let Some(rest) = body.strip_prefix("ここで") {
        let (style, level) = parse_heading_close_level(rest.strip_suffix("終わり")?)?;
        return Some(EmitKind::BlockClose(RegionClose::Heading {
            level,
            style,
            padded: true,
        }));
    }
    if let Some(inner) = body.strip_suffix("終わり") {
        let (style, level) = parse_heading_close_level(inner)?;
        // The paired (`…終わり`, no `ここで`) close REQUIRES an explicit level: a
        // bare `見出し終わり` has neither the `ここで` block-scope signal nor a
        // level word, so it is too ambiguous to claim (0 corpus occ) and stays
        // Unknown. Only the `ここで` block close (below) admits the level-less form.
        let level = Some(level?);
        return Some(EmitKind::BlockClose(RegionClose::Heading {
            level,
            style,
            padded: false,
        }));
    }
    let (style, level) = parse_heading_keyword(body)?;
    Some(EmitKind::BlockOpen(RegionFormat::Heading {
        level,
        style,
        padded: false,
    }))
}

/// Signed stage count for a `ここから{N}段階大きな/小さな文字` block opener,
/// where `tail` is the body after the `ここから{N}` prefix and `magnitude`
/// is `N`. `大きな` → `+N`, `小さな` → `-N`; `None` for a zero/overflowing
/// magnitude or any other tail.
fn font_size_block_close(body: &str) -> Option<RegionClose> {
    let rest = body.strip_prefix("ここで").unwrap_or(body);
    let (magnitude, rest) = if let Some((count, tail)) = parse_decimal_u8_prefix(rest) {
        if count > 127 {
            return None;
        }
        (Some(NonZeroU8::new(count)?), tail.strip_prefix("段階")?)
    } else {
        (None, rest)
    };
    let larger = match rest {
        "大きな文字終わり" => true,
        "小さな文字終わり" => false,
        _ => return None,
    };
    Some(RegionClose::FontSize { larger, magnitude })
}

fn font_size_block_open_steps(tail: &str, magnitude: u8) -> Option<i8> {
    let steps = i8::try_from(magnitude).ok()?;
    if steps == 0 {
        return None;
    }
    match tail {
        "段階大きな文字" => Some(steps),
        "段階小さな文字" => Some(-steps),
        _ => None,
    }
}

/// One independently supplied axis; disagreement never selects a value by order.
#[derive(Clone, Copy)]
enum ClauseAxis<T> {
    Absent,
    Value(T, Span),
    Conflict(Span),
}

impl<T: Copy + PartialEq> ClauseAxis<T> {
    fn observe(&mut self, value: T, span: Span) -> Option<Span> {
        match *self {
            Self::Absent => {
                *self = Self::Value(value, span);
                None
            }
            Self::Value(previous, _) if previous == value => None,
            Self::Value(_, previous) | Self::Conflict(previous) => {
                let conflict =
                    Span::new(previous.start.min(span.start), previous.end.max(span.end));
                *self = Self::Conflict(conflict);
                Some(conflict)
            }
        }
    }

    fn value(self) -> Option<T> {
        match self {
            Self::Value(value, _) => Some(value),
            Self::Absent | Self::Conflict(_) => None,
        }
    }
}

/// Keep independent axes while retaining unsupported or contradictory clauses.
fn parse_indent_compound(
    amount: u8,
    after: &str,
    source: &AnnotationBody<'_>,
    alloc: &mut Allocator,
) -> Option<IndentBlock> {
    let initial = IndentBlock {
        purpose: None,
        partial: None,
        column_count: None,
        amount,
        wrap: None,
        page_horizontal_center: false,
        align: None,
        end_offset: None,
        layout: IndentLayout::None,
        styles: BlockStyles::EMPTY,
    };
    let mut block = initial;
    let mut wrap = ClauseAxis::Absent;
    let mut align = ClauseAxis::Absent;
    let mut horizontal_align = ClauseAxis::Absent;
    let mut end_offset = ClauseAxis::Absent;
    let mut layout = ClauseAxis::Absent;
    let mut font = ClauseAxis::Absent;
    let mut frame = ClauseAxis::Absent;
    let mut columns = ClauseAxis::Absent;
    let mut purpose = ClauseAxis::Absent;
    let mut clauses = Vec::new();
    for (segment, span) in source.clauses(after) {
        let mut candidate = initial;
        let problem = if resolve_indent_segment(segment, &mut candidate).is_none() {
            // A second primary indentation value makes the container's own scope uncertain.
            if parse_decimal_u8_prefix(segment).is_some_and(|(_, tail)| tail == "字下げ") {
                return None;
            }
            Some(span)
        } else {
            let mut problem = None;
            let conflicts = [
                candidate
                    .purpose
                    .and_then(|value| purpose.observe(value, span)),
                candidate.wrap.and_then(|value| wrap.observe(value, span)),
                candidate.align.and_then(|value| align.observe(value, span)),
                candidate
                    .styles
                    .horizontal
                    .and_then(|value| value.align)
                    .and_then(|value| horizontal_align.observe(value, span)),
                candidate
                    .end_offset
                    .and_then(|value| end_offset.observe(value, span)),
                (!matches!(candidate.layout, IndentLayout::None))
                    .then(|| layout.observe(candidate.layout, span))
                    .flatten(),
                candidate
                    .styles
                    .font
                    .and_then(|value| font.observe(value, span)),
                candidate
                    .styles
                    .frame
                    .and_then(|value| frame.observe(value, span)),
                candidate
                    .column_count
                    .and_then(|value| columns.observe(value, span)),
            ];
            for conflict in conflicts.into_iter().flatten() {
                problem = Some(problem.map_or(conflict, |previous: Span| {
                    Span::new(
                        previous.start.min(conflict.start),
                        previous.end.max(conflict.end),
                    )
                }));
            }
            block.styles.gothic |= candidate.styles.gothic;
            block.styles.bold |= candidate.styles.bold;
            block.page_horizontal_center |= candidate.page_horizontal_center;
            block.styles.horizontal =
                merge_horizontal(block.styles.horizontal, candidate.styles.horizontal);
            problem
        };
        if let Some(problem) = problem {
            clauses.push(problem);
        }
    }
    block.wrap = wrap.value();
    block.align = align.value();
    if let Some(horizontal) = &mut block.styles.horizontal {
        horizontal.align = horizontal_align.value();
    }
    block.end_offset = end_offset.value();
    block.layout = layout.value().unwrap_or(IndentLayout::None);
    block.styles.font = font.value();
    block.styles.frame = frame.value();
    block.column_count = columns.value();
    block.purpose = purpose.value();
    if !clauses.is_empty() {
        block.partial = Some(alloc.partial_layout(source.text, clauses));
    }
    Some(block)
}

fn parse_column_compound(
    count: ColumnCount,
    after: &str,
    source: &AnnotationBody<'_>,
    alloc: &mut Allocator,
) -> Option<ColumnBlock> {
    let mut block = ColumnBlock {
        column_rule: false,
        count,
        styles: BlockStyles::EMPTY,
        partial: None,
    };
    if after.is_empty() {
        return Some(block);
    }
    let after = after.strip_prefix('、')?;
    let mut clauses = Vec::new();
    let mut font = ClauseAxis::Absent;
    let mut frame = ClauseAxis::Absent;
    for (segment, span) in source.clauses(after) {
        if segment == "段間に罫" {
            block.column_rule = true;
            continue;
        }
        let mut candidate = BlockStyles::EMPTY;
        if resolve_block_style(segment, &mut candidate).is_none() {
            clauses.push(span);
        } else {
            block.styles.gothic |= candidate.gothic;
            block.styles.bold |= candidate.bold;
            block.styles.horizontal =
                merge_horizontal(block.styles.horizontal, candidate.horizontal);
            if let Some(conflict) = candidate.frame.and_then(|value| frame.observe(value, span)) {
                clauses.push(conflict);
            }
            if let Some(conflict) = candidate.font.and_then(|value| font.observe(value, span)) {
                clauses.push(conflict);
            }
        }
    }
    block.styles.font = font.value();
    block.styles.frame = frame.value();
    if !clauses.is_empty() {
        block.partial = Some(alloc.partial_layout(source.text, clauses));
    }
    Some(block)
}

/// Interpret one independent `字下げ、` clause into a fresh candidate payload.
fn resolve_indent_segment(segment: &str, block: &mut IndentBlock) -> Option<()> {
    if segment == "ここから数式" {
        block.purpose = Some(ab_aozora_syntax::BlockPurpose::Formula);
        return Some(());
    }
    let column_clause = segment.strip_prefix("ここから").unwrap_or(segment);
    if let Some((count, "段組" | "段組み")) = parse_layout_count_prefix(column_clause) {
        block.column_count = Some(ColumnCount(NonZeroU8::new(count)?));
        return Some(());
    }
    // 折り返して{M}字下げ: hanging-indent continuation width.
    if let Some(rest) = segment.strip_prefix("折り返して") {
        let (m, tail) = parse_layout_count_prefix(rest)?;
        if tail != "字下げ" || block.wrap.is_some() {
            return None;
        }
        block.wrap = Some(m);
        return Some(());
    }
    // Physical page placement does not change text alignment within a line.
    if matches!(
        segment,
        "ページの左右中央" | "ページの左右中央に" | "ページ左右中央" | "左右中央"
    ) {
        block.page_horizontal_center = true;
        return Some(());
    }
    if matches!(segment, "横組み右揃えで" | "横組み中央揃えで") {
        block.styles.horizontal = Some(HorizontalPresentation {
            align: Some(if segment == "横組み右揃えで" {
                ab_aozora_syntax::LineAlignment::Right
            } else {
                ab_aozora_syntax::LineAlignment::Center
            }),
        });
        return Some(());
    }
    if matches!(segment, "中央揃え" | "右揃え") {
        block.align = Some(if segment == "中央揃え" {
            ab_aozora_syntax::LineAlignment::Center
        } else {
            ab_aozora_syntax::LineAlignment::Right
        });
        return Some(());
    }
    if let Some(rest) = segment
        .strip_prefix("地より")
        .or_else(|| segment.strip_prefix("地から"))
    {
        let (offset, tail) = parse_layout_count_prefix(rest)?;
        if tail != "字上げ" {
            return None;
        }
        block.end_offset = Some(offset);
        return Some(());
    }
    // {W}字詰め / {L}行{W}字組み[で]: secondary line layout.
    if let Some(layout) = parse_indent_line_layout(segment) {
        if !matches!(block.layout, IndentLayout::None) {
            return None;
        }
        block.layout = layout;
        return Some(());
    }
    resolve_block_style(segment, &mut block.styles)
}

fn merge_horizontal(
    existing: Option<HorizontalPresentation>,
    supplied: Option<HorizontalPresentation>,
) -> Option<HorizontalPresentation> {
    supplied
        .map(|presentation| HorizontalPresentation {
            align: presentation
                .align
                .or_else(|| existing.and_then(|value| value.align)),
        })
        .or(existing)
}

fn resolve_block_style(segment: &str, styles: &mut BlockStyles) -> Option<()> {
    match segment {
        "ゴシック体" if !styles.gothic => styles.gothic = true,
        "太字" if !styles.bold => styles.bold = true,
        "横書き" | "横組み" | "横組みで" | "文章は横組み" if styles.horizontal.is_none() =>
        {
            styles.horizontal = Some(HorizontalPresentation { align: None });
        }
        "罫囲み" | "罫囲みで" if styles.frame.is_none() => {
            styles.frame = Some(EnclosureKind::Rule);
        }
        "枠囲み" if styles.frame.is_none() => {
            styles.frame = Some(EnclosureKind::Unspecified);
        }
        "破線枠囲み" if styles.frame.is_none() => {
            styles.frame = Some(EnclosureKind::DashedRule);
        }
        "小さい活字" if styles.font.is_none() => {
            styles.font = Some(QualitativeFontSize::Smaller);
        }
        "字のポイントはやや小さくしてある。" if styles.font.is_none() => {
            styles.font = Some(QualitativeFontSize::SlightlySmaller);
        }
        "本文よりひとまわり大きい" if styles.font.is_none() => {
            styles.font = Some(QualitativeFontSize::HitomawariLarger);
        }
        "本文よりひとまわり大きい太ゴシック体" if styles.font.is_none() => {
            styles.font = Some(QualitativeFontSize::HitomawariLarger);
            styles.gothic = true;
            styles.bold = true;
        }
        _ => return None,
    }
    Some(())
}

/// Parse the both-margin compound tail of a head-indent directive into a
/// single-line [`LineFormat::Indent`] carrying both margins.
///
/// `amount` is the head indent already parsed from the digits before `字下げ`;
/// `tail` is the remainder starting at that `字下げ` verb. This one recogniser
/// covers the whole single-line family
/// (`［＃{N}字下げ[て][、]地より{M}字(あき|上げ)[で|て]］`, and the
/// `天より`-headed variant via [`BodyFamily::TopIndentPrefix`]): the `字あき`
/// spelling is normalised to `字上げ` (both lift `M` full-width chars off the
/// foot edge), the `、` join is optional (a bare join also appears), and the
/// trailing `で` / `て` connective is optional. Returns `None` for a tail that
/// is not a well-formed both-margin compound, so the caller falls through to
/// the `Directive{Unknown}` catch-all (lossless). The region opener
/// `［＃ここから…、地から…字下げ］` and the count-less `［＃下げて、…］` never reach
/// here: the former routes through `IndentBlockParamPrefix`, the latter has no
/// anchored needle.
fn parse_both_margin_tail(amount: u8, tail: &str) -> Option<LineFormat> {
    // Head verb: 字下げ, with an optional て connective (字下げて).
    let rest = tail.strip_prefix("字下げ")?;
    let rest = rest.strip_prefix('て').unwrap_or(rest);
    // Optional 、 between the head and bottom clauses (a bare join also appears).
    let rest = rest.strip_prefix('、').unwrap_or(rest);
    // Bottom clause: 地より{M}字(あき|上げ)[で|て].
    let rest = rest.strip_prefix("地より")?;
    let (offset, rest) = parse_decimal_u8_prefix(rest)?;
    let rest = rest
        .strip_prefix("字あき")
        .or_else(|| rest.strip_prefix("字上げ"))?;
    // Optional trailing で / て connective; nothing else may follow.
    let rest = rest
        .strip_prefix('で')
        .or_else(|| rest.strip_prefix('て'))
        .unwrap_or(rest);
    (amount >= 1 && offset >= 1 && rest.is_empty()).then_some(LineFormat::Indent {
        amount,
        end_offset: Some(offset),
    })
}

fn parse_indent_line_layout(after: &str) -> Option<IndentLayout> {
    let (lead, rest) = parse_decimal_u8_prefix(after)?;
    let lead = NonZeroU8::new(lead)?; // folds the `lead >= 1` guard
    if rest == "字詰め" {
        return Some(IndentLayout::LineWidth(LineWidth(lead)));
    }
    // `{L}行{W}字組み[で]`: the leading number is the line count.
    let after_lines = rest.strip_prefix('行')?;
    let (width, tail) = parse_decimal_u8_prefix(after_lines)?;
    let width = NonZeroU8::new(width)?; // folds the `width >= 1` guard
    if lead.get() == 1 && tail == "字" {
        return Some(IndentLayout::LineWidth(LineWidth(width)));
    }
    if matches!(tail, "字組み" | "字組みで") {
        return Some(IndentLayout::Kumi(Kumi { lines: lead, width }));
    }
    None
}

/// Map the trailing keyword (after `に`) to a [`BoutenKind`].
///
/// The reverse of [`BoutenKind::keyword`], derived by walking the single
/// [`BOUTEN_KINDS`] source rather than a hand-maintained second table:
/// so a mark can never be recognised in the forward direction
/// (`keyword`) yet silently missed here. `×傍点` is accepted as an input
/// alias for ばつ傍点; 黒丸傍点 is the equivalent explicit-color spelling of
/// 丸傍点. Canonical mark-prefix keywords
/// (`白丸傍点`, …) are recognised; the non-canonical `傍点（白丸）` /
/// `傍点◎` marker-suffix spellings decline to `Directive{Unknown}`,
/// served by a Tier1 lint suggesting the canonical keyword. Unknown suffixes
/// return `None`, letting the annotation fall through to the
/// `Directive{Unknown}` catch-all. Lookup is a short linear scan (14 entries,
/// dominated by the leading-byte mismatch on the first compare).
pub(super) fn bouten_kind_from_suffix(s: &str) -> Option<BoutenKind> {
    match s {
        "×傍点" => return Some(BoutenKind::Cross),
        "黒丸傍点" => return Some(BoutenKind::Circle),
        _ => {}
    }
    BOUTEN_KINDS.iter().copied().find(|k| k.keyword() == s)
}

/// Parse a 傍点/傍線 range-form body into `(kind, position, is_close)`.
/// Strips an optional `左に` left-side prefix and an optional `終わり`
/// close suffix; the remainder must be a [`bouten_kind_from_suffix`]
/// keyword (all fourteen kinds, incl. the rare 鎖線 / 破線 / 黒三角傍点).
/// Returns `None` (→ `Directive{Unknown}`) for any non-bouten body.
fn parse_bouten_range_body(body: &str) -> Option<(BoutenKind, BoutenPosition, bool)> {
    let (position, rest) = body
        .strip_prefix("左に")
        .map_or((BoutenPosition::Right, body), |r| (BoutenPosition::Left, r));
    let (is_close, kind_str) = rest
        .strip_suffix("終わり")
        .map_or((false, rest), |k| (true, k));
    let kind = bouten_kind_from_suffix(kind_str)?;
    Some((kind, position, is_close))
}

/// Parse a 小書き range body into `(side, is_close)`. `行右小書き` →
/// `BoutenPosition::Right`, `行左小書き` → `Left`; an optional `終わり`
/// suffix marks the close. Returns `None` (→ `Directive{Unknown}`) for any
/// other body, so a needle-prefix-but-longer body like `行右小書きほげ`
/// declines cleanly.
fn parse_small_script_range_body(body: &str) -> Option<(BoutenPosition, bool)> {
    let (is_close, core) = body
        .strip_suffix("終わり")
        .map_or((false, body), |c| (true, c));
    let side = match core {
        "行右小書き" => BoutenPosition::Right,
        "行左小書き" => BoutenPosition::Left,
        _ => return None,
    };
    Some((side, is_close))
}

/// Parse complete caption and below-figure explanation boundary spellings.
fn parse_caption_body(body: &str) -> Option<(CaptionScope, bool)> {
    Some(match body {
        "キャプション" => (CaptionScope::Inline, false),
        "キャプション終わり" => (CaptionScope::Inline, true),
        "ここからキャプション" => (CaptionScope::Block, false),
        "ここでキャプション終わり" => (CaptionScope::Block, true),
        "ここから図表下部解説文" => (CaptionScope::FigureExplanationBelow, false),
        "ここで図表下部解説文終わり" => (CaptionScope::FigureExplanationBelow, true),
        _ => return None,
    })
}

/// Parse an absolute font-size line body into `(size, bold)`. The body is a
/// size keyword (`特大文字` / `大文字` / `中文字` / `小文字`) optionally followed
/// by the `、太字` compound. Any other shape (a trailing run, an unknown
/// compound) declines to `Directive{Unknown}`; keeping `大文字下げ` and the like
/// out.
///
/// Only the `、太字` spelling is recognised: `bold` is a flag, not a spelling, so
/// serialization is canonical `、太字`; recognising the rarer `、ゴシック体`
/// (1 corpus occurrence) would lose its spelling and break the verbatim
/// round-trip; it stays `Directive{Unknown}` instead (mirrors §6.12 `この行は
/// ゴシック体` recognising a single spelling).
fn parse_line_font_size(body: &str) -> Option<(AbsoluteSize, bool)> {
    let (size, rest) = if let Some(r) = body.strip_prefix("特大文字") {
        (AbsoluteSize::ExtraLarge, r)
    } else if let Some(r) = body.strip_prefix("大文字") {
        (AbsoluteSize::Large, r)
    } else if let Some(r) = body.strip_prefix("中文字") {
        (AbsoluteSize::Medium, r)
    } else if let Some(r) = body.strip_prefix("小文字") {
        (AbsoluteSize::Small, r)
    } else {
        return None;
    };
    let bold = match rest {
        "" => false,
        "、太字" => true,
        _ => return None,
    };
    Some((size, bold))
}

/// Parse supplied emphasis and typeface scope spellings directly to their
/// authoritative opening attribute and whether this marker closes it.
pub(super) fn parse_emphasis_body(body: &str) -> Option<(RegionFormat, bool)> {
    use RegionFormat::{Bold, Gothic, Italic, Textbook};
    Some(match body {
        "太字" => (Bold { padded: false }, false),
        "太字終わり" => (Bold { padded: false }, true),
        "ここから太字" => (Bold { padded: true }, false),
        "ここで太字終わり" => (Bold { padded: true }, true),
        "ゴシック体" => (Gothic { padded: false }, false),
        "ゴシック体終わり" => (Gothic { padded: false }, true),
        "ここからゴシック体" => (Gothic { padded: true }, false),
        "ここでゴシック体終わり" => (Gothic { padded: true }, true),
        "斜体" => (Italic { padded: false }, false),
        "斜体終わり" => (Italic { padded: false }, true),
        "ここから斜体" => (Italic { padded: true }, false),
        "ここで斜体終わり" => (Italic { padded: true }, true),
        "ここから教科書体" => (Textbook, false),
        "ここで教科書体終わり" => (Textbook, true),
        _ => return None,
    })
}

pub(super) fn parse_end_spacing(source: &str) -> Option<u8> {
    let (offset, tail) = parse_layout_count_prefix(source)?;
    (offset > 0 && matches!(tail, "字アキ" | "字あき")).then_some(offset)
}

fn parse_layout_count_prefix(source: &str) -> Option<(u8, &str)> {
    if let Some(parsed) = parse_decimal_u8_prefix(source) {
        return Some(parsed);
    }
    let mut chars = source.chars();
    let digit = match chars.next()? {
        '一' => 1,
        '二' => 2,
        '三' => 3,
        '四' => 4,
        '五' => 5,
        '六' => 6,
        '七' => 7,
        '八' => 8,
        '九' => 9,
        _ => return None,
    };
    Some((digit, chars.as_str()))
}

/// Parse a leading run of ASCII / full-width decimal digits into a
/// [`u8`] and return the remainder slice.
///
/// Returns `None` if the leading char is not a digit, or if the value
/// overflows `u8` (> 255). `saturating_mul` / `saturating_add` during
/// accumulation keep the `u32` intermediate bounded, but the final
/// `try_from` enforces the `u8` range; a body like `300字下げ` fails
/// cleanly rather than wrapping to 44.
pub(super) fn parse_decimal_u8_prefix(s: &str) -> Option<(u8, &str)> {
    let mut value: u32 = 0;
    let mut consumed = 0;
    for (idx, ch) in s.char_indices() {
        let digit = match ch {
            '0'..='9' => Some(u32::from(ch) - u32::from('0')),
            '０'..='９' => Some(u32::from(ch) - u32::from('０')),
            _ => None,
        };
        let Some(d) = digit else { break };
        value = value.saturating_mul(10).saturating_add(d);
        consumed = idx + ch.len_utf8();
    }
    if consumed == 0 {
        return None;
    }
    let value_u8 = u8::try_from(value).ok()?;
    Some((value_u8, &s[consumed..]))
}

#[cfg(test)]
mod both_margin_tests {
    use super::*;

    /// Every both-margin spelling attested in the corpus resolves to the same
    /// `Indent { amount, end_offset: Some }` leaf. `tail` is the remainder after
    /// the head digits (the `字下げ…` verb onward); `天より`/`天から` heads reach
    /// the helper with the same tail after the prefix + digits are stripped.
    #[test]
    fn corpus_both_margin_spellings_all_resolve() {
        for (amount, tail, want) in [
            // ［＃２１字下げ、地より２字あきで］: 字あき normalises to 字上げ.
            (21, "字下げ、地より2字あきで", (21u8, 2u8)),
            // ［＃天より３１字下げ、地より２字上げで］ (post-prefix tail).
            (31, "字下げ、地より2字上げで", (31, 2)),
            // ［＃２８字下げて、地より３字上げて］: て head + て tail.
            (28, "字下げて、地より3字上げて", (28, 3)),
            // ［＃２０字下げて、地より１字あきで］: て head + 字あき + で.
            (20, "字下げて、地より1字あきで", (20, 1)),
            // ［＃天より３２字下げて地より３字上げで］: bare join (no 、).
            (32, "字下げて地より3字上げで", (32, 3)),
        ] {
            assert_eq!(
                parse_both_margin_tail(amount, tail),
                Some(LineFormat::Indent {
                    amount: want.0,
                    end_offset: Some(want.1),
                }),
                "both-margin tail {tail:?} must resolve",
            );
        }
    }

    /// Forms that do not match both-margin syntax decline (fall through to the
    /// `Directive{Unknown}` catch-all): a plain head indent with no bottom
    /// clause, and a bottom clause missing its explicit count.
    #[test]
    fn non_both_margin_tails_decline() {
        // Plain head indent, no 地より clause: the caller's plain arm owns this.
        assert_eq!(parse_both_margin_tail(2, "字下げ"), None);
        // Bottom clause without an explicit count declines.
        assert_eq!(parse_both_margin_tail(2, "字下げ、地より字上げで"), None);
        // A trailing remnant after the bottom clause declines (lossless).
        assert_eq!(parse_both_margin_tail(2, "字下げ、地より2字上げでX"), None);
    }
}
