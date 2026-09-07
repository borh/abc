//! Source-stated witness alternatives, editorial notes, and concealment.

use std::{num::NonZeroU32, ops::Range};

/// Content addressed by a quoted base-text note.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextVariantTarget {
    /// The reading attached to a base character sequence.
    RubyReading,
    /// Principal text.
    Text,
}

/// Edition described by a retained editorial statement.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EditionNoteKind {
    /// The edition used as the source of the transcription.
    BaseEdition,
    /// The work's first publication, without asserting it is the base edition.
    FirstPublication,
}

fn split_attribution(body: &str, edition: EditionNoteKind) -> Option<(&str, &str)> {
    let delimiters = match edition {
        EditionNoteKind::BaseEdition => ["」は底本では", "」は、底本では"],
        EditionNoteKind::FirstPublication => ["」は初出では", "」は、初出では"],
    };
    if delimiters
        .iter()
        .map(|delimiter| body.matches(delimiter).count())
        .sum::<usize>()
        != 1
    {
        return None;
    }
    let (current, witness) = delimiters
        .iter()
        .find_map(|delimiter| body.split_once(delimiter))?;
    Some((
        current,
        witness.strip_prefix(['、', ' ']).unwrap_or(witness),
    ))
}

/// A supplied reading or text and its explicitly stated base-text alternative.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TextVariant<'s> {
    /// Which content sequence the note addresses.
    pub target: TextVariantTarget,
    /// The spelling supplied in the Aozora text.
    pub current: &'s str,
    /// The alternative attributed to the base text; empty only for explicit absence.
    pub base_text: &'s str,
    /// UTF-8 byte range of the quoted current fragment within the annotation.
    pub current_span: Range<usize>,
    /// UTF-8 byte range of the witness fragment; absent for stated omission.
    pub base_span: Option<Range<usize>>,
    /// Complete editorial statement when the note makes an additional assertion.
    pub editorial_statement: Option<&'s str>,
}

/// Recognize an exact quoted base-text note, including its annotation delimiters.
/// Other editorial prose remains unclassified by this recognizer.
#[must_use]
pub fn text_variant(source: &str) -> Option<TextVariant<'_>> {
    let statement = source.strip_prefix("［＃")?.strip_suffix('］')?;
    let (target, body) = if let Some(body) = statement.strip_prefix("ルビの「") {
        (TextVariantTarget::RubyReading, body)
    } else {
        (TextVariantTarget::Text, statement.strip_prefix('「')?)
    };
    let (current, witness) = split_attribution(body, EditionNoteKind::BaseEdition)?;
    let witness_start = source.len() - '］'.len_utf8() - witness.len();
    let current_start = source.len() - '］'.len_utf8() - body.len();
    variant_parts(
        statement,
        target,
        current,
        current_start,
        witness,
        witness_start,
    )
}

/// Separate an edition clause from a shared-target formatting suffix.
/// The caller must interpret the returned formatting suffix in its own closed vocabulary.
#[must_use]
pub fn formatted_text_variant(source: &str) -> Option<(&str, TextVariant<'_>)> {
    let statement = source.strip_prefix("［＃")?.strip_suffix('］')?;
    let (formatted, suffix) = statement.strip_prefix('「')?.split_once("」は")?;
    if formatted.is_empty() || formatted.contains(['「', '」', '\n']) {
        return None;
    }
    let (formatting, current, current_start, witness) =
        if let Some((formatting, witness)) = suffix.split_once("、底本では") {
            (formatting, formatted, "［＃「".len(), witness)
        } else {
            let (formatting, assertion) = suffix.split_once("、「")?;
            let (current, witness) = assertion.split_once("」が底本では")?;
            if current.contains(['「', '」', '\n']) {
                return None;
            }
            let start = source.len() - '］'.len_utf8() - assertion.len();
            (formatting, current, start, witness)
        };
    let witness_start = source.len() - '］'.len_utf8() - witness.len();
    Some((
        formatting,
        variant_parts(
            statement,
            TextVariantTarget::Text,
            current,
            current_start,
            witness,
            witness_start,
        )?,
    ))
}

fn variant_parts<'s>(
    statement: &'s str,
    target: TextVariantTarget,
    current: &'s str,
    current_start: usize,
    witness: &'s str,
    witness_start: usize,
) -> Option<TextVariant<'s>> {
    let asserted = ["と誤記", "と誤植", "となっている", "と欠字"]
        .iter()
        .find_map(|suffix| {
            witness
                .strip_suffix(suffix)
                .filter(|quoted| quoted.ends_with('」'))
        });
    let editorial_statement = asserted.map(|_| statement);
    let witness = asserted.unwrap_or(witness);
    let base_text = match witness {
        "欠落" | "なし" | "無し" | "脱落" | "欠如" | "欠" | "脱字" => "",
        quoted => {
            let text = quoted.strip_prefix('「')?.strip_suffix('」')?;
            if text.is_empty() {
                return None;
            }
            text
        }
    };
    if current.is_empty() || current.contains('\n') || base_text.contains('\n') {
        return None;
    }
    let base_span = (!base_text.is_empty()).then(|| {
        let start = witness_start + '「'.len_utf8();
        start..start + base_text.len()
    });
    let (target, current, base_text, current_start, base_span) = if target
        == TextVariantTarget::Text
        && let (Some(current), Some(base_text), Some(span)) = (
            current
                .strip_prefix('《')
                .and_then(|text| text.strip_suffix('》')),
            base_text
                .strip_prefix('《')
                .and_then(|text| text.strip_suffix('》')),
            base_span.as_ref(),
        )
        && !current.is_empty()
        && !base_text.is_empty()
    {
        (
            TextVariantTarget::RubyReading,
            current,
            base_text,
            current_start + '《'.len_utf8(),
            Some(span.start + '《'.len_utf8()..span.end - '》'.len_utf8()),
        )
    } else {
        (target, current, base_text, current_start, base_span)
    };
    Some(TextVariant {
        target,
        current,
        base_text,
        current_span: current_start..current_start + current.len(),
        base_span,
        editorial_statement,
    })
}

/// The supplied placeholder explicitly identified as concealed text, without a quantity.
#[must_use]
pub fn concealed_placeholder(source: &str) -> Option<&str> {
    let target = source
        .strip_prefix("［＃「")?
        .strip_suffix("」は伏せ字］")?;
    (!target.is_empty() && !target.contains(['「', '」', '［', '］', '\n', '\r'])).then_some(target)
}

/// Number of concealed characters explicitly attributed to the base edition.
/// Placeholder identity and quantity agreement are established at the target.
#[must_use]
pub fn base_edition_concealed_characters(source: &str) -> Option<NonZeroU32> {
    let digits = source.strip_prefix("［＃底本")?.strip_suffix("字伏字］")?;
    let count = digits.chars().try_fold(0u32, |count, ch| {
        let digit = match ch {
            '0'..='9' => u32::from(ch) - u32::from('0'),
            '０'..='９' => u32::from(ch) - u32::from('０'),
            _ => return None,
        };
        count.checked_mul(10)?.checked_add(digit)
    })?;
    NonZeroU32::new(count)
}

/// Preserve edition statements as apparatus without applying their descriptions
/// to the principal text. Mixed transcription instructions and malformed quoted
/// alternatives remain outside this grammar.
#[must_use]
pub fn edition_note(source: &str) -> Option<(EditionNoteKind, &str)> {
    edition_statement(source.strip_prefix("［＃")?.strip_suffix('］')?)
}

/// Interpret one source-owned edition statement, including a clause within a layout marker.
#[must_use]
pub fn edition_statement(body: &str) -> Option<(EditionNoteKind, &str)> {
    if body
        .strip_prefix("底本では")
        .is_some_and(|text| !text.is_empty())
    {
        return Some((EditionNoteKind::BaseEdition, body));
    }
    if let Some(quoted) = body
        .strip_prefix("ルビの「")
        .or_else(|| body.strip_prefix('「'))
    {
        if let Some((current, witness)) =
            split_attribution(quoted, EditionNoteKind::FirstPublication)
            && !current.is_empty()
            && !current.contains(['「', '」', '\n'])
            && witness.starts_with('「')
            && witness.ends_with('」')
        {
            return Some((EditionNoteKind::FirstPublication, body));
        }
        if let Some((current, witness)) = split_attribution(quoted, EditionNoteKind::BaseEdition)
            && !current.is_empty()
            && witness.starts_with('「')
            && let Some((base, bibliography)) = witness.split_once("」。以下の本では「")
            && base.len() > '「'.len_utf8()
            && bibliography.contains('『')
            && bibliography.contains('』')
        {
            return Some((EditionNoteKind::BaseEdition, body));
        }
        if let Some((current, description)) =
            split_attribution(quoted, EditionNoteKind::BaseEdition)
            && !current.is_empty()
            && !current.contains(['「', '」', '\n'])
            && !description.is_empty()
            && !description.starts_with(['「', '※'])
        {
            return Some((EditionNoteKind::BaseEdition, body));
        }
    }
    if let Some((subject, description)) = body
        .split_once("は底本では")
        .or_else(|| body.split_once("は、底本では"))
        && let Some((qualifier, quoted)) = subject.split_once('「')
        && !qualifier.is_empty()
        && !qualifier.starts_with("ここから")
        && !qualifier.starts_with("ここまで")
        && !qualifier.contains(['、', '」', '［', '］', '\n'])
        && (qualifier != "ルビの" || quoted.matches('「').count() > 0)
        && quoted.contains('」')
        && !description.is_empty()
        && !description.contains("は底本では")
    {
        return Some((EditionNoteKind::BaseEdition, body));
    }
    if let Some((subject, description)) = body.split_once("は底本では")
        && !subject.is_empty()
        && !subject.contains(['、', '「', '」', '［', '］', '\n'])
        && !description.is_empty()
    {
        return Some((EditionNoteKind::BaseEdition, body));
    }
    None
}

#[cfg(test)]
mod tests {
    use super::{TextVariantTarget, base_edition_concealed_characters, text_variant};

    #[test]
    fn concealment_quantity_retains_only_explicit_base_edition_character_counts() {
        for (source, quantity) in [
            ("［＃底本１字伏字］", 1),
            ("［＃底本２字伏字］", 2),
            ("［＃底本４字伏字］", 4),
            ("［＃底本12字伏字］", 12),
        ] {
            assert_eq!(
                base_edition_concealed_characters(source).unwrap().get(),
                quantity
            );
        }
        for source in [
            "［＃底本０字伏字］",
            "［＃底本字伏字］",
            "［＃底本2行伏字］",
            "［＃初出２字伏字］",
            "［＃底本２字伏字、復元］",
            "［＃底本4294967296字伏字］",
        ] {
            assert!(
                base_edition_concealed_characters(source).is_none(),
                "{source}"
            );
        }
    }

    #[test]
    fn quoted_alternatives_preserve_the_attribution_without_asserting_error() {
        let reading = text_variant("［＃ルビの「ざる」は底本では「さる」］").unwrap();
        assert_eq!(reading.target, TextVariantTarget::RubyReading);
        assert_eq!((reading.current, reading.base_text), ("ざる", "さる"));
        let text = text_variant("［＃「甍の」は底本では「薨の」］").unwrap();
        assert_eq!(text.target, TextVariantTarget::Text);
        assert_eq!((text.current, text.base_text), ("甍の", "薨の"));
    }

    #[test]
    fn quotation_characters_are_target_data_and_keep_exact_fragment_extents() {
        for source in [
            "［＃「……』」は底本では「……」」］",
            "［＃「』」は、底本では「」」］",
            "［＃「』」は底本では、「」」］",
        ] {
            let variant = text_variant(source).unwrap();
            assert_eq!(&source[variant.current_span.clone()], variant.current);
            assert_eq!(
                &source[variant.base_span.clone().unwrap()],
                variant.base_text
            );
            assert!(variant.base_text.ends_with('」'));
        }
    }

    #[test]
    fn ambiguous_or_incomplete_notes_remain_unclassified() {
        for source in [
            "［＃ルビの「ざる」は底本では「さる」",
            "［＃ルビの「」は底本では「さる」］",
            "［＃ルビの「ざる」は底本では「さる」とある］",
            "［＃「字」は底本では「字」は底本では「字」］",
            "［＃ルビの「ざる」は底本では「さ\nる」］",
        ] {
            assert!(text_variant(source).is_none(), "{source}");
        }
    }
}
