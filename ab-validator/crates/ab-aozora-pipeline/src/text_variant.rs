//! Explicit witness alternatives and absence supplied by Aozora base-text notes.

use std::ops::Range;

/// Content addressed by a quoted base-text note.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextVariantTarget {
    /// The reading attached to a base character sequence.
    RubyReading,
    /// Principal text.
    Text,
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
    let delimiters = ["」は底本では", "」は、底本では"];
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
    let witness = witness.strip_prefix(['、', ' ']).unwrap_or(witness);
    let witness_start = source.len() - '］'.len_utf8() - witness.len();
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
    let current_start = source.len() - '］'.len_utf8() - body.len();
    let base_span = (!base_text.is_empty()).then(|| {
        let start = witness_start + '「'.len_utf8();
        start..start + base_text.len()
    });
    Some(TextVariant {
        target,
        current,
        base_text,
        current_span: current_start..current_start + current.len(),
        base_span,
        editorial_statement,
    })
}

/// An editorial statement explicitly about the base edition, without a
/// principal-text instruction preceding that attribution.
#[must_use]
pub fn base_edition_note(source: &str) -> Option<&str> {
    let body = source.strip_prefix("［＃")?.strip_suffix('］')?;
    let statement = body.strip_prefix("底本では")?;
    (!statement.is_empty()).then_some(body)
}

#[cfg(test)]
mod tests {
    use super::{TextVariantTarget, text_variant};

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
