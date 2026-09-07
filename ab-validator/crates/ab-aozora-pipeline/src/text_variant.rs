//! Explicit witness alternatives and absence supplied by Aozora base-text notes.

/// Content addressed by a quoted base-text note.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TextVariantTarget {
    /// The reading attached to a base character sequence.
    RubyReading,
    /// Principal text.
    Text,
}

/// A supplied reading or text and its explicitly stated base-text alternative.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TextVariant<'s> {
    /// Which content sequence the note addresses.
    pub target: TextVariantTarget,
    /// The spelling supplied in the Aozora text.
    pub current: &'s str,
    /// The alternative attributed to the base text; empty only for explicit absence.
    pub base_text: &'s str,
}

/// Recognize an exact quoted base-text note, including its annotation delimiters.
/// Other editorial prose remains unclassified by this recognizer.
#[must_use]
pub fn text_variant(source: &str) -> Option<TextVariant<'_>> {
    let body = source.strip_prefix("［＃")?.strip_suffix('］')?;
    let (target, body) = if let Some(body) = body.strip_prefix("ルビの「") {
        (TextVariantTarget::RubyReading, body)
    } else {
        (TextVariantTarget::Text, body.strip_prefix('「')?)
    };
    let (current, witness) = body.split_once("」は底本では")?;
    let base_text = match witness {
        "欠落" | "なし" => "",
        quoted => {
            let text = quoted.strip_prefix('「')?.strip_suffix('」')?;
            if text.is_empty() {
                return None;
            }
            text
        }
    };
    if current.is_empty()
        || current.contains(['「', '」', '\n'])
        || base_text.contains(['「', '」', '\n'])
    {
        return None;
    }
    Some(TextVariant {
        target,
        current,
        base_text,
    })
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
    fn ambiguous_or_incomplete_notes_remain_unclassified() {
        for source in [
            "［＃ルビの「ざる」は底本では「さる」",
            "［＃ルビの「」は底本では「さる」］",
            "［＃ルビの「ざる」は底本では「さる」とある］",
            "［＃ルビの「ざ「る」は底本では「さる」］",
            "［＃ルビの「ざる」は底本では「さ\nる」］",
        ] {
            assert!(text_variant(source).is_none(), "{source}");
        }
    }
}
