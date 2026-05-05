use serde::{Deserialize, Serialize};

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ScriptCategory {
    Whitespace,
    Japanese,
    LatinCode,
    Numeric,
    Mixed,
    #[default]
    Other,
}

impl ScriptCategory {
    #[must_use] 
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Whitespace => "whitespace",
            Self::Japanese => "japanese",
            Self::LatinCode => "latin-code",
            Self::Numeric => "numeric",
            Self::Mixed => "mixed",
            Self::Other => "other",
        }
    }
}

pub fn classify_text(text: &str) -> ScriptCategory {
    let mut japanese = 0usize;
    let mut latin_code = 0usize;
    let mut numeric = 0usize;
    let mut other = 0usize;

    for ch in text.chars() {
        if ch.is_whitespace() {
            continue;
        }
        if is_japanese(ch) {
            japanese += 1;
        } else if is_latin_code(ch) {
            latin_code += 1;
        } else if ch.is_numeric() {
            numeric += 1;
        } else {
            other += 1;
        }
    }

    let non_whitespace = japanese + latin_code + numeric + other;
    if non_whitespace == 0 {
        return ScriptCategory::Whitespace;
    }
    if japanese > 0 {
        if japanese >= latin_code + numeric {
            ScriptCategory::Japanese
        } else {
            ScriptCategory::Mixed
        }
    } else if latin_code > 0 {
        ScriptCategory::LatinCode
    } else if numeric > 0 && other == 0 {
        ScriptCategory::Numeric
    } else if numeric > 0 {
        ScriptCategory::Mixed
    } else {
        ScriptCategory::Other
    }
}

fn is_japanese(ch: char) -> bool {
    matches!(
        ch as u32,
        0x3040..=0x309f
            | 0x30a0..=0x30ff
            | 0x3400..=0x4dbf
            | 0x4e00..=0x9fff
            | 0xf900..=0xfaff
            | 0xff66..=0xff9f
    )
}

fn is_latin_code(ch: char) -> bool {
    ch.is_ascii_alphabetic()
        || ch.is_ascii_punctuation()
        || matches!(ch as u32, 0xff21..=0xff3a | 0xff41..=0xff5a)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn classifies_common_script_categories() {
        assert_eq!(classify_text("\n　"), ScriptCategory::Whitespace);
        assert_eq!(classify_text("徳川時代"), ScriptCategory::Japanese);
        assert_eq!(classify_text("JIS/ASCII"), ScriptCategory::LatinCode);
        assert_eq!(classify_text("0208"), ScriptCategory::Numeric);
        assert_eq!(classify_text("JIS漢字"), ScriptCategory::Mixed);
        assert_eq!(classify_text("JIS0208漢字"), ScriptCategory::Mixed);
    }
}
