use ab_ortho_detect::{OrthoToken, OrthoTokenizer};
use ab_plaintext::{PlainTextDocument, SourceFormat};

use crate::MorphAnalyzer;
use crate::VibratoAnalyzer;

impl OrthoTokenizer for VibratoAnalyzer {
    fn tokenize(&self, text: &str) -> Vec<OrthoToken> {
        let doc = PlainTextDocument {
            text_id: String::new(),
            source_format: SourceFormat::AozoraHonbun,
            text: text.to_owned(),
        };
        match self.analyze(&doc) {
            Ok(analysis) => analysis
                .morphemes
                .iter()
                .map(|m| OrthoToken {
                    surface: m.surface.clone(),
                    pos2: m
                        .features
                        .get("pos2")
                        .and_then(|opt| opt.as_deref())
                        .map(String::from),
                })
                .collect(),
            Err(_) => Vec::new(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compile-time assertion that `VibratoAnalyzer` satisfies the
    /// `OrthoTokenizer: Send + Sync` supertrait bound.
    #[test]
    fn vibrato_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<VibratoAnalyzer>();
    }
}
