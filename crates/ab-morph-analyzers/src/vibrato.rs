use std::path::{Path, PathBuf};

use ab_morph_diff::Analysis;
use ab_plaintext::PlainTextDocument;
use vibrato_rkyv::{CacheStrategy, Dictionary, LoadMode, Tokenizer};

use crate::features::parse_vibrato_feature_string;
use crate::span_builder::{RawToken, build_analysis_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

pub struct VibratoAnalyzer {
    analyzer_id: String,
    tokenizer: Tokenizer,
}

impl VibratoAnalyzer {
    pub fn from_dictionary_path(
        analyzer_id: impl Into<String>,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        let analyzer_id = analyzer_id.into();
        let dictionary_path = dictionary_path.as_ref();
        let dictionary = if dictionary_path.extension().and_then(|ext| ext.to_str()) == Some("zst")
        {
            Dictionary::from_zstd(dictionary_path, CacheStrategy::Local)
        } else {
            Dictionary::from_path(dictionary_path, LoadMode::TrustCache)
        }
        .map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.clone(),
            message: err.to_string(),
        })?;

        Ok(Self {
            analyzer_id,
            tokenizer: Tokenizer::new(dictionary),
        })
    }

    pub fn from_zstd(
        analyzer_id: impl Into<String>,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        Self::from_dictionary_path(analyzer_id, dictionary_path)
    }

    pub fn unidic_cwj_default() -> Result<Self, AnalyzerError> {
        Self::from_zstd(
            "vibrato:unidic-cwj-202512",
            workspace_path("dictionary/optimized/unidic-cwj-202512.dic.zst"),
        )
    }
}

fn workspace_path(path: impl AsRef<Path>) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(path.as_ref())
}

impl MorphAnalyzer for VibratoAnalyzer {
    fn analyzer_id(&self) -> &str {
        &self.analyzer_id
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis, AnalyzerError> {
        let mut worker = self.tokenizer.new_worker();
        worker.reset_sentence(&document.text);
        worker.tokenize();

        build_analysis_from_tokens(
            self.analyzer_id.clone(),
            document.text_id.clone(),
            document.text.clone(),
            worker.token_iter().map(|token| RawToken {
                emitted_surface: token.surface().to_owned(),
                byte_span: None,
                features: parse_vibrato_feature_string(token.feature()),
            }),
        )
    }
}

#[cfg(test)]
mod tests {
    use ab_plaintext::{PlainTextDocument, SourceFormat};

    use super::*;

    #[test]
    #[ignore = "requires dictionary symlink"]
    fn vibrato_tokenizes_plaintext_with_default_dictionary() {
        let analyzer = VibratoAnalyzer::unidic_cwj_default().unwrap();
        let doc = PlainTextDocument {
            text_id: "smoke".to_owned(),
            source_format: SourceFormat::AozoraHonbun,
            text: "吾輩は猫である。".to_owned(),
        };

        let analysis = analyzer.analyze(&doc).unwrap();
        assert_eq!(analysis.analyzer, "vibrato:unidic-cwj-202512");
        assert_eq!(analysis.text_id, "smoke");
        assert_eq!(analysis.source_text, doc.text);
        assert!(!analysis.morphemes.is_empty());
    }
}
