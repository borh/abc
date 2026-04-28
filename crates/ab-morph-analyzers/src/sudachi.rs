use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ab_morph_diff::{Analysis, FeatureMap};
use ab_plaintext::PlainTextDocument;
use sudachi::analysis::stateful_tokenizer::StatefulTokenizer;
use sudachi::analysis::stateless_tokenizer::DictionaryAccess;
use sudachi::config::Config;
use sudachi::dic::dictionary::JapaneseDictionary;
use sudachi::dic::subset::InfoSubset;
use sudachi::prelude::{Mode, Morpheme, MorphemeList};

use crate::features::feature_value;
use crate::span_builder::{RawToken, build_analysis_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SudachiMode {
    A,
    B,
    C,
}

impl SudachiMode {
    fn as_sudachi(self) -> Mode {
        match self {
            SudachiMode::A => Mode::A,
            SudachiMode::B => Mode::B,
            SudachiMode::C => Mode::C,
        }
    }

    fn analyzer_suffix(self) -> &'static str {
        match self {
            SudachiMode::A => "a",
            SudachiMode::B => "b",
            SudachiMode::C => "c",
        }
    }
}

pub struct SudachiAnalyzer {
    analyzer_id: String,
    mode: SudachiMode,
    dictionary: Arc<JapaneseDictionary>,
}

impl SudachiAnalyzer {
    pub fn from_dictionary_path(
        mode: SudachiMode,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        let analyzer_id = format!("sudachi-{}", mode.analyzer_suffix());
        let dictionary_path = prepare_dictionary_path(&analyzer_id, dictionary_path.as_ref())?;
        let config = Config::new(None, None, Some(dictionary_path)).map_err(|err| {
            AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.clone(),
                message: err.to_string(),
            }
        })?;
        let dictionary =
            JapaneseDictionary::from_cfg(&config).map_err(|err| AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.clone(),
                message: err.to_string(),
            })?;

        Ok(Self {
            analyzer_id,
            mode,
            dictionary: Arc::new(dictionary),
        })
    }
}

impl MorphAnalyzer for SudachiAnalyzer {
    fn analyzer_id(&self) -> &str {
        &self.analyzer_id
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis, AnalyzerError> {
        let mut tokenizer =
            StatefulTokenizer::new(self.dictionary.as_ref(), self.mode.as_sudachi());
        tokenizer.set_subset(InfoSubset::empty());
        let mut morphemes = MorphemeList::empty(self.dictionary.as_ref());

        tokenizer.reset().push_str(&document.text);
        tokenizer
            .do_tokenize()
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;
        morphemes
            .collect_results(&mut tokenizer)
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;

        let tokens = morphemes
            .iter()
            .map(|morpheme| RawToken {
                emitted_surface: morpheme.surface().to_string(),
                byte_span: Some(morpheme.begin()..morpheme.end()),
                features: sudachi_features(&morpheme),
            })
            .collect();

        build_analysis_from_tokens(
            self.analyzer_id.clone(),
            document.text_id.clone(),
            document.text.clone(),
            tokens,
        )
    }
}

fn prepare_dictionary_path(analyzer_id: &str, path: &Path) -> Result<PathBuf, AnalyzerError> {
    if path.extension().and_then(|ext| ext.to_str()) != Some("zst") {
        return Ok(path.to_owned());
    }

    let parent = path.parent().unwrap_or_else(|| Path::new("."));
    let output_name = path
        .file_stem()
        .ok_or_else(|| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "cannot derive dictionary cache name from {}",
                path.display()
            ),
        })?;
    let cache_dir = parent.join(".cache").join("ab-validator").join("sudachi");
    let output_path = cache_dir.join(output_name);

    if output_path.exists() {
        return Ok(output_path);
    }

    fs::create_dir_all(&cache_dir).map_err(|err| AnalyzerError::DictionaryLoad {
        analyzer: analyzer_id.to_owned(),
        message: format!("failed to create {}: {err}", cache_dir.display()),
    })?;

    let input = File::open(path).map_err(|err| AnalyzerError::DictionaryLoad {
        analyzer: analyzer_id.to_owned(),
        message: format!("failed to open {}: {err}", path.display()),
    })?;
    let mut output = File::create(&output_path).map_err(|err| AnalyzerError::DictionaryLoad {
        analyzer: analyzer_id.to_owned(),
        message: format!("failed to create {}: {err}", output_path.display()),
    })?;
    zstd::stream::copy_decode(input, &mut output).map_err(|err| AnalyzerError::DictionaryLoad {
        analyzer: analyzer_id.to_owned(),
        message: format!("failed to decompress {}: {err}", path.display()),
    })?;

    Ok(output_path)
}

fn sudachi_features<T>(morpheme: &Morpheme<'_, T>) -> FeatureMap
where
    T: DictionaryAccess,
{
    let mut features = FeatureMap::new();

    for (index, key) in ["pos1", "pos2", "pos3", "pos4", "c_type", "c_form"]
        .iter()
        .enumerate()
    {
        features.insert(
            (*key).to_owned(),
            morpheme.part_of_speech().get(index).and_then(feature_value),
        );
    }

    features.insert(
        "dictionary_form".to_owned(),
        feature_value(morpheme.dictionary_form()),
    );
    features.insert(
        "normalized_form".to_owned(),
        feature_value(morpheme.normalized_form()),
    );
    features.insert(
        "reading_form".to_owned(),
        feature_value(morpheme.reading_form()),
    );

    features
}

#[cfg(test)]
mod tests {
    use ab_plaintext::{PlainTextDocument, SourceFormat};

    use super::*;

    #[test]
    fn mode_suffixes_are_stable() {
        assert_eq!(SudachiMode::A.analyzer_suffix(), "a");
        assert_eq!(SudachiMode::B.analyzer_suffix(), "b");
        assert_eq!(SudachiMode::C.analyzer_suffix(), "c");
    }

    #[test]
    #[ignore = "requires dictionary symlink"]
    fn sudachi_tokenizes_plaintext_with_env_dictionary() {
        let Some(dictionary_path) = std::env::var_os("AB_SUDACHI_DICT") else {
            eprintln!("skipping Sudachi smoke test: AB_SUDACHI_DICT is not set");
            return;
        };
        let doc = PlainTextDocument {
            text_id: "smoke".to_owned(),
            source_format: SourceFormat::AozoraHonbun,
            text: "吾輩は猫である。".to_owned(),
        };

        for mode in [SudachiMode::A, SudachiMode::B, SudachiMode::C] {
            let analyzer = SudachiAnalyzer::from_dictionary_path(mode, &dictionary_path).unwrap();
            let analysis = analyzer.analyze(&doc).unwrap();
            assert_eq!(analysis.text_id, "smoke");
            assert_eq!(analysis.source_text, doc.text);
            assert!(!analysis.morphemes.is_empty());
        }
    }
}
