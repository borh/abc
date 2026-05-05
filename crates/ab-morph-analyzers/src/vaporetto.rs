use std::fs::File;
use std::path::{Path, PathBuf};

use ab_morph_diff::Analysis;
use ab_plaintext::PlainTextDocument;
use vaporetto::{Model, Predictor, Sentence};
use zstd::stream::read::Decoder;

use crate::features::parse_vaporetto_feature_string;
use crate::span_builder::{RawToken, build_analysis_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

const DEFAULT_VAPORETTO_DICTIONARY: &str = "unidic-cwj-202512";
const VAPORETTO_DICTIONARY_SEARCH_PATHS: [&str; 2] = ["compiled", "optimized"];
const VAPORETTO_DICTIONARY_EXTENSIONS: [&str; 6] = [
    ".model",
    ".model.zst",
    ".bin",
    ".bin.zst",
    ".dic",
    ".dic.zst",
];

pub struct VaporettoAnalyzer {
    analyzer_id: String,
    predictor: Predictor,
}

impl VaporettoAnalyzer {
    /// Load a Vaporetto predictor from an explicit dictionary path.
    ///
    /// # Errors
    ///
    /// Returns an error when the model cannot be loaded.
    pub fn from_dictionary_path(
        analyzer_id: impl Into<String>,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        let analyzer_id = analyzer_id.into();
        let dictionary_path = dictionary_path.as_ref();
        let model = load_vaporetto_model(&analyzer_id, dictionary_path)?;
        let predictor =
            Predictor::new(model, true).map_err(|err| AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.clone(),
                message: err.to_string(),
            })?;

        Ok(Self {
            analyzer_id,
            predictor,
        })
    }

    /// Load a Vaporetto model from `.zst`-compressed path.
    ///
    /// # Errors
    ///
    /// Returns an error when the compressed model cannot be loaded.
    pub fn from_zstd(
        analyzer_id: impl Into<String>,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        Self::from_dictionary_path(analyzer_id, dictionary_path)
    }

    /// Load and parse a dictionary from an explicit `.model` path.
    ///
    /// # Errors
    ///
    /// Returns an error when the dictionary name cannot be resolved or loaded.
    pub fn from_dictionary_name(dictionary_name: impl AsRef<str>) -> Result<Self, AnalyzerError> {
        let dictionary_name = dictionary_name.as_ref();
        let analyzer_id = format!("vaporetto:{dictionary_name}");
        let dictionary_path = resolve_dictionary_path(dictionary_name)?;

        Self::from_dictionary_path(analyzer_id, dictionary_path)
    }

    /// Load the default Unidic-CWJ dictionary.
    ///
    /// # Errors
    ///
    /// Returns an error when the default dictionary cannot be resolved or loaded.
    pub fn unidic_cwj_default() -> Result<Self, AnalyzerError> {
        let path = default_dictionary_path()?;
        Self::from_dictionary_path(format!("vaporetto:{DEFAULT_VAPORETTO_DICTIONARY}"), path)
    }
}

impl MorphAnalyzer for VaporettoAnalyzer {
    fn analyzer_id(&self) -> &str {
        &self.analyzer_id
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis, AnalyzerError> {
        let mut sentence = Sentence::default();
        sentence
            .update_raw(&document.text)
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;
        self.predictor.predict(&mut sentence);
        sentence.fill_tags();

        let morphemes = sentence
            .iter_tokens()
            .map(|token| {
                let byte_span =
                    char_range_to_byte_range(&document.text, token.start(), token.end()).map_err(
                        |message| AnalyzerError::Tokenize {
                            analyzer: self.analyzer_id.clone(),
                            message,
                        },
                    )?;

                Ok(RawToken {
                    emitted_surface: token.surface().to_owned(),
                    byte_span: Some(byte_span),
                    features: parse_vaporetto_feature_string(token.tags().to_vec()),
                })
            })
            .collect::<Result<Vec<_>, AnalyzerError>>()?;

        build_analysis_from_tokens(
            self.analyzer_id.clone(),
            document.text_id.clone(),
            document.text.clone(),
            morphemes,
        )
    }
}

fn load_vaporetto_model(analyzer_id: &str, dictionary_path: &Path) -> Result<Model, AnalyzerError> {
    if dictionary_path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
        let file = File::open(dictionary_path).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "failed to open compressed model {}: {err}",
                dictionary_path.display()
            ),
        })?;
        let decoder = Decoder::new(file).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "failed to create zstd decoder for {}: {err}",
                dictionary_path.display()
            ),
        })?;
        Model::read(decoder).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "failed to read dictionary {}: {err}",
                dictionary_path.display()
            ),
        })
    } else {
        let file = File::open(dictionary_path).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!("failed to open model {}: {err}", dictionary_path.display()),
        })?;
        Model::read(file).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "failed to read dictionary {}: {err}",
                dictionary_path.display()
            ),
        })
    }
}

fn default_dictionary_path() -> Result<PathBuf, AnalyzerError> {
    default_dictionary_path_from_env(std::env::var_os("AB_VAPORETTO_DICT"))
}

fn default_dictionary_path_from_env(
    override_path: Option<std::ffi::OsString>,
) -> Result<PathBuf, AnalyzerError> {
    override_path
        .map(PathBuf::from)
        .map(Ok)
        .unwrap_or_else(resolve_default_dictionary)
}

fn resolve_default_dictionary() -> Result<PathBuf, AnalyzerError> {
    resolve_dictionary_path(DEFAULT_VAPORETTO_DICTIONARY)
}

fn resolve_dictionary_path(dictionary_name: &str) -> Result<PathBuf, AnalyzerError> {
    let direct = Path::new(dictionary_name);
    if direct.is_file() {
        return Ok(direct.to_path_buf());
    }

    let candidate_bases = [
        dictionary_name,
        dictionary_name
            .strip_suffix(".model")
            .unwrap_or(dictionary_name),
        dictionary_name
            .strip_suffix(".model.zst")
            .unwrap_or(dictionary_name),
        dictionary_name
            .strip_suffix(".dic")
            .unwrap_or(dictionary_name),
        dictionary_name
            .strip_suffix(".dic.zst")
            .unwrap_or(dictionary_name),
        dictionary_name
            .strip_suffix(".bin")
            .unwrap_or(dictionary_name),
        dictionary_name
            .strip_suffix(".bin.zst")
            .unwrap_or(dictionary_name),
    ];

    for path in VAPORETTO_DICTIONARY_SEARCH_PATHS {
        for name in candidate_bases {
            for extension in VAPORETTO_DICTIONARY_EXTENSIONS {
                let candidate = workspace_path(format!("dictionary/{path}/{name}{extension}"));
                if candidate.is_file() {
                    return Ok(candidate);
                }
            }
        }
    }

    Err(AnalyzerError::DictionaryLoad {
        analyzer: "vaporetto".to_owned(),
        message: format!(
            "could not resolve Vaporetto dictionary `{dictionary_name}` in dictionary/{}/ or dictionary/{}/ directories",
            VAPORETTO_DICTIONARY_SEARCH_PATHS[0], VAPORETTO_DICTIONARY_SEARCH_PATHS[1],
        ),
    })
}

fn workspace_path(path: impl AsRef<Path>) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(path.as_ref())
}

fn char_range_to_byte_range(
    text: &str,
    start_char: usize,
    end_char: usize,
) -> Result<std::ops::Range<usize>, String> {
    let start = char_offset_to_byte_index(text, start_char)
        .ok_or_else(|| format!("start char offset {start_char} is invalid"))?;
    let end = char_offset_to_byte_index(text, end_char).ok_or_else(|| {
        format!(
            "end char offset {end_char} is invalid, source has {} chars",
            text.chars().count()
        )
    })?;
    Ok(start..end)
}

fn char_offset_to_byte_index(text: &str, char_offset: usize) -> Option<usize> {
    if char_offset == 0 {
        return Some(0);
    }

    for (index, (byte_index, _)) in text.char_indices().enumerate() {
        if index == char_offset {
            return Some(byte_index);
        }
    }

    (char_offset == text.chars().count()).then_some(text.len())
}

#[cfg(test)]
mod tests {
    use ab_plaintext::{PlainTextDocument, SourceFormat};

    use super::*;

    #[test]
    #[ignore = "requires dictionary symlink"]
    fn vaporetto_tokenizes_plaintext_with_default_dictionary() {
        let analyzer = VaporettoAnalyzer::unidic_cwj_default().unwrap();
        let doc = PlainTextDocument {
            text_id: "smoke".to_owned(),
            source_format: SourceFormat::AozoraHonbun,
            text: "吾輩は猫である。".to_owned(),
        };

        let analysis = analyzer.analyze(&doc).unwrap();
        assert_eq!(analysis.analyzer, "vaporetto:unidic-cwj-202512");
        assert_eq!(analysis.text_id, doc.text_id);
        assert_eq!(analysis.source_text, doc.text);
        assert!(!analysis.morphemes.is_empty());
    }
}
