use std::fs;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::Duration;

use ab_morph_diff::{Analysis, AnalyzerWarning};
use ab_plaintext::PlainTextDocument;
use vibrato_rkyv::{CacheStrategy, Dictionary, LoadMode, Tokenizer};

use crate::chunking::semantic_chunks;
use crate::features::parse_vibrato_feature_string;
use crate::span_builder::{RawToken, build_analysis_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

const VIBRATO_CHUNK_BYTES: usize = 32_000;
const DEFAULT_VIBRATO_DICTIONARY: &str = "unidic-cwj-202512";
// Nix-built dictionaries only: `just dictionary-build-*` symlinks flake outputs
// into dictionary/compiled/. No other directory is consulted, so a stale
// hand-built artifact can never shadow the nix package.
const VIBRATO_DICTIONARY_SEARCH_PATHS: [&str; 1] = ["compiled"];
const ZSTD_DICTIONARY_LOAD_LOCK_RETRIES: usize = 600;
const ZSTD_DICTIONARY_LOAD_LOCK_SLEEP: Duration = Duration::from_millis(50);
static ZSTD_DICTIONARY_LOAD_LOCK: Mutex<()> = Mutex::new(());

pub struct VibratoAnalyzer {
    analyzer_id: String,
    tokenizer: Tokenizer,
}

impl VibratoAnalyzer {
    /// Load a Vibrato tokenizer from an explicit dictionary path.
    ///
    /// # Errors
    ///
    /// Returns an error when the tokenizer cannot be created from the dictionary.
    pub fn from_dictionary_path(
        analyzer_id: impl Into<String>,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        let analyzer_id = analyzer_id.into();
        let dictionary_path = dictionary_path.as_ref();
        let dictionary =
            if dictionary_path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
                let _guard = ZSTD_DICTIONARY_LOAD_LOCK.lock().map_err(|err| {
                    AnalyzerError::DictionaryLoad {
                        analyzer: analyzer_id.clone(),
                        message: format!("zstd dictionary load lock poisoned: {err}"),
                    }
                })?;
                let _cache_guard = ZstdCacheLock::acquire(&analyzer_id, dictionary_path)?;
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

    /// Load a Vibrato dictionary from an explicit dictionary identifier.
    ///
    /// # Errors
    ///
    /// Returns an error when the dictionary cannot be resolved or loaded.
    pub fn from_dictionary_name(dictionary_name: impl AsRef<str>) -> Result<Self, AnalyzerError> {
        let dictionary_name = dictionary_name.as_ref();
        let analyzer_id = format!("vibrato:{dictionary_name}");
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
        Self::from_dictionary_path(format!("vibrato:{DEFAULT_VIBRATO_DICTIONARY}"), path)
    }

    /// Load a Vibrato model from a `.zst`-compressed path.
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
}

fn default_dictionary_path() -> Result<PathBuf, AnalyzerError> {
    default_dictionary_path_from_env(std::env::var_os("AB_VIBRATO_DICT"))
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
    resolve_dictionary_path(DEFAULT_VIBRATO_DICTIONARY)
}

fn resolve_dictionary_path(dictionary_name: &str) -> Result<PathBuf, AnalyzerError> {
    let direct = Path::new(dictionary_name);
    if direct.is_file() {
        return Ok(direct.to_path_buf());
    }

    if let Some(without_suffix) = dictionary_name.strip_suffix(".dic.zst") {
        return resolve_dictionary_path_from_basename(without_suffix);
    }

    if let Some(without_suffix) = dictionary_name.strip_suffix(".dic") {
        return resolve_dictionary_path_from_basename(without_suffix);
    }

    resolve_dictionary_path_from_basename(dictionary_name)
}

fn resolve_dictionary_path_from_basename(name: &str) -> Result<PathBuf, AnalyzerError> {
    for dict_dir in VIBRATO_DICTIONARY_SEARCH_PATHS {
        for extension in [".dic.zst", ".dic"] {
            let candidate = workspace_path(format!("dictionary/{dict_dir}/{name}{extension}",));
            if candidate.is_file() {
                return Ok(candidate);
            }
        }
    }

    Err(AnalyzerError::DictionaryLoad {
        analyzer: format!("vibrato:{name}"),
        message: format!(
            "could not resolve Vibrato dictionary `{name}` in the dictionary/{}/ directory \
             (nix-built; run `just dictionary-build-all` to (re)link the flake outputs)",
            VIBRATO_DICTIONARY_SEARCH_PATHS[0],
        ),
    })
}

struct ZstdCacheLock {
    lock_dir: PathBuf,
}

impl ZstdCacheLock {
    fn acquire(analyzer_id: &str, dictionary_path: &Path) -> Result<Self, AnalyzerError> {
        let parent = dictionary_path
            .parent()
            .ok_or_else(|| AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.to_owned(),
                message: format!(
                    "cannot derive zstd dictionary cache directory from {}",
                    dictionary_path.display()
                ),
            })?;
        let cache_dir = parent.join(".cache");
        fs::create_dir_all(&cache_dir).map_err(|err| AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: err.to_string(),
        })?;
        let lock_dir = cache_dir.join(".ab-validator-zstd-load.lock");

        for _ in 0..ZSTD_DICTIONARY_LOAD_LOCK_RETRIES {
            match fs::create_dir(&lock_dir) {
                Ok(()) => return Ok(Self { lock_dir }),
                Err(err) if err.kind() == ErrorKind::AlreadyExists => {
                    std::thread::sleep(ZSTD_DICTIONARY_LOAD_LOCK_SLEEP);
                }
                Err(err) => {
                    return Err(AnalyzerError::DictionaryLoad {
                        analyzer: analyzer_id.to_owned(),
                        message: err.to_string(),
                    });
                }
            }
        }

        Err(AnalyzerError::DictionaryLoad {
            analyzer: analyzer_id.to_owned(),
            message: format!(
                "timed out waiting for zstd dictionary cache lock at {}",
                lock_dir.display()
            ),
        })
    }
}

impl Drop for ZstdCacheLock {
    fn drop(&mut self) {
        let _ = fs::remove_dir(&self.lock_dir);
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
        let chunks = semantic_chunks(&document.text, VIBRATO_CHUNK_BYTES);
        let mut hard_split_count = 0usize;
        let mut first_hard_split_offset = None;
        let mut warnings = Vec::new();
        let mut tokens = Vec::new();

        for chunk in chunks {
            if chunk.hard_split {
                hard_split_count += 1;
                first_hard_split_offset.get_or_insert(chunk.byte_offset);
            }
            worker.reset_sentence(chunk.text);
            worker.tokenize();
            tokens.extend(worker.token_iter().map(|token| RawToken {
                emitted_surface: token.surface().to_owned(),
                byte_span: None,
                features: parse_vibrato_feature_string(token.feature()),
            }));
        }
        if hard_split_count > 0 {
            warnings.push(AnalyzerWarning {
                analyzer_id: self.analyzer_id.clone(),
                text_id: document.text_id.clone(),
                stage: "vibrato_chunk".to_owned(),
                message: "hard_split_without_sentence_boundary".to_owned(),
                count: hard_split_count,
                first_byte_offset: first_hard_split_offset.unwrap_or(0),
                hard_limit_bytes: VIBRATO_CHUNK_BYTES,
            });
        }

        let mut analysis = build_analysis_from_tokens(
            self.analyzer_id.clone(),
            document.text_id.clone(),
            document.text.clone(),
            tokens,
        )?;
        analysis.warnings = warnings;
        Ok(analysis)
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

    #[test]
    fn default_dictionary_path_honors_env_override() {
        let override_path = std::env::temp_dir().join("unidic-cwj-202512.dic");

        assert_eq!(
            default_dictionary_path_from_env(Some(override_path.clone().into_os_string())).unwrap(),
            override_path
        );
    }
}
