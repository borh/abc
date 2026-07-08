use std::fs;
use std::io::ErrorKind;
use std::path::{Path, PathBuf};
use std::sync::Mutex;
use std::time::Duration;

use ab_morph_diff::{Analysis, AnalyzerWarning};
use ab_plaintext::PlainTextDocument;
use vibrato_rkyv::{Dictionary, LoadMode, Tokenizer};

use crate::chunking::semantic_chunks;
use crate::features::parse_vibrato_feature_string;
use crate::span_builder::{RawToken, build_analysis_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

const VIBRATO_CHUNK_BYTES: usize = 32_000;
const DEFAULT_VIBRATO_DICTIONARY: &str = "unidic-cwj-202512";
/// Analyzer id assigned by [`VibratoAnalyzer::unidic_cwj_default`]; callers
/// can match on it to share an already-loaded default dictionary.
pub const DEFAULT_VIBRATO_ANALYZER_ID: &str = "vibrato:unidic-cwj-202512";
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
    /// The dictionary archive this analyzer was loaded from, retained so its
    /// content can be hashed on demand for policy provenance
    /// ([`archive_content_hash`](Self::archive_content_hash)).
    source_path: PathBuf,
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
                let cache_dir = zstd_cache_dir();
                let _cache_guard = ZstdCacheLock::acquire(&analyzer_id, &cache_dir)?;
                Dictionary::from_zstd_with_options(dictionary_path, &cache_dir, true)
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
            source_path: dictionary_path.to_path_buf(),
        })
    }

    /// SHA-256 (`sha256:<hex>`) of the dictionary archive this analyzer loaded.
    ///
    /// Hashes the on-disk archive bytes (the `.dic.zst`/`.dic` file), so it pins
    /// the exact dictionary content — the value bound as the `dictionary_hash`
    /// of the M2 historical detector (I2-D17) and mirrored by the manifest's
    /// `tokenizer_dictionary_hash`. Read on demand rather than at load so the
    /// common analyze path pays no hashing cost.
    ///
    /// # Errors
    ///
    /// Returns [`AnalyzerError::DictionaryLoad`] if the archive cannot be read.
    pub fn archive_content_hash(&self) -> Result<String, AnalyzerError> {
        hash_archive_file(&self.source_path, &self.analyzer_id)
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
        Self::from_dictionary_path(DEFAULT_VIBRATO_ANALYZER_ID, path)
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

/// SHA-256 (`sha256:<hex>`) of a named dictionary archive **without loading it**.
///
/// Resolves the archive by the same name logic as
/// [`VibratoAnalyzer::from_dictionary_name`], reads the compressed bytes, and
/// hashes them — cheap (no zstd decompression, no tokenizer build). Lets a
/// caller bind the M2 oracle dictionary into policy identity before the full
/// analyzer is constructed; the value equals the loaded analyzer's
/// [`archive_content_hash`](VibratoAnalyzer::archive_content_hash).
///
/// # Errors
///
/// Returns [`AnalyzerError::DictionaryLoad`] if the archive cannot be resolved
/// or read.
pub fn dictionary_archive_hash(dictionary_name: &str) -> Result<String, AnalyzerError> {
    let path = resolve_dictionary_path(dictionary_name)?;
    hash_archive_file(&path, dictionary_name)
}

fn hash_archive_file(path: &Path, analyzer_id: &str) -> Result<String, AnalyzerError> {
    use sha2::{Digest, Sha256};
    let bytes = fs::read(path).map_err(|err| AnalyzerError::DictionaryLoad {
        analyzer: analyzer_id.to_owned(),
        message: format!(
            "cannot read dictionary archive {} for content hash: {err}",
            path.display()
        ),
    })?;
    let mut hasher = Sha256::new();
    hasher.update(&bytes);
    Ok(format!("sha256:{:x}", hasher.finalize()))
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
    // 1. `AB_VIBRATO_DICT_DIR`: one or more `:`-separated directories that each
    //    hold `<name>.dic.zst` files. Point this at the flake's combined
    //    `vibrato-dictionaries` output (`<store>/share/vibrato`) and every dict
    //    resolves by name with no symlinking into the repo — this is what the
    //    dev shell exports, so `dictionary/compiled/` linking is not required.
    for dir in vibrato_dict_search_dirs() {
        for extension in [".dic.zst", ".dic"] {
            let candidate = dir.join(format!("{name}{extension}"));
            if candidate.is_file() {
                return Ok(candidate);
            }
        }
    }

    // 2. Fallback: `dictionary/compiled/` under the workspace, where
    //    `just dictionary-build-*` symlinks flake outputs for non-shell use.
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
            "could not resolve Vibrato dictionary `{name}`: not found under any \
             AB_VIBRATO_DICT_DIR directory nor in dictionary/{}/ \
             (nix-built; enter `nix develop` or run `just dictionary-build-all`)",
            VIBRATO_DICTIONARY_SEARCH_PATHS[0],
        ),
    })
}

/// Where the decompressed-dictionary cache (and its load lock) live. This is
/// decoupled from the source `.dic.zst` location so the source can be a
/// read-only nix store path (via `AB_VIBRATO_DICT_DIR`) while the multi-GB
/// decompressed cache lands on a writable, roomy filesystem.
///
/// Order: `AB_VIBRATO_CACHE_DIR` if set, else the workspace-local
/// `dictionary/compiled/.cache/` (the historical location, on the project
/// filesystem — not the potentially small `$HOME`). The cache is content-hash
/// keyed by vibrato-rkyv, so pointing store-path and symlink loads at the same
/// dir reuses one decompressed artifact per dictionary.
fn zstd_cache_dir() -> PathBuf {
    zstd_cache_dir_from(std::env::var_os("AB_VIBRATO_CACHE_DIR"))
}

fn zstd_cache_dir_from(override_dir: Option<std::ffi::OsString>) -> PathBuf {
    override_dir
        .map(PathBuf::from)
        .filter(|p| !p.as_os_str().is_empty())
        .unwrap_or_else(|| workspace_path("dictionary/compiled/.cache"))
}

/// Directories to search for named vibrato dictionaries, highest precedence
/// first, taken from the `:`-separated `AB_VIBRATO_DICT_DIR` env var. Empty
/// entries are skipped. Returns an empty vec when the var is unset.
fn vibrato_dict_search_dirs() -> Vec<PathBuf> {
    dict_search_dirs_from(std::env::var_os("AB_VIBRATO_DICT_DIR"))
}

fn dict_search_dirs_from(value: Option<std::ffi::OsString>) -> Vec<PathBuf> {
    value
        .map(|value| {
            std::env::split_paths(&value)
                .filter(|p| !p.as_os_str().is_empty())
                .collect()
        })
        .unwrap_or_default()
}

struct ZstdCacheLock {
    lock_dir: PathBuf,
}

impl ZstdCacheLock {
    fn acquire(analyzer_id: &str, cache_dir: &Path) -> Result<Self, AnalyzerError> {
        fs::create_dir_all(cache_dir).map_err(|err| AnalyzerError::DictionaryLoad {
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
                emitted_surface: Some(token.surface().to_owned()),
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
            &document.text,
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
        assert_eq!(analysis.source_text.as_ref(), doc.text.as_str());
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

    #[test]
    fn dict_search_dirs_parses_path_list_and_skips_empties() {
        // Unset → no extra search roots (falls back to dictionary/compiled/).
        assert!(dict_search_dirs_from(None).is_empty());
        // `:`-separated list, empty entries skipped.
        let dirs = dict_search_dirs_from(Some("/a/share/vibrato::/b".into()));
        assert_eq!(dirs, vec![PathBuf::from("/a/share/vibrato"), PathBuf::from("/b")]);
    }

    #[test]
    fn zstd_cache_dir_honors_env_override_else_workspace_local() {
        // Override wins.
        assert_eq!(
            zstd_cache_dir_from(Some("/big/fs/cache".into())),
            PathBuf::from("/big/fs/cache")
        );
        // Empty override is ignored; falls back to the workspace-local cache
        // (on the project filesystem, not a potentially small $HOME).
        assert_eq!(
            zstd_cache_dir_from(Some(std::ffi::OsString::new())),
            workspace_path("dictionary/compiled/.cache")
        );
        assert_eq!(
            zstd_cache_dir_from(None),
            workspace_path("dictionary/compiled/.cache")
        );
    }
}
