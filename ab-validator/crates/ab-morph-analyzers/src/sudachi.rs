use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ab_morph_diff::{Analysis, AnalyzerWarning, FeatureMap, Morpheme as DiffMorpheme};
use ab_plaintext::PlainTextDocument;
use sudachi::analysis::stateful_tokenizer::StatefulTokenizer;
use sudachi::analysis::stateless_tokenizer::DictionaryAccess;
use sudachi::config::Config;
use sudachi::dic::dictionary::JapaneseDictionary;
use sudachi::dic::subset::InfoSubset;
use sudachi::prelude::{Mode, Morpheme, MorphemeList};

use crate::chunking::{TextChunk, semantic_chunks};
use crate::features::feature_value;
use crate::span_builder::{RawToken, build_morphemes_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

const SUDACHI_HARD_CHUNK_BYTES: usize = 32_000;
const SUDACHI_SETTINGS_FILE: &str = "sudachi.json";

/// Directory holding Sudachi's `sudachi.json`, `char.def`, `unk.def`, and
/// `rewrite.def`.
///
/// The `sudachi` crate resolves these relative to the `CARGO_MANIFEST_DIR` it
/// was compiled under, which is a build-sandbox path for nix-built binaries and
/// a cargo checkout path for local builds. `AB_SUDACHI_RESOURCE_DIR` at run
/// time wins; otherwise the value the packager baked in at compile time; and
/// only then the crate's own default.
fn sudachi_resource_dir() -> Option<PathBuf> {
    if let Some(dir) = std::env::var_os("AB_SUDACHI_RESOURCE_DIR") {
        return Some(PathBuf::from(dir));
    }
    option_env!("AB_SUDACHI_RESOURCE_DIR").map(PathBuf::from)
}
const SUDACHI_FEATURE_SUBSET: InfoSubset = InfoSubset::SURFACE
    .union(InfoSubset::POS_ID)
    .union(InfoSubset::NORMALIZED_FORM)
    .union(InfoSubset::DIC_FORM_WORD_ID)
    .union(InfoSubset::READING_FORM);

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
    /// Create a Sudachi analyzer from a dictionary path.
    ///
    /// # Errors
    ///
    /// Returns an error when dictionary path cannot be converted or loaded.
    pub fn from_dictionary_path(
        mode: SudachiMode,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Self, AnalyzerError> {
        let analyzer_id = format!("sudachi-{}", mode.analyzer_suffix());
        let dictionary = Self::load_dictionary(&analyzer_id, dictionary_path)?;

        Ok(Self::from_dictionary(mode, dictionary))
    }

    /// Load a Sudachi dictionary from disk.
    ///
    /// # Errors
    ///
    /// Returns an error when the dictionary cannot be loaded.
    pub fn load_dictionary(
        analyzer_id: &str,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Arc<JapaneseDictionary>, AnalyzerError> {
        let dictionary_path = prepare_dictionary_path(analyzer_id, dictionary_path.as_ref())?;
        let resource_dir = sudachi_resource_dir();
        let config_file = resource_dir
            .as_ref()
            .map(|dir| dir.join(SUDACHI_SETTINGS_FILE));
        let config = Config::new(config_file, resource_dir, Some(dictionary_path)).map_err(|err| {
            AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.to_owned(),
                message: err.to_string(),
            }
        })?;
        let dictionary =
            JapaneseDictionary::from_cfg(&config).map_err(|err| AnalyzerError::DictionaryLoad {
                analyzer: analyzer_id.to_owned(),
                message: err.to_string(),
            })?;

        Ok(Arc::new(dictionary))
    }

    /// Construct a Sudachi analyzer from an already-loaded dictionary.
    #[must_use]
    pub fn from_dictionary(mode: SudachiMode, dictionary: Arc<JapaneseDictionary>) -> Self {
        Self {
            analyzer_id: format!("sudachi-{}", mode.analyzer_suffix()),
            mode,
            dictionary,
        }
    }
}

impl MorphAnalyzer for SudachiAnalyzer {
    fn analyzer_id(&self) -> &str {
        &self.analyzer_id
    }

    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis, AnalyzerError> {
        let chunks = sudachi_chunks(&document.text, SUDACHI_HARD_CHUNK_BYTES);
        let mut all_morphemes = Vec::new();
        let mut hard_split_count = 0usize;
        let mut first_hard_split_offset = None;
        let mut warnings = Vec::new();
        let mut tokenizer =
            StatefulTokenizer::new(self.dictionary.as_ref(), self.mode.as_sudachi());
        tokenizer.set_subset(SUDACHI_FEATURE_SUBSET);

        for chunk in chunks {
            if chunk.hard_split {
                hard_split_count += 1;
                first_hard_split_offset.get_or_insert(chunk.byte_offset);
            }
            let mut morphemes = self.analyze_chunk(document, &chunk, &mut tokenizer)?;
            offset_chunk_morphemes(&mut morphemes, chunk.byte_offset, chunk.char_offset);
            all_morphemes.extend(morphemes);
        }
        if hard_split_count > 0 {
            warnings.push(AnalyzerWarning {
                analyzer_id: self.analyzer_id.clone(),
                text_id: document.text_id.clone(),
                stage: "sudachi_chunk".to_owned(),
                message: "hard_split_without_sentence_boundary".to_owned(),
                count: hard_split_count,
                first_byte_offset: first_hard_split_offset.unwrap_or(0),
                hard_limit_bytes: SUDACHI_HARD_CHUNK_BYTES,
            });
        }

        Ok(Analysis {
            analyzer: self.analyzer_id.clone(),
            text_id: document.text_id.clone(),
            source_text: Arc::from(document.text.as_str()),
            morphemes: all_morphemes,
            warnings,
            ortho_annotations: None,
            ortho_offset_map: None,
        })
    }
}

impl SudachiAnalyzer {
    fn analyze_chunk(
        &self,
        document: &PlainTextDocument,
        chunk: &TextChunk<'_>,
        tokenizer: &mut StatefulTokenizer<&JapaneseDictionary>,
    ) -> Result<Vec<DiffMorpheme>, AnalyzerError> {
        tokenizer.reset().push_str(chunk.text);
        tokenizer
            .do_tokenize()
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;
        let mut morphemes = MorphemeList::empty(self.dictionary.as_ref());
        morphemes
            .collect_results(&mut *tokenizer)
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;

        build_morphemes_from_tokens(
            &self.analyzer_id,
            &document.text_id,
            chunk.text,
            morphemes.iter().map(|morpheme| RawToken {
                // The reported span re-slices the surface from the source
                // text, so no per-token surface String is needed.
                emitted_surface: None,
                byte_span: Some(morpheme.begin()..morpheme.end()),
                features: sudachi_features(&morpheme),
            }),
        )
    }
}

fn sudachi_chunks(text: &str, max_bytes: usize) -> Vec<TextChunk<'_>> {
    semantic_chunks(text, max_bytes)
}

fn offset_chunk_morphemes(morphemes: &mut [DiffMorpheme], byte_offset: usize, char_offset: usize) {
    for morpheme in morphemes {
        morpheme.byte_span.start += byte_offset;
        morpheme.byte_span.end += byte_offset;
        morpheme.char_span.start += char_offset;
        morpheme.char_span.end += char_offset;
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

// Interned key names shared across tokens: cloning an Arc<str> is a refcount
// bump, while `&'static str -> Arc<str>` allocates per token.
static SUDACHI_POS_KEY_ARCS: std::sync::LazyLock<[ab_morph_diff::FeatureKey; 6]> =
    std::sync::LazyLock::new(|| {
        [
            "pos1".into(),
            "pos2".into(),
            "pos3".into(),
            "pos4".into(),
            "c_type".into(),
            "c_form".into(),
        ]
    });
static SUDACHI_FORM_KEY_ARCS: std::sync::LazyLock<[ab_morph_diff::FeatureKey; 3]> =
    std::sync::LazyLock::new(|| {
        [
            "dictionary_form".into(),
            "normalized_form".into(),
            "reading_form".into(),
        ]
    });

fn sudachi_features<T>(morpheme: &Morpheme<'_, T>) -> FeatureMap
where
    T: DictionaryAccess,
{
    let pos_entries = SUDACHI_POS_KEY_ARCS.iter().enumerate().map(|(index, key)| {
        (
            Arc::clone(key),
            morpheme.part_of_speech().get(index).and_then(feature_value),
        )
    });
    let form_values = [
        feature_value(morpheme.dictionary_form()),
        feature_value(morpheme.normalized_form()),
        feature_value(morpheme.reading_form()),
    ];
    let form_entries = SUDACHI_FORM_KEY_ARCS
        .iter()
        .map(Arc::clone)
        .zip(form_values);
    FeatureMap::from_entries(pos_entries.chain(form_entries))
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
    fn chunking_prefers_sentence_boundaries_under_limit() {
        let chunks = sudachi_chunks("吾輩は猫である。名前はまだ無い。", 10_000);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "吾輩は猫である。");
        assert_eq!(chunks[0].byte_offset, 0);
        assert_eq!(chunks[0].char_offset, 0);
        assert!(!chunks[0].hard_split);
        assert_eq!(chunks[1].text, "名前はまだ無い。");
        assert_eq!(chunks[1].byte_offset, "吾輩は猫である。".len());
        assert_eq!(chunks[1].char_offset, "吾輩は猫である。".chars().count());
        assert!(!chunks[1].hard_split);
    }

    #[test]
    fn chunking_marks_hard_split_when_no_sentence_boundary_fits() {
        let chunks = sudachi_chunks("abcdef", 3);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "abc");
        assert!(chunks[0].hard_split);
        assert_eq!(chunks[1].text, "def");
        assert!(!chunks[1].hard_split);
    }

    #[test]
    fn chunking_keeps_adjacent_sentence_punctuation_together() {
        let chunks = sudachi_chunks("本当！？そう！！！はい。", 10_000);

        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].text, "本当！？");
        assert_eq!(chunks[1].text, "そう！！！");
        assert_eq!(chunks[2].text, "はい。");
    }

    #[test]
    fn chunking_does_not_split_decimal_points() {
        let chunks = sudachi_chunks("値は5.4です。値は５．４です。", 10_000);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "値は5.4です。");
        assert_eq!(chunks[1].text, "値は５．４です。");
    }

    #[test]
    fn offsets_chunk_analysis_spans_to_original_document() {
        let mut morphemes = build_morphemes_from_tokens(
            "sudachi-c",
            "work",
            "名前",
            vec![RawToken {
                emitted_surface: None,
                byte_span: Some(0..6),
                features: FeatureMap::new(),
            }],
        )
        .unwrap();

        offset_chunk_morphemes(&mut morphemes, 24, 8);

        assert_eq!(morphemes[0].byte_span, 24..30);
        assert_eq!(morphemes[0].char_span, 8..10);
    }

    #[test]
    #[ignore = "requires dictionary symlink"]
    fn sudachi_tokenizes_plaintext_with_env_dictionary() {
        let Some(dictionary_path) = std::env::var_os("AB_SUDACHI_DICT") else {
            println!("skipping Sudachi smoke test: AB_SUDACHI_DICT is not set");
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
            assert_eq!(analysis.source_text.as_ref(), doc.text.as_str());
            assert!(!analysis.morphemes.is_empty());
        }
    }

    #[test]
    fn sudachi_chunking_does_not_reuse_morpheme_features_across_chunks() {
        let Some(dictionary_path) = std::env::var_os("AB_SUDACHI_DICT") else {
            println!("skipping Sudachi chunk feature test: AB_SUDACHI_DICT is not set");
            return;
        };
        let analyzer = SudachiAnalyzer::from_dictionary_path(SudachiMode::C, dictionary_path)
            .expect("load Sudachi dictionary");
        let repeated_sentence = "吾輩は猫である。";
        let repetitions = (SUDACHI_HARD_CHUNK_BYTES / repeated_sentence.len()) + 2;
        let doc = PlainTextDocument {
            text_id: "chunked".to_owned(),
            source_format: SourceFormat::AozoraHonbun,
            text: repeated_sentence.repeat(repetitions),
        };

        let analysis = analyzer.analyze(&doc).expect("analyze chunked text");
        let lexical_blank_features = analysis
            .morphemes
            .iter()
            .filter(|morpheme| {
                !morpheme.surface.chars().all(char::is_whitespace)
                    && morpheme
                        .features
                        .get("pos1")
                        .and_then(|value| value.as_ref())
                        == Some(&"空白".into())
            })
            .take(5)
            .map(|morpheme| morpheme.surface.clone())
            .collect::<Vec<_>>();

        assert!(
            lexical_blank_features.is_empty(),
            "lexical morphemes inherited blank POS features: {lexical_blank_features:?}"
        );
    }
}
