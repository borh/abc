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

const SUDACHI_MAX_INPUT_BYTES: usize = 40_000;

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
        let dictionary = Self::load_dictionary(&analyzer_id, dictionary_path)?;

        Ok(Self::from_dictionary(mode, dictionary))
    }

    pub fn load_dictionary(
        analyzer_id: &str,
        dictionary_path: impl AsRef<Path>,
    ) -> Result<Arc<JapaneseDictionary>, AnalyzerError> {
        let dictionary_path = prepare_dictionary_path(analyzer_id, dictionary_path.as_ref())?;
        let config = Config::new(None, None, Some(dictionary_path)).map_err(|err| {
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
        let chunks = sudachi_chunks(&document.text, SUDACHI_MAX_INPUT_BYTES);
        let mut all_morphemes = Vec::new();

        for chunk in chunks {
            let mut analysis = self.analyze_chunk(document, &chunk)?;
            offset_chunk_analysis(&mut analysis, chunk.byte_offset, chunk.char_offset);
            all_morphemes.extend(analysis.morphemes);
        }

        Ok(Analysis {
            analyzer: self.analyzer_id.clone(),
            text_id: document.text_id.clone(),
            source_text: document.text.clone(),
            morphemes: all_morphemes,
        })
    }
}

impl SudachiAnalyzer {
    fn analyze_chunk(
        &self,
        document: &PlainTextDocument,
        chunk: &SudachiChunk<'_>,
    ) -> Result<Analysis, AnalyzerError> {
        let mut tokenizer =
            StatefulTokenizer::new(self.dictionary.as_ref(), self.mode.as_sudachi());
        tokenizer.set_subset(InfoSubset::empty());
        let mut morphemes = MorphemeList::empty(self.dictionary.as_ref());

        tokenizer.reset().push_str(chunk.text);
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
            chunk.text.to_owned(),
            tokens,
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SudachiChunk<'a> {
    text: &'a str,
    byte_offset: usize,
    char_offset: usize,
}

fn sudachi_chunks(text: &str, max_bytes: usize) -> Vec<SudachiChunk<'_>> {
    if text.is_empty() {
        return Vec::new();
    }

    let mut chunks = Vec::new();
    let mut start = 0usize;
    let mut char_offset = 0usize;

    while start < text.len() {
        let end = choose_chunk_end(text, start, max_bytes);
        let chunk_text = &text[start..end];
        let chunk_chars = chunk_text.chars().count();
        chunks.push(SudachiChunk {
            text: chunk_text,
            byte_offset: start,
            char_offset,
        });
        start = end;
        char_offset += chunk_chars;
    }

    chunks
}

fn choose_chunk_end(text: &str, start: usize, max_bytes: usize) -> usize {
    let hard_end = next_char_boundary_at_or_before(text, (start + max_bytes).min(text.len()));
    if hard_end == text.len() {
        return hard_end;
    }

    for predicate in [is_paragraph_boundary, is_sentence_boundary] {
        if let Some(end) = find_last_boundary(text, start, hard_end, predicate)
            && end > start
        {
            return end;
        }
    }

    hard_end.max(next_char_boundary_after(text, start))
}

fn find_last_boundary(
    text: &str,
    start: usize,
    hard_end: usize,
    predicate: fn(char) -> bool,
) -> Option<usize> {
    text[start..hard_end]
        .char_indices()
        .filter_map(|(relative_index, ch)| {
            if predicate(ch) {
                Some(start + relative_index + ch.len_utf8())
            } else {
                None
            }
        })
        .next_back()
}

fn is_paragraph_boundary(ch: char) -> bool {
    ch == '\n' || ch == '\r'
}

fn is_sentence_boundary(ch: char) -> bool {
    matches!(ch, '。' | '！' | '？' | '!' | '?')
}

fn next_char_boundary_at_or_before(text: &str, mut index: usize) -> usize {
    while index > 0 && !text.is_char_boundary(index) {
        index -= 1;
    }
    index
}

fn next_char_boundary_after(text: &str, mut index: usize) -> usize {
    index += 1;
    while index < text.len() && !text.is_char_boundary(index) {
        index += 1;
    }
    index.min(text.len())
}

fn offset_chunk_analysis(analysis: &mut Analysis, byte_offset: usize, char_offset: usize) {
    for morpheme in &mut analysis.morphemes {
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
    fn chunking_prefers_sentence_boundaries_under_limit() {
        let chunks = sudachi_chunks("吾輩は猫である。名前はまだ無い。", 24);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "吾輩は猫である。");
        assert_eq!(chunks[0].byte_offset, 0);
        assert_eq!(chunks[0].char_offset, 0);
        assert_eq!(chunks[1].text, "名前はまだ無い。");
        assert_eq!(chunks[1].byte_offset, "吾輩は猫である。".len());
        assert_eq!(chunks[1].char_offset, "吾輩は猫である。".chars().count());
    }

    #[test]
    fn offsets_chunk_analysis_spans_to_original_document() {
        let mut analysis = build_analysis_from_tokens(
            "sudachi-c".to_owned(),
            "work".to_owned(),
            "名前".to_owned(),
            vec![RawToken {
                emitted_surface: "名前".to_owned(),
                byte_span: Some(0..6),
                features: FeatureMap::new(),
            }],
        )
        .unwrap();

        offset_chunk_analysis(&mut analysis, 24, 8);

        assert_eq!(analysis.morphemes[0].byte_span, 24..30);
        assert_eq!(analysis.morphemes[0].char_span, 8..10);
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
