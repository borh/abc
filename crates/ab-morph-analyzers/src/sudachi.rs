use std::fs::{self, File};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ab_morph_diff::{Analysis, FeatureMap, Morpheme as DiffMorpheme};
use ab_plaintext::PlainTextDocument;
use sudachi::analysis::stateful_tokenizer::StatefulTokenizer;
use sudachi::analysis::stateless_tokenizer::DictionaryAccess;
use sudachi::config::Config;
use sudachi::dic::dictionary::JapaneseDictionary;
use sudachi::dic::subset::InfoSubset;
use sudachi::prelude::{Mode, Morpheme, MorphemeList};

use crate::features::feature_value;
use crate::span_builder::{RawToken, build_morphemes_from_tokens};
use crate::{AnalyzerError, MorphAnalyzer};

const SUDACHI_HARD_CHUNK_BYTES: usize = 32_000;

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
        let chunks = sudachi_chunks(&document.text, SUDACHI_HARD_CHUNK_BYTES);
        let mut all_morphemes = Vec::new();
        let mut hard_split_count = 0usize;
        let mut first_hard_split_offset = None;
        let mut tokenizer =
            StatefulTokenizer::new(self.dictionary.as_ref(), self.mode.as_sudachi());
        tokenizer.set_subset(InfoSubset::empty());
        let mut sudachi_morphemes = MorphemeList::empty(self.dictionary.as_ref());

        for chunk in chunks {
            if chunk.hard_split {
                hard_split_count += 1;
                first_hard_split_offset.get_or_insert(chunk.byte_offset);
            }
            let mut morphemes =
                self.analyze_chunk(document, &chunk, &mut tokenizer, &mut sudachi_morphemes)?;
            offset_chunk_morphemes(&mut morphemes, chunk.byte_offset, chunk.char_offset);
            all_morphemes.extend(morphemes);
        }
        if hard_split_count > 0 {
            eprintln!(
                "ab-morph-analyzers: analyzer={} text_id={} stage=sudachi_chunk warning=hard_split_without_sentence_boundary count={} first_byte_offset={} hard_limit_bytes={}",
                self.analyzer_id,
                document.text_id,
                hard_split_count,
                first_hard_split_offset.unwrap_or(0),
                SUDACHI_HARD_CHUNK_BYTES
            );
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
        tokenizer: &mut StatefulTokenizer<&JapaneseDictionary>,
        morphemes: &mut MorphemeList<&JapaneseDictionary>,
    ) -> Result<Vec<DiffMorpheme>, AnalyzerError> {
        tokenizer.reset().push_str(chunk.text);
        tokenizer
            .do_tokenize()
            .map_err(|err| AnalyzerError::Tokenize {
                analyzer: self.analyzer_id.clone(),
                message: err.to_string(),
            })?;
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
                emitted_surface: morpheme.surface().to_string(),
                byte_span: Some(morpheme.begin()..morpheme.end()),
                features: sudachi_features(&morpheme),
            }),
        )
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
struct SudachiChunk<'a> {
    text: &'a str,
    byte_offset: usize,
    char_offset: usize,
    hard_split: bool,
}

fn sudachi_chunks(text: &str, max_bytes: usize) -> Vec<SudachiChunk<'_>> {
    if text.is_empty() {
        return Vec::new();
    }

    let mut chunks = Vec::new();
    let mut start = 0usize;
    let mut char_offset = 0usize;

    while start < text.len() {
        let (end, hard_split) = choose_chunk_end(text, start, max_bytes);
        let chunk_text = &text[start..end];
        let chunk_chars = chunk_text.chars().count();
        chunks.push(SudachiChunk {
            text: chunk_text,
            byte_offset: start,
            char_offset,
            hard_split,
        });
        start = end;
        char_offset += chunk_chars;
    }

    chunks
}

fn choose_chunk_end(text: &str, start: usize, max_bytes: usize) -> (usize, bool) {
    let hard_end = next_char_boundary_at_or_before(text, (start + max_bytes).min(text.len()));
    if hard_end == text.len() {
        if let Some(end) = find_first_boundary(text, start, hard_end) {
            return (end, false);
        }
        return (hard_end, false);
    }

    if let Some(end) = find_first_boundary(text, start, hard_end)
        && end > start
    {
        return (end, false);
    }

    (hard_end.max(next_char_boundary_after(text, start)), true)
}

fn find_first_boundary(text: &str, start: usize, hard_end: usize) -> Option<usize> {
    let mut iter = text[start..hard_end].char_indices().peekable();
    while let Some((relative_index, ch)) = iter.next() {
        if !is_chunk_boundary(ch) {
            continue;
        }
        let mut end = start + relative_index + ch.len_utf8();
        while let Some((next_relative_index, next_ch)) = iter.peek().copied() {
            if !is_chunk_boundary(next_ch) {
                break;
            }
            iter.next();
            end = start + next_relative_index + next_ch.len_utf8();
        }
        return Some(end);
    }
    None
}

fn is_chunk_boundary(ch: char) -> bool {
    ch == '\n' || ch == '\r' || matches!(ch, '。' | '！' | '？' | '!' | '?')
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

fn sudachi_features<T>(morpheme: &Morpheme<'_, T>) -> FeatureMap
where
    T: DictionaryAccess,
{
    let mut features = FeatureMap::with_capacity(9);

    for (index, key) in ["pos1", "pos2", "pos3", "pos4", "c_type", "c_form"]
        .iter()
        .enumerate()
    {
        features.insert(
            (*key).into(),
            morpheme.part_of_speech().get(index).and_then(feature_value),
        );
    }

    features.insert(
        "dictionary_form".into(),
        feature_value(morpheme.dictionary_form()),
    );
    features.insert(
        "normalized_form".into(),
        feature_value(morpheme.normalized_form()),
    );
    features.insert(
        "reading_form".into(),
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
                emitted_surface: "名前".to_owned(),
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
