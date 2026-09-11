//! `tokenize`: run one analyzer over text read from stdin and print every
//! morpheme with its dictionary features, one JSON object per line. This is
//! the command for checking by hand how an analyzer segments and tags a
//! phrase under a given dictionary, using the same analyzer code and
//! dictionary resolution as the corpus runs.

use std::io::Write;

use ab_morph_diff::{Analysis, FeatureMap};
use ab_plaintext::{PlainTextDocument, SourceFormat};
use anyhow::{Context, Result, bail};
use serde::Serialize;

use crate::{load_analyzers, parse_analyzer_specs};

#[derive(Serialize)]
struct MorphemeRow<'a> {
    surface: &'a str,
    char_start: usize,
    char_end: usize,
    features: &'a FeatureMap,
}

/// Tokenize `text` with exactly one analyzer and write one compact JSON
/// object per morpheme to `out`. Analyzer warnings go to stderr as JSON
/// lines so they never mix with the token stream.
///
/// # Errors
///
/// Returns an error when more than one analyzer is requested, the analyzer
/// cannot be loaded, analysis fails, or writing fails.
pub fn run_tokenize_text(analyzer_ids: &[String], text: &str, out: &mut impl Write) -> Result<()> {
    if analyzer_ids.len() != 1 {
        bail!(
            "tokenize requires exactly one --analyzer (got {})",
            analyzer_ids.len()
        );
    }
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer = analyzers
        .into_iter()
        .next()
        .context("internal error: no analyzer loaded")?;
    let document = PlainTextDocument {
        text_id: "stdin".to_owned(),
        source_format: SourceFormat::AatVisibleText,
        text: text.to_owned(),
    };
    let analysis = analyzer.analyze(&document)?;
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    for warning in &analysis.warnings {
        serde_json::to_writer(&mut stderr, warning)?;
        stderr.write_all(b"\n")?;
    }
    write_morphemes(&analysis, out)
}

fn write_morphemes(analysis: &Analysis, out: &mut impl Write) -> Result<()> {
    for morpheme in &analysis.morphemes {
        serde_json::to_writer(
            &mut *out,
            &MorphemeRow {
                surface: &morpheme.surface,
                char_start: morpheme.char_span.start,
                char_end: morpheme.char_span.end,
                features: &morpheme.features,
            },
        )?;
        out.write_all(b"\n")?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::sync::Arc;

    use ab_morph_diff::Morpheme;

    use super::*;

    #[test]
    fn writes_one_object_per_morpheme_with_features() {
        let analysis = Analysis {
            analyzer: "test".to_owned(),
            text_id: "stdin".to_owned(),
            source_text: Arc::from("今日"),
            morphemes: vec![
                Morpheme {
                    surface: "今".to_owned(),
                    byte_span: 0..3,
                    char_span: 0..1,
                    features: FeatureMap::from_entries([
                        (Arc::from("pos1"), Some(Arc::from("名詞"))),
                        (Arc::from("lemma"), None),
                    ]),
                },
                Morpheme {
                    surface: "日".to_owned(),
                    byte_span: 3..6,
                    char_span: 1..2,
                    features: FeatureMap::new(),
                },
            ],
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        };

        let mut out = Vec::new();
        write_morphemes(&analysis, &mut out).unwrap();

        assert_eq!(
            String::from_utf8(out).unwrap(),
            concat!(
                "{\"surface\":\"今\",\"char_start\":0,\"char_end\":1,",
                "\"features\":{\"lemma\":null,\"pos1\":\"名詞\"}}\n",
                "{\"surface\":\"日\",\"char_start\":1,\"char_end\":2,\"features\":{}}\n",
            )
        );
    }

    #[test]
    fn refuses_more_than_one_analyzer() {
        let error = run_tokenize_text(
            &["test:single".to_owned(), "test:split".to_owned()],
            "今日",
            &mut Vec::new(),
        )
        .unwrap_err();
        assert!(error.to_string().contains("exactly one --analyzer"));
    }
}
