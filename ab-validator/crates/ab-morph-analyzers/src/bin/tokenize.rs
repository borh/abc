//! `tokenize`: run one analyzer over the text on stdin and print every
//! morpheme with its dictionary features, one JSON object per line. This is
//! the command for checking by hand how an analyzer segments and tags a
//! phrase under a given dictionary. Vibrato dictionaries resolve through
//! AB_VIBRATO_DICT_DIR and Sudachi through AB_SUDACHI_DICT.

use std::io::{Read, Write};

use ab_morph_analyzers::{
    MorphAnalyzer, SudachiAnalyzer, SudachiMode, VaporettoAnalyzer, VibratoAnalyzer,
};
use ab_morph_diff::{Analysis, FeatureMap};
use ab_plaintext::{PlainTextDocument, SourceFormat};
use anyhow::{Context, Result, bail};
use clap::Parser;
use serde::Serialize;

#[derive(Debug, Parser)]
struct Args {
    /// One of `vibrato`, `vibrato:<dictionary>`, `vaporetto`,
    /// `vaporetto:<dictionary>`, `sudachi-a`, `sudachi-b` or `sudachi-c`.
    #[arg(long)]
    analyzer: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum AnalyzerSpec {
    Vibrato(Option<String>),
    Vaporetto(Option<String>),
    Sudachi(SudachiMode),
}

impl AnalyzerSpec {
    fn parse(value: &str) -> Result<Self> {
        if let Some(name) = value.strip_prefix("vibrato:") {
            if name.is_empty() {
                bail!("vibrato analyzer requires a dictionary name");
            }
            return Ok(Self::Vibrato(Some(name.to_owned())));
        }
        if let Some(name) = value.strip_prefix("vaporetto:") {
            if name.is_empty() {
                bail!("vaporetto analyzer requires a dictionary name");
            }
            return Ok(Self::Vaporetto(Some(name.to_owned())));
        }
        match value {
            "vibrato" => Ok(Self::Vibrato(None)),
            "vaporetto" => Ok(Self::Vaporetto(None)),
            "sudachi-a" => Ok(Self::Sudachi(SudachiMode::A)),
            "sudachi-b" => Ok(Self::Sudachi(SudachiMode::B)),
            "sudachi-c" => Ok(Self::Sudachi(SudachiMode::C)),
            other => bail!("unknown analyzer `{other}`"),
        }
    }

    fn load(&self) -> Result<Box<dyn MorphAnalyzer>> {
        Ok(match self {
            Self::Vibrato(None) => Box::new(VibratoAnalyzer::unidic_cwj_default()?),
            Self::Vibrato(Some(name)) => Box::new(VibratoAnalyzer::from_dictionary_name(name)?),
            Self::Vaporetto(None) => Box::new(VaporettoAnalyzer::unidic_cwj_default()?),
            Self::Vaporetto(Some(name)) => Box::new(VaporettoAnalyzer::from_dictionary_name(name)?),
            Self::Sudachi(mode) => {
                let dict = std::env::var_os("AB_SUDACHI_DICT")
                    .context("AB_SUDACHI_DICT is required for Sudachi analyzers")?;
                let dictionary = SudachiAnalyzer::load_dictionary("sudachi", dict)?;
                Box::new(SudachiAnalyzer::from_dictionary(*mode, dictionary))
            }
        })
    }
}

#[derive(Serialize)]
struct MorphemeRow<'a> {
    surface: &'a str,
    char_start: usize,
    char_end: usize,
    features: &'a FeatureMap,
}

fn main() -> Result<()> {
    let args = Args::parse();
    let analyzer = AnalyzerSpec::parse(&args.analyzer)?.load()?;
    let mut text = String::new();
    std::io::stdin()
        .read_to_string(&mut text)
        .context("failed to read stdin")?;
    let document = PlainTextDocument {
        text_id: "stdin".to_owned(),
        source_format: SourceFormat::AatVisibleText,
        text,
    };
    let analysis = analyzer.analyze(&document)?;
    let stderr = std::io::stderr();
    let mut stderr = stderr.lock();
    for warning in &analysis.warnings {
        serde_json::to_writer(&mut stderr, warning)?;
        stderr.write_all(b"\n")?;
    }
    let stdout = std::io::stdout();
    write_morphemes(&analysis, &mut stdout.lock())
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
    fn parses_every_analyzer_spelling() {
        assert_eq!(
            AnalyzerSpec::parse("vibrato").unwrap(),
            AnalyzerSpec::Vibrato(None)
        );
        assert_eq!(
            AnalyzerSpec::parse("vibrato:unidic-csj-202512").unwrap(),
            AnalyzerSpec::Vibrato(Some("unidic-csj-202512".to_owned()))
        );
        assert_eq!(
            AnalyzerSpec::parse("vaporetto:unidic-cwj-202512").unwrap(),
            AnalyzerSpec::Vaporetto(Some("unidic-cwj-202512".to_owned()))
        );
        assert_eq!(
            AnalyzerSpec::parse("sudachi-b").unwrap(),
            AnalyzerSpec::Sudachi(SudachiMode::B)
        );
        assert!(AnalyzerSpec::parse("vibrato:").is_err());
        assert!(AnalyzerSpec::parse("mecab").is_err());
    }

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
}
