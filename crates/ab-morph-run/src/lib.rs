use std::collections::BTreeSet;
use std::fs::{self, File};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use ab_morph_analyzers::{MorphAnalyzer, SudachiAnalyzer, SudachiMode, VibratoAnalyzer};
use ab_morph_diff::Analysis;
use ab_plaintext::{PlainTextDocument, from_aat_value};
use anyhow::{Context, Result, bail};
use serde::Serialize;
use serde_json::Value;

pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    analyses_output: &Path,
    _comparisons_output: Option<&Path>,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }

    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;

    create_parent_dir(analyses_output)?;
    let analyses_file = File::create(analyses_output)
        .with_context(|| format!("failed to create {}", analyses_output.display()))?;
    let mut analyses_writer = BufWriter::new(analyses_file);

    for input in inputs {
        let aat = read_aat_value(&input)?;
        let document = from_aat_value(&aat)?;
        for analyzer in &analyzers {
            let analysis = analyzer.analyze(&document)?;
            let row = AnalysisRow {
                text_id: analysis.text_id.clone(),
                analyzer: analysis.analyzer.clone(),
                analysis,
            };
            write_jsonl_row(&mut analyses_writer, &row)?;
        }
    }

    analyses_writer.flush()?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnalyzerSpec {
    Vibrato,
    Sudachi(SudachiMode),
}

impl AnalyzerSpec {
    fn parse(value: &str) -> Result<Self> {
        match value {
            "vibrato" => Ok(Self::Vibrato),
            "sudachi-a" => Ok(Self::Sudachi(SudachiMode::A)),
            "sudachi-b" => Ok(Self::Sudachi(SudachiMode::B)),
            "sudachi-c" => Ok(Self::Sudachi(SudachiMode::C)),
            other => bail!("unknown analyzer `{other}`"),
        }
    }
}

fn parse_analyzer_specs(values: &[String]) -> Result<Vec<AnalyzerSpec>> {
    let mut seen = BTreeSet::new();
    let mut specs = Vec::new();

    for value in values {
        if seen.insert(value.clone()) {
            specs.push(AnalyzerSpec::parse(value)?);
        }
    }

    Ok(specs)
}

fn discover_aat_inputs(aat: Option<&Path>, aat_dir: Option<&Path>) -> Result<Vec<PathBuf>> {
    match (aat, aat_dir) {
        (Some(path), None) => {
            if !path.is_file() {
                bail!("--aat must point to a regular file: {}", path.display());
            }
            Ok(vec![path.to_owned()])
        }
        (None, Some(dir)) => {
            if !dir.is_dir() {
                bail!("--aat-dir must point to an existing directory: {}", dir.display());
            }
            let mut paths = fs::read_dir(dir)
                .with_context(|| format!("failed to read {}", dir.display()))?
                .map(|entry| entry.map(|entry| entry.path()))
                .collect::<std::io::Result<Vec<_>>>()?;
            paths.retain(|path| path.extension().and_then(|ext| ext.to_str()) == Some("json"));
            paths.sort();
            if paths.is_empty() {
                bail!("no AAT JSON files found in {}", dir.display());
            }
            Ok(paths)
        }
        _ => bail!("provide exactly one of --aat or --aat-dir"),
    }
}

fn load_analyzers(specs: &[AnalyzerSpec]) -> Result<Vec<LoadedAnalyzer>> {
    let mut analyzers = Vec::new();

    for spec in specs {
        match spec {
            AnalyzerSpec::Vibrato => {
                analyzers.push(LoadedAnalyzer::Vibrato(VibratoAnalyzer::unidic_cwj_default()?));
            }
            AnalyzerSpec::Sudachi(mode) => {
                let dict = std::env::var_os("AB_SUDACHI_DICT")
                    .context("AB_SUDACHI_DICT is required for Sudachi analyzers")?;
                analyzers.push(LoadedAnalyzer::Sudachi(SudachiAnalyzer::from_dictionary_path(
                    *mode, dict,
                )?));
            }
        }
    }

    Ok(analyzers)
}

fn read_aat_value(path: &Path) -> Result<Value> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    serde_json::from_reader(file).with_context(|| format!("failed to parse {}", path.display()))
}

fn create_parent_dir(path: &Path) -> Result<()> {
    if let Some(parent) = path.parent().filter(|parent| !parent.as_os_str().is_empty()) {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    Ok(())
}

fn write_jsonl_row<T: Serialize>(writer: &mut impl Write, row: &T) -> Result<()> {
    serde_json::to_writer(&mut *writer, row)?;
    writer.write_all(b"\n")?;
    Ok(())
}

#[derive(Serialize)]
struct AnalysisRow {
    text_id: String,
    analyzer: String,
    analysis: Analysis,
}

enum LoadedAnalyzer {
    Vibrato(VibratoAnalyzer),
    Sudachi(SudachiAnalyzer),
}

impl LoadedAnalyzer {
    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis> {
        match self {
            Self::Vibrato(analyzer) => Ok(analyzer.analyze(document)?),
            Self::Sudachi(analyzer) => Ok(analyzer.analyze(document)?),
        }
    }
}

#[cfg(test)]
mod tests {
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn rejects_missing_input() {
        let err = run_analyze_aat(
            None,
            None,
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_both_input_modes() {
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            Some(Path::new("aat")),
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_empty_analyzer_list() {
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            None,
            &[],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("at least one"));
    }

    #[test]
    fn parses_analyzer_specs() {
        assert_eq!(AnalyzerSpec::parse("vibrato").unwrap(), AnalyzerSpec::Vibrato);
        assert_eq!(
            AnalyzerSpec::parse("sudachi-c").unwrap(),
            AnalyzerSpec::Sudachi(SudachiMode::C)
        );
    }

    #[test]
    fn rejects_unknown_analyzer_spec() {
        let err = AnalyzerSpec::parse("unknown").unwrap_err();
        assert!(err.to_string().contains("unknown analyzer"));
    }

    #[test]
    fn dedupes_analyzer_specs_in_first_seen_order() {
        let specs = parse_analyzer_specs(&[
            "vibrato".to_owned(),
            "sudachi-c".to_owned(),
            "vibrato".to_owned(),
        ])
        .unwrap();

        assert_eq!(specs, vec![AnalyzerSpec::Vibrato, AnalyzerSpec::Sudachi(SudachiMode::C)]);
    }

    #[test]
    fn discovers_single_aat_file() {
        let dir = temp_dir("single");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("work.json");
        fs::write(&file, "{}").unwrap();

        assert_eq!(discover_aat_inputs(Some(&file), None).unwrap(), vec![file.clone()]);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn discovers_sorted_json_files_in_directory() {
        let dir = temp_dir("sorted");
        fs::create_dir_all(&dir).unwrap();
        fs::write(dir.join("b.json"), "{}").unwrap();
        fs::write(dir.join("a.json"), "{}").unwrap();
        fs::write(dir.join("ignored.txt"), "{}").unwrap();

        let paths = discover_aat_inputs(None, Some(&dir)).unwrap();
        assert_eq!(paths, vec![dir.join("a.json"), dir.join("b.json")]);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn rejects_empty_aat_directory() {
        let dir = temp_dir("empty");
        fs::create_dir_all(&dir).unwrap();

        let err = discover_aat_inputs(None, Some(&dir)).unwrap_err();
        assert!(err.to_string().contains("no AAT JSON files"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn rejects_non_file_aat_path() {
        let dir = temp_dir("not-file");
        fs::create_dir_all(&dir).unwrap();

        let err = discover_aat_inputs(Some(&dir), None).unwrap_err();
        assert!(err.to_string().contains("regular file"));

        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!("ab-morph-run-{label}-{}-{unique}", std::process::id()))
    }
}
