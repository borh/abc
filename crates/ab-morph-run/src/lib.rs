use std::collections::BTreeSet;
use std::fs::{self, File, OpenOptions};
use std::io::{BufWriter, Write};
use std::path::{Path, PathBuf};

use ab_morph_analyzers::{MorphAnalyzer, SudachiAnalyzer, SudachiMode, VibratoAnalyzer};
use ab_morph_diff::{Analysis, Comparison, compare_pair};
use ab_plaintext::{PlainTextDocument, from_aat_value};
use anyhow::{Context, Result, bail};
use serde::Serialize;
use serde_json::Value;

pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }

    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let specs = parse_analyzer_specs(analyzer_ids)?;
    if jobs > 1 {
        return run_analyze_aat_parallel(
            inputs,
            specs,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            jobs,
        );
    }
    let analyzers = load_analyzers(&specs)?;
    let resume_text_ids = if resume {
        read_resume_text_ids(analyses_output, errors_output)?
    } else {
        BTreeSet::new()
    };

    create_parent_dir(analyses_output)?;
    let analyses_file = open_output_file(analyses_output, resume)?;
    let mut analyses_writer = BufWriter::new(analyses_file);

    let mut comparisons_writer = if let Some(path) = comparisons_output {
        create_parent_dir(path)?;
        let file = open_output_file(path, resume)?;
        Some(BufWriter::new(file))
    } else {
        None
    };

    let mut errors_writer = if let Some(path) = errors_output {
        create_parent_dir(path)?;
        let file = open_output_file(path, resume)?;
        Some(BufWriter::new(file))
    } else {
        None
    };

    for input in inputs {
        let input_path = input.display().to_string();
        let aat = match read_aat_value(&input) {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        writer,
                        &RunErrorRow {
                            input_path,
                            text_id: None,
                            analyzer: None,
                            stage: "read_aat".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                    continue;
                }
                return Err(error);
            }
        };
        let document = match from_aat_value(&aat) {
            Ok(document) => document,
            Err(error) => {
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        writer,
                        &RunErrorRow {
                            input_path,
                            text_id: None,
                            analyzer: None,
                            stage: "project_aat".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                    continue;
                }
                return Err(error.into());
            }
        };
        if resume_text_ids.contains(&document.text_id) {
            continue;
        }
        let mut analyses = Vec::new();

        for analyzer in &analyzers {
            let analysis = match analyzer.analyze(&document) {
                Ok(analysis) => analysis,
                Err(error) => {
                    if let Some(writer) = &mut errors_writer {
                        write_error_row(
                            writer,
                            &RunErrorRow {
                                input_path: input_path.clone(),
                                text_id: Some(document.text_id.clone()),
                                analyzer: Some(analyzer.analyzer_id().to_owned()),
                                stage: "analyze".to_owned(),
                                error: error.to_string(),
                            },
                        )?;
                        continue;
                    }
                    return Err(error);
                }
            };
            let row = AnalysisRow {
                text_id: analysis.text_id.clone(),
                analyzer: analysis.analyzer.clone(),
                analysis: analysis.clone(),
            };
            write_jsonl_row(&mut analyses_writer, &row)?;
            analyses.push(analysis);
        }

        if let Some(writer) = &mut comparisons_writer {
            if let Err(error) = write_comparison_rows(writer, &analyses) {
                if let Some(error_writer) = &mut errors_writer {
                    write_error_row(
                        error_writer,
                        &RunErrorRow {
                            input_path,
                            text_id: Some(document.text_id),
                            analyzer: None,
                            stage: "compare".to_owned(),
                            error: error.to_string(),
                        },
                    )?;
                } else {
                    return Err(error);
                }
            }
        }
    }

    analyses_writer.flush()?;
    if let Some(writer) = &mut comparisons_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut errors_writer {
        writer.flush()?;
    }
    Ok(())
}

fn run_analyze_aat_parallel(
    inputs: Vec<PathBuf>,
    specs: Vec<AnalyzerSpec>,
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
) -> Result<()> {
    let resume_text_ids = if resume {
        read_resume_text_ids(analyses_output, errors_output)?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_text_ids)?;
    let partitions = partition_inputs(inputs, jobs);
    let analyzer_ids = specs
        .iter()
        .map(AnalyzerSpec::as_arg)
        .map(str::to_owned)
        .collect::<Vec<_>>();
    let temp_root = std::env::temp_dir().join(format!(
        "ab-morph-run-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
    fs::create_dir_all(&temp_root)
        .with_context(|| format!("failed to create {}", temp_root.display()))?;

    let result = std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for (job_index, partition) in partitions.into_iter().enumerate() {
            if partition.is_empty() {
                continue;
            }
            let analyzer_ids = analyzer_ids.clone();
            let input_dir = temp_root.join(format!("inputs-{job_index}"));
            let output_dir = temp_root.join(format!("outputs-{job_index}"));
            handles.push(scope.spawn(move || -> Result<ShardOutput> {
                fs::create_dir_all(&input_dir)
                    .with_context(|| format!("failed to create {}", input_dir.display()))?;
                fs::create_dir_all(&output_dir)
                    .with_context(|| format!("failed to create {}", output_dir.display()))?;
                for input in partition {
                    let link = input_dir.join(input.file_name().ok_or_else(|| {
                        anyhow::anyhow!("missing file name for {}", input.display())
                    })?);
                    symlink_input_file(&input, &link)?;
                }

                let analyses = output_dir.join("analyses.jsonl");
                let comparisons = output_dir.join("comparisons.jsonl");
                let errors = output_dir.join("errors.jsonl");
                run_analyze_aat(
                    None,
                    Some(&input_dir),
                    &analyzer_ids,
                    &analyses,
                    comparisons_output.map(|_| comparisons.as_path()),
                    errors_output.map(|_| errors.as_path()),
                    false,
                    1,
                )?;
                Ok(ShardOutput {
                    job_index,
                    analyses,
                    comparisons: comparisons_output.map(|_| comparisons),
                    errors: errors_output.map(|_| errors),
                })
            }));
        }

        let mut outputs = Vec::new();
        for handle in handles {
            outputs.push(handle.join().expect("morph worker panicked")?);
        }
        Ok::<_, anyhow::Error>(outputs)
    });

    let mut outputs = match result {
        Ok(outputs) => outputs,
        Err(error) => {
            let _ = fs::remove_dir_all(&temp_root);
            return Err(error);
        }
    };
    outputs.sort_by_key(|output| output.job_index);

    merge_shard_files(
        outputs.iter().map(|output| output.analyses.as_path()),
        analyses_output,
        resume,
    )?;
    if let Some(path) = comparisons_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.comparisons.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = errors_output {
        merge_shard_files(
            outputs.iter().filter_map(|output| output.errors.as_deref()),
            path,
            resume,
        )?;
    }

    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
    Ok(())
}

struct ShardOutput {
    job_index: usize,
    analyses: PathBuf,
    comparisons: Option<PathBuf>,
    errors: Option<PathBuf>,
}

fn partition_inputs(inputs: Vec<PathBuf>, jobs: usize) -> Vec<Vec<PathBuf>> {
    let mut partitions = vec![Vec::new(); jobs];
    for (index, input) in inputs.into_iter().enumerate() {
        partitions[index % jobs].push(input);
    }
    partitions
}

fn symlink_input_file(input: &Path, link: &Path) -> Result<()> {
    let target = input
        .canonicalize()
        .with_context(|| format!("failed to canonicalize {}", input.display()))?;
    std::os::unix::fs::symlink(&target, link).with_context(|| {
        format!(
            "failed to symlink {} to {}",
            target.display(),
            link.display()
        )
    })
}

fn filter_resume_inputs(
    inputs: Vec<PathBuf>,
    resume_text_ids: &BTreeSet<String>,
) -> Result<Vec<PathBuf>> {
    if resume_text_ids.is_empty() {
        return Ok(inputs);
    }

    let mut filtered = Vec::new();
    for input in inputs {
        let should_skip = read_aat_value(&input)
            .ok()
            .and_then(|value| {
                value
                    .get("work_id")
                    .and_then(Value::as_str)
                    .map(|text_id| resume_text_ids.contains(text_id))
            })
            .unwrap_or(false);
        if !should_skip {
            filtered.push(input);
        }
    }
    Ok(filtered)
}

fn merge_shard_files<'a>(
    shard_paths: impl IntoIterator<Item = &'a Path>,
    output_path: &Path,
    append: bool,
) -> Result<()> {
    create_parent_dir(output_path)?;
    let mut output = open_output_file(output_path, append)?;
    for shard_path in shard_paths {
        if !shard_path.exists() {
            continue;
        }
        let mut input = File::open(shard_path)
            .with_context(|| format!("failed to open {}", shard_path.display()))?;
        std::io::copy(&mut input, &mut output)?;
    }
    output.flush()?;
    Ok(())
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AnalyzerSpec {
    Vibrato,
    Sudachi(SudachiMode),
}

impl AnalyzerSpec {
    fn as_arg(&self) -> &'static str {
        match self {
            Self::Vibrato => "vibrato",
            Self::Sudachi(SudachiMode::A) => "sudachi-a",
            Self::Sudachi(SudachiMode::B) => "sudachi-b",
            Self::Sudachi(SudachiMode::C) => "sudachi-c",
        }
    }

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
                bail!(
                    "--aat-dir must point to an existing directory: {}",
                    dir.display()
                );
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
                analyzers.push(LoadedAnalyzer::Vibrato(
                    VibratoAnalyzer::unidic_cwj_default()?,
                ));
            }
            AnalyzerSpec::Sudachi(mode) => {
                let dict = std::env::var_os("AB_SUDACHI_DICT")
                    .context("AB_SUDACHI_DICT is required for Sudachi analyzers")?;
                analyzers.push(LoadedAnalyzer::Sudachi(
                    SudachiAnalyzer::from_dictionary_path(*mode, dict)?,
                ));
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
    if let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    Ok(())
}

fn open_output_file(path: &Path, append: bool) -> Result<File> {
    if append {
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .with_context(|| format!("failed to open {}", path.display()))
    } else {
        File::create(path).with_context(|| format!("failed to create {}", path.display()))
    }
}

fn read_resume_text_ids(
    analyses_output: &Path,
    errors_output: Option<&Path>,
) -> Result<BTreeSet<String>> {
    let mut ids = BTreeSet::new();
    read_text_ids_from_jsonl(analyses_output, &mut ids)?;
    if let Some(path) = errors_output {
        read_text_ids_from_jsonl(path, &mut ids)?;
    }
    Ok(ids)
}

fn read_text_ids_from_jsonl(path: &Path, ids: &mut BTreeSet<String>) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }

    let content =
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))?;
    for (line_index, line) in content.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let value: Value = serde_json::from_str(line).with_context(|| {
            format!("failed to parse {} line {}", path.display(), line_index + 1)
        })?;
        if let Some(text_id) = value.get("text_id").and_then(Value::as_str) {
            ids.insert(text_id.to_owned());
        }
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

#[derive(Serialize)]
struct ComparisonRow {
    text_id: String,
    from_analyzer: String,
    to_analyzer: String,
    comparison: Comparison,
}

#[derive(Serialize)]
struct RunErrorRow {
    input_path: String,
    text_id: Option<String>,
    analyzer: Option<String>,
    stage: String,
    error: String,
}

fn write_error_row(writer: &mut impl Write, row: &RunErrorRow) -> Result<()> {
    write_jsonl_row(writer, row)
}

fn write_comparison_rows(writer: &mut impl Write, analyses: &[Analysis]) -> Result<()> {
    for from_index in 0..analyses.len() {
        for to_index in (from_index + 1)..analyses.len() {
            let comparison = compare_pair(&analyses[from_index], &analyses[to_index], &[])?;
            let row = ComparisonRow {
                text_id: comparison.text_id.clone(),
                from_analyzer: comparison.from_analyzer.clone(),
                to_analyzer: comparison.to_analyzer.clone(),
                comparison,
            };
            write_jsonl_row(writer, &row)?;
        }
    }
    Ok(())
}

enum LoadedAnalyzer {
    Vibrato(VibratoAnalyzer),
    Sudachi(SudachiAnalyzer),
}

impl LoadedAnalyzer {
    fn analyzer_id(&self) -> &str {
        match self {
            Self::Vibrato(analyzer) => analyzer.analyzer_id(),
            Self::Sudachi(analyzer) => analyzer.analyzer_id(),
        }
    }

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

    use ab_morph_diff::{FeatureMap, Morpheme};

    use super::*;

    #[test]
    fn rejects_missing_input() {
        let err = run_analyze_aat(
            None,
            None,
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
            None,
            false,
            1,
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
            None,
            false,
            1,
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
            None,
            false,
            1,
        )
        .unwrap_err();
        assert!(err.to_string().contains("at least one"));
    }

    #[test]
    fn rejects_zero_jobs() {
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            None,
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
            None,
            false,
            0,
        )
        .unwrap_err();
        assert!(err.to_string().contains("at least 1"));
    }

    #[test]
    fn parses_analyzer_specs() {
        assert_eq!(
            AnalyzerSpec::parse("vibrato").unwrap(),
            AnalyzerSpec::Vibrato
        );
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

        assert_eq!(
            specs,
            vec![AnalyzerSpec::Vibrato, AnalyzerSpec::Sudachi(SudachiMode::C)]
        );
    }

    #[test]
    fn discovers_single_aat_file() {
        let dir = temp_dir("single");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("work.json");
        fs::write(&file, "{}").unwrap();

        assert_eq!(
            discover_aat_inputs(Some(&file), None).unwrap(),
            vec![file.clone()]
        );

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

    #[test]
    fn writes_one_comparison_row_for_two_valid_analyses() {
        let analyses = vec![valid_analysis("from"), valid_analysis("to")];
        let mut out = Vec::new();

        write_comparison_rows(&mut out, &analyses).unwrap();

        let lines = String::from_utf8(out).unwrap();
        let rows = lines.lines().collect::<Vec<_>>();
        assert_eq!(rows.len(), 1);
        assert!(rows[0].contains("\"text_id\":\"t1\""));
        assert!(rows[0].contains("\"from_analyzer\":\"from\""));
        assert!(rows[0].contains("\"to_analyzer\":\"to\""));
    }

    #[test]
    fn writes_error_row_for_failed_analyzer() {
        let mut out = Vec::new();
        write_error_row(
            &mut out,
            &RunErrorRow {
                input_path: "aat/work.json".to_owned(),
                text_id: Some("work".to_owned()),
                analyzer: Some("sudachi-c".to_owned()),
                stage: "analyze".to_owned(),
                error: "input too long".to_owned(),
            },
        )
        .unwrap();

        let row: serde_json::Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(row["input_path"], "aat/work.json");
        assert_eq!(row["text_id"], "work");
        assert_eq!(row["analyzer"], "sudachi-c");
        assert_eq!(row["stage"], "analyze");
        assert_eq!(row["error"], "input too long");
    }

    #[test]
    fn reads_resume_text_ids_from_existing_jsonl_outputs() {
        let dir = temp_dir("resume");
        fs::create_dir_all(&dir).unwrap();
        let analyses = dir.join("analyses.jsonl");
        let errors = dir.join("errors.jsonl");
        fs::write(
            &analyses,
            "{\"text_id\":\"done-analysis\",\"analyzer\":\"vibrato\"}\n",
        )
        .unwrap();
        fs::write(
            &errors,
            "{\"text_id\":\"done-error\",\"stage\":\"analyze\"}\n{\"stage\":\"read_aat\"}\n",
        )
        .unwrap();

        let ids = read_resume_text_ids(&analyses, Some(&errors)).unwrap();
        assert!(ids.contains("done-analysis"));
        assert!(ids.contains("done-error"));
        assert!(!ids.contains("read_aat"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn partitions_inputs_round_robin_by_job() {
        let inputs = (0..7)
            .map(|index| PathBuf::from(format!("work-{index}.json")))
            .collect::<Vec<_>>();

        let partitions = partition_inputs(inputs, 3);

        assert_eq!(
            partitions,
            vec![
                vec![
                    PathBuf::from("work-0.json"),
                    PathBuf::from("work-3.json"),
                    PathBuf::from("work-6.json"),
                ],
                vec![PathBuf::from("work-1.json"), PathBuf::from("work-4.json")],
                vec![PathBuf::from("work-2.json"), PathBuf::from("work-5.json")],
            ]
        );
    }

    #[test]
    fn symlink_input_file_uses_readable_absolute_target() {
        let dir = temp_dir("symlink");
        let source_dir = dir.join("source");
        let link_dir = dir.join("links");
        fs::create_dir_all(&source_dir).unwrap();
        fs::create_dir_all(&link_dir).unwrap();
        let source = source_dir.join("work.json");
        let link = link_dir.join("work.json");
        fs::write(&source, "{}").unwrap();

        symlink_input_file(&source, &link).unwrap();

        assert_eq!(fs::read_to_string(&link).unwrap(), "{}");

        let _ = fs::remove_dir_all(dir);
    }

    fn valid_analysis(analyzer: &str) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t1".to_owned(),
            source_text: "今日".to_owned(),
            morphemes: vec![Morpheme {
                surface: "今日".to_owned(),
                byte_span: 0..6,
                char_span: 0..2,
                features: FeatureMap::new(),
            }],
        }
    }

    fn temp_dir(label: &str) -> PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
