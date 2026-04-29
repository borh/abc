mod compact;
mod output;

use std::collections::BTreeSet;
use std::fs::{self, File, OpenOptions};
use std::io::Write;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use ab_morph_analyzers::{MorphAnalyzer, SudachiAnalyzer, SudachiMode, VibratoAnalyzer};
use ab_morph_diff::{Analysis, Comparison, compare_pair, compare_pair_compact_with_source_text};
use ab_plaintext::{PlainTextDocument, from_aat_value};
use anyhow::{Context, Result, bail};
use clap::ValueEnum;
use output::{open_output_writer, read_jsonl_or_zst_to_string};
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum OutputProfile {
    Full,
    Compact,
}

impl OutputProfile {
    fn as_str(self) -> &'static str {
        match self {
            OutputProfile::Full => "full",
            OutputProfile::Compact => "compact",
        }
    }
}

pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
    manifest_output: Option<&Path>,
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
    let input_file_count = inputs.len();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;

    if jobs > 1 {
        run_analyze_aat_parallel(
            inputs,
            analyzers,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            jobs,
            output_profile,
            examples_output,
            max_examples_per_comparison,
        )?;
    } else {
        run_analyze_aat_serial(
            inputs,
            &analyzers,
            analyses_output,
            comparisons_output,
            errors_output,
            resume,
            output_profile,
            examples_output,
            max_examples_per_comparison,
        )?;
    }

    if let Some(path) = manifest_output {
        write_manifest(
            path,
            output_profile,
            analyzer_ids,
            jobs,
            aat,
            aat_dir,
            input_file_count,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
        )?;
    }

    Ok(())
}

fn run_analyze_aat_serial(
    inputs: Vec<PathBuf>,
    analyzers: &[Arc<LoadedAnalyzer>],
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
) -> Result<()> {
    let resume_ids = if resume {
        read_resume_ids(analyses_output, errors_output, output_profile)?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_ids, output_profile)?;

    let mut analyses_writer = open_output_writer(analyses_output, resume)?;
    let mut comparisons_writer = if let Some(path) = comparisons_output {
        Some(open_output_writer(path, resume)?)
    } else {
        None
    };
    let mut examples_writer = if let Some(path) = examples_output {
        Some(open_output_writer(path, resume)?)
    } else {
        None
    };
    let mut errors_writer = if let Some(path) = errors_output {
        Some(open_output_writer(path, resume)?)
    } else {
        None
    };

    for input in inputs {
        let input_path = input.display().to_string();
        let source_id = compact::source_id_from_aat_path(&input);
        let aat = match read_aat_value(&input) {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut errors_writer {
                    write_error_row(
                        &mut **writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
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
                        &mut **writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
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
        let mut analyses = Vec::new();

        for analyzer in analyzers {
            let mut analysis = match analyzer.analyze(&document) {
                Ok(analysis) => analysis,
                Err(error) => {
                    if let Some(writer) = &mut errors_writer {
                        write_error_row(
                            &mut **writer,
                            &RunErrorRow {
                                input_path: input_path.clone(),
                                source_id: Some(source_id.clone()),
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

            write_analysis_row(&mut *analyses_writer, output_profile, &source_id, &analysis)?;
            if output_profile == OutputProfile::Compact {
                analysis.source_text.clear();
            }
            analyses.push(analysis);
        }

        if comparisons_writer.is_some() || examples_writer.is_some() {
            if let Err(error) = write_comparison_rows(
                comparisons_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                examples_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                &analyses,
                &source_id,
                &document.text,
                output_profile,
                max_examples_per_comparison,
            ) {
                if let Some(error_writer) = &mut errors_writer {
                    write_error_row(
                        &mut **error_writer,
                        &RunErrorRow {
                            input_path,
                            source_id: Some(source_id),
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
    if let Some(writer) = &mut examples_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut errors_writer {
        writer.flush()?;
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn run_analyze_aat_parallel(
    inputs: Vec<PathBuf>,
    analyzers: Vec<Arc<LoadedAnalyzer>>,
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    errors_output: Option<&Path>,
    resume: bool,
    jobs: usize,
    output_profile: OutputProfile,
    examples_output: Option<&Path>,
    max_examples_per_comparison: usize,
) -> Result<()> {
    let resume_ids = if resume {
        read_resume_ids(analyses_output, errors_output, output_profile)?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_ids, output_profile)?;
    let partitions = partition_inputs(inputs, jobs);
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
            let analyzers = analyzers.clone();
            let input_dir = temp_root.join(format!("inputs-{job_index}"));
            let output_dir = temp_root.join(format!("outputs-{job_index}"));
            handles.push(scope.spawn(move || -> Result<ShardOutput> {
                fs::create_dir_all(&input_dir)
                    .with_context(|| format!("failed to create {}", input_dir.display()))?;
                fs::create_dir_all(&output_dir)
                    .with_context(|| format!("failed to create {}", output_dir.display()))?;
                let mut shard_inputs = Vec::new();
                for input in partition {
                    let link = input_dir.join(input.file_name().ok_or_else(|| {
                        anyhow::anyhow!("missing file name for {}", input.display())
                    })?);
                    symlink_input_file(&input, &link)?;
                    shard_inputs.push(link);
                }

                let analyses = shard_output_path(&output_dir, analyses_output, "analyses");
                let comparisons = comparisons_output
                    .map(|path| shard_output_path(&output_dir, path, "comparisons"));
                let examples =
                    examples_output.map(|path| shard_output_path(&output_dir, path, "examples"));
                let errors =
                    errors_output.map(|path| shard_output_path(&output_dir, path, "errors"));
                run_analyze_aat_serial(
                    shard_inputs,
                    &analyzers,
                    &analyses,
                    comparisons.as_deref(),
                    errors.as_deref(),
                    false,
                    output_profile,
                    examples.as_deref(),
                    max_examples_per_comparison,
                )?;
                Ok(ShardOutput {
                    job_index,
                    analyses,
                    comparisons,
                    examples,
                    errors,
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
    if let Some(path) = examples_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.examples.as_deref()),
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
    examples: Option<PathBuf>,
    errors: Option<PathBuf>,
}

fn shard_output_path(output_dir: &Path, final_path: &Path, stem: &str) -> PathBuf {
    let file_name = final_path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or(stem);
    if file_name.ends_with(".jsonl.zst") {
        output_dir.join(format!("{stem}.jsonl.zst"))
    } else {
        output_dir.join(format!("{stem}.jsonl"))
    }
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
    resume_ids: &BTreeSet<String>,
    output_profile: OutputProfile,
) -> Result<Vec<PathBuf>> {
    if resume_ids.is_empty() {
        return Ok(inputs);
    }

    let mut filtered = Vec::new();
    for input in inputs {
        let should_skip = match output_profile {
            OutputProfile::Compact => {
                resume_ids.contains(&compact::source_id_from_aat_path(&input))
            }
            OutputProfile::Full => read_aat_value(&input)
                .ok()
                .and_then(|value| {
                    value
                        .get("work_id")
                        .and_then(Value::as_str)
                        .map(|text_id| resume_ids.contains(text_id))
                })
                .unwrap_or(false),
        };
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
    let mut output: Box<dyn Write> = if append {
        Box::new(
            OpenOptions::new()
                .create(true)
                .append(true)
                .open(output_path)
                .with_context(|| format!("failed to open {}", output_path.display()))?,
        )
    } else {
        Box::new(
            File::create(output_path)
                .with_context(|| format!("failed to create {}", output_path.display()))?,
        )
    };
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
            let mut paths = Vec::new();
            collect_aat_json_files(dir, &mut paths)?;
            paths.sort();
            if paths.is_empty() {
                bail!("no AAT JSON files found in {}", dir.display());
            }
            Ok(paths)
        }
        _ => bail!("provide exactly one of --aat or --aat-dir"),
    }
}

fn collect_aat_json_files(dir: &Path, paths: &mut Vec<PathBuf>) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let path = entry
            .with_context(|| format!("failed to read entry in {}", dir.display()))?
            .path();
        if path.is_dir() {
            collect_aat_json_files(&path, paths)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
            paths.push(path);
        }
    }
    Ok(())
}

fn load_analyzers(specs: &[AnalyzerSpec]) -> Result<Vec<Arc<LoadedAnalyzer>>> {
    let mut analyzers = Vec::new();

    for spec in specs {
        match spec {
            AnalyzerSpec::Vibrato => {
                analyzers.push(Arc::new(LoadedAnalyzer::Vibrato(
                    VibratoAnalyzer::unidic_cwj_default()?,
                )));
            }
            AnalyzerSpec::Sudachi(mode) => {
                let dict = std::env::var_os("AB_SUDACHI_DICT")
                    .context("AB_SUDACHI_DICT is required for Sudachi analyzers")?;
                analyzers.push(Arc::new(LoadedAnalyzer::Sudachi(
                    SudachiAnalyzer::from_dictionary_path(*mode, dict)?,
                )));
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

fn read_resume_ids(
    analyses_output: &Path,
    errors_output: Option<&Path>,
    output_profile: OutputProfile,
) -> Result<BTreeSet<String>> {
    let mut ids = BTreeSet::new();
    let prefer_source_id = output_profile == OutputProfile::Compact;
    read_resume_ids_from_path(analyses_output, prefer_source_id, &mut ids)?;
    if let Some(path) = errors_output {
        read_resume_ids_from_path(path, prefer_source_id, &mut ids)?;
    }
    Ok(ids)
}

fn read_resume_ids_from_path(
    path: &Path,
    prefer_source_id: bool,
    ids: &mut BTreeSet<String>,
) -> Result<()> {
    if !path.exists() {
        return Ok(());
    }

    let content = read_jsonl_or_zst_to_string(path)?;
    for (line_index, line) in content.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let value: Value = serde_json::from_str(line).with_context(|| {
            format!("failed to parse {} line {}", path.display(), line_index + 1)
        })?;
        let key = if prefer_source_id {
            value.get("source_id").and_then(Value::as_str)
        } else {
            value.get("text_id").and_then(Value::as_str)
        };
        if let Some(key) = key {
            ids.insert(key.to_owned());
        }
    }
    Ok(())
}

fn write_jsonl_row<T: Serialize, W: Write + ?Sized>(writer: &mut W, row: &T) -> Result<()> {
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
    source_id: Option<String>,
    text_id: Option<String>,
    analyzer: Option<String>,
    stage: String,
    error: String,
}

fn write_error_row<W: Write + ?Sized>(writer: &mut W, row: &RunErrorRow) -> Result<()> {
    write_jsonl_row(writer, row)
}

fn write_analysis_row<W: Write + ?Sized>(
    writer: &mut W,
    output_profile: OutputProfile,
    source_id: &str,
    analysis: &Analysis,
) -> Result<()> {
    match output_profile {
        OutputProfile::Full => {
            let row = AnalysisRow {
                text_id: analysis.text_id.clone(),
                analyzer: analysis.analyzer.clone(),
                analysis: analysis.clone(),
            };
            write_jsonl_row(writer, &row)
        }
        OutputProfile::Compact => {
            let row = compact::AnalysisSummaryRow::from_analysis(source_id.to_owned(), analysis);
            write_jsonl_row(writer, &row)
        }
    }
}

fn write_comparison_rows(
    mut writer: Option<&mut dyn Write>,
    mut examples_writer: Option<&mut dyn Write>,
    analyses: &[Analysis],
    source_id: &str,
    source_text: &str,
    output_profile: OutputProfile,
    max_examples_per_comparison: usize,
) -> Result<()> {
    for from_index in 0..analyses.len() {
        for to_index in (from_index + 1)..analyses.len() {
            match output_profile {
                OutputProfile::Full => {
                    let comparison = compare_pair(&analyses[from_index], &analyses[to_index], &[])?;
                    if let Some(writer) = writer.as_deref_mut() {
                        let row = ComparisonRow {
                            text_id: comparison.text_id.clone(),
                            from_analyzer: comparison.from_analyzer.clone(),
                            to_analyzer: comparison.to_analyzer.clone(),
                            comparison: comparison.clone(),
                        };
                        write_jsonl_row(writer, &row)?;
                    }

                    if let Some(examples_writer) = examples_writer.as_deref_mut() {
                        for row in compact::example_rows_from_comparison(
                            source_id.to_owned(),
                            source_text,
                            &comparison,
                            analyses,
                            max_examples_per_comparison,
                        ) {
                            write_jsonl_row(examples_writer, &row)?;
                        }
                    }
                }
                OutputProfile::Compact => {
                    let comparison = compare_pair_compact_with_source_text(
                        &analyses[from_index],
                        &analyses[to_index],
                        source_text,
                        &[],
                        max_examples_per_comparison,
                    )?;
                    if let Some(writer) = writer.as_deref_mut() {
                        let row = compact::ComparisonSummaryRow::from_compact_comparison(
                            source_id.to_owned(),
                            &comparison,
                        );
                        write_jsonl_row(writer, &row)?;
                    }

                    if let Some(examples_writer) = examples_writer.as_deref_mut() {
                        for row in compact::example_rows_from_compact_comparison(
                            source_id.to_owned(),
                            source_text,
                            &comparison,
                        ) {
                            write_jsonl_row(examples_writer, &row)?;
                        }
                    }
                }
            }
        }
    }
    Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_manifest(
    path: &Path,
    output_profile: OutputProfile,
    analyzer_ids: &[String],
    jobs: usize,
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    input_file_count: usize,
    analyses_output: &Path,
    comparisons_output: Option<&Path>,
    examples_output: Option<&Path>,
    errors_output: Option<&Path>,
) -> Result<()> {
    create_parent_dir(path)?;
    let manifest = compact::RunManifest {
        version: 1,
        output_profile: output_profile.as_str().to_owned(),
        analyzer_args: analyzer_ids.to_vec(),
        jobs,
        input_mode: if aat.is_some() { "aat" } else { "aat_dir" }.to_owned(),
        input_path: aat
            .or(aat_dir)
            .map(|path| path.display().to_string())
            .unwrap_or_default(),
        input_file_count,
        analyses_output: analyses_output.display().to_string(),
        comparisons_output: comparisons_output.map(|path| path.display().to_string()),
        examples_output: examples_output.map(|path| path.display().to_string()),
        errors_output: errors_output.map(|path| path.display().to_string()),
    };
    let file =
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?;
    let mut writer = std::io::BufWriter::new(file);
    serde_json::to_writer_pretty(&mut writer, &manifest)?;
    writeln!(writer)?;
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

    const TINY_AAT: &str = r#"{"version":1,"work_id":"source-a","blocks":[{"kind":"paragraph","content":[{"kind":"text","value":"吾輩は猫である。"}]}],"meta":{"adapter":"fixture","adapter_version":"fixture","source_encoding":"utf-8","source_hash":"sha256:0000000000000000000000000000000000000000000000000000000000000000","parse_complete":true,"warnings":[]}}"#;

    #[test]
    fn rejects_missing_input() {
        let err = run_default(None, None, &["vibrato".to_owned()]).unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_both_input_modes() {
        let err = run_default(
            Some(Path::new("a.json")),
            Some(Path::new("aat")),
            &["vibrato".to_owned()],
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_empty_analyzer_list() {
        let err = run_default(Some(Path::new("a.json")), None, &[]).unwrap_err();
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
            OutputProfile::Full,
            None,
            10,
            None,
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
    fn discovers_json_files_recursively_in_directory() {
        let dir = temp_dir("recursive");
        let nested = dir.join("aozora-rs-adapter");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("b.json"), "{}").unwrap();
        fs::write(dir.join("a.json"), "{}").unwrap();
        fs::write(nested.join("ignored.txt"), "{}").unwrap();

        let paths = discover_aat_inputs(None, Some(&dir)).unwrap();
        assert_eq!(paths, vec![dir.join("a.json"), nested.join("b.json")]);

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

        write_comparison_rows(
            Some(&mut out),
            None,
            &analyses,
            "source-a",
            "今日",
            OutputProfile::Full,
            10,
        )
        .unwrap();

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
                source_id: Some("source-a".to_owned()),
                text_id: Some("work".to_owned()),
                analyzer: Some("sudachi-c".to_owned()),
                stage: "analyze".to_owned(),
                error: "input too long".to_owned(),
            },
        )
        .unwrap();

        let row: serde_json::Value = serde_json::from_slice(&out).unwrap();
        assert_eq!(row["input_path"], "aat/work.json");
        assert_eq!(row["source_id"], "source-a");
        assert_eq!(row["text_id"], "work");
        assert_eq!(row["analyzer"], "sudachi-c");
        assert_eq!(row["stage"], "analyze");
        assert_eq!(row["error"], "input too long");
    }

    #[test]
    fn reads_resume_ids_from_existing_jsonl_outputs() {
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

        let ids = read_resume_ids(&analyses, Some(&errors), OutputProfile::Full).unwrap();
        assert!(ids.contains("done-analysis"));
        assert!(ids.contains("done-error"));
        assert!(!ids.contains("read_aat"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_resume_uses_source_id_from_zstd_outputs() {
        let dir = temp_dir("resume-zst");
        fs::create_dir_all(&dir).unwrap();
        let analyses = dir.join("analyses.jsonl.zst");
        {
            let mut writer = open_output_writer(&analyses, false).unwrap();
            writer
                .write_all(b"{\"source_id\":\"source-a\",\"text_id\":\"same\"}\n")
                .unwrap();
            writer.flush().unwrap();
        }

        let ids = read_resume_ids(&analyses, None, OutputProfile::Compact).unwrap();
        assert!(ids.contains("source-a"));
        assert!(!ids.contains("same"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_resume_filter_preserves_duplicate_text_ids() {
        let dir = temp_dir("resume-filter");
        fs::create_dir_all(&dir).unwrap();
        let first = dir.join("same-a.json");
        let second = dir.join("same-b.json");
        fs::write(&first, TINY_AAT).unwrap();
        fs::write(&second, TINY_AAT.replace("source-a", "source-b")).unwrap();
        let mut resume_ids = BTreeSet::new();
        resume_ids.insert("same-a".to_owned());

        let filtered = filter_resume_inputs(
            vec![first.clone(), second.clone()],
            &resume_ids,
            OutputProfile::Compact,
        )
        .unwrap();

        assert_eq!(filtered, vec![second]);
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

    #[test]
    fn compact_profile_writes_summary_rows_without_full_regions() {
        let dir = temp_dir("compact-profile");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let analyses = dir.join("analyses.jsonl");
        let comparisons = dir.join("comparisons.jsonl");
        let errors = dir.join("errors.jsonl");

        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["vibrato".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = fs::read_to_string(&analyses).unwrap();
        assert!(analysis_text.contains("\"source_id\":\"source-a\""));
        assert!(analysis_text.contains("\"morpheme_count\""));
        assert!(!analysis_text.contains("source_text"));

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_parallel_profile_writes_compressed_summary_rows() {
        let dir = temp_dir("compact-parallel");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        for name in ["source-a", "source-b"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }

        let analyses = dir.join("analyses.jsonl.zst");
        let comparisons = dir.join("comparisons.jsonl.zst");
        let errors = dir.join("errors.jsonl.zst");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["vibrato".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = read_jsonl_or_zst_to_string(&analyses).unwrap();
        assert_eq!(analysis_text.lines().count(), 2);
        assert!(analysis_text.contains("\"source_id\":\"source-a\""));
        assert!(analysis_text.contains("\"source_id\":\"source-b\""));
        assert!(!analysis_text.contains("source_text"));

        let error_text = read_jsonl_or_zst_to_string(&errors).unwrap();
        assert_eq!(error_text.lines().count(), 0);

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn compact_parallel_resume_appends_zstd_outputs_by_source_id() {
        let dir = temp_dir("compact-parallel-resume-zstd");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        for name in ["source-a", "source-b"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }

        let analyses = dir.join("analyses.jsonl.zst");
        let comparisons = dir.join("comparisons.jsonl.zst");
        let errors = dir.join("errors.jsonl.zst");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["vibrato".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            false,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        for name in ["source-c", "source-d"] {
            fs::write(aat_dir.join(format!("{name}.json")), tiny_aat(name)).unwrap();
        }
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["vibrato".to_owned()],
            &analyses,
            Some(&comparisons),
            Some(&errors),
            true,
            2,
            OutputProfile::Compact,
            None,
            10,
            None,
        )
        .unwrap();

        let analysis_text = read_jsonl_or_zst_to_string(&analyses).unwrap();
        assert_eq!(analysis_text.lines().count(), 4);
        for name in ["source-a", "source-b", "source-c", "source-d"] {
            assert_eq!(
                analysis_text
                    .matches(&format!("\"source_id\":\"{name}\""))
                    .count(),
                1
            );
        }

        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_manifest_for_compact_run() {
        let dir = temp_dir("manifest");
        let aat_dir = dir.join("aat");
        fs::create_dir_all(&aat_dir).unwrap();
        fs::write(aat_dir.join("source-a.json"), TINY_AAT).unwrap();

        let manifest = dir.join("manifest.json");
        run_analyze_aat(
            None,
            Some(&aat_dir),
            &["vibrato".to_owned()],
            &dir.join("analyses.jsonl"),
            Some(&dir.join("comparisons.jsonl")),
            Some(&dir.join("errors.jsonl")),
            false,
            1,
            OutputProfile::Compact,
            None,
            10,
            Some(&manifest),
        )
        .unwrap();

        let value: serde_json::Value =
            serde_json::from_str(&fs::read_to_string(&manifest).unwrap()).unwrap();
        assert_eq!(value["output_profile"], "compact");
        assert_eq!(value["jobs"], 1);
        assert_eq!(value["input_file_count"], 1);

        let _ = fs::remove_dir_all(dir);
    }

    fn run_default(
        aat: Option<&Path>,
        aat_dir: Option<&Path>,
        analyzer_ids: &[String],
    ) -> Result<()> {
        run_analyze_aat(
            aat,
            aat_dir,
            analyzer_ids,
            Path::new("out.jsonl"),
            None,
            None,
            false,
            1,
            OutputProfile::Full,
            None,
            10,
            None,
        )
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

    fn tiny_aat(work_id: &str) -> String {
        TINY_AAT.replace("source-a", work_id)
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
