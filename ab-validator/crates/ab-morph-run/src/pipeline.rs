use super::*;
use crate::options::OrthoDetectMode;
use crate::output::for_each_jsonl_or_zst_line;

/// Lets the ortho-detect heuristic tokenize through a vibrato analyzer that
/// the run already loaded, instead of loading the dictionary a second time.
struct SharedVibratoOrthoTokenizer(Arc<LoadedAnalyzer>);

impl ab_ortho_detect::OrthoTokenizer for SharedVibratoOrthoTokenizer {
    fn tokenize(&self, text: &str) -> Vec<ab_ortho_detect::OrthoToken> {
        match self.0.as_ref() {
            LoadedAnalyzer::Vibrato(analyzer) => {
                ab_ortho_detect::OrthoTokenizer::tokenize(analyzer, text)
            }
            _ => unreachable!("SharedVibratoOrthoTokenizer wraps a vibrato analyzer"),
        }
    }
}

#[cfg(test)]
thread_local! {
    static DETECTOR_BUILD_COUNT: std::cell::Cell<usize> = const { std::cell::Cell::new(0) };
    static TEST_ORTHO_DETECTOR: std::cell::RefCell<Option<PreparedOrthoDetector>> = const {
        std::cell::RefCell::new(None)
    };
}

fn build_heuristic_detector(analyzers: &[Arc<LoadedAnalyzer>]) -> Result<Arc<dyn OrthoDetector>> {
    let shared_vibrato = analyzers
        .iter()
        .find(|analyzer| {
            matches!(analyzer.as_ref(), LoadedAnalyzer::Vibrato(_))
                && analyzer.analyzer_id() == ab_morph_analyzers::DEFAULT_VIBRATO_ANALYZER_ID
        })
        .cloned();
    let vibrato: Arc<dyn ab_ortho_detect::OrthoTokenizer> = match shared_vibrato {
        Some(analyzer) => Arc::new(SharedVibratoOrthoTokenizer(analyzer)),
        None => Arc::new(ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default()?),
    };
    Ok(Arc::new(ab_ortho_detect::heuristic::HeuristicV1::new(
        vibrato,
        ab_ortho_detect::heuristic::HeuristicConfig::default(),
    )))
}

fn build_ml_detector(ml_model: Option<&Path>) -> Result<Arc<dyn OrthoDetector>> {
    let path = ml_model.ok_or_else(|| {
        anyhow::anyhow!(
            "--ortho-ml-model is required for --ortho-detect=ml (this should have been caught at CLI parse)"
        )
    })?;
    let model = ab_ortho_detect::ml::MlLogisticRegression::load(path).map_err(|error| {
        anyhow::anyhow!("failed to load ML model from {}: {error}", path.display())
    })?;
    Ok(Arc::new(model))
}

fn build_historical_detector() -> Result<Arc<dyn OrthoDetector>> {
    let oracle = Arc::new(ab_morph_analyzers::VibratoAnalyzer::from_dictionary_name(
        crate::M2_ORACLE_DICTIONARY,
    )?);
    let detector = ab_morph_analyzers::historical_rewrite_detector(oracle)
        .context("failed to build M2 historical detector")?;
    Ok(Arc::new(detector))
}

fn prepare_ortho_detector(
    analyzers: &[Arc<LoadedAnalyzer>],
    mode: OrthoDetectMode,
    ml_model: Option<&Path>,
) -> Result<PreparedOrthoDetector> {
    #[cfg(test)]
    DETECTOR_BUILD_COUNT.with(|count| count.set(count.get() + 1));
    #[cfg(test)]
    if let Some(detector) = TEST_ORTHO_DETECTOR.with(|slot| slot.borrow().clone()) {
        return Ok(detector);
    }

    let detector = match mode {
        OrthoDetectMode::Off => None,
        OrthoDetectMode::Heuristic => Some(build_heuristic_detector(analyzers)?),
        OrthoDetectMode::Ml => Some(build_ml_detector(ml_model)?),
        OrthoDetectMode::Historical => Some(build_historical_detector()?),
    };
    Ok(PreparedOrthoDetector(detector))
}

#[cfg(test)]
pub(crate) fn reset_detector_build_count() {
    DETECTOR_BUILD_COUNT.with(|count| count.set(0));
}

#[cfg(test)]
pub(crate) fn detector_build_count() -> usize {
    DETECTOR_BUILD_COUNT.with(std::cell::Cell::get)
}

#[cfg(test)]
pub(crate) fn set_test_ortho_detector(detector: Option<Arc<dyn OrthoDetector>>) {
    TEST_ORTHO_DETECTOR.with(|slot| {
        *slot.borrow_mut() = detector.map(|detector| PreparedOrthoDetector(Some(detector)));
    });
}

/// Wall-time split for a serial analyze run: time spent in the per-document
/// analyzer loop, time spent building adjudication rows (oracle evidence +
/// n-way row construction), time spent in the warehouse parquet write/encode
/// (`WarehouseWriter::finalize`), and the run's total wall time. `other()`
/// derives everything not accounted for by the three measured phases (IO,
/// AAT parsing/projection, etc).
#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct PhaseTimings {
    pub(crate) analysis: std::time::Duration,
    pub(crate) adjudication: std::time::Duration,
    pub(crate) warehouse_write: std::time::Duration,
    pub(crate) total: std::time::Duration,
}

impl PhaseTimings {
    pub(crate) fn other(&self) -> std::time::Duration {
        self.total
            .saturating_sub(self.analysis + self.adjudication + self.warehouse_write)
    }
}

impl std::ops::AddAssign for PhaseTimings {
    fn add_assign(&mut self, rhs: Self) {
        self.analysis += rhs.analysis;
        self.adjudication += rhs.adjudication;
        self.warehouse_write += rhs.warehouse_write;
        self.total += rhs.total;
    }
}

/// Log a one-line, stderr-only diagnostic summary of a run's phase-timing
/// split (mirrors the `auto-jobs:` line style in `auto_jobs.rs`). `n` is the
/// number of shards summed into `timings`. Never written to any
/// parquet/JSONL output — diagnostic only.
fn log_phase_timings(n: usize, timings: &PhaseTimings) {
    let pct = |d: std::time::Duration, total: std::time::Duration| -> f64 {
        if total.is_zero() {
            0.0
        } else {
            100.0 * d.as_secs_f64() / total.as_secs_f64()
        }
    };
    eprintln!(
        "phase-timings (summed across {n} shards): total={total:.1}s analysis={a:.1}s ({ap:.1}%) adjudication={adj:.1}s ({adjp:.1}%) warehouse-write={w:.1}s ({wp:.1}%) other={o:.1}s ({op:.1}%)",
        total = timings.total.as_secs_f64(),
        a = timings.analysis.as_secs_f64(),
        ap = pct(timings.analysis, timings.total),
        adj = timings.adjudication.as_secs_f64(),
        adjp = pct(timings.adjudication, timings.total),
        w = timings.warehouse_write.as_secs_f64(),
        wp = pct(timings.warehouse_write, timings.total),
        o = timings.other().as_secs_f64(),
        op = pct(timings.other(), timings.total),
    );
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat(
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
    run_analyze_aat_with_nway_impl(
        aat,
        aat_dir,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        None,
        None,
        None,
        None,
        OrthoDetectMode::Off,
        None,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_with_nway(
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
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: Option<usize>,
    string_stats_output: Option<&Path>,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    run_analyze_aat_with_nway_impl(
        aat,
        aat_dir,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        nway_output,
        nway_pattern_counts_output,
        max_nway_examples_per_text,
        string_stats_output,
        ortho_detect,
        ortho_ml_model,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_with_nway_impl(
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
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: Option<usize>,
    string_stats_output: Option<&Path>,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    let jobs = crate::auto_jobs::resolve_jobs(jobs, analyzer_ids.len());
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat
        .or(aat_dir)
        .map(|path| path.display().to_string())
        .unwrap_or_default();
    run_analyze_aat_inputs(
        inputs,
        input_mode,
        &input_path,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        nway_output,
        nway_pattern_counts_output,
        max_nway_examples_per_text,
        string_stats_output,
        ortho_detect,
        ortho_ml_model,
    )
}

/// Run the selected analysis pipeline and write warehouse outputs.
///
/// # Errors
///
/// Returns an error when input selection is invalid, analyzers cannot be loaded,
/// job parameters are invalid, or warehouse/IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_warehouse(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
    warehouse_profile: WarehouseProfile,
    zstd_level: i32,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
    eligible_source_ids: Option<&BTreeSet<String>>,
) -> Result<()> {
    run_analyze_aat_warehouse_impl(
        aat,
        aat_dir,
        analyzer_ids,
        warehouse_dir,
        run_id,
        jobs,
        warehouse_profile,
        zstd_level,
        ortho_detect,
        ortho_ml_model,
        eligible_source_ids,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_warehouse_impl(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
    warehouse_profile: WarehouseProfile,
    zstd_level: i32,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
    eligible_source_ids: Option<&BTreeSet<String>>,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if run_id.starts_with(STAGING_SHARD_PREFIX) {
        bail!(r#"run_id must not start with "shards-" (reserved for shard staging)"#);
    }
    let jobs = crate::auto_jobs::resolve_jobs(jobs, analyzer_ids.len());
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    // Lane A: run-eligibility filter by orthographic_style (outside
    // normalization, I2-D17b). Drops works not in the eligible set before any
    // analysis; logged so the narrowing is never silent.
    let inputs = match eligible_source_ids {
        Some(eligible) => {
            let discovered = inputs.len();
            let kept = crate::orthographic_select::filter_inputs_by_source_ids(inputs, eligible);
            eprintln!(
                "ab-morph-run: orthographic_style eligibility filter kept {}/{} works ({} eligible source_ids)",
                kept.len(),
                discovered,
                eligible.len()
            );
            kept
        }
        None => inputs,
    };
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat
        .or(aat_dir)
        .map(|path| path.display().to_string())
        .unwrap_or_default();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer_rows = warehouse_analyzer_rows(run_id, &specs, &analyzers)?;
    let normalization = resolve_run_normalization(ortho_detect, ortho_ml_model.as_deref())?;
    let prepared_ortho_detector =
        prepare_ortho_detector(&analyzers, ortho_detect, ortho_ml_model.as_deref())?;
    if jobs == 1 {
        let input_count = inputs.len();
        let (_string_stats, timings) = run_analyze_aat_serial(
            inputs,
            &analyzers,
            SerialRunOptions {
                analyses_output: None,
                comparisons_output: None,
                errors_output: None,
                resume: false,
                output_profile: OutputProfile::Compact,
                examples_output: None,
                max_examples_per_comparison: 0,
                nway_output: None,
                nway_pattern_counts_output: None,
                max_nway_examples_per_text: 0,
                collect_string_stats: false,
                warehouse: Some(WarehouseRunOptions {
                    paths: WarehousePaths::new(warehouse_dir, run_id),
                    input_mode,
                    input_path,
                    analyzer_rows,
                    warehouse_profile,
                    zstd_level,
                    normalization,
                }),
                progress: Some(SerialProgress {
                    label: format!("warehouse:{run_id}"),
                    total: input_count,
                }),
                ortho_detect,
                ortho_ml_model,
                prepared_ortho_detector: Some(prepared_ortho_detector),
            },
        )?;
        log_phase_timings(1, &timings);
    } else {
        run_analyze_aat_warehouse_parallel(
            inputs,
            analyzers,
            WarehouseParallelOptions {
                warehouse_dir: warehouse_dir.to_path_buf(),
                run_id: run_id.to_owned(),
                jobs,
                input_mode,
                input_path,
                analyzer_rows,
                warehouse_profile,
                zstd_level,
                ortho_detect,
                ortho_ml_model,
                prepared_ortho_detector,
                normalization,
            },
        )?;
    }
    Ok(())
}

/// Run over an explicit list of AAT inputs.
///
/// # Errors
///
/// Returns an error when inputs are empty, selection is invalid, analyzer ids are
/// missing, or IO/serialization fails.
#[allow(clippy::too_many_arguments)]
pub fn run_analyze_aat_selected(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
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
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    run_analyze_aat_selected_impl(
        inputs,
        input_mode,
        input_path,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        ortho_detect,
        ortho_ml_model,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_selected_impl(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
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
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    if inputs.is_empty() {
        bail!("provide at least one AAT input");
    }
    run_analyze_aat_inputs(
        inputs,
        input_mode,
        input_path,
        analyzer_ids,
        analyses_output,
        comparisons_output,
        errors_output,
        resume,
        jobs,
        output_profile,
        examples_output,
        max_examples_per_comparison,
        manifest_output,
        None,
        None,
        None,
        None,
        ortho_detect,
        ortho_ml_model,
    )
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_inputs(
    inputs: Vec<PathBuf>,
    input_mode: &str,
    input_path: &str,
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
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: Option<usize>,
    string_stats_output: Option<&Path>,
    ortho_detect: OrthoDetectMode,
    ortho_ml_model: Option<PathBuf>,
) -> Result<()> {
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
    if (nway_output.is_some() || nway_pattern_counts_output.is_some())
        && output_profile != OutputProfile::Compact
    {
        bail!(
            "--nway-output and --nway-pattern-counts-output require --output-profile compact in phase 1"
        );
    }
    if (nway_output.is_some() || nway_pattern_counts_output.is_some()) && analyzer_ids.len() < 2 {
        bail!("N-way outputs require at least two --analyzer values");
    }
    let max_nway_examples_per_text =
        max_nway_examples_per_text.unwrap_or(max_examples_per_comparison);
    let collect_string_stats = string_stats_output.is_some();

    let input_file_count = inputs.len();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;

    let string_stats = if jobs > 1 {
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
            nway_output,
            nway_pattern_counts_output,
            max_nway_examples_per_text,
            collect_string_stats,
        )?
    } else {
        let (string_stats, timings) = run_analyze_aat_serial(
            inputs,
            &analyzers,
            SerialRunOptions {
                analyses_output: Some(analyses_output),
                comparisons_output,
                errors_output,
                resume,
                output_profile,
                examples_output,
                max_examples_per_comparison,
                nway_output,
                nway_pattern_counts_output,
                max_nway_examples_per_text,
                collect_string_stats,
                warehouse: None,
                progress: None,
                ortho_detect,
                ortho_ml_model,
                prepared_ortho_detector: None,
            },
        )?;
        log_phase_timings(1, &timings);
        string_stats
    };

    if let Some(path) = manifest_output {
        write_manifest(
            path,
            output_profile,
            analyzer_ids,
            jobs,
            input_mode,
            input_path,
            input_file_count,
            analyses_output,
            comparisons_output,
            examples_output,
            errors_output,
            nway_output,
            nway_pattern_counts_output,
        )?;
    }
    if let Some(path) = string_stats_output {
        write_string_stats_report(path, &string_stats)?;
    }

    Ok(())
}

pub(crate) fn run_analyze_aat_serial(
    inputs: Vec<PathBuf>,
    analyzers: &[Arc<LoadedAnalyzer>],
    options: SerialRunOptions<'_>,
) -> Result<(StringStatsReport, PhaseTimings)> {
    let run_start = std::time::Instant::now();
    let mut analysis_time = std::time::Duration::ZERO;
    let mut adjudication_time = std::time::Duration::ZERO;
    let resume_ids = if options.resume {
        let analyses_output = options
            .analyses_output
            .context("resume requires an analyses output path")?;
        read_resume_ids(
            analyses_output,
            options.errors_output,
            options.nway_output,
            options.nway_pattern_counts_output,
            options.output_profile,
        )?
    } else {
        BTreeSet::new()
    };
    let inputs = filter_resume_inputs(inputs, &resume_ids, options.output_profile)?;

    let mut analyses_writer = if let Some(path) = options.analyses_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut comparisons_writer = if let Some(path) = options.comparisons_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut examples_writer = if let Some(path) = options.examples_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut errors_writer = if let Some(path) = options.errors_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut nway_writer = if let Some(path) = options.nway_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut nway_pattern_counts_writer = if let Some(path) = options.nway_pattern_counts_output {
        Some(open_output_writer(path, options.resume)?)
    } else {
        None
    };
    let mut warehouse_writer = if let Some(warehouse) = &options.warehouse {
        let mut writer = WarehouseWriter::create_for_tables(
            warehouse.paths.clone(),
            warehouse.warehouse_profile.tables(),
            warehouse.zstd_level,
        )?;
        writer.append_run_analyzers(&warehouse.analyzer_rows)?;
        Some(writer)
    } else {
        None
    };
    let mut warehouse_error_count = 0u64;
    // Count sources for which a Sources row is actually written, so runs.source_count
    // excludes ingest-time rejections (parse_incomplete / read / project failures).
    // Mirrors the sharded merge, which derives source_count from the Sources table.
    let mut written_source_count = 0u64;
    let mut string_stats = StringStatsReport::default();
    let progress = options.progress.clone();

    let detector = match options.prepared_ortho_detector {
        Some(prepared) => prepared.0,
        None => match prepare_ortho_detector(
            analyzers,
            options.ortho_detect,
            options.ortho_ml_model.as_deref(),
        ) {
            Ok(prepared) => prepared.0,
            Err(error) => {
                if options.ortho_detect != OrthoDetectMode::Ml {
                    if let Some(writer) = &mut errors_writer {
                        write_error_row(
                            &mut **writer,
                            &RunErrorRow {
                                input_path: String::new(),
                                source_id: None,
                                text_id: None,
                                analyzer: None,
                                stage: "ortho_detect_load".to_owned(),
                                error: error.to_string(),
                            },
                        )?;
                    } else {
                        eprintln!("ab-morph-run: failed to load ortho detector: {error}");
                    }
                }
                return Err(error);
            }
        },
    };

    for (input_index, input) in inputs.into_iter().enumerate() {
        let input_path = input.display().to_string();
        let source_id = compact::source_id_from_aat_path(&input);
        if let Some(progress) = &progress {
            string_stats.record_warning(
                "input_progress",
                format!(
                    "ab-morph-run: {} analyzing {}/{} source_id={}",
                    progress.label,
                    input_index + 1,
                    progress.total,
                    source_id
                ),
            );
        }
        let aat = match read_aat_value(&input) {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut warehouse_writer {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        options
                            .warehouse
                            .as_ref()
                            .expect("warehouse options")
                            .paths
                            .run_id
                            .as_str(),
                        Some(source_id.clone()),
                        None,
                        None,
                        "read_aat",
                        "read_aat_failed",
                        &error.to_string(),
                    )])?;
                    continue;
                }
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
        // Honor the adapter's parse-completeness signal. An AAT with
        // meta.parse_complete == false is a failed/aborted parse (e.g. the
        // aozora2html gem crashing mid-document) that leaves empty or truncated
        // blocks. Route it to the errors lane instead of analyzing it as a
        // normal source, which would launder an ingest failure into a
        // valid-but-empty source row and inflate source_count. A missing flag
        // (older adapters that never emit it) is treated as complete.
        let parse_incomplete = aat
            .get("meta")
            .and_then(|meta| meta.get("parse_complete"))
            .and_then(serde_json::Value::as_bool)
            == Some(false);
        if parse_incomplete {
            let message = aat
                .get("meta")
                .and_then(|meta| meta.get("warnings"))
                .and_then(|warnings| warnings.as_array())
                .and_then(|warnings| warnings.first())
                .and_then(|warning| warning.get("message"))
                .and_then(serde_json::Value::as_str)
                .unwrap_or("adapter reported parse_complete=false")
                .to_owned();
            if let Some(writer) = &mut warehouse_writer {
                warehouse_error_count += 1;
                writer.append_errors(&[warehouse_error_row(
                    options
                        .warehouse
                        .as_ref()
                        .expect("warehouse options")
                        .paths
                        .run_id
                        .as_str(),
                    Some(source_id.clone()),
                    None,
                    None,
                    "read_aat",
                    "parse_incomplete",
                    &message,
                )])?;
                continue;
            }
            if let Some(writer) = &mut errors_writer {
                write_error_row(
                    &mut **writer,
                    &RunErrorRow {
                        input_path,
                        source_id: Some(source_id),
                        text_id: None,
                        analyzer: None,
                        stage: "parse_incomplete".to_owned(),
                        error: message,
                    },
                )?;
                continue;
            }
            continue;
        }
        let collect_projection_spans = warehouse_writer.as_ref().is_some_and(|writer| {
            writer.writes_table(WarehouseTable::ProjectionSpans)
                || writer.writes_table(WarehouseTable::NwayRegionOracleEvidence)
        });
        let projected = if collect_projection_spans {
            from_aat_value_with_spans(&aat).map(|(document, spans)| (document, Some(spans)))
        } else {
            from_aat_value(&aat).map(|document| (document, None))
        };
        let (document, projection_spans) = match projected {
            Ok(value) => value,
            Err(error) => {
                if let Some(writer) = &mut warehouse_writer {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        options
                            .warehouse
                            .as_ref()
                            .expect("warehouse options")
                            .paths
                            .run_id
                            .as_str(),
                        Some(source_id.clone()),
                        None,
                        None,
                        "project_aat",
                        "project_aat_failed",
                        &error.to_string(),
                    )])?;
                    continue;
                }
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
        // Ruby-oracle bases from the projected ruby spans + AAT node readings.
        // MUST be built before `drop(aat)` below. Empty unless the oracle table is
        // requested and the run has ≥2 analyzers (spec §Precondition).
        let ruby_bases = match (&projection_spans, &warehouse_writer) {
            (Some(spans), Some(writer))
                if analyzers.len() >= 2
                    && writer.writes_table(WarehouseTable::NwayRegionOracleEvidence) =>
            {
                crate::oracle::ruby::ruby_bases(&aat, spans)
            }
            _ => Vec::new(),
        };
        // P1: the parsed AAT DOM is unused past projection; drop it now so the
        // per-document peak excludes it (several× file size on the large tail).
        drop(aat);
        // Orthographic normalization (katakana→hiragana) for pre-war text.
        // `None` means "no normalization applied" — analyze the document text
        // directly with NO extra copy (P2; warehouse runs always take this path).
        let (normalized_text_opt, offset_map_opt, annotations_opt): (
            Option<String>,
            Option<ab_ortho_detect::OffsetMap>,
            Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
        ) = if let Some(ref det) = detector {
            let sentences = ab_plaintext::sentence_split(&document.text);
            let annotations = det.detect(&sentences);
            if annotations.is_empty() {
                (None, None, None)
            } else {
                let (norm_text, map) =
                    ab_ortho_detect::ortho_normalize(&document.text, &annotations);
                (Some(norm_text), Some(map), Some(annotations))
            }
        } else {
            (None, None, None)
        };

        let normalized_doc_storage;
        let norm_doc: &ab_plaintext::PlainTextDocument = match normalized_text_opt {
            Some(text) => {
                normalized_doc_storage = ab_plaintext::PlainTextDocument {
                    text_id: document.text_id.clone(),
                    source_format: document.source_format,
                    text,
                };
                &normalized_doc_storage
            }
            None => &document,
        };
        // One allocation per document, shared by every per-analyzer Analysis.
        let shared_normalized: Arc<str> = Arc::from(norm_doc.text.as_str());
        // The original text is only needed when ortho remap can fire.
        let shared_original: Option<Arc<str>> = offset_map_opt
            .is_some()
            .then(|| Arc::from(document.text.as_str()));
        let mut analyses = Vec::new();

        let analysis_start = std::time::Instant::now();
        for analyzer in analyzers {
            let mut analysis = match analyzer.analyze(norm_doc) {
                Ok(analysis) => analysis,
                Err(error) => {
                    if let Some(writer) = &mut warehouse_writer {
                        warehouse_error_count += 1;
                        writer.append_errors(&[warehouse_error_row(
                            options
                                .warehouse
                                .as_ref()
                                .expect("warehouse options")
                                .paths
                                .run_id
                                .as_str(),
                            Some(source_id.clone()),
                            Some(document.text_id.clone()),
                            Some(analyzer.analyzer_id().to_owned()),
                            "analyze",
                            "analyze_failed",
                            &error.to_string(),
                        )])?;
                        continue;
                    }
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
            // Drop the analyzer's own text copy; share the per-document allocation.
            analysis.source_text = Arc::clone(&shared_normalized);
            // Remap morpheme byte_spans / char_spans / surfaces from normalized
            // coords to original-doc coords, and attach the ortho provenance to
            // the analysis. `source_text` is set to the original document text so
            // that downstream `compare_pair` / `compare_pair_with_source_text`
            // / `compare_nway_with_source_text` are all consistent (spec
            // invariant #2: byte_span, char_span, surface, source_text all
            // reference the original doc).
            if let Some(ref map) = offset_map_opt {
                match ab_morph_analyzers::span_builder::remap_spans(
                    &mut analysis,
                    map,
                    &document.text,
                ) {
                    Ok(report) => {
                        // Honor spec invariant #2: byte_span/char_span/surface now in
                        // original-doc coords, so source_text must be the original.
                        analysis.source_text = Arc::clone(
                            shared_original
                                .as_ref()
                                .expect("offset_map_opt is Some in this arm"),
                        );
                        // Widened / folded morphemes are a coarsening of this
                        // analyzer's segmentation, not a failure; record the
                        // counts so consumers can weigh the comparison
                        // (Invariant 4: never silent), on every output path.
                        if report.snapped > 0 {
                            let message = format!(
                                "{} morpheme spans widened to the covering original span, {} folded into their predecessor",
                                report.snapped, report.merged
                            );
                            if let Some(writer) = &mut warehouse_writer {
                                warehouse_error_count += 1;
                                writer.append_errors(&[warehouse_error_row(
                                    options
                                        .warehouse
                                        .as_ref()
                                        .expect("warehouse options")
                                        .paths
                                        .run_id
                                        .as_str(),
                                    Some(source_id.clone()),
                                    Some(document.text_id.clone()),
                                    Some(analyzer.analyzer_id().to_owned()),
                                    "ortho_remap",
                                    "ortho_remap_snapped",
                                    &message,
                                )])?;
                            } else if let Some(writer) = &mut errors_writer {
                                write_error_row(
                                    &mut **writer,
                                    &RunErrorRow {
                                        input_path: input_path.clone(),
                                        source_id: Some(source_id.clone()),
                                        text_id: Some(document.text_id.clone()),
                                        analyzer: Some(analyzer.analyzer_id().to_owned()),
                                        stage: "ortho_remap".to_owned(),
                                        error: format!("ortho_remap_snapped: {message}"),
                                    },
                                )?;
                            }
                        }
                    }
                    Err(e) => {
                        // Morphemes remain in normalized coords. Leave source_text as
                        // the normalized text the analyzer produced (consistent with
                        // the morphemes). Record the failure so it is never silently
                        // dropped (Invariant 4) — including on the warehouse path,
                        // which publication uses and which has no errors_writer.
                        let (code, message) = match &e {
                            ab_ortho_detect::OrthoMapError::CrossesBoundary { range, boundary } => {
                                (
                                    "ortho_remap_crosses_boundary",
                                    format!("range {range:?} crosses boundary at byte {boundary}"),
                                )
                            }
                            ab_ortho_detect::OrthoMapError::UncoveredOffset { offset } => (
                                "ortho_remap_uncovered_offset",
                                format!("offset {offset} not covered"),
                            ),
                        };
                        if let Some(writer) = &mut warehouse_writer {
                            warehouse_error_count += 1;
                            writer.append_errors(&[warehouse_error_row(
                                options
                                    .warehouse
                                    .as_ref()
                                    .expect("warehouse options")
                                    .paths
                                    .run_id
                                    .as_str(),
                                Some(source_id.clone()),
                                Some(document.text_id.clone()),
                                Some(analyzer.analyzer_id().to_owned()),
                                "ortho_remap",
                                code,
                                &message,
                            )])?;
                        } else if let Some(writer) = &mut errors_writer {
                            write_ortho_remap_error(&mut **writer, &source_id, &e)?;
                        } else {
                            eprintln!("ortho_remap error for {source_id}: {e}");
                        }
                    }
                }
            }
            analysis.ortho_annotations = annotations_opt.clone();
            analysis.ortho_offset_map = offset_map_opt.clone();
            if options.collect_string_stats {
                string_stats.record_analysis(&analysis);
            }

            if let Some(writer) = &mut analyses_writer {
                write_analysis_row(&mut **writer, options.output_profile, &source_id, &analysis)?;
            }
            if options.output_profile == OutputProfile::Compact && options.warehouse.is_none() {
                // Drop this analysis's handle to the shared text; other analyses'
                // clones of the same Arc are unaffected.
                analysis.source_text = Arc::from("");
            }
            analyses.push(analysis);
        }
        analysis_time += analysis_start.elapsed();

        // Cross-analyzer (n-way / pairwise) comparison requires every analysis
        // to byte-match the source. A single analyzer whose surfaces diverge —
        // e.g. a dictionary that lexicalizes a decorative run and sweeps leading
        // full-width spaces into a token — must not poison the whole work's
        // comparison for the other analyzers. Validate each analysis here and
        // drop only the offending analyzers from the comparison set below; their
        // raw per-analyzer morphemes are still written unchanged.
        let invalid_for_compare = invalid_analyzers_for_compare(&analyses, &document.text);

        let adj_start = std::time::Instant::now();
        let write_before = warehouse_writer
            .as_ref()
            .map(|w| w.write_time())
            .unwrap_or_default();
        if let Some(writer) = &mut warehouse_writer
            && let Some(first_analysis) = analyses.first()
        {
            let run_id = options
                .warehouse
                .as_ref()
                .expect("warehouse options")
                .paths
                .run_id
                .as_str();
            let source =
                warehouse::rows::source_row(run_id, &source_id, &input_path, first_analysis);
            writer.append_sources(&[source])?;
            written_source_count += 1;
            if let Some(spans) = &projection_spans {
                for chunk in spans.chunks(WAREHOUSE_MORPHEME_ROW_BATCH_SIZE) {
                    let rows = warehouse::rows::projection_span_rows(
                        run_id,
                        &source_id,
                        &document.text_id,
                        chunk,
                    );
                    writer.append_projection_spans(&rows)?;
                }
            }
            let analysis_rows = analyses
                .iter()
                .map(|analysis| warehouse::rows::analysis_row(run_id, &source_id, analysis))
                .collect::<Vec<_>>();
            writer.append_analyses(&analysis_rows)?;
            for analysis in &mut analyses {
                // Drop this analysis's handle to the shared text; other analyses'
                // clones of the same Arc are unaffected.
                analysis.source_text = Arc::from("");
            }
            for analysis in &analyses {
                for start in
                    (0..analysis.morphemes.len()).step_by(WAREHOUSE_MORPHEME_ROW_BATCH_SIZE)
                {
                    let end =
                        (start + WAREHOUSE_MORPHEME_ROW_BATCH_SIZE).min(analysis.morphemes.len());
                    let morphemes = warehouse::rows::morpheme_rows_for_range(
                        run_id,
                        &source_id,
                        analysis,
                        start..end,
                    );
                    writer.append_morphemes(&morphemes)?;
                    if writer.writes_table(WarehouseTable::MorphemeFeatures) {
                        let mut morpheme_feature_columns = MorphemeFeaturesColumns::new();
                        warehouse::rows::push_morpheme_features_for_range(
                            run_id,
                            &source_id,
                            analysis,
                            start..end,
                            &mut morpheme_feature_columns,
                        );
                        writer.append_morpheme_feature_columns(morpheme_feature_columns)?;
                    }
                }
            }
            // Record each dropped analyzer, then retain only the valid ones for
            // the n-way comparison so one analyzer's surface mismatch does not
            // fail compare_nway for the whole work (all analyzers).
            for (analyzer, message) in &invalid_for_compare {
                warehouse_error_count += 1;
                writer.append_errors(&[warehouse_error_row(
                    run_id,
                    Some(source_id.clone()),
                    Some(document.text_id.clone()),
                    Some(analyzer.clone()),
                    "validate_analysis",
                    "analysis_invalid",
                    message,
                )])?;
            }
            analyses.retain(|analysis| {
                !invalid_for_compare
                    .iter()
                    .any(|(id, _)| id == &analysis.analyzer)
            });
            match append_warehouse_nway_fact_rows(
                writer,
                run_id,
                &source_id,
                &document.text,
                &analyses,
                &ruby_bases,
            ) {
                Ok(()) => {}
                Err(error) if error.downcast_ref::<MorphDiffError>().is_some() => {
                    warehouse_error_count += 1;
                    writer.append_errors(&[warehouse_error_row(
                        run_id,
                        Some(source_id.clone()),
                        Some(document.text_id.clone()),
                        None,
                        "compare_nway",
                        "compare_nway_failed",
                        &error.to_string(),
                    )])?;
                }
                Err(error) => return Err(error),
            }
        }
        let write_after = warehouse_writer
            .as_ref()
            .map(|w| w.write_time())
            .unwrap_or_default();
        adjudication_time += adj_start
            .elapsed()
            .saturating_sub(write_after.saturating_sub(write_before));

        // Non-warehouse runs: the warehouse branch above never ran, so drop the
        // invalid analyzers here (and record them) before the JSONL comparison
        // consumers. When the warehouse branch did run, `analyses` was already
        // retained and this is a no-op.
        if warehouse_writer.is_none() && !invalid_for_compare.is_empty() {
            if let Some(error_writer) = &mut errors_writer {
                for (analyzer, message) in &invalid_for_compare {
                    write_error_row(
                        &mut **error_writer,
                        &RunErrorRow {
                            input_path: input_path.clone(),
                            source_id: Some(source_id.clone()),
                            text_id: Some(document.text_id.clone()),
                            analyzer: Some(analyzer.clone()),
                            stage: "validate_analysis".to_owned(),
                            error: message.clone(),
                        },
                    )?;
                }
            }
            analyses.retain(|analysis| {
                !invalid_for_compare
                    .iter()
                    .any(|(id, _)| id == &analysis.analyzer)
            });
        }
        let comparison_result = if comparisons_writer.is_some() || examples_writer.is_some() {
            write_comparison_rows(
                comparisons_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                examples_writer
                    .as_mut()
                    .map(|writer| &mut **writer as &mut dyn Write),
                &analyses,
                &source_id,
                &document.text,
                options.output_profile,
                options.max_examples_per_comparison,
            )
        } else {
            Ok(())
        };
        if let Err(error) = comparison_result {
            if let Some(error_writer) = &mut errors_writer {
                write_error_row(
                    &mut **error_writer,
                    &RunErrorRow {
                        input_path: input_path.clone(),
                        source_id: Some(source_id.clone()),
                        text_id: Some(document.text_id.clone()),
                        analyzer: None,
                        stage: "compare".to_owned(),
                        error: error.to_string(),
                    },
                )?;
            } else {
                return Err(error);
            }
        }
        if nway_writer.is_some() || nway_pattern_counts_writer.is_some() {
            match nway::row_and_pattern_counts_from_analyses(
                source_id.clone(),
                &document.text,
                &analyses,
                options.max_nway_examples_per_text,
            ) {
                Ok((row, pattern_counts)) => {
                    if let Some(writer) = &mut nway_writer {
                        write_jsonl_row(&mut **writer, &row)?;
                    }
                    if let Some(writer) = &mut nway_pattern_counts_writer {
                        for pattern_count in pattern_counts {
                            write_jsonl_row(&mut **writer, &pattern_count)?;
                        }
                    }
                }
                Err(error) => {
                    if let Some(error_writer) = &mut errors_writer {
                        write_error_row(
                            &mut **error_writer,
                            &RunErrorRow {
                                input_path: input_path.clone(),
                                source_id: Some(source_id.clone()),
                                text_id: Some(document.text_id.clone()),
                                analyzer: None,
                                stage: "compare_nway".to_owned(),
                                error: error.to_string(),
                            },
                        )?;
                    } else {
                        return Err(error.into());
                    }
                }
            }
        }
    }

    if let Some(writer) = &mut analyses_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut comparisons_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut examples_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut errors_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut nway_writer {
        writer.flush()?;
    }
    if let Some(writer) = &mut nway_pattern_counts_writer {
        writer.flush()?;
    }
    let warehouse_write = if let Some(mut writer) = warehouse_writer {
        let warehouse = options.warehouse.as_ref().expect("warehouse options");
        writer.append_runs(&[RunRow {
            schema_version: warehouse::schema::SCHEMA_VERSION,
            run_id: warehouse.paths.run_id.clone(),
            created_at_utc: chrono::Utc::now().to_rfc3339(),
            input_mode: warehouse.input_mode.to_owned(),
            input_path: warehouse.input_path.clone(),
            source_count: written_source_count,
            analyzer_count: warehouse.analyzer_rows.len() as u64,
            error_count: warehouse_error_count,
            ortho_detect_mode: warehouse.normalization.mode.clone(),
            input_normalization_detector_id: warehouse.normalization.detector_id.clone(),
            input_normalization_policy_hash: warehouse.normalization.policy_hash.clone(),
        }])?;
        let dur = writer.finalize()?;
        write_run_normalization_provenance(
            &warehouse.paths.final_dir,
            &warehouse.paths.run_id,
            &warehouse.normalization,
        )?;
        dur
    } else {
        std::time::Duration::ZERO
    };
    let timings = PhaseTimings {
        analysis: analysis_time,
        adjudication: adjudication_time,
        warehouse_write,
        total: run_start.elapsed(),
    };
    Ok((string_stats, timings))
}

pub(crate) fn run_analyze_aat_warehouse_parallel(
    inputs: Vec<PathBuf>,
    analyzers: Vec<Arc<LoadedAnalyzer>>,
    options: WarehouseParallelOptions,
) -> Result<StringStatsReport> {
    let total_inputs = inputs.len();
    let large_lanes = bounded_large_lane_count(options.jobs);
    let queue = Arc::new(Mutex::new(WarehouseWorkQueue::new(inputs, large_lanes)));
    cleanup_orphaned_shard_staging(&options.warehouse_dir)?;
    let temp_root = shard_staging_root(&options.warehouse_dir, &options.run_id);
    claim_shard_staging(&temp_root)?;
    let shard_warehouse_dir = temp_root.join("warehouse");
    fs::create_dir_all(&shard_warehouse_dir)
        .with_context(|| format!("failed to create {}", shard_warehouse_dir.display()))?;

    let result = std::thread::scope(|scope| {
        let mut handles = Vec::new();
        for job_index in 0..options.jobs {
            let analyzers = analyzers.clone();
            let shard_warehouse_dir = shard_warehouse_dir.clone();
            let shard_run_id = options.run_id.clone();
            let input_mode = options.input_mode;
            let input_path = options.input_path.clone();
            let analyzer_rows = options.analyzer_rows.clone();
            let ortho_detect = options.ortho_detect;
            let ortho_ml_model = options.ortho_ml_model.clone();
            let prepared_ortho_detector = options.prepared_ortho_detector.clone();
            let normalization = options.normalization.clone();
            let queue = Arc::clone(&queue);
            handles.push(scope.spawn(move || -> Result<WarehouseShardOutput> {
                let mut shard_run_dirs = Vec::new();
                let mut warnings = Vec::new();
                let mut timings = PhaseTimings::default();
                while let Some(batch) = take_warehouse_work_batch(&queue) {
                    let shard_index = batch.shard_index;
                    let batch_is_large = batch.is_large;
                    let batch_len = batch.inputs.len();
                    let paths = WarehousePaths::new(
                        shard_warehouse_dir.join(format!("shard-{shard_index}")),
                        shard_run_id.clone(),
                    );
                    let result = run_analyze_aat_serial(
                        batch.inputs,
                        &analyzers,
                        SerialRunOptions {
                            analyses_output: None,
                            comparisons_output: None,
                            errors_output: None,
                            resume: false,
                            output_profile: OutputProfile::Compact,
                            examples_output: None,
                            max_examples_per_comparison: 0,
                            nway_output: None,
                            nway_pattern_counts_output: None,
                            max_nway_examples_per_text: 0,
                            collect_string_stats: false,
                            warehouse: Some(WarehouseRunOptions {
                                paths: paths.clone(),
                                input_mode,
                                input_path: input_path.clone(),
                                analyzer_rows: analyzer_rows.clone(),
                                warehouse_profile: options.warehouse_profile,
                                zstd_level: options.zstd_level,
                                normalization: normalization.clone(),
                            }),
                            progress: Some(SerialProgress {
                                label: format!("warehouse-worker-{job_index}/shard-{shard_index}"),
                                total: batch_len,
                            }),
                            ortho_detect,
                            ortho_ml_model: ortho_ml_model.clone(),
                            prepared_ortho_detector: Some(prepared_ortho_detector.clone()),
                        },
                    );
                    complete_warehouse_work_batch(&queue, batch_is_large);
                    let (batch_report, batch_timings) = result?;
                    warnings.extend(batch_report.warnings);
                    timings += batch_timings;
                    shard_run_dirs.push(paths.final_dir);
                }
                Ok(WarehouseShardOutput {
                    shard_run_dirs,
                    warnings,
                    timings,
                })
            }));
        }

        let mut outputs = Vec::new();
        for handle in handles {
            outputs.push(handle.join().expect("warehouse worker panicked")?);
        }
        Ok::<_, anyhow::Error>(outputs)
    });

    let outputs = match result {
        Ok(outputs) => outputs,
        Err(error) => {
            let _ = fs::remove_dir_all(&temp_root);
            return Err(error);
        }
    };
    let mut report = StringStatsReport::default();
    let mut outputs = outputs;
    let mut phase_timings = PhaseTimings::default();
    for output in &mut outputs {
        report.warnings.append(&mut output.warnings);
        phase_timings += output.timings;
    }
    let mut shard_run_dirs = outputs
        .into_iter()
        .flat_map(|output| output.shard_run_dirs)
        .collect::<Vec<_>>();
    shard_run_dirs.sort();
    report.record_warning(
        "warehouse_merge",
        format!(
            "ab-morph-run: warehouse merging {} dynamic shard(s) from {} input(s) into run_id={}",
            shard_run_dirs.len(),
            total_inputs,
            options.run_id
        ),
    );
    merge_warehouse_shard_runs(&options, &shard_run_dirs)?;
    report.record_warning(
        "warehouse_merge",
        format!(
            "ab-morph-run: warehouse merge complete run_id={}",
            options.run_id
        ),
    );
    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
    log_phase_timings(shard_run_dirs.len(), &phase_timings);
    Ok(report)
}

struct WarehouseShardOutput {
    shard_run_dirs: Vec<PathBuf>,
    warnings: Vec<RunWarning>,
    timings: PhaseTimings,
}

pub(crate) struct WarehouseWorkBatch {
    pub(crate) shard_index: usize,
    pub(crate) inputs: Vec<PathBuf>,
    pub(crate) is_large: bool,
}

pub(crate) struct WarehouseWorkQueue {
    regular: VecDeque<PathBuf>,
    large: VecDeque<PathBuf>,
    next_shard_index: usize,
    active_large_batches: usize,
    large_lanes: usize,
}

impl WarehouseWorkQueue {
    pub(crate) fn new(inputs: Vec<PathBuf>, large_lanes: usize) -> Self {
        let mut regular = Vec::new();
        let mut large = Vec::new();
        for input in inputs {
            let size = fs::metadata(&input)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            if size >= LARGE_INPUT_THRESHOLD_BYTES {
                large.push((input, size));
            } else {
                regular.push((input, size));
            }
        }
        regular.sort_by(|(left_path, left_size), (right_path, right_size)| {
            left_size
                .cmp(right_size)
                .then_with(|| left_path.cmp(right_path))
        });
        // Large docs biggest-first: front-loading the makespan-dominating inputs (P1).
        large.sort_by(|(left_path, left_size), (right_path, right_size)| {
            right_size
                .cmp(left_size)
                .then_with(|| left_path.cmp(right_path))
        });
        Self {
            regular: regular.into_iter().map(|(path, _)| path).collect(),
            large: large.into_iter().map(|(path, _)| path).collect(),
            next_shard_index: 0,
            active_large_batches: 0,
            large_lanes,
        }
    }

    /// Pop one large doc as its own batch if a large lane is free. Caller has already
    /// checked nothing; this enforces the `large_lanes` cap and increments the counter.
    fn pop_large_batch(&mut self) -> Option<WarehouseWorkBatch> {
        if self.active_large_batches >= self.large_lanes {
            return None;
        }
        let input = self.large.pop_front()?;
        let shard_index = self.next_shard_index;
        self.next_shard_index += 1;
        self.active_large_batches += 1;
        Some(WarehouseWorkBatch {
            shard_index,
            inputs: vec![input],
            is_large: true,
        })
    }

    pub(crate) fn take_batch(&mut self) -> Option<WarehouseWorkBatch> {
        // P1: front-load large docs (biggest-first) up to the memory-bounded lane count
        // so they overlap the abundant regular work instead of forming an idle tail.
        if let Some(batch) = self.pop_large_batch() {
            return Some(batch);
        }
        if !self.regular.is_empty() {
            let shard_index = self.next_shard_index;
            self.next_shard_index += 1;
            let mut inputs = Vec::new();
            for _ in 0..WAREHOUSE_REGULAR_BATCH_SIZE {
                let Some(input) = self.regular.pop_front() else {
                    break;
                };
                inputs.push(input);
            }
            return Some(WarehouseWorkBatch {
                shard_index,
                inputs,
                is_large: false,
            });
        }
        // Regular exhausted: keep draining large within the lane cap (the short tail).
        self.pop_large_batch()
    }

    pub(crate) fn complete_batch(&mut self, is_large: bool) {
        if is_large {
            self.active_large_batches = self.active_large_batches.saturating_sub(1);
        }
    }
}

#[cfg(test)]
mod phase_timings_tests {
    use super::PhaseTimings;

    #[test]
    fn phase_timings_sum_and_derive_other() {
        use std::time::Duration;
        let mut a = PhaseTimings {
            analysis: Duration::from_secs(2),
            adjudication: Duration::from_secs(1),
            warehouse_write: Duration::from_secs(3),
            total: Duration::from_secs(10),
        };
        let b = PhaseTimings {
            analysis: Duration::from_secs(1),
            adjudication: Duration::from_secs(2),
            warehouse_write: Duration::from_secs(1),
            total: Duration::from_secs(4),
        };
        a += b;
        assert_eq!(a.analysis, Duration::from_secs(3));
        assert_eq!(a.adjudication, Duration::from_secs(3));
        assert_eq!(a.warehouse_write, Duration::from_secs(4));
        assert_eq!(a.total, Duration::from_secs(14));
        assert_eq!(a.other(), Duration::from_secs(4)); // 14 - 3 - 3 - 4
    }

    #[test]
    fn phase_timings_other_saturates() {
        use std::time::Duration;
        // Overlap/measurement skew must never underflow.
        let t = PhaseTimings {
            analysis: Duration::from_secs(6),
            adjudication: Duration::from_secs(2),
            warehouse_write: Duration::from_secs(6),
            total: Duration::from_secs(10),
        };
        assert_eq!(t.other(), Duration::ZERO);
    }
}

#[cfg(test)]
mod work_queue_tests {
    use super::WarehouseWorkQueue;
    use crate::LARGE_INPUT_THRESHOLD_BYTES;
    use std::path::PathBuf;

    fn tmp_file(dir: &std::path::Path, name: &str, size: u64) -> PathBuf {
        let p = dir.join(name);
        std::fs::write(&p, vec![b'x'; size as usize]).unwrap();
        p
    }

    #[test]
    fn front_loads_largest_first_within_lane_cap() {
        let dir = std::env::temp_dir().join(format!("wq-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let big = LARGE_INPUT_THRESHOLD_BYTES;
        let l30 = tmp_file(&dir, "l30", big + 30);
        let l20 = tmp_file(&dir, "l20", big + 20);
        let l10 = tmp_file(&dir, "l10", big + 10);
        let mut regulars = Vec::new();
        for i in 0..5 {
            regulars.push(tmp_file(&dir, &format!("r{i}"), 100));
        }
        let mut inputs = vec![l10.clone(), l30.clone(), l20.clone()];
        inputs.extend(regulars.clone());

        let mut q = WarehouseWorkQueue::new(inputs, 2); // large_lanes = 2

        // First two batches are the two BIGGEST large docs, before regular is drained.
        let b1 = q.take_batch().unwrap();
        assert!(b1.is_large);
        assert_eq!(b1.inputs, vec![l30.clone()]);
        let b2 = q.take_batch().unwrap();
        assert!(b2.is_large);
        assert_eq!(b2.inputs, vec![l20.clone()]);
        // Lane cap hit (2 active): next batch is REGULAR, not the 10-byte-over large doc.
        let b3 = q.take_batch().unwrap();
        assert!(!b3.is_large);
        // Freeing a large lane lets the last large doc dispatch.
        q.complete_batch(true);
        let mut saw_l10 = false;
        while let Some(b) = q.take_batch() {
            if b.is_large {
                assert_eq!(b.inputs, vec![l10.clone()]);
                saw_l10 = true;
            }
            if b.is_large {
                q.complete_batch(true);
            }
        }
        assert!(saw_l10, "the last large doc must eventually dispatch");
        let _ = std::fs::remove_dir_all(dir);
    }

    #[test]
    fn covers_every_input_exactly_once() {
        let dir = std::env::temp_dir().join(format!("wq2-{}", std::process::id()));
        std::fs::create_dir_all(&dir).unwrap();
        let big = LARGE_INPUT_THRESHOLD_BYTES;
        let mut inputs = vec![tmp_file(&dir, "l1", big + 1), tmp_file(&dir, "l2", big + 2)];
        for i in 0..70 {
            inputs.push(tmp_file(&dir, &format!("r{i}"), 50));
        }
        let expected = inputs.len();
        let mut q = WarehouseWorkQueue::new(inputs, 1);
        let mut seen = 0;
        while let Some(b) = q.take_batch() {
            seen += b.inputs.len();
            if b.is_large {
                q.complete_batch(true);
            }
        }
        assert_eq!(seen, expected, "every input returned exactly once");
        let _ = std::fs::remove_dir_all(dir);
    }
}

pub(crate) fn take_warehouse_work_batch(
    queue: &Arc<Mutex<WarehouseWorkQueue>>,
) -> Option<WarehouseWorkBatch> {
    queue
        .lock()
        .expect("warehouse work queue poisoned")
        .take_batch()
}

pub(crate) fn complete_warehouse_work_batch(
    queue: &Arc<Mutex<WarehouseWorkQueue>>,
    is_large: bool,
) {
    queue
        .lock()
        .expect("warehouse work queue poisoned")
        .complete_batch(is_large);
}

pub(crate) fn bounded_large_lane_count(jobs: usize) -> usize {
    (jobs / 4).max(1)
}

/// Identifies analyzers whose analysis does not byte-match `source_text`,
/// returning each offending analyzer id with its validation error message.
///
/// Cross-analyzer comparison (n-way and pairwise) requires every analysis to be
/// consistent with the source. Rather than let one analyzer's surface mismatch
/// fail the comparison for the whole work, the caller drops only the analyzers
/// returned here and compares the rest.
fn invalid_analyzers_for_compare(
    analyses: &[ab_morph_diff::Analysis],
    source_text: &str,
) -> Vec<(String, String)> {
    analyses
        .iter()
        .filter_map(|analysis| {
            match ab_morph_diff::validate_analysis_against_source(analysis, source_text) {
                Ok(()) => None,
                Err(error) => Some((analysis.analyzer.clone(), error.to_string())),
            }
        })
        .collect()
}

pub(crate) fn merge_warehouse_shard_runs(
    options: &WarehouseParallelOptions,
    shard_run_dirs: &[PathBuf],
) -> Result<()> {
    let paths = WarehousePaths::new(&options.warehouse_dir, &options.run_id);
    let error_count = shard_run_dirs
        .iter()
        .map(|run_dir| parquet_table_row_count(run_dir, WarehouseTable::Errors))
        .try_fold(0u64, |total, count| count.map(|count| total + count))?;
    let source_count = shard_run_dirs
        .iter()
        .map(|run_dir| parquet_table_row_count(run_dir, WarehouseTable::Sources))
        .try_fold(0u64, |total, count| count.map(|count| total + count))?;
    let mut writer = WarehouseWriter::create_for_tables(
        paths.clone(),
        &[WarehouseTable::Runs, WarehouseTable::RunAnalyzers],
        options.zstd_level,
    )?;
    writer.append_run_analyzers(&options.analyzer_rows)?;
    for table in options.warehouse_profile.merged_data_tables() {
        for (shard_index, run_dir) in shard_run_dirs.iter().enumerate() {
            stage_parquet_table_part(
                &paths.staging_dir,
                *table,
                shard_index,
                &run_dir.join(table.file_name()),
            )?;
        }
    }
    // §3.12: coalesce small-part tables (analyses, sources, feature_pattern_counts
    // on the full corpus) into a single file each. Large tables are skipped
    // inside compact_staged_table.
    for &table in options.warehouse_profile.merged_data_tables() {
        warehouse::writer::compact_staged_table(&paths, table, options.zstd_level)?;
    }
    writer.append_runs(&[RunRow {
        schema_version: warehouse::schema::SCHEMA_VERSION,
        run_id: options.run_id.clone(),
        created_at_utc: chrono::Utc::now().to_rfc3339(),
        input_mode: options.input_mode.to_owned(),
        input_path: options.input_path.clone(),
        source_count,
        analyzer_count: options.analyzer_rows.len() as u64,
        error_count,
        ortho_detect_mode: options.normalization.mode.clone(),
        input_normalization_detector_id: options.normalization.detector_id.clone(),
        input_normalization_policy_hash: options.normalization.policy_hash.clone(),
    }])?;
    let _ = writer.finalize()?;
    write_run_normalization_provenance(&paths.final_dir, &options.run_id, &options.normalization)?;
    Ok(())
}

#[allow(clippy::too_many_arguments)]
pub(crate) fn run_analyze_aat_parallel(
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
    nway_output: Option<&Path>,
    nway_pattern_counts_output: Option<&Path>,
    max_nway_examples_per_text: usize,
    collect_string_stats: bool,
) -> Result<StringStatsReport> {
    let resume_ids = if resume {
        read_resume_ids(
            analyses_output,
            errors_output,
            nway_output,
            nway_pattern_counts_output,
            output_profile,
        )?
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
                let nway = nway_output.map(|path| shard_output_path(&output_dir, path, "nway"));
                let nway_pattern_counts = nway_pattern_counts_output
                    .map(|path| shard_output_path(&output_dir, path, "nway-pattern-counts"));
                let errors =
                    errors_output.map(|path| shard_output_path(&output_dir, path, "errors"));
                let (string_stats, timings) = run_analyze_aat_serial(
                    shard_inputs,
                    &analyzers,
                    SerialRunOptions {
                        analyses_output: Some(&analyses),
                        comparisons_output: comparisons.as_deref(),
                        errors_output: errors.as_deref(),
                        resume: false,
                        output_profile,
                        examples_output: examples.as_deref(),
                        max_examples_per_comparison,
                        nway_output: nway.as_deref(),
                        nway_pattern_counts_output: nway_pattern_counts.as_deref(),
                        max_nway_examples_per_text,
                        collect_string_stats,
                        warehouse: None,
                        progress: None,
                        // --ortho-detect is not yet threaded through the parallel/warehouse/selected paths.
                        ortho_detect: OrthoDetectMode::Off,
                        ortho_ml_model: None,
                        prepared_ortho_detector: None,
                    },
                )?;
                Ok(ShardOutput {
                    job_index,
                    analyses,
                    comparisons,
                    examples,
                    nway,
                    nway_pattern_counts,
                    errors,
                    string_stats,
                    timings,
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
    if let Some(path) = nway_output {
        merge_shard_files(
            outputs.iter().filter_map(|output| output.nway.as_deref()),
            path,
            resume,
        )?;
    }
    if let Some(path) = nway_pattern_counts_output {
        merge_shard_files(
            outputs
                .iter()
                .filter_map(|output| output.nway_pattern_counts.as_deref()),
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
    let mut string_stats = StringStatsReport::default();
    if collect_string_stats {
        for output in &outputs {
            string_stats.merge(&output.string_stats);
        }
    }
    let mut phase_timings = PhaseTimings::default();
    for output in &outputs {
        phase_timings += output.timings;
    }

    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
    log_phase_timings(outputs.len(), &phase_timings);
    Ok(string_stats)
}

struct ShardOutput {
    job_index: usize,
    analyses: PathBuf,
    comparisons: Option<PathBuf>,
    examples: Option<PathBuf>,
    nway: Option<PathBuf>,
    nway_pattern_counts: Option<PathBuf>,
    errors: Option<PathBuf>,
    string_stats: StringStatsReport,
    timings: PhaseTimings,
}

pub(crate) fn shard_output_path(output_dir: &Path, final_path: &Path, stem: &str) -> PathBuf {
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

pub(crate) fn partition_inputs(inputs: Vec<PathBuf>, jobs: usize) -> Vec<Vec<PathBuf>> {
    let mut partitions = vec![Vec::new(); jobs];
    let mut partition_sizes = vec![0u64; jobs];
    let inputs = inputs
        .into_iter()
        .map(|input| {
            let size = fs::metadata(&input)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            (input, size)
        })
        .collect::<Vec<_>>();
    let (large_inputs, regular_inputs): (Vec<_>, Vec<_>) = inputs
        .into_iter()
        .partition(|(_, size)| *size >= LARGE_INPUT_THRESHOLD_BYTES);
    assign_inputs_to_partitions(&mut partitions, &mut partition_sizes, regular_inputs, jobs);
    let large_lanes = if large_inputs.is_empty() {
        jobs
    } else {
        (jobs / 4).max(1)
    };
    assign_inputs_to_partitions(
        &mut partitions,
        &mut partition_sizes,
        large_inputs,
        large_lanes,
    );
    for partition in &mut partitions {
        partition.sort_by(|left, right| {
            let left_size = fs::metadata(left)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            let right_size = fs::metadata(right)
                .map(|metadata| metadata.len())
                .unwrap_or(0);
            left_size.cmp(&right_size).then_with(|| left.cmp(right))
        });
    }
    partitions
}

pub(crate) fn assign_inputs_to_partitions(
    partitions: &mut [Vec<PathBuf>],
    partition_sizes: &mut [u64],
    mut inputs: Vec<(PathBuf, u64)>,
    lane_count: usize,
) {
    inputs.sort_by(|(left_path, left_size), (right_path, right_size)| {
        right_size
            .cmp(left_size)
            .then_with(|| left_path.cmp(right_path))
    });
    for (input, size) in inputs {
        let partition_index = partition_sizes
            .iter()
            .take(lane_count)
            .enumerate()
            .min_by(|(left_index, left_size), (right_index, right_size)| {
                left_size
                    .cmp(right_size)
                    .then_with(|| left_index.cmp(right_index))
            })
            .map(|(index, _)| index)
            .expect("at least one partition");
        partition_sizes[partition_index] += size;
        partitions[partition_index].push(input);
    }
}

pub(crate) fn symlink_input_file(input: &Path, link: &Path) -> Result<()> {
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

pub(crate) fn filter_resume_inputs(
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
    shard_files: impl IntoIterator<Item = &'a Path>,
    output_path: &Path,
    resume: bool,
) -> Result<()> {
    let mut output = open_output_writer(output_path, resume)?;
    for path in shard_files {
        for_each_jsonl_or_zst_line(path, |line| {
            output.write_all(line.as_bytes())?;
            output.write_all(b"\n")?;
            Ok(())
        })?;
    }
    output.flush()?;
    Ok(())
}

const STAGING_SHARD_PREFIX: &str = "shards-";
const STAGING_OWNER_NEEDLE: &str = "ab-morph-run";

fn shard_staging_root(warehouse_dir: &Path, run_id: &str) -> PathBuf {
    warehouse_dir
        .join(".staging")
        .join(format!("{STAGING_SHARD_PREFIX}{run_id}"))
}

/// A staging owner is alive iff its PID exists and its cmdline names this
/// binary — the cmdline check closes the PID-reuse hole (an unrelated process
/// that recycled the PID does not block cleanup).
///
/// Any error reading `/proc/<pid>/cmdline` (including a permissions error, not
/// just "no such process") is treated as dead. On default Linux configs (no
/// `hidepid` mount option restricting `/proc` visibility), an unreadable
/// cmdline is effectively equivalent to ESRCH — the process is gone — so this
/// is a reasonable default rather than a conservative approximation.
fn staging_owner_alive(pid: u32, cmdline_needle: &str) -> bool {
    match std::fs::read(format!("/proc/{pid}/cmdline")) {
        Ok(bytes) => String::from_utf8_lossy(&bytes).contains(cmdline_needle),
        Err(_) => false,
    }
}

fn staging_entry_owner(entry: &Path) -> Option<u32> {
    std::fs::read_to_string(entry.join("pid"))
        .ok()?
        .trim()
        .parse()
        .ok()
}

fn cleanup_orphaned_shard_staging_with_needle(warehouse_dir: &Path, needle: &str) -> Result<()> {
    let staging_root = warehouse_dir.join(".staging");
    if !staging_root.exists() {
        return Ok(());
    }
    for entry in fs::read_dir(&staging_root)? {
        let entry = entry?;
        let name = entry.file_name();
        let Some(name) = name.to_str() else { continue };
        if !name.starts_with(STAGING_SHARD_PREFIX) {
            continue; // merge-owned staging ({run_id}.{pid}) has its own cleanup
        }
        let alive =
            staging_entry_owner(&entry.path()).is_some_and(|pid| staging_owner_alive(pid, needle));
        if !alive {
            eprintln!(
                "removing orphaned shard staging {} (owner dead or marker missing)",
                entry.path().display()
            );
            let is_file = entry.file_type().is_ok_and(|ft| ft.is_file());
            let result = if is_file {
                fs::remove_file(entry.path())
            } else {
                fs::remove_dir_all(entry.path())
            };
            if let Err(e) = result {
                eprintln!(
                    "warning: failed to remove orphaned shard staging {}: {e}",
                    entry.path().display()
                );
            }
        }
    }
    Ok(())
}

fn cleanup_orphaned_shard_staging(warehouse_dir: &Path) -> Result<()> {
    cleanup_orphaned_shard_staging_with_needle(warehouse_dir, STAGING_OWNER_NEEDLE)
}

fn claim_shard_staging_with_needle(shard_root: &Path, needle: &str) -> Result<()> {
    if shard_root.exists() {
        if staging_entry_owner(shard_root).is_some_and(|pid| staging_owner_alive(pid, needle)) {
            bail!(
                "warehouse run already in progress: {} is claimed by a live process",
                shard_root.display()
            );
        }
        fs::remove_dir_all(shard_root)
            .with_context(|| format!("failed to replace dead staging {}", shard_root.display()))?;
    }
    if let Some(parent) = shard_root.parent() {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }
    // Atomic claim: create_dir (not create_dir_all) on the final component so a
    // concurrent claimant racing us on the same shard_root loses with
    // AlreadyExists rather than both succeeding.
    if let Err(e) = fs::create_dir(shard_root) {
        if e.kind() == std::io::ErrorKind::AlreadyExists {
            bail!(
                "warehouse run already in progress: {} is claimed by a live process",
                shard_root.display()
            );
        }
        return Err(e).with_context(|| format!("failed to create {}", shard_root.display()));
    }
    fs::write(shard_root.join("pid"), std::process::id().to_string())
        .with_context(|| format!("failed to write pid marker in {}", shard_root.display()))?;
    Ok(())
}

fn claim_shard_staging(shard_root: &Path) -> Result<()> {
    claim_shard_staging_with_needle(shard_root, STAGING_OWNER_NEEDLE)
}

#[cfg(test)]
mod staging_guard_tests {
    use super::*;

    #[test]
    fn staging_owner_liveness_checks_pid_and_cmdline() {
        // Dead PID: far beyond pid_max.
        assert!(!staging_owner_alive(999_999_999, "ab-morph-run"));
        // Live PID, foreign cmdline: PID 1 is init/systemd, never ab-morph-run.
        assert!(!staging_owner_alive(1, "ab-morph-run"));
        // Live PID, matching cmdline: this very test process, matched against
        // its own binary name.
        let me = std::process::id();
        let exe = std::env::current_exe().unwrap();
        let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
        assert!(staging_owner_alive(me, &needle));
    }

    #[test]
    fn orphaned_shard_staging_is_removed_and_live_is_kept() {
        let root = std::env::temp_dir().join(format!("staging-test-{}", std::process::id()));
        let staging = root.join(".staging");
        // Orphan: dead PID marker.
        let dead = staging.join("shards-old-run");
        std::fs::create_dir_all(&dead).unwrap();
        std::fs::write(dead.join("pid"), "999999999").unwrap();
        // Live: this process's PID (cmdline needle in production is "ab-morph-run";
        // the cleanup fn takes the needle as a parameter so this test can pass its
        // own binary name).
        let live = staging.join("shards-live-run");
        std::fs::create_dir_all(&live).unwrap();
        std::fs::write(live.join("pid"), std::process::id().to_string()).unwrap();
        // Merge-owned staging entry (no shards- prefix): must never be touched.
        let merge = staging.join("some-run.12345");
        std::fs::create_dir_all(&merge).unwrap();

        let exe = std::env::current_exe().unwrap();
        let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
        cleanup_orphaned_shard_staging_with_needle(&root, &needle).unwrap();

        assert!(!dead.exists(), "dead-PID orphan must be removed");
        assert!(live.exists(), "live staging must be kept");
        assert!(merge.exists(), "merge-owned staging must not be touched");
        std::fs::remove_dir_all(&root).unwrap();
    }

    #[test]
    fn claim_errors_on_live_collision_and_replaces_dead() {
        let root = std::env::temp_dir().join(format!("claim-test-{}", std::process::id()));
        let shard_root = root.join(".staging").join("shards-run-x");
        // Dead prior claim → replaced silently.
        std::fs::create_dir_all(&shard_root).unwrap();
        std::fs::write(shard_root.join("pid"), "999999999").unwrap();
        claim_shard_staging_with_needle(&shard_root, "no-such-needle").unwrap();
        assert_eq!(
            std::fs::read_to_string(shard_root.join("pid")).unwrap(),
            std::process::id().to_string()
        );
        // Live claim (our own PID, matched by our own binary name) → collision error.
        let exe = std::env::current_exe().unwrap();
        let needle = exe.file_name().unwrap().to_str().unwrap().to_owned();
        let err = claim_shard_staging_with_needle(&shard_root, &needle).unwrap_err();
        assert!(err.to_string().contains("already in progress"), "{err}");
        std::fs::remove_dir_all(&root).unwrap();
    }
}

#[cfg(test)]
mod compare_isolation_tests {
    use std::sync::Arc;

    use ab_morph_diff::{Analysis, FeatureMap, Morpheme};

    use super::invalid_analyzers_for_compare;

    fn morpheme(source: &str, byte_start: usize, byte_end: usize, surface: &str) -> Morpheme {
        Morpheme {
            surface: surface.to_owned(),
            byte_span: byte_start..byte_end,
            char_span: source[..byte_start].chars().count()..source[..byte_end].chars().count(),
            features: FeatureMap::new(),
        }
    }

    fn analysis(analyzer: &str, source: &str, morphemes: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: analyzer.to_owned(),
            text_id: "t".to_owned(),
            source_text: Arc::from(source),
            morphemes,
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }

    #[test]
    fn drops_only_the_surface_mismatched_analyzer() {
        let source = "犬と猫";
        // Two analyzers segment the source consistently (surfaces byte-match).
        let fine = analysis(
            "fine",
            source,
            vec![
                morpheme(source, 0, 3, "犬"),
                morpheme(source, 3, 6, "と"),
                morpheme(source, 6, 9, "猫"),
            ],
        );
        let whole = analysis("whole", source, vec![morpheme(source, 0, 9, "犬と猫")]);
        // This analyzer's surface does not match the source bytes at its span
        // (as when a dictionary lexicalizes a decorative run): it must be the
        // only one flagged, so the other two still get compared.
        let mismatch = analysis("mismatch", source, vec![morpheme(source, 0, 3, "X")]);

        let mut analyses = vec![fine, whole, mismatch];
        let invalid = invalid_analyzers_for_compare(&analyses, source);

        assert_eq!(
            invalid
                .iter()
                .map(|(id, _)| id.as_str())
                .collect::<Vec<_>>(),
            vec!["mismatch"],
            "only the surface-mismatched analyzer should be dropped"
        );

        analyses.retain(|a| !invalid.iter().any(|(id, _)| id == &a.analyzer));
        assert_eq!(
            analyses
                .iter()
                .map(|a| a.analyzer.as_str())
                .collect::<Vec<_>>(),
            vec!["fine", "whole"],
            "the two valid analyzers survive for n-way comparison"
        );
        // The retained set is >= 2 and every survivor validates cleanly, so
        // n-way comparison over it will not fail.
        assert!(analyses.len() >= 2);
        assert!(invalid_analyzers_for_compare(&analyses, source).is_empty());
    }

    #[test]
    fn all_valid_analyzers_are_kept() {
        let source = "犬と猫";
        let a = analysis("a", source, vec![morpheme(source, 0, 9, "犬と猫")]);
        let b = analysis(
            "b",
            source,
            vec![morpheme(source, 0, 3, "犬"), morpheme(source, 3, 9, "と猫")],
        );
        assert!(invalid_analyzers_for_compare(&[a, b], source).is_empty());
    }
}
