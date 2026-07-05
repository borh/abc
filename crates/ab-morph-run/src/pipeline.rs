use super::*;
use crate::options::OrthoDetectMode;
use crate::output::for_each_jsonl_or_zst_line;

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
    if jobs == 0 {
        bail!("--jobs must be at least 1");
    }
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
) -> Result<()> {
    run_analyze_aat_warehouse_impl(
        aat,
        aat_dir,
        analyzer_ids,
        warehouse_dir,
        run_id,
        jobs,
        warehouse_profile,
    )
}

pub(crate) fn run_analyze_aat_warehouse_impl(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    warehouse_dir: &Path,
    run_id: &str,
    jobs: usize,
    warehouse_profile: WarehouseProfile,
) -> Result<()> {
    if jobs == 0 {
        bail!("--jobs must be greater than zero");
    }
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    let inputs = discover_aat_inputs(aat, aat_dir)?;
    let input_mode = if aat.is_some() { "aat" } else { "aat_dir" };
    let input_path = aat
        .or(aat_dir)
        .map(|path| path.display().to_string())
        .unwrap_or_default();
    let specs = parse_analyzer_specs(analyzer_ids)?;
    let analyzers = load_analyzers(&specs)?;
    let analyzer_rows = warehouse_analyzer_rows(run_id, &specs, &analyzers)?;
    if jobs == 1 {
        let input_count = inputs.len();
        run_analyze_aat_serial(
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
                }),
                progress: Some(SerialProgress {
                    label: format!("warehouse:{run_id}"),
                    total: input_count,
                }),
                // TODO(phase2-followup): thread --ortho-detect through the parallel/warehouse/selected paths
                ortho_detect: OrthoDetectMode::Off,
                ortho_ml_model: None,
            },
        )?;
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
        // TODO(phase2-followup): thread --ortho-detect through the parallel/warehouse/selected paths
        OrthoDetectMode::Off,
        None,
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
        run_analyze_aat_serial(
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
            },
        )?
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
) -> Result<StringStatsReport> {
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
    let input_count = inputs.len() as u64;

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
        )?;
        writer.append_run_analyzers(&warehouse.analyzer_rows)?;
        Some(writer)
    } else {
        None
    };
    let mut warehouse_error_count = 0u64;
    let mut string_stats = StringStatsReport::default();
    let progress = options.progress.clone();

    // Construct the ortho detector ONCE per pipeline invocation.
    // Both `Heuristic` and `Ml` end up as `Arc<dyn OrthoDetector>` so the
    // per-document detection dispatch is uniform (the Phase 1 inlined
    // `HeuristicV1` special-case is removed).
    let detector: Option<Arc<dyn OrthoDetector>> = match options.ortho_detect {
        OrthoDetectMode::Off => None,
        OrthoDetectMode::Heuristic => {
            let vibrato = match ab_morph_analyzers::VibratoAnalyzer::unidic_cwj_default() {
                Ok(v) => Arc::new(v) as Arc<dyn ab_ortho_detect::OrthoTokenizer>,
                Err(error) => {
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
                        eprintln!("ab-morph-run: failed to load Vibrato for ortho detection: {error}");
                    }
                    return Err(error.into());
                }
            };
            Some(Arc::new(
                ab_ortho_detect::heuristic::HeuristicV1::new(
                    vibrato,
                    ab_ortho_detect::heuristic::HeuristicConfig::default(),
                ),
            ))
        }
        OrthoDetectMode::Ml => {
            let path = options.ortho_ml_model.as_ref().ok_or_else(|| {
                anyhow::anyhow!(
                    "--ortho-ml-model is required for --ortho-detect=ml (this should have been caught at CLI parse)"
                )
            })?;
            let model = ab_ortho_detect::ml::MlLogisticRegression::load(path).map_err(|e| {
                anyhow::anyhow!(
                    "failed to load ML model from {}: {}",
                    path.display(),
                    e
                )
            })?;
            Some(Arc::new(model))
        }
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
        let document = match from_aat_value(&aat) {
            Ok(document) => document,
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
        // Orthographic normalization (katakana→hiragana) for pre-war text.
        // Detection dispatch is polymorphic: the detector (heuristic or ML) was
        // constructed once above behind `Arc<dyn OrthoDetector>`.
        let (normalized_text, offset_map_opt, annotations_opt): (
            String,
            Option<ab_ortho_detect::OffsetMap>,
            Option<Vec<ab_ortho_detect::OrthoAnnotation>>,
        ) = if let Some(ref det) = detector {
            let sentences = ab_plaintext::sentence_split(&document.text);
            let annotations = det.detect(&sentences);
            if annotations.is_empty() {
                (document.text.clone(), None, None)
            } else {
                let (norm_text, map) =
                    ab_ortho_detect::ortho_normalize(&document.text, &annotations);
                (norm_text, Some(map), Some(annotations))
            }
        } else {
            (document.text.clone(), None, None)
        };

        let norm_doc = ab_plaintext::PlainTextDocument {
            text_id: document.text_id.clone(),
            source_format: document.source_format,
            text: normalized_text,
        };
        // One allocation per document, shared by every per-analyzer Analysis.
        let shared_normalized: Arc<str> = Arc::from(norm_doc.text.as_str());
        // The original text is only needed when ortho remap can fire.
        let shared_original: Option<Arc<str>> =
            offset_map_opt.is_some().then(|| Arc::from(document.text.as_str()));
        let mut analyses = Vec::new();

        for analyzer in analyzers {
            let mut analysis = match analyzer.analyze(&norm_doc) {
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
                    Ok(()) => {
                        // Honor spec invariant #2: byte_span/char_span/surface now in
                        // original-doc coords, so source_text must be the original.
                        analysis.source_text = Arc::clone(
                            shared_original
                                .as_ref()
                                .expect("offset_map_opt is Some in this arm"),
                        );
                    }
                    Err(e) => {
                        // Morphemes remain in normalized coords. Leave source_text as
                        // the normalized text the analyzer produced (consistent with
                        // the morphemes). Route the error to errors_writer for
                        // diagnosis.
                        if let Some(writer) = &mut errors_writer {
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
                        let features = warehouse::rows::morpheme_feature_rows_for_range(
                            run_id,
                            &source_id,
                            analysis,
                            start..end,
                        );
                        writer.append_morpheme_features(&features)?;
                    }
                }
            }
            match append_warehouse_nway_fact_rows(
                writer,
                run_id,
                &source_id,
                &document.text,
                &analyses,
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
    if let Some(mut writer) = warehouse_writer {
        let warehouse = options.warehouse.as_ref().expect("warehouse options");
        writer.append_runs(&[RunRow {
            schema_version: warehouse::schema::SCHEMA_VERSION,
            run_id: warehouse.paths.run_id.clone(),
            created_at_utc: chrono::Utc::now().to_rfc3339(),
            input_mode: warehouse.input_mode.to_owned(),
            input_path: warehouse.input_path.clone(),
            source_count: input_count,
            analyzer_count: warehouse.analyzer_rows.len() as u64,
            error_count: warehouse_error_count,
        }])?;
        writer.finalize()?;
    }
    Ok(string_stats)
}

pub(crate) fn run_analyze_aat_warehouse_parallel(
    inputs: Vec<PathBuf>,
    analyzers: Vec<Arc<LoadedAnalyzer>>,
    options: WarehouseParallelOptions,
) -> Result<StringStatsReport> {
    let total_inputs = inputs.len();
    let large_lanes = bounded_large_lane_count(options.jobs);
    let queue = Arc::new(Mutex::new(WarehouseWorkQueue::new(inputs, large_lanes)));
    let temp_root = std::env::temp_dir().join(format!(
        "ab-morph-run-warehouse-{}-{}",
        std::process::id(),
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .unwrap_or_default()
            .as_nanos()
    ));
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
            let queue = Arc::clone(&queue);
            handles.push(scope.spawn(move || -> Result<WarehouseShardOutput> {
                let mut shard_run_dirs = Vec::new();
                let mut warnings = Vec::new();
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
                            }),
                            progress: Some(SerialProgress {
                                label: format!("warehouse-worker-{job_index}/shard-{shard_index}"),
                                total: batch_len,
                            }),
                            // TODO(phase2-followup): thread --ortho-detect through the parallel/warehouse/selected paths
                            ortho_detect: OrthoDetectMode::Off,
                            ortho_ml_model: None,
                        },
                    );
                    complete_warehouse_work_batch(&queue, batch_is_large);
                    let batch_report = result?;
                    warnings.extend(batch_report.warnings);
                    shard_run_dirs.push(paths.final_dir);
                }
                Ok(WarehouseShardOutput {
                    shard_run_dirs,
                    warnings,
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
    for output in &mut outputs {
        report.warnings.append(&mut output.warnings);
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
    Ok(report)
}

struct WarehouseShardOutput {
    shard_run_dirs: Vec<PathBuf>,
    warnings: Vec<RunWarning>,
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
        large.sort_by(|(left_path, left_size), (right_path, right_size)| {
            left_size
                .cmp(right_size)
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

    pub(crate) fn take_batch(&mut self) -> Option<WarehouseWorkBatch> {
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

        if self.active_large_batches < self.large_lanes
            && let Some(input) = self.large.pop_front()
        {
            let shard_index = self.next_shard_index;
            self.next_shard_index += 1;
            self.active_large_batches += 1;
            return Some(WarehouseWorkBatch {
                shard_index,
                inputs: vec![input],
                is_large: true,
            });
        }

        None
    }

    pub(crate) fn complete_batch(&mut self, is_large: bool) {
        if is_large {
            self.active_large_batches = self.active_large_batches.saturating_sub(1);
        }
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
        warehouse::writer::compact_staged_table(&paths, table)?;
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
    }])?;
    writer.finalize()
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
                let string_stats = run_analyze_aat_serial(
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
                        // TODO(phase2-followup): thread --ortho-detect through the parallel/warehouse/selected paths
                        ortho_detect: OrthoDetectMode::Off,
                        ortho_ml_model: None,
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

    fs::remove_dir_all(&temp_root)
        .with_context(|| format!("failed to remove {}", temp_root.display()))?;
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
