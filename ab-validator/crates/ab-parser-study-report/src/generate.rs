//! Deterministic report generator.
//!
//! Builds the machine-readable [`StudyReport`] and a narrative report as a pure
//! function of the committed raw run manifests (parse-outcome counts) and the
//! frozen preregistration. It never imputes: every axis without committed raw
//! data is emitted as an explicit missing/non-comparable row with a caveat that
//! names the exact blocker. Only parse-completion robustness is backed by the
//! committed outcomes, so only those rows are `measured`.

use std::collections::BTreeMap;
use std::fmt::Write as _;

use serde::Deserialize;
use thiserror::Error;

use crate::{
    Axis, Candidate, ContractError, MeasurementMode, Missingness, ProvenanceStage, ResultRow,
    RowStatus, StudyReport,
};

/// Two-sided 95% normal quantile used for Wilson score intervals.
const WILSON_Z_95: f64 = 1.959963984540054;

/// The inventory whose per-work parse outcomes back the robustness axis.
const ROBUSTNESS_INVENTORY: &str = "aozorabunko-source-snapshot";
/// The inventory of official notation vectors (secondary parse-completion).
const VECTORS_INVENTORY: &str = "official-notation-vectors";

/// Both generated report representations.
#[derive(Clone, Debug)]
pub struct GeneratedReports {
    /// Canonical machine-readable `StudyReport` JSON (trailing newline).
    pub machine_json: String,
    /// Narrative report separating observations from interpretation.
    pub narrative_markdown: String,
}

/// Report-generation failure.
#[derive(Debug, Error)]
pub enum GenerateError {
    /// A committed input document was not valid JSON.
    #[error("failed to parse {input}: {source}")]
    Parse {
        /// Which committed document failed to parse.
        input: &'static str,
        /// Underlying serde error.
        source: serde_json::Error,
    },
    /// The two committed inputs disagree on the study identity.
    #[error("study identity mismatch: run manifests {manifests:?} vs preregistration {prereg:?}")]
    StudyIdentityMismatch {
        /// Study id declared by the run manifests.
        manifests: String,
        /// Study id declared by the preregistration.
        prereg: String,
    },
    /// A required inventory was absent from the run manifests.
    #[error("run manifests are missing the {0:?} inventory")]
    MissingInventory(&'static str),
    /// An executable candidate's corpus robustness lane was absent.
    #[error("missing {inventory} robustness lane for {candidate} ({mode})")]
    MissingRun {
        /// Candidate id.
        candidate: String,
        /// Measurement mode.
        mode: String,
        /// Inventory id.
        inventory: String,
    },
    /// A measured lane's denominator disagreed with the pinned inventory size.
    #[error(
        "denominator mismatch for {candidate} ({mode}): outcomes sum to {observed}, inventory pins {expected}"
    )]
    DenominatorMismatch {
        /// Candidate id.
        candidate: String,
        /// Measurement mode.
        mode: String,
        /// Sum of the outcome counts.
        observed: u64,
        /// Inventory `items`.
        expected: u64,
    },
    /// The result matrix or a row violated the frozen IR contract.
    #[error("result contract violation: {0}")]
    Contract(#[from] ContractError),
    /// Serializing the assembled report failed.
    #[error("failed to serialize the machine report: {0}")]
    Serialize(serde_json::Error),
}

/// Generates both report representations from committed input bytes.
///
/// The custom-baseline appendix manifest carries only ab-aozora's native
/// parse-outcome counts and run provenance hashes; it never changes an
/// existing-parser row or metric definition.
pub fn generate_reports(
    run_manifests_json: &str,
    preregistration_json: &str,
    appendix_manifests_json: &str,
) -> Result<GeneratedReports, GenerateError> {
    let manifests: RunManifests =
        serde_json::from_str(run_manifests_json).map_err(|source| GenerateError::Parse {
            input: "run-manifests.json",
            source,
        })?;
    let prereg: Preregistration =
        serde_json::from_str(preregistration_json).map_err(|source| GenerateError::Parse {
            input: "aozora-parser-comparison-preregistration.json",
            source,
        })?;
    let appendix: AppendixManifests =
        serde_json::from_str(appendix_manifests_json).map_err(|source| GenerateError::Parse {
            input: "appendix-run-manifests.json",
            source,
        })?;

    if manifests.study_id != prereg.study_id {
        return Err(GenerateError::StudyIdentityMismatch {
            manifests: manifests.study_id,
            prereg: prereg.study_id,
        });
    }
    if appendix.study_id != prereg.study_id {
        return Err(GenerateError::StudyIdentityMismatch {
            manifests: appendix.study_id,
            prereg: prereg.study_id,
        });
    }

    let corpus_inventory = manifests
        .inventories
        .get(ROBUSTNESS_INVENTORY)
        .ok_or(GenerateError::MissingInventory(ROBUSTNESS_INVENTORY))?;
    let corpus_hash = corpus_inventory
        .corpus_sha256
        .clone()
        .ok_or(GenerateError::MissingInventory(ROBUSTNESS_INVENTORY))?;

    let candidates: BTreeMap<&str, &PreregCandidate> = prereg
        .candidates
        .iter()
        .map(|c| (c.id.as_str(), c))
        .collect();

    let mut rows = Vec::with_capacity(108);
    for candidate in Candidate::ALL {
        let id = candidate_id(candidate);
        let meta = candidates.get(id);
        for axis in Axis::ALL {
            for &mode in candidate.required_modes() {
                rows.push(build_row(
                    candidate,
                    axis,
                    mode,
                    id,
                    meta.copied(),
                    &corpus_hash,
                    &manifests,
                    &appendix,
                )?);
            }
        }
    }

    let report = StudyReport::new(manifests.study_id.clone(), rows)?;
    let mut machine_json =
        serde_json::to_string_pretty(&report).map_err(GenerateError::Serialize)?;
    machine_json.push('\n');

    let narrative_markdown =
        render_narrative(&report, &manifests, &prereg, &corpus_hash, &appendix);

    Ok(GeneratedReports {
        machine_json,
        narrative_markdown,
    })
}

/// Serde id for a candidate, matching the frozen result schema spelling.
const fn candidate_id(candidate: Candidate) -> &'static str {
    match candidate {
        Candidate::Aozora => "aozora",
        Candidate::Aozora2 => "aozora2",
        Candidate::AozoraRs => "aozora-rs",
        Candidate::Aozora2html => "aozora2html",
        Candidate::AozoraEpub3 => "aozora-epub3",
        Candidate::AozoraParserJs => "aozora-parser.js",
        Candidate::AbAozora => "ab-aozora",
    }
}

fn mode_id(mode: MeasurementMode) -> &'static str {
    match mode {
        MeasurementMode::Native => "native",
        MeasurementMode::AdapterNormalized => "adapter_normalized",
    }
}

fn provenance_for(mode: MeasurementMode) -> Vec<ProvenanceStage> {
    match mode {
        MeasurementMode::Native => vec![ProvenanceStage::NativeParser],
        MeasurementMode::AdapterNormalized => {
            vec![ProvenanceStage::NativeParser, ProvenanceStage::Adapter]
        }
    }
}

#[allow(clippy::too_many_arguments)]
fn build_row(
    candidate: Candidate,
    axis: Axis,
    mode: MeasurementMode,
    id: &str,
    meta: Option<&PreregCandidate>,
    corpus_hash: &str,
    manifests: &RunManifests,
    appendix: &AppendixManifests,
) -> Result<ResultRow, GenerateError> {
    let (parser_revision, adapter_revision, disposition, reason) = match meta {
        Some(meta) => (
            meta.revision.clone(),
            meta.adapter_revision.clone(),
            meta.disposition.as_str(),
            meta.reason.clone(),
        ),
        None => (id.to_string(), None, "included", String::new()),
    };

    let adapter_revision = match mode {
        MeasurementMode::Native => None,
        MeasurementMode::AdapterNormalized => adapter_revision,
    };
    let provenance = provenance_for(mode);

    // Excluded candidates: the failed build is itself the frozen result.
    if disposition == "excluded" {
        return ResultRow::new(
            candidate,
            axis,
            mode,
            parser_revision,
            adapter_revision,
            corpus_hash,
            RowStatus::Failed,
            None,
            None,
            Missingness::BuildFailure,
            vec![format!(
                "Excluded from execution by the preregistration: {reason} No reproducible \
                 executable build exists, so no {axis:?} measurement can be attempted."
            )],
            provenance,
        )
        .map_err(GenerateError::from);
    }

    // Custom baseline: measured on the shared instruments in the appendix. Its
    // native parse-completion is really measured; owned-contract axes with no
    // native competitor analogue are non-comparable (never a competitor zero);
    // axes with no valid instrument stay caveated missing. Ownership grants no
    // comparison pass and no axis is imputed.
    if disposition == "shared_instrument_appendix" {
        return build_appendix_row(
            candidate,
            axis,
            mode,
            parser_revision,
            corpus_hash,
            appendix,
            provenance,
        );
    }

    // Included candidate. Robustness parse-completion is the only axis backed by
    // the committed parse-outcome counts.
    if axis == Axis::Robustness {
        let run = manifests
            .runs
            .iter()
            .find(|run| {
                run.candidate == id
                    && run.inventory == ROBUSTNESS_INVENTORY
                    && run.mode == mode_id(mode)
            })
            .ok_or_else(|| GenerateError::MissingRun {
                candidate: id.to_string(),
                mode: mode_id(mode).to_string(),
                inventory: ROBUSTNESS_INVENTORY.to_string(),
            })?;
        let numerator = run.outcomes.success;
        let denominator = run.outcomes.success + run.outcomes.failure + run.outcomes.timeout;
        let expected = manifests
            .inventories
            .get(ROBUSTNESS_INVENTORY)
            .map(|inv| inv.items)
            .unwrap_or_default();
        if denominator != expected {
            return Err(GenerateError::DenominatorMismatch {
                candidate: id.to_string(),
                mode: mode_id(mode).to_string(),
                observed: denominator,
                expected,
            });
        }
        return ResultRow::new(
            candidate,
            axis,
            mode,
            parser_revision,
            adapter_revision,
            corpus_hash,
            RowStatus::Measured,
            Some(numerator),
            Some(denominator),
            Missingness::None,
            vec![format!(
                "Parse-completion (successful parses / all {denominator} attempted works) over \
                 the pinned {ROBUSTNESS_INVENTORY} corpus, failures and timeouts retained in the \
                 denominator. A completed parse is not an assertion of output fidelity, and this \
                 count excludes the malformed-input robustness fixture, whose executable run is \
                 not committed here."
            )],
            provenance,
        )
        .map_err(GenerateError::from);
    }

    // Every other axis lacks committed raw data: missing, never zero.
    ResultRow::new(
        candidate,
        axis,
        mode,
        parser_revision,
        adapter_revision,
        corpus_hash,
        RowStatus::Failed,
        None,
        None,
        Missingness::Unavailable,
        vec![missing_axis_caveat(axis)],
        provenance,
    )
    .map_err(GenerateError::from)
}

/// Builds one ab-aozora native appendix row from the committed appendix counts.
fn build_appendix_row(
    candidate: Candidate,
    axis: Axis,
    mode: MeasurementMode,
    parser_revision: String,
    corpus_hash: &str,
    appendix: &AppendixManifests,
    provenance: Vec<ProvenanceStage>,
) -> Result<ResultRow, GenerateError> {
    let id = candidate_id(candidate);

    // Parse-completion robustness is really measured from the appendix corpus run
    // over the SAME pinned inventory and denominator as the existing-parser lanes.
    if axis == Axis::Robustness {
        let run = appendix
            .runs
            .iter()
            .find(|run| {
                run.candidate == id
                    && run.inventory == ROBUSTNESS_INVENTORY
                    && run.mode == mode_id(mode)
            })
            .ok_or_else(|| GenerateError::MissingRun {
                candidate: id.to_string(),
                mode: mode_id(mode).to_string(),
                inventory: ROBUSTNESS_INVENTORY.to_string(),
            })?;
        let numerator = run.outcomes.success;
        let denominator = run.outcomes.success + run.outcomes.failure + run.outcomes.timeout;
        let expected = appendix
            .inventories
            .get(ROBUSTNESS_INVENTORY)
            .map(|inv| inv.items)
            .unwrap_or_default();
        if denominator != expected {
            return Err(GenerateError::DenominatorMismatch {
                candidate: id.to_string(),
                mode: mode_id(mode).to_string(),
                observed: denominator,
                expected,
            });
        }
        let short_rev: String = parser_revision.chars().take(8).collect();
        let timeout_seconds = appendix.timeout_seconds;
        return ResultRow::new(
            candidate,
            axis,
            mode,
            parser_revision,
            None,
            corpus_hash,
            RowStatus::Measured,
            Some(numerator),
            Some(denominator),
            Missingness::None,
            vec![format!(
                "Project-owned custom baseline; ownership grants no comparison pass. \
                 Parse-completion (successful native `--mode aat` parses / all {denominator} \
                 attempted works) over the pinned {ROBUSTNESS_INVENTORY} corpus at the frozen \
                 baseline revision {short_rev}, failures and timeouts retained in the denominator \
                 under the frozen {timeout_seconds} s per-work timeout. A completed parse is not \
                 an assertion of output fidelity, and this count excludes the malformed-input \
                 robustness fixture, whose executable run is not committed here."
            )],
            provenance,
        )
        .map_err(GenerateError::from);
    }

    // Owned-contract axes ab-aozora emits natively as first-class parser-IR output
    // (source spans; structured diagnostics via `--mode diagnostics`) have no
    // native competitor analogue: the existing parsers reach AAT only through their
    // adapter-normalized lane, which ab-aozora does not have. Non-comparable, never
    // a competitor zero and never a blocker-missing.
    if matches!(axis, Axis::Spans | Axis::Diagnostics) {
        return ResultRow::new(
            candidate,
            axis,
            mode,
            parser_revision,
            None,
            corpus_hash,
            RowStatus::NonComparable,
            None,
            None,
            Missingness::NonComparable,
            vec![non_comparable_axis_caveat(axis)],
            provenance,
        )
        .map_err(GenerateError::from);
    }

    // Every remaining axis lacks a committed instrument for any parser: missing,
    // never zero, carrying the same blocker text as the existing-parser rows.
    ResultRow::new(
        candidate,
        axis,
        mode,
        parser_revision,
        None,
        corpus_hash,
        RowStatus::Failed,
        None,
        None,
        Missingness::Unavailable,
        vec![missing_axis_caveat(axis)],
        provenance,
    )
    .map_err(GenerateError::from)
}

fn non_comparable_axis_caveat(axis: Axis) -> String {
    match axis {
        Axis::Spans => "Owned-contract axis with no native competitor analogue: ab-aozora emits \
            source spans on every node as first-class parser-IR (`--mode aat`) output, but the \
            existing parsers produce no native source-span output and reach AAT only through their \
            adapter-normalized lane, which the custom baseline does not have. There is therefore no \
            like-for-like native comparison to draw; this is non-comparable, not a competitor zero \
            and not a measurement blocker."
            .to_string(),
        Axis::Diagnostics => "Owned-contract axis with no native competitor analogue: ab-aozora \
            emits structured diagnostics natively (`--mode diagnostics`), whereas the existing \
            parsers' native diagnostic schemas differ per parser and expose no comparable native \
            field. No cross-parser native diagnostic metric exists to compare against, so this is \
            non-comparable, not a competitor zero and not a measurement blocker."
            .to_string(),
        _ => unreachable!("only spans and diagnostics are owned-contract non-comparable axes"),
    }
}

fn missing_axis_caveat(axis: Axis) -> String {
    match axis {
        Axis::ConstructCoverage => "No committed construct-recognition oracle: the run manifests \
            carry only parse-outcome counts, not per-construct recognized/total counts. A real \
            measurement needs an authoritative construct inventory and per-parser recognition \
            capture over the official notation vectors."
            .to_string(),
        Axis::Fidelity => "No committed fidelity oracle: visible-text byte agreement, structure \
            agreement, and ruby/gaiji/note counts require an authoritative reference rendering \
            that is absent from the committed artifacts."
            .to_string(),
        Axis::Diagnostics => "No committed diagnostic capture: the diagnostics fixture is frozen, \
            but no per-parser diagnostic output and no cross-parser severity/span matching run is \
            committed. Native diagnostic schemas differ per parser, so no comparable metric can \
            be derived from parse-outcome counts alone."
            .to_string(),
        Axis::Spans => "No committed span oracle: exact/overlapping source-span, covered-byte, \
            and invalid-span counts require an authoritative span-bearing reference that is \
            absent from the committed artifacts."
            .to_string(),
        Axis::Performance => "No committed latency measurements: the run manifests carry \
            parse-outcome and timeout counts but not per-repetition wall time, and host \
            comparability is unavailable for every lane except the aozora-rs native replacement. \
            No Kaplan-Meier or bootstrap latency statistic can be computed."
            .to_string(),
        Axis::Maintenance => "No committed maintenance observations: last release/commit at the \
            pin, bus-factor indicators, and issue/release cadence are absent from the committed \
            artifacts."
            .to_string(),
        Axis::Packaging => "No committed packaging measurement: the reproducible build produced \
            the pinned execution binaries, but locked-dependency audit, runtime/toolchain \
            requirements, and artifact size are not recorded in the committed artifacts."
            .to_string(),
        Axis::License => "No committed license audit: declared project and bundled-dependency \
            licenses and redistribution constraints are absent from the committed artifacts."
            .to_string(),
        Axis::Robustness => unreachable!("robustness is measured from committed outcomes"),
    }
}

/// Wilson score interval bounds for a binomial proportion at 95%.
fn wilson_interval(successes: u64, total: u64) -> (f64, f64) {
    if total == 0 {
        return (0.0, 0.0);
    }
    let n = total as f64;
    let phat = successes as f64 / n;
    let z = WILSON_Z_95;
    let z2 = z * z;
    let denom = 1.0 + z2 / n;
    let center = (phat + z2 / (2.0 * n)) / denom;
    let margin = (z / denom) * (phat * (1.0 - phat) / n + z2 / (4.0 * n * n)).sqrt();
    ((center - margin).max(0.0), (center + margin).min(1.0))
}

fn axis_label(axis: Axis) -> &'static str {
    match axis {
        Axis::ConstructCoverage => "construct_coverage",
        Axis::Fidelity => "fidelity",
        Axis::Robustness => "robustness",
        Axis::Diagnostics => "diagnostics",
        Axis::Spans => "spans",
        Axis::Performance => "performance",
        Axis::Maintenance => "maintenance",
        Axis::Packaging => "packaging",
        Axis::License => "license",
    }
}

fn mode_label(mode: MeasurementMode) -> &'static str {
    match mode {
        MeasurementMode::Native => "native",
        MeasurementMode::AdapterNormalized => "adapter-normalized",
    }
}

/// Candidates whose *native* lane is produced by a `*-adapter` wrapper binary
/// run in `--mode html` rather than a direct parser invocation, derived from the
/// committed execution contracts. Sorted and deduped for deterministic output.
fn wrapper_native_candidates(manifests: &RunManifests) -> Vec<String> {
    let mut names: Vec<String> = manifests
        .execution_contracts
        .values()
        .filter(|contract| {
            contract.mode == "native"
                && contract.required_program_basename.ends_with("-adapter")
                && contract.required_argv.iter().any(|arg| arg == "html")
        })
        .map(|contract| contract.candidate.clone())
        .collect();
    names.sort();
    names.dedup();
    names
}

fn render_narrative(
    report: &StudyReport,
    manifests: &RunManifests,
    prereg: &Preregistration,
    corpus_hash: &str,
    appendix: &AppendixManifests,
) -> String {
    let mut out = String::new();
    let study = report.study_id();

    out.push_str("# Aozora parser neutral comparison — generated report\n\n");
    out.push_str(
        "This report is regenerated byte-for-byte from the committed raw run manifests and the \
         frozen preregistration; it is a neutral research artifact and cannot admit or \
         release-qualify any parser. Native and adapter-normalized behavior are separate result \
         lanes and are never conflated.\n\n",
    );

    out.push_str("## Provenance\n\n");
    let _ = writeln!(out, "- Study: `{study}`");
    let _ = writeln!(out, "- Protocol hash: `{}`", manifests.protocol_sha256);
    let _ = writeln!(out, "- Corpus content hash: `{corpus_hash}`");
    for (name, inv) in &manifests.inventories {
        let _ = writeln!(
            out,
            "- Inventory `{name}`: {} items, revision `{}`, inventory hash `{}`",
            inv.items, inv.revision, inv.inventory_sha256
        );
    }
    let _ = writeln!(
        out,
        "- Timeout: {} seconds; measured lanes: {}",
        manifests.timeout_seconds,
        report
            .rows()
            .iter()
            .filter(|r| r.status() == RowStatus::Measured)
            .count()
    );
    out.push('\n');

    // Observations: measured facts only, native and adapter lanes separated.
    out.push_str("## Observations\n\n");
    out.push_str(
        "Only parse-completion robustness is backed by the committed parse-outcome counts. Each \
         count below is exact, with failures and timeouts retained in the denominator, and a \
         two-sided 95% Wilson score interval. The denominator is the pinned corpus size \
         (`aozorabunko-source-snapshot`, unit = work).\n\n",
    );

    for mode in [MeasurementMode::Native, MeasurementMode::AdapterNormalized] {
        let _ = writeln!(
            out,
            "### Robustness — parse completion — {} lane\n",
            mode_label(mode)
        );
        out.push_str("| Candidate | Successes | Denominator | Rate | Wilson 95% CI |\n");
        out.push_str("| --- | ---: | ---: | ---: | --- |\n");
        for candidate in Candidate::ALL {
            // The custom baseline is measured separately in the appendix below.
            if candidate == Candidate::AbAozora {
                continue;
            }
            let Some(row) = report.rows().iter().find(|r| {
                r.candidate() == candidate
                    && r.axis() == Axis::Robustness
                    && r.measurement_mode() == mode
            }) else {
                continue;
            };
            if row.status() != RowStatus::Measured {
                continue;
            }
            let (num, den) = (
                row.numerator().unwrap_or_default(),
                row.denominator().unwrap_or_default(),
            );
            let rate = if den == 0 {
                0.0
            } else {
                num as f64 / den as f64
            };
            let (lo, hi) = wilson_interval(num, den);
            let _ = writeln!(
                out,
                "| `{}` | {num} | {den} | {rate:.6} | [{lo:.6}, {hi:.6}] |",
                candidate_id(candidate)
            );
        }
        out.push('\n');
    }

    out.push_str("### Secondary parse completion over the official notation vectors\n\n");
    out.push_str(
        "The notation vectors are the construct-coverage corpus, not a robustness corpus; their \
         parse-outcome counts are reported here only as a secondary parse-completion observation \
         and never as construct coverage.\n\n",
    );
    out.push_str("| Candidate | Mode | Successes | Denominator |\n");
    out.push_str("| --- | --- | ---: | ---: |\n");
    for run in &manifests.runs {
        if run.inventory != VECTORS_INVENTORY {
            continue;
        }
        let den = run.outcomes.success + run.outcomes.failure + run.outcomes.timeout;
        let mode = if run.mode == "native" {
            "native"
        } else {
            "adapter-normalized"
        };
        let _ = writeln!(
            out,
            "| `{}` | {mode} | {} | {den} |",
            run.candidate, run.outcomes.success
        );
    }
    out.push('\n');

    // Interpretations: explicitly labelled, no aggregate winner.
    out.push_str("## Interpretations\n\n");
    out.push_str(
        "The following statements are interpretation, not measurement. The preregistration bans \
         any unqualified aggregate score or overall winner, and this report publishes none.\n\n",
    );
    out.push_str(
        "- High native parse-completion over the pinned corpus indicates only that a parser \
          terminated without a hard failure on the corpus; it says nothing about output \
          fidelity, diagnostics, spans, or performance, all of which are unmeasured here.\n",
    );
    out.push_str(
        "- Adapter-normalized completion may differ from native completion for the same parser \
          (for example the aozora2 adapter incurs timeouts, and the aozora-epub3 adapter incurs \
          more failures than its native lane). Adapter-introduced behavior is never credited as \
          native capability.\n",
    );
    let wrapper_native = wrapper_native_candidates(manifests);
    if !wrapper_native.is_empty() {
        let list = wrapper_native
            .iter()
            .map(|candidate| format!("`{candidate}`"))
            .collect::<Vec<_>>()
            .join(", ");
        let _ = writeln!(
            out,
            "- Native-lane attribution: for {list} the \"native\" lane is produced by the \
              parser's `*-adapter` binary run in `--mode html`, so \"native\" here denotes the \
              parser-native output format produced via a thin wrapper, not a direct parser \
              invocation; that same binary's adapter-normalized lane is `--mode aat`, so the two \
              lanes share one binary differing only by `--mode`."
        );
    }
    out.push_str(
        "- Because only one axis is measured, no cross-axis ranking is possible; any downstream \
          conclusion must name its use case and its weighting.\n\n",
    );

    // Limitations.
    out.push_str("## Limitations\n\n");
    out.push_str(
        "- Parse completion is a robustness signal, not a fidelity signal: a successful parse can \
          still drop or corrupt content, which this study does not detect.\n",
    );
    out.push_str(
        "- Host performance comparability is unavailable for all lanes except the aozora-rs \
          native replacement, so no performance stratum can be compared.\n",
    );
    out.push_str(
        "- The counts derive from external corpus-scale artifacts summarized as outcome counts in \
          the committed manifests; the report re-derives no per-work bytes.\n\n",
    );

    // Missing data: every missing axis and why.
    out.push_str("## Missing data\n\n");
    out.push_str(
        "Every axis without committed raw data is retained as an explicit missing row (never a \
         zero). The blocker per axis and candidate is below.\n\n",
    );
    out.push_str("| Candidate | Axis | Mode | Missingness | Blocker |\n");
    out.push_str("| --- | --- | --- | --- | --- |\n");
    for row in report.rows() {
        if row.status() == RowStatus::Measured {
            continue;
        }
        // The custom baseline's missing and non-comparable axes are enumerated in
        // its dedicated appendix, not mixed into the neutral existing-parser table.
        if row.candidate() == Candidate::AbAozora {
            continue;
        }
        let caveat = row.caveats().first().cloned().unwrap_or_default();
        let _ = writeln!(
            out,
            "| `{}` | {} | {} | {:?} | {} |",
            candidate_id(row.candidate()),
            axis_label(row.axis()),
            mode_label(row.measurement_mode()),
            row.missingness(),
            caveat.replace('\n', " ").replace('|', "\\|")
        );
    }
    out.push('\n');

    // Use-case sensitivity.
    out.push_str("## Use-case sensitivity\n\n");
    out.push_str(
        "Any weighting must name its downstream use case; the preregistration requires a \
         published sensitivity analysis for every non-zero weight. With a single measured axis, \
         the sensitivity space is bounded:\n\n",
    );
    out.push_str(
        "- Batch-conversion reliability use case (weight parse completion only): the corpus \
          parse-completion rates above are the entire ranking input; ties within the Wilson \
          intervals are not separable. No other axis contributes because none is measured.\n",
    );
    out.push_str(
        "- Fidelity-sensitive or diagnostics-sensitive use cases: not decidable here, because \
          the fidelity, diagnostics, spans, and performance axes are all missing. A weighting \
          that assigns them non-zero weight cannot be evaluated until those axes are measured, \
          and this report must not be used to rank for such a use case.\n\n",
    );

    render_custom_parser_appendix(&mut out, report, appendix);

    out.push_str("## Candidate dispositions\n\n");
    for candidate in &prereg.candidates {
        let _ = writeln!(
            out,
            "- `{}`: {} (revision `{}`)",
            candidate.id, candidate.disposition, candidate.revision
        );
    }
    out.push('\n');

    out
}

/// Success and attempted-work counts for one ab-aozora appendix lane.
fn appendix_run_counts(appendix: &AppendixManifests, inventory: &str) -> Option<(u64, u64)> {
    appendix
        .runs
        .iter()
        .find(|run| run.candidate == "ab-aozora" && run.inventory == inventory)
        .map(|run| {
            (
                run.outcomes.success,
                run.outcomes.success + run.outcomes.failure + run.outcomes.timeout,
            )
        })
}

/// Renders the project-owned custom-parser appendix: real native measurement,
/// non-comparable owned-contract axes, missing axes, and a falsifiable
/// sensitivity analysis for the only claim the appendix actually makes.
fn render_custom_parser_appendix(
    out: &mut String,
    report: &StudyReport,
    appendix: &AppendixManifests,
) {
    // Revision and timeout are stamped from data (the ab-aozora machine row's
    // parser revision and the committed appendix timeout), not literals.
    let baseline_revision = report
        .rows()
        .iter()
        .find(|row| row.candidate() == Candidate::AbAozora)
        .map(|row| row.parser_revision().to_string())
        .unwrap_or_default();
    let timeout_seconds = appendix.timeout_seconds;

    out.push_str("## Custom-parser shared-instrument appendix\n\n");
    let _ = write!(
        out,
        "`ab-aozora` is the project-owned baseline. It is measured here separately, native-only \
         (it emits parser-IR/AAT natively via `--mode aat`; it has no adapter-normalized lane), on \
         the same frozen instruments as the existing parsers and at the frozen baseline revision \
         `{baseline_revision}`. Ownership grants it no comparison pass: this \
         appendix can neither admit nor release-qualify the parser, and no axis is imputed.\n\n"
    );

    // Measured native robustness, with the same denominator and Wilson interval
    // as the existing-parser robustness lane.
    out.push_str("### Robustness — native parse completion (measured)\n\n");
    if let Some((corpus_num, corpus_den)) = appendix_run_counts(appendix, ROBUSTNESS_INVENTORY) {
        let rate = if corpus_den == 0 {
            0.0
        } else {
            corpus_num as f64 / corpus_den as f64
        };
        let (lo, hi) = wilson_interval(corpus_num, corpus_den);
        out.push_str("| Candidate | Lane | Successes | Denominator | Rate | Wilson 95% CI |\n");
        out.push_str("| --- | --- | ---: | ---: | ---: | --- |\n");
        let _ = writeln!(
            out,
            "| `ab-aozora` | native | {corpus_num} | {corpus_den} | {rate:.6} | [{lo:.6}, {hi:.6}] |"
        );
        out.push('\n');
        if let Some((vec_num, vec_den)) = appendix_run_counts(appendix, VECTORS_INVENTORY) {
            let _ = writeln!(
                out,
                "Secondary parse completion over the official notation vectors (construct-coverage \
                 corpus, reported only as a parse-completion observation, never as construct \
                 coverage): {vec_num} / {vec_den} native `--mode aat` successes.\n"
            );
        }
    }

    // Falsifiable sensitivity analysis for the measured claim.
    out.push_str("### Sensitivity analysis (falsifiable)\n\n");
    if let Some((corpus_num, corpus_den)) = appendix_run_counts(appendix, ROBUSTNESS_INVENTORY) {
        out.push_str(
            "Claim under test: `ab-aozora` native parse-completion sits at the pinned corpus \
             ceiling and is *matched but not exceeded* by the strongest existing-parser native \
             lanes (`aozora2`, `aozora-rs`, `aozora2html`, each also at the ceiling over the same \
             denominator); parse-completion therefore does not separate the custom baseline from \
             the strongest existing parsers. This is a comparative claim between measured lanes, \
             so it is falsifiable.\n\n",
        );
        let _ = write!(
            out,
            "Adversarial failure/timeout reweighting: reclassify the k worst works as failures \
             (the frozen {timeout_seconds} s run produced zero ab-aozora failures or timeouts), \
             for k drawn from the failure/timeout counts actually observed on competitor lanes. \
             The reweighted rate is (den − k) / den with a two-sided 95% Wilson interval:\n\n"
        );
        out.push_str(
            "| Adversarial k | Source of k | Reweighted successes | Rate | Wilson 95% CI |\n",
        );
        out.push_str("| ---: | --- | ---: | ---: | --- |\n");
        for (k, source) in [
            (1_u64, "aozora native corpus failures"),
            (12, "aozora2 adapter corpus timeouts"),
            (50, "aozora-epub3 native corpus failures"),
            (113, "aozora-epub3 adapter corpus failures"),
        ] {
            if k > corpus_num {
                continue;
            }
            let num = corpus_num - k;
            let rate = num as f64 / corpus_den as f64;
            let (lo, hi) = wilson_interval(num, corpus_den);
            let _ = writeln!(
                out,
                "| {k} | {source} | {num} | {rate:.6} | [{lo:.6}, {hi:.6}] |"
            );
        }
        out.push('\n');
        let _ = write!(
            out,
            "Falsifier: for any k ≥ 1 the reweighted rate drops below 1 and its Wilson upper bound \
             falls below 1, so the exact-ceiling reading is fragile to even a single adversarial \
             reclassification. The measured result must be read as \"parse-completion ceiling under \
             the frozen {timeout_seconds} s per-work timeout on the measurement host,\" not as an \
             absolute or host-independent guarantee, and never as fidelity, diagnostics, span, or \
             performance superiority (those axes are non-comparable or missing below).\n\n"
        );
    }

    // Owned-contract non-comparable axes and missing axes, from the machine rows.
    out.push_str("### Owned-contract non-comparable and missing axes\n\n");
    out.push_str(
        "Owned-contract axes ab-aozora emits natively but for which the existing parsers offer no \
         native analogue are non-comparable (never a competitor zero); axes with no committed \
         instrument for any parser stay caveated missing. No value is imputed for either.\n\n",
    );
    out.push_str("| Axis | Mode | Status | Missingness | Blocker |\n");
    out.push_str("| --- | --- | --- | --- | --- |\n");
    for row in report.rows() {
        if row.candidate() != Candidate::AbAozora || row.status() == RowStatus::Measured {
            continue;
        }
        let caveat = row.caveats().first().cloned().unwrap_or_default();
        let _ = writeln!(
            out,
            "| {} | {} | {:?} | {:?} | {} |",
            axis_label(row.axis()),
            mode_label(row.measurement_mode()),
            row.status(),
            row.missingness(),
            caveat.replace('\n', " ").replace('|', "\\|")
        );
    }
    out.push('\n');
}

// --- Committed input document shapes (parse-only; unknown fields ignored). ---

#[derive(Debug, Deserialize)]
struct RunManifests {
    study_id: String,
    protocol_sha256: String,
    timeout_seconds: u64,
    inventories: BTreeMap<String, Inventory>,
    runs: Vec<CompactRun>,
    /// Per-lane execution contracts, keyed by program hash. Used to disclose
    /// which candidates' "native" lane is actually produced by a `*-adapter`
    /// wrapper binary rather than a direct parser invocation.
    #[serde(default)]
    execution_contracts: BTreeMap<String, ExecutionContract>,
}

/// One lane's frozen execution contract. Only the fields the report reads are
/// named; the committed provenance hashes are ignored here.
#[derive(Debug, Deserialize)]
struct ExecutionContract {
    candidate: String,
    mode: String,
    required_program_basename: String,
    #[serde(default)]
    required_argv: Vec<String>,
}

/// Custom-baseline appendix: ab-aozora's native parse-outcome counts and run
/// provenance. Extra provenance fields (execution contracts, program/derivation
/// hashes) are committed for verification but ignored here.
#[derive(Debug, Deserialize)]
struct AppendixManifests {
    study_id: String,
    timeout_seconds: u64,
    inventories: BTreeMap<String, Inventory>,
    runs: Vec<CompactRun>,
}

#[derive(Debug, Deserialize)]
struct Inventory {
    #[serde(default)]
    corpus_sha256: Option<String>,
    inventory_sha256: String,
    items: u64,
    revision: String,
}

#[derive(Debug, Deserialize)]
struct CompactRun {
    candidate: String,
    inventory: String,
    mode: String,
    #[serde(default)]
    outcomes: Outcomes,
}

#[derive(Debug, Default, Deserialize)]
struct Outcomes {
    #[serde(default)]
    success: u64,
    #[serde(default)]
    failure: u64,
    #[serde(default)]
    timeout: u64,
}

#[derive(Clone, Debug, Deserialize)]
struct Preregistration {
    study_id: String,
    candidates: Vec<PreregCandidate>,
}

#[derive(Clone, Debug, Deserialize)]
struct PreregCandidate {
    id: String,
    disposition: String,
    revision: String,
    #[serde(default)]
    adapter_revision: Option<String>,
    #[serde(default)]
    reason: String,
}
