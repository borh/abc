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
pub fn generate_reports(
    run_manifests_json: &str,
    preregistration_json: &str,
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

    if manifests.study_id != prereg.study_id {
        return Err(GenerateError::StudyIdentityMismatch {
            manifests: manifests.study_id,
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
                )?);
            }
        }
    }

    let report = StudyReport::new(manifests.study_id.clone(), rows)?;
    let mut machine_json =
        serde_json::to_string_pretty(&report).map_err(GenerateError::Serialize)?;
    machine_json.push('\n');

    let narrative_markdown = render_narrative(&report, &manifests, &prereg, &corpus_hash);

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

    // Custom baseline: measured only in the shared-instrument appendix.
    if disposition == "shared_instrument_appendix" {
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
            Missingness::Unavailable,
            vec![
                "Project-owned custom baseline; ownership grants no comparison pass. It is \
                 measured separately on shared instruments in the appendix (remediation Task 5), \
                 so this neutral existing-parser study emits no measurement for it."
                    .to_string(),
            ],
            provenance,
        )
        .map_err(GenerateError::from);
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

fn render_narrative(
    report: &StudyReport,
    manifests: &RunManifests,
    prereg: &Preregistration,
    corpus_hash: &str,
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

// --- Committed input document shapes (parse-only; unknown fields ignored). ---

#[derive(Debug, Deserialize)]
struct RunManifests {
    study_id: String,
    protocol_sha256: String,
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
