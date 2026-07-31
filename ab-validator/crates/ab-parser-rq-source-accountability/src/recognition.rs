use ab_aozora_aat::decode_source_bytes;
use ab_aozora_capture::{CaptureGeneration, verify_capture_generation};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};

use crate::interval::{Interval, intersect, normalize, subtract, total_len};

const POLICY_SCHEMA_BYTES: &[u8] =
    include_bytes!("../../../../abc/schemas/parser-rq-classified-source-policy.schema.json");
const AUTHORITY_BYTES: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-classified-source-authority-v1.json");
/// v4 because the accent round-trip rule changed; the wire format did not.
///
/// At v2, `eligible_bytes` moved from the whole decoded file to the body
/// region and a metadata attribution measure appeared beside it. At v3 the
/// metadata attribution ratio moved off the region denominator and onto a
/// content one: `metadata.content_bytes`, the same regions with line
/// terminators and wholly blank lines removed. See `RecognitionMetadata` for
/// why. At v4 the `accent_decomposition` proof obligation changed from "the
/// whole `〔…〕` span re-decomposes to the normalized form" to "the proof
/// pair is exactly one row of the policy's `accent_mappings`" — the capture
/// adapter now proves each digraph substitution at its own site, which is
/// what makes the obligation checkable at all (a whole-span form conflated
/// the accent rewrite with CRLF normalization and failed round-trip on any
/// multi-line span of a CRLF work). A v3 record and a v4 record can carry
/// identical field names and mean different things, which is exactly what a
/// version string exists to prevent — the record shape
/// (`abc/parser-rq-source-recognition-work/v3`) is unchanged, so nothing but
/// the instrument string distinguishes which obligation the numbers passed.
///
/// The predicate set binds this version since 2026-07-31, when both of its
/// thresholds were consciously declared for what v4 measures rather than
/// inherited: `source_span_coverage` `:= 1.0` over the body-region
/// denominator, and `metadata_attribution` `:= 1.0` over
/// `metadata.content_bytes`. Because the qualification identity takes
/// `instrument_versions` FROM the predicate set and the readers require the
/// record's `instrument_version` to match it, the next semantic change here
/// reopens the same quarantine: the versions disagree and both observations
/// are `:unavailable` until the predicate's owner redeclares, never scored
/// against a threshold declared for different semantics.
///
/// The metadata threshold's `:= 1.0` is met by every work of both the design
/// and the held-out exploratory sample, so today it refuses nothing. That is
/// its designed value, not a validation claim: it fails the gate the first
/// time the archive produces packaging these classifiers do not know — a
/// header with no fence, a colophon field in an unseen form, a new
/// boilerplate sentence.
const INSTRUMENT_VERSION: &str = "parser-rq-source-recognition-v4";

#[derive(Clone, Debug)]
pub struct RecognitionInput {
    pub decoded_source: Vec<u8>,
    pub parser_output: Vec<u8>,
    pub raw_diagnostics: Vec<u8>,
    pub ledger_bytes: Vec<u8>,
    pub policy_bytes: Vec<u8>,
    pub generation_manifest: Vec<u8>,
    pub qualification_identity_ref: String,
    pub work_id: String,
    pub ledger_locator: String,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(rename_all = "lowercase")]
pub enum RecognitionStatus {
    Ok,
    Unavailable,
}

#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionInterval {
    pub start: u64,
    pub end: u64,
}

/// The declared three-region partition of the decoded source, as published.
///
/// Recorded even though v1 still measures over the whole file, so the regions
/// are auditable in the artifact before any measurement moves onto them: a
/// reader can check the conservation identity from the published record alone.
#[derive(Clone, Copy, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionRegions {
    pub header: RecognitionInterval,
    pub body: RecognitionInterval,
    pub tail: RecognitionInterval,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionBlobRef {
    pub sha256: String,
    pub bytes: u64,
    pub media_type: String,
    pub locator: String,
}

/// The metadata population: the header and tail regions measured together.
///
/// They are recorded as distinct regions so a failure localizes to one end of
/// the file, but they qualify under a single conjunctive predicate, so their
/// totals are folded here. This block is what keeps the partition from being a
/// denominator reduction: the metadata bytes leave the body measure for a
/// different accounted region, not for nowhere.
///
/// The measure is ATTRIBUTION, not byte accounting, and the words are not
/// interchangeable. A byte is attributed when a fact whose role the policy
/// lists in `metadata_attributing_roles` covers it, and `preserved_opaque`
/// never attributes. An earlier draft counted every accounted interval here,
/// which let a `preserved_opaque` fact -- the explicit statement that nothing
/// is claimed about the bytes -- raise the number, and let the line-ending
/// normalizations the sanitizer emits across the whole file stand in for
/// knowledge about the packaging.
///
/// ## Two denominators, and which one the ratio is over
///
/// `eligible_bytes` is the header and tail regions whole. It is what makes the
/// partition assertable — `eligible + metadata.eligible == decoded`, disjoint —
/// and it is published for that reason and kept for it.
///
/// `content_bytes` is the same regions with **line terminators and wholly
/// blank lines removed**, and it is the denominator the attribution ratio is
/// over. A line terminator is not packaging that can be understood or
/// misunderstood; it is the delimiter between packaging items, present in
/// identical quantity whether the instrument recognizes every construct or
/// none. Leaving it in the denominator had two consequences, both wrong:
///
/// - the measure could not reach 1.0 however complete the classifiers were,
///   because 34,464 bytes of a 597-work sample were delimiters no construct is
///   permitted to claim; and
/// - **the measure scored a CRLF work strictly below a byte-identical LF work**
///   whose packaging is understood exactly as well, because CRLF puts two
///   bytes per line into the denominator where LF puts one. Keeping
///   `structural_newline` out of `metadata_attributing_roles` was meant to stop
///   the measure moving with line endings; excluding terminators from the
///   numerator while keeping them in the denominator did not remove that
///   sensitivity, it inverted it.
///
/// A wholly blank line is excluded on the same footing, and one step further:
/// a line of nothing but layout whitespace is a blank line spelled
/// differently, and counting one but not the other would make the measure
/// depend on whether a transcriber's editor stripped trailing spaces. That is
/// the failure this instrument already fixed once, in `region_lines`.
///
/// This is not a denominator reduction of the kind Q11 rejected. No byte
/// leaves the accounting: the region totals are still published, still sum to
/// the decoded file, and still carry their unattributed intervals. What
/// changed is which of the two published denominators the ratio is taken over.
#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionMetadata {
    pub eligible_bytes: u64,
    pub content_bytes: u64,
    pub attributed_bytes: u64,
    pub unattributed_bytes: u64,
    pub unattributed_content_bytes: u64,
    pub attributed: Vec<RecognitionInterval>,
    pub unattributed: Vec<RecognitionInterval>,
    pub unattributed_content: Vec<RecognitionInterval>,
}

#[derive(Clone, Debug, Deserialize, Eq, PartialEq, Serialize)]
#[serde(deny_unknown_fields)]
pub struct RecognitionWorkRecord {
    pub schema_version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub qualification_identity_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capture_generation_ref: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub policy_hash: Option<String>,
    pub instrument_version: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub work_id: Option<String>,
    pub coordinate_system: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ledger: Option<RecognitionBlobRef>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub regions: Option<RecognitionRegions>,
    pub status: RecognitionStatus,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub eligible_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recognized_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounted_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_gap_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unaccounted_bytes: Option<u64>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub recognized: Option<Vec<RecognitionInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub accounted: Option<Vec<RecognitionInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub semantic_gaps: Option<Vec<RecognitionInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub unaccounted: Option<Vec<RecognitionInterval>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<RecognitionMetadata>,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub errors: Vec<String>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct RecognitionAnalysis {
    pub record: RecognitionWorkRecord,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Policy {
    #[serde(rename = "schema_version")]
    _schema_version: String,
    policy_id: String,
    policy_hash: String,
    #[serde(rename = "parser")]
    _parser: String,
    #[serde(rename = "coordinate_system")]
    _coordinate_system: String,
    #[serde(rename = "roles")]
    _roles: Vec<String>,
    #[serde(rename = "dispositions")]
    _dispositions: Vec<String>,
    metadata_attributing_roles: Vec<String>,
    accent_mappings: Vec<Value>,
    rules: Vec<PolicyRule>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct PolicyRule {
    construct_id: String,
    source_role: String,
    disposition: String,
    evidence_class: String,
    target_relation: Option<String>,
    witness_kind: Option<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct Ledger {
    #[serde(rename = "schema_version")]
    _schema_version: String,
    #[serde(rename = "ledger_schema_hash")]
    _ledger_schema_hash: String,
    #[serde(rename = "qualification_identity_ref")]
    _qualification_identity_ref: String,
    #[serde(rename = "parser")]
    _parser: String,
    #[serde(rename = "instrument_version")]
    _instrument_version: String,
    #[serde(rename = "original_source")]
    _original_source: Value,
    #[serde(rename = "decoded_source")]
    _decoded_source: Value,
    policy_id: String,
    policy_hash: String,
    #[serde(rename = "coordinate_system")]
    _coordinate_system: String,
    parser_status: String,
    entries: Vec<LedgerEntry>,
    errors: Vec<String>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct LedgerEntry {
    start: u64,
    end: u64,
    construct_id: String,
    source_role: String,
    disposition: String,
    evidence_class: String,
    target_identity: Option<TargetIdentity>,
    target_pointer: Option<String>,
    parser_evidence_code: String,
    construct_witness: Option<ConstructWitness>,
    normalization_proof: Option<NormalizationProof>,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct TargetIdentity {
    artifact_ref: String,
    value_hash: String,
    relation: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct ConstructWitness {
    construct_id: String,
    start: u64,
    end: u64,
    source_form: String,
}

#[derive(Deserialize)]
#[serde(deny_unknown_fields)]
struct NormalizationProof {
    source_form: String,
    normalized_form: String,
    source_bytes_hash: String,
    normalized_bytes_hash: String,
    inverse_rule: String,
}

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn canonical_json(value: &Value) -> Option<Vec<u8>> {
    let text = ab_diff_utils::canonical_json_string(value).ok()?;
    let mut bytes = text.into_bytes();
    bytes.push(b'\n');
    Some(bytes)
}

/// One interval for a non-empty range, none for an empty one. An empty region
/// is legitimate, and an empty interval is not representable.
/// The content lines of a metadata region: every line carrying at least one
/// non-whitespace character, its terminator excluded.
///
/// The span kept for such a line is the WHOLE line, layout whitespace
/// included, because that is the span its fact claims — a colophon
/// continuation is recognized by its indentation, so a denominator that
/// excluded the indentation would not contain its own numerator.
///
/// A line with no non-whitespace character contributes nothing. That covers
/// the blank line between the body and the colophon, the blank lines between
/// legend entries, and the handful of lines written as nothing but spaces,
/// which are the same thing spelled differently.
fn content_lines(text: &str, region_start: usize, bound: usize) -> Vec<Interval> {
    let mut intervals = Vec::new();
    let mut offset = region_start;
    for raw in text.split_inclusive('\n') {
        let stripped = raw.trim_end_matches(['\n', '\r']);
        if !stripped.trim().is_empty()
            && let Ok(interval) = Interval::new(offset, offset + stripped.len(), bound)
        {
            intervals.push(interval);
        }
        offset += raw.len();
    }
    intervals
}

fn interval_list(range: &core::ops::Range<usize>, bound: usize) -> Vec<Interval> {
    Interval::new(range.start, range.end, bound)
        .ok()
        .filter(|_| range.start < range.end)
        .into_iter()
        .collect()
}

fn interval_of(range: &core::ops::Range<usize>) -> RecognitionInterval {
    RecognitionInterval {
        start: range.start as u64,
        end: range.end as u64,
    }
}

fn wire(intervals: &[Interval]) -> Vec<RecognitionInterval> {
    intervals
        .iter()
        .map(|interval| RecognitionInterval {
            start: interval.start() as u64,
            end: interval.end() as u64,
        })
        .collect()
}

fn base_record() -> RecognitionWorkRecord {
    RecognitionWorkRecord {
        // Record shape is unchanged at the v4 instrument bump; see the
        // INSTRUMENT_VERSION note for what changed instead.
        schema_version: "abc/parser-rq-source-recognition-work/v3".to_owned(),
        qualification_identity_ref: None,
        capture_generation_ref: None,
        policy_hash: None,
        instrument_version: INSTRUMENT_VERSION.to_owned(),
        work_id: None,
        coordinate_system: "decoded_utf8".to_owned(),
        ledger: None,
        regions: None,
        status: RecognitionStatus::Unavailable,
        eligible_bytes: None,
        recognized_bytes: None,
        accounted_bytes: None,
        semantic_gap_bytes: None,
        unaccounted_bytes: None,
        recognized: None,
        accounted: None,
        semantic_gaps: None,
        unaccounted: None,
        metadata: None,
        errors: Vec::new(),
    }
}

fn unavailable(mut record: RecognitionWorkRecord, error: &str) -> RecognitionAnalysis {
    record.errors.push(error.to_owned());
    RecognitionAnalysis { record }
}

fn validate_policy(bytes: &[u8]) -> Option<(Policy, Value)> {
    let value: Value = serde_json::from_slice(bytes).ok()?;
    let schema: Value = serde_json::from_slice(POLICY_SCHEMA_BYTES).ok()?;
    jsonschema::validator_for(&schema)
        .ok()?
        .validate(&value)
        .ok()?;
    let policy: Policy = serde_json::from_value(value.clone()).ok()?;
    let authority: Value = serde_json::from_slice(AUTHORITY_BYTES).ok()?;
    (sha256(bytes) == authority.pointer("/policy/raw_bytes_hash")?.as_str()?
        && policy.policy_hash == authority.pointer("/policy/identity_hash")?.as_str()?)
    .then_some((policy, value))
}

fn policy_allows(policy: &Policy, entry: &LedgerEntry) -> bool {
    policy.rules.iter().any(|rule| {
        rule.construct_id == entry.construct_id
            && rule.source_role == entry.source_role
            && rule.disposition == entry.disposition
            && rule.evidence_class == entry.evidence_class
            && rule.target_relation.as_deref()
                == entry
                    .target_identity
                    .as_ref()
                    .map(|target| target.relation.as_str())
            && rule.witness_kind.as_deref()
                == entry
                    .construct_witness
                    .as_ref()
                    .map(|witness| witness.construct_id.as_str())
    })
}

/// Whether `(source, normalized)` is exactly one row of the policy's
/// `accent_mappings`.
///
/// Since v4 the capture adapter proves each digraph substitution at its own
/// site, so the obligation is site-local: the proof pair must be a mapping
/// row verbatim. This is deliberately stronger than re-running a
/// decomposition — a row lookup cannot be satisfied by two errors that
/// cancel, and it is independent of *which* sites the parser chooses to
/// rewrite, so a parser-side applicability rule can change without touching
/// this instrument.
fn is_accent_mapping_row(source: &str, normalized: &str, policy: &Policy) -> bool {
    policy.accent_mappings.iter().any(|mapping| {
        mapping.get("source").and_then(Value::as_str) == Some(source)
            && mapping.get("normalized").and_then(Value::as_str) == Some(normalized)
    })
}

fn validate_entry_evidence(
    entry: &LedgerEntry,
    decoded: &str,
    parser_output: &[u8],
    policy: &Policy,
) -> bool {
    let Ok(start) = usize::try_from(entry.start) else {
        return false;
    };
    let Ok(end) = usize::try_from(entry.end) else {
        return false;
    };
    let Some(source) = decoded.get(start..end) else {
        return false;
    };
    if source.is_empty() || entry.parser_evidence_code.is_empty() {
        return false;
    }
    if let Some(witness) = &entry.construct_witness
        && (witness.start != entry.start
            || witness.end != entry.end
            || witness.source_form != source)
    {
        return false;
    }
    if let Some(target) = &entry.target_identity {
        let parser_hash = sha256(parser_output);
        let digest = parser_hash.strip_prefix("sha256:").unwrap_or_default();
        if target.value_hash != parser_hash
            || target.artifact_ref != format!("sha256/{}/{}.json", &digest[..2], digest)
        {
            return false;
        }
        if let Some(pointer) = &entry.target_pointer {
            let Ok(parser) = serde_json::from_slice::<Value>(parser_output) else {
                return false;
            };
            if parser.pointer(pointer).is_none() {
                return false;
            }
        }
    }
    if let Some(proof) = &entry.normalization_proof {
        if proof.source_form != source
            || sha256(source.as_bytes()) != proof.source_bytes_hash
            || sha256(proof.normalized_form.as_bytes()) != proof.normalized_bytes_hash
        {
            return false;
        }
        let round_trips = match proof.inverse_rule.as_str() {
            "crlf" => proof.source_form == "\r\n" && proof.normalized_form == "\n",
            "bare_cr" => proof.source_form == "\r" && proof.normalized_form == "\n",
            "accent_decomposition" => {
                is_accent_mapping_row(&proof.source_form, &proof.normalized_form, policy)
                    && proof.source_form != proof.normalized_form
            }
            _ => false,
        };
        if !round_trips {
            return false;
        }
    }
    true
}

/// Authenticate one P4A1 generation and derive semantic recognition without
/// invoking or reinterpreting parser behavior.
#[must_use]
pub fn analyze_recognition(input: RecognitionInput) -> RecognitionAnalysis {
    let mut record = base_record();
    let generation = CaptureGeneration {
        decoded_source: input.decoded_source.clone(),
        parser_output: input.parser_output.clone(),
        raw_diagnostics: input.raw_diagnostics.clone(),
        classified_source_ledger: input.ledger_bytes.clone(),
        manifest: input.generation_manifest.clone(),
    };
    if verify_capture_generation(&generation).is_err() {
        return unavailable(record, "capture-generation-invalid");
    }
    let Ok(manifest) = serde_json::from_slice::<Value>(&input.generation_manifest) else {
        return unavailable(record, "capture-generation-invalid");
    };
    if canonical_json(&manifest).as_deref() != Some(input.generation_manifest.as_slice()) {
        return unavailable(record, "capture-generation-invalid");
    }
    record.qualification_identity_ref = manifest["qualification_identity_ref"]
        .as_str()
        .map(str::to_owned);
    record.capture_generation_ref = manifest["generation_ref"].as_str().map(str::to_owned);
    record.work_id = manifest["work_id"].as_str().map(str::to_owned);
    if manifest["qualification_identity_ref"].as_str()
        != Some(input.qualification_identity_ref.as_str())
    {
        return unavailable(record, "qualification-identity-mismatch");
    }
    if manifest["work_id"].as_str() != Some(input.work_id.as_str()) {
        return unavailable(record, "work-identity-mismatch");
    }
    let Some((policy, _policy_value)) = validate_policy(&input.policy_bytes) else {
        return unavailable(record, "classified-source-policy-invalid");
    };
    record.policy_hash = Some(policy.policy_hash.clone());
    let Ok(ledger) = serde_json::from_slice::<Ledger>(&input.ledger_bytes) else {
        return unavailable(record, "classified-source-ledger-invalid");
    };
    if ledger.policy_hash != policy.policy_hash || ledger.policy_id != policy.policy_id {
        return unavailable(record, "ledger-policy-identity-mismatch");
    }
    if input.ledger_locator.is_empty() {
        return unavailable(record, "ledger-locator-invalid");
    }
    record.ledger = Some(RecognitionBlobRef {
        sha256: sha256(&input.ledger_bytes),
        bytes: input.ledger_bytes.len() as u64,
        media_type: "application/json".to_owned(),
        locator: input.ledger_locator.clone(),
    });
    let Ok(decoded) = std::str::from_utf8(&input.decoded_source) else {
        return unavailable(record, "decoded-source-invalid-utf8");
    };
    // The three declared regions, derived from the authenticated decoded
    // source rather than carried alongside it.
    //
    // `aozora_body_range` runs over SANITIZED text and its boundaries must be
    // mapped back, so the derivation has to go through `decode_source_bytes`
    // -- deriving straight off the decoded text would produce a different
    // boundary from the one the ledger's facts came from, which is precisely
    // the coordinate error this partition exists to remove. Re-decoding
    // already-valid UTF-8 takes the identity branch, so `text` is the same
    // string and the sanitize map is the same map.
    //
    // Recomputing from authenticated bytes is not weaker than carrying the
    // regions in the ledger: a carried value can disagree with the bytes it
    // describes, and a derived one cannot. What it does mean is that the fact
    // producer and this denominator agree because they call the same code on
    // the same input, so `every_ledger_entry_lies_inside_the_body_region`
    // holds that coupling to account.
    let Ok(regions) = decode_source_bytes(&input.decoded_source)
        .map_err(|_| ())
        .and_then(|source| source.source_regions().map_err(|_| ()))
    else {
        return unavailable(record, "source-region-partition-failed");
    };
    record.regions = Some(RecognitionRegions {
        header: interval_of(&regions.header()),
        body: interval_of(&regions.body()),
        tail: interval_of(&regions.tail()),
    });
    if ledger.parser_status != "complete" || !ledger.errors.is_empty() {
        return unavailable(record, "ledger-parser-incomplete");
    }

    for entry in &ledger.entries {
        if !policy_allows(&policy, entry) {
            return unavailable(record, "ledger-policy-row-unknown");
        }
        let (Ok(start), Ok(end)) = (usize::try_from(entry.start), usize::try_from(entry.end))
        else {
            return unavailable(record, "ledger-interval-invalid");
        };
        if start >= end
            || end > decoded.len()
            || !decoded.is_char_boundary(start)
            || !decoded.is_char_boundary(end)
        {
            return unavailable(record, "ledger-interval-invalid");
        }
        if !validate_entry_evidence(entry, decoded, &input.parser_output, &policy) {
            return unavailable(record, "ledger-evidence-invalid");
        }
    }

    let mut recognized = Vec::new();
    let mut opaque = Vec::new();
    // The metadata numerator, kept separate from the body one from the start.
    //
    // A byte is ATTRIBUTED only by a fact whose role the policy names as
    // attributing packaging, and never by a `preserved_opaque` fact. Those two
    // exclusions are what stop the measure from rising for the wrong reasons.
    //
    // `preserved_opaque` is the disposition that records "these bytes are
    // carried through and nothing is claimed about them". Counting it would
    // make the measure improve by DECLINING to classify, which is not a
    // weakness of degree but a contradiction.
    //
    // A role outside the attributing set can still account for the byte. Every
    // metadata line ending in a CRLF source carries a `crlf_normalization`
    // fact under `structural_newline`, because the sanitizer walks the whole
    // text. That fact is true and it is not knowledge about the packaging: it
    // holds of every line in the file whether or not anything understands it.
    // Counting it would give a CRLF work a few percent of attribution for
    // free and make the number move when line endings change rather than when
    // packaging becomes understood.
    let mut attributing = Vec::new();
    for entry in &ledger.entries {
        let start = usize::try_from(entry.start).expect("entry endpoints were validated");
        let end = usize::try_from(entry.end).expect("entry endpoints were validated");
        let interval = Interval::new(start, end, decoded.len()).expect("validated interval");
        if entry.disposition == "preserved_opaque" {
            opaque.push(interval);
        } else {
            recognized.push(interval);
            if policy
                .metadata_attributing_roles
                .contains(&entry.source_role)
            {
                attributing.push(interval);
            }
        }
    }
    let recognized = normalize(recognized);
    let attributing = normalize(attributing);
    let mut accounted_entries = recognized.clone();
    accounted_entries.extend(opaque);
    let accounted = normalize(accounted_entries);
    if !subtract(&recognized, &accounted).is_empty() {
        return unavailable(record, "recognition-subset-failed");
    }
    // Eligibility is the BODY region, not the whole decoded file.
    //
    // The numerator has always been body-derived -- the ledger's facts come
    // from lexing `span_text`, which is the body projection -- while the
    // denominator was the whole file. Dividing across that boundary averaged
    // parser fidelity against packaging attribution and could mean neither.
    //
    // Facts do fall outside the body: `sanitizer_entries` walks the whole
    // sanitized text, so every header and tail line ending in a CRLF source
    // carries a normalization fact. Those belong to the metadata population,
    // so the body measure intersects against the body and the metadata measure
    // takes the remainder. No byte leaves the accounting; the metadata bytes
    // move to a different accounted region.
    //
    // The two populations are measured by two DIFFERENT questions, and that is
    // deliberate. The body asks whether the parser recognized the byte; the
    // metadata asks whether the byte is attributed to an understood packaging
    // construct. Publishing both under one word would invite a single ratio
    // over their sum, which is the whole-file average this partition removed.
    let body = interval_list(&regions.body(), decoded.len());
    let metadata_regions = normalize(
        regions
            .metadata()
            .iter()
            .flat_map(|region| interval_list(region, decoded.len()))
            .collect(),
    );
    let recognized = intersect(&recognized, &body);
    let accounted_in_body = intersect(&accounted, &body);
    let metadata_attributed = intersect(&attributing, &metadata_regions);
    let metadata_unattributed = subtract(&metadata_regions, &attributing);
    // The attribution denominator: the metadata regions' content lines. See
    // `RecognitionMetadata` for why the ratio is over this and not over the
    // regions whole.
    let metadata_content = normalize(
        regions
            .metadata()
            .iter()
            .filter_map(|region| {
                decoded
                    .get(region.clone())
                    .map(|text| content_lines(text, region.start, decoded.len()))
            })
            .flatten()
            .collect(),
    );
    let metadata_unattributed_content = subtract(&metadata_content, &attributing);
    let semantic_gaps = subtract(&body, &recognized);
    let unaccounted = subtract(&body, &accounted_in_body);
    let accounted = accounted_in_body;
    let (
        Ok(eligible_bytes),
        Ok(recognized_bytes),
        Ok(accounted_bytes),
        Ok(semantic_gap_bytes),
        Ok(unaccounted_bytes),
        Ok(metadata_eligible_bytes),
        Ok(metadata_attributed_bytes),
        Ok(metadata_unattributed_bytes),
        Ok(metadata_content_bytes),
        Ok(metadata_unattributed_content_bytes),
    ) = (
        total_len(&body),
        total_len(&recognized),
        total_len(&accounted),
        total_len(&semantic_gaps),
        total_len(&unaccounted),
        total_len(&metadata_regions),
        total_len(&metadata_attributed),
        total_len(&metadata_unattributed),
        total_len(&metadata_content),
        total_len(&metadata_unattributed_content),
    )
    else {
        return unavailable(record, "recognition-total-overflow");
    };
    // The original whole-file conservation assertions, restated per region.
    if recognized_bytes > accounted_bytes
        || accounted_bytes > eligible_bytes
        || recognized_bytes.checked_add(semantic_gap_bytes) != Some(eligible_bytes)
        || accounted_bytes.checked_add(unaccounted_bytes) != Some(eligible_bytes)
    {
        return unavailable(record, "recognition-conservation-failed");
    }
    if metadata_attributed_bytes.checked_add(metadata_unattributed_bytes)
        != Some(metadata_eligible_bytes)
    {
        return unavailable(record, "metadata-conservation-failed");
    }
    // The content denominator has to sit between the numerator and the region
    // total, and this is the check that it does. If a producer ever claimed a
    // byte outside a content line -- a terminator, a blank line -- the ratio
    // would exceed 1 and the first inequality catches it. If `content_lines`
    // and `region_lines` ever disagreed about where a line starts, the second
    // catches that. Both are the kind of invariant this partition exists to
    // make assertable rather than to hope for.
    if metadata_attributed_bytes > metadata_content_bytes
        || metadata_content_bytes > metadata_eligible_bytes
        || metadata_attributed_bytes.checked_add(metadata_unattributed_content_bytes)
            != Some(metadata_content_bytes)
        || !subtract(&metadata_attributed, &metadata_content).is_empty()
    {
        return unavailable(record, "metadata-content-conservation-failed");
    }
    // The cross-region identity. Body and metadata partition the decoded file,
    // so no byte is measured twice and none is measured by nothing. This is
    // the check that distinguishes a partition from a denominator reduction,
    // and it is the one that can actually fail.
    let Ok(decoded_bytes) = u64::try_from(decoded.len()) else {
        return unavailable(record, "recognition-total-overflow");
    };
    if eligible_bytes.checked_add(metadata_eligible_bytes) != Some(decoded_bytes)
        || !intersect(&body, &metadata_regions).is_empty()
    {
        return unavailable(record, "region-conservation-failed");
    }

    record.status = RecognitionStatus::Ok;
    record.eligible_bytes = Some(eligible_bytes);
    record.recognized_bytes = Some(recognized_bytes);
    record.accounted_bytes = Some(accounted_bytes);
    record.semantic_gap_bytes = Some(semantic_gap_bytes);
    record.unaccounted_bytes = Some(unaccounted_bytes);
    record.recognized = Some(wire(&recognized));
    record.accounted = Some(wire(&accounted));
    record.semantic_gaps = Some(wire(&semantic_gaps));
    record.unaccounted = Some(wire(&unaccounted));
    record.metadata = Some(RecognitionMetadata {
        eligible_bytes: metadata_eligible_bytes,
        content_bytes: metadata_content_bytes,
        attributed_bytes: metadata_attributed_bytes,
        unattributed_bytes: metadata_unattributed_bytes,
        unattributed_content_bytes: metadata_unattributed_content_bytes,
        attributed: wire(&metadata_attributed),
        unattributed: wire(&metadata_unattributed),
        unattributed_content: wire(&metadata_unattributed_content),
    });
    RecognitionAnalysis { record }
}
