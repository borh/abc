//! Authenticated classified-source capture at the decoded-coordinate boundary.

use std::collections::BTreeMap;
use std::path::Path;

use ab_aozora_pipeline::{
    ClassifiedSourceDisposition, ClassifiedSourceEvidenceClass, ClassifiedSourceFact,
    ClassifiedSourceRole, ConstructId, Span, lex,
};
use ab_rq_artifact_store::{PublishedBlob, publish_blob};
use anyhow::{Context, Result, ensure};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};

use super::{
    DecodedSource, aat_json_from_bytes, adapter_version, decode_source_bytes,
    diagnostics_json_from_bytes,
};

const POLICY_BYTES: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-ab-aozora-classified-source-v1.json");
const LEDGER_SCHEMA_BYTES: &[u8] =
    include_bytes!("../../../../abc/schemas/parser-rq-classified-source-ledger.schema.json");
const GENERATION_SCHEMA_BYTES: &[u8] =
    include_bytes!("../../../../abc/schemas/parser-rq-capture-generation.schema.json");
const AUTHORITY_BYTES: &[u8] =
    include_bytes!("../../../../abc/data/parser-rq-classified-source-authority-v1.json");
const POLICY_ID: &str = "parser-rq-ab-aozora-classified-source-v1";
const LEDGER_VERSION: &str = "abc/parser-rq-classified-source-ledger/v1";
const GENERATION_VERSION: &str = "abc/parser-rq-capture-generation/v1";

/// In-memory member bytes for one closed capture generation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct CaptureGeneration {
    /// UTF-8 bytes of `DecodedSource.text`.
    pub decoded_source: Vec<u8>,
    /// Complete AAT JSON member bytes.
    pub parser_output: Vec<u8>,
    /// Complete raw-diagnostics JSON member bytes.
    pub raw_diagnostics: Vec<u8>,
    /// Canonical classified-source ledger JSON member bytes.
    pub classified_source_ledger: Vec<u8>,
    /// Canonical closed generation-manifest JSON bytes.
    pub manifest: Vec<u8>,
}

/// Content-addressed identities returned after publishing a generation.
#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PublishedCaptureGeneration {
    /// Published decoded-source identity.
    pub decoded_source: PublishedBlob,
    /// Published parser-output identity.
    pub parser_output: PublishedBlob,
    /// Published raw-diagnostics identity.
    pub raw_diagnostics: PublishedBlob,
    /// Published classified-source-ledger identity.
    pub classified_source_ledger: PublishedBlob,
    /// Published manifest identity.
    pub manifest: PublishedBlob,
}

impl CaptureGeneration {
    /// Publish immutable members first and the closed generation manifest last.
    ///
    /// # Errors
    ///
    /// Returns an error if generation authentication or any immutable store
    /// publication fails.
    pub fn publish(&self, root: &Path) -> Result<PublishedCaptureGeneration> {
        verify_capture_generation(self)?;
        let decoded_source = publish_blob(root, "txt", &self.decoded_source)?;
        let parser_output = publish_blob(root, "json", &self.parser_output)?;
        let raw_diagnostics = publish_blob(root, "json", &self.raw_diagnostics)?;
        let classified_source_ledger = publish_blob(root, "json", &self.classified_source_ledger)?;
        let manifest = publish_blob(root, "json", &self.manifest)?;
        Ok(PublishedCaptureGeneration {
            decoded_source,
            parser_output,
            raw_diagnostics,
            classified_source_ledger,
            manifest,
        })
    }
}

#[derive(Clone)]
struct MemberIdentity {
    artifact_ref: String,
    value_hash: String,
}

fn sha256(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn member_identity(extension: &str, bytes: &[u8]) -> MemberIdentity {
    let value_hash = sha256(bytes);
    let digest = value_hash.strip_prefix("sha256:").expect("prefixed hash");
    MemberIdentity {
        artifact_ref: format!("sha256/{}/{}.{}", &digest[..2], digest, extension),
        value_hash,
    }
}

fn member_json(member: &MemberIdentity) -> Value {
    json!({"artifact_ref": member.artifact_ref, "value_hash": member.value_hash})
}

fn canonical_value(value: &Value, out: &mut String) {
    match value {
        Value::Null => out.push_str("null"),
        Value::Bool(value) => out.push_str(if *value { "true" } else { "false" }),
        Value::Number(value) => out.push_str(&value.to_string()),
        Value::String(value) => out.push_str(&serde_json::to_string(value).expect("string JSON")),
        Value::Array(values) => {
            out.push('[');
            for (index, value) in values.iter().enumerate() {
                if index != 0 {
                    out.push(',');
                }
                canonical_value(value, out);
            }
            out.push(']');
        }
        Value::Object(values) => {
            out.push('{');
            let ordered = values.iter().collect::<BTreeMap<_, _>>();
            for (index, (key, value)) in ordered.into_iter().enumerate() {
                if index != 0 {
                    out.push(',');
                }
                out.push_str(&serde_json::to_string(key).expect("object key JSON"));
                out.push(':');
                canonical_value(value, out);
            }
            out.push('}');
        }
    }
}

fn canonical_json(value: &Value) -> Vec<u8> {
    let mut out = String::new();
    canonical_value(value, &mut out);
    out.push('\n');
    out.into_bytes()
}

fn canonical_hash_without(value: &Value, excluded: &str) -> Result<String> {
    authenticate_generation_ref_contract(excluded)?;
    let mut identity = value.clone();
    identity
        .as_object_mut()
        .context("hash identity must be an object")?
        .remove(excluded);
    let mut bytes = canonical_json(&identity);
    bytes.pop();
    Ok(sha256(&bytes))
}

fn authenticate_generation_ref_contract(excluded: &str) -> Result<()> {
    let authority: Value = serde_json::from_slice(AUTHORITY_BYTES)?;
    validate_generation_ref_contract(&authority, excluded)
}

fn validate_generation_ref_contract(authority: &Value, excluded: &str) -> Result<()> {
    ensure!(
        authority["generation_ref_contract"]
            == json!({
                "algorithm_id": "sha256-rfc8785-string-domain-v1",
                "canonicalization": "RFC 8785 JCS",
                "hash": "SHA-256",
                "projection": "closed-manifest-minus-generation_ref",
                "excluded_fields": ["generation_ref"],
                "value_domain": "objects-arrays-strings-only",
                "object_key_domain": "schema-fixed-ascii"
            }),
        "ABC generation-reference contract is unsupported"
    );
    ensure!(
        excluded == "generation_ref",
        "generation-reference projection excludes an unauthorized field"
    );
    Ok(())
}

fn authority_hash(section: &str, field: &str) -> Result<String> {
    let authority: Value = serde_json::from_slice(AUTHORITY_BYTES)?;
    ensure!(
        authority["schema_version"] == "abc/parser-rq-classified-source-authority/v1",
        "ABC classified-source authority descriptor version mismatch"
    );
    authority[section][field]
        .as_str()
        .map(str::to_owned)
        .with_context(|| format!("ABC authority {section}.{field} is absent"))
}

fn authenticate_authority_bytes(section: &str, bytes: &[u8]) -> Result<()> {
    ensure!(
        sha256(bytes) == authority_hash(section, "raw_bytes_hash")?,
        "compiled ABC {section} bytes drift from the authority descriptor"
    );
    Ok(())
}

fn policy_hash() -> Result<String> {
    authenticate_authority_bytes("policy", POLICY_BYTES)?;
    let policy: Value = serde_json::from_slice(POLICY_BYTES)?;
    let asserted = policy["policy_hash"]
        .as_str()
        .context("policy_hash is absent")?;
    let authoritative = authority_hash("policy", "identity_hash")?;
    ensure!(
        asserted == authoritative,
        "compiled classified-source policy identity differs from ABC authority"
    );
    Ok(authoritative)
}

fn ledger_schema_hash() -> Result<String> {
    authenticate_authority_bytes("ledger_schema", LEDGER_SCHEMA_BYTES)?;
    let schema: Value = serde_json::from_slice(LEDGER_SCHEMA_BYTES)?;
    jsonschema::validator_for(&schema).context("compiled ledger schema is invalid")?;
    authority_hash("ledger_schema", "identity_hash")
}

fn authenticate_generation_schema() -> Result<Value> {
    authenticate_authority_bytes("generation_schema", GENERATION_SCHEMA_BYTES)?;
    let schema: Value = serde_json::from_slice(GENERATION_SCHEMA_BYTES)?;
    jsonschema::validator_for(&schema).context("compiled generation schema is invalid")?;
    Ok(schema)
}

fn validate_schema(schema: &Value, instance: &Value, label: &str) -> Result<()> {
    let validator = jsonschema::validator_for(schema)
        .with_context(|| format!("invalid compiled {label} schema"))?;
    if let Err(error) = validator.validate(instance) {
        anyhow::bail!("{label} contract violation: {error}");
    }
    Ok(())
}

fn wire_construct(value: ConstructId) -> &'static str {
    match value {
        ConstructId::PlainText => "plain_text",
        ConstructId::RecoveredVerbatim => "recovered_verbatim",
        ConstructId::Newline => "newline",
        ConstructId::UnknownDirective => "unknown_directive",
        ConstructId::Ruby => "ruby",
        ConstructId::Bouten => "bouten",
        ConstructId::CombineUpright => "combine_upright",
        ConstructId::Gaiji => "gaiji",
        ConstructId::Indent => "indent",
        ConstructId::AlignEnd => "align_end",
        ConstructId::Center => "center",
        ConstructId::WarichuOpen => "warichu_open",
        ConstructId::FramedOpen => "framed_open",
        ConstructId::LineGothic => "line_gothic",
        ConstructId::LineFontSize => "line_font_size",
        ConstructId::PageBreak => "page_break",
        ConstructId::SectionBreak => "section_break",
        ConstructId::BodyEnd => "body_end",
        ConstructId::ForcedBreak => "forced_break",
        ConstructId::Heading => "heading",
        ConstructId::HeadingHint => "heading_hint",
        ConstructId::Illustration => "illustration",
        ConstructId::Kaeriten => "kaeriten",
        ConstructId::AngleQuote => "angle_quote",
        ConstructId::Emphasis => "emphasis",
        ConstructId::MarginNote => "margin_note",
        ConstructId::ContainerOpen => "container_open",
        ConstructId::ContainerClose => "container_close",
    }
}

fn wire_role(value: ClassifiedSourceRole) -> &'static str {
    match value {
        ClassifiedSourceRole::VisibleText => "visible_text",
        ClassifiedSourceRole::StructuralNewline => "structural_newline",
        ClassifiedSourceRole::Ruby => "ruby",
        ClassifiedSourceRole::Typography => "typography",
        ClassifiedSourceRole::Gaiji => "gaiji",
        ClassifiedSourceRole::Layout => "layout",
        ClassifiedSourceRole::Break => "break",
        ClassifiedSourceRole::Heading => "heading",
        ClassifiedSourceRole::Illustration => "illustration",
        ClassifiedSourceRole::Kunten => "kunten",
        ClassifiedSourceRole::SourceAnnotation => "source_annotation",
        ClassifiedSourceRole::ContainerSyntax => "container_syntax",
        ClassifiedSourceRole::TerminalProvenance => "terminal_provenance",
        ClassifiedSourceRole::PublicationMetadata => "publication_metadata",
        ClassifiedSourceRole::UnrecognizedSourceForm => "unrecognized_source_form",
    }
}

fn wire_disposition(value: ClassifiedSourceDisposition) -> &'static str {
    match value {
        ClassifiedSourceDisposition::EmittedSemanticValue => "emitted_semantic_value",
        ClassifiedSourceDisposition::PreservedSidecarValue => "preserved_sidecar_value",
        ClassifiedSourceDisposition::StructuralControl => "structural_control",
        ClassifiedSourceDisposition::LosslessNormalization => "lossless_normalization",
        ClassifiedSourceDisposition::PreservedOpaque => "preserved_opaque",
    }
}

fn wire_evidence(value: ClassifiedSourceEvidenceClass) -> &'static str {
    match value {
        ClassifiedSourceEvidenceClass::AcceptedText => "accepted_text",
        ClassifiedSourceEvidenceClass::RecoveredVerbatim => "recovered_verbatim",
        ClassifiedSourceEvidenceClass::TypedNode => "typed_node",
        ClassifiedSourceEvidenceClass::StructuralToken => "structural_token",
        ClassifiedSourceEvidenceClass::TypedContainer => "typed_container",
        ClassifiedSourceEvidenceClass::UnknownDirective => "unknown_directive",
    }
}

fn witness_kind(construct: ConstructId) -> Option<&'static str> {
    match construct {
        ConstructId::RecoveredVerbatim => Some("recovered_verbatim"),
        ConstructId::Newline => Some("newline"),
        ConstructId::UnknownDirective => Some("unknown_directive"),
        ConstructId::WarichuOpen => Some("warichu_open"),
        ConstructId::FramedOpen | ConstructId::ContainerOpen => Some("container_open"),
        ConstructId::PageBreak => Some("page_break"),
        ConstructId::SectionBreak => Some("section_break"),
        ConstructId::BodyEnd => Some("body_end"),
        ConstructId::ForcedBreak => Some("forced_break"),
        ConstructId::ContainerClose => Some("container_close"),
        _ => None,
    }
}

fn target_identity(parser: &MemberIdentity) -> Value {
    json!({
        "artifact_ref": parser.artifact_ref,
        "value_hash": parser.value_hash,
        "relation": "emits"
    })
}

fn fact_entry(
    fact: ClassifiedSourceFact,
    decoded: &DecodedSource,
    parser: &MemberIdentity,
) -> Option<Value> {
    let start = decoded.span_ctx.to_decoded(fact.source_span.start as usize);
    let end = decoded
        .span_ctx
        .to_decoded_end(fact.source_span.end as usize);
    if start >= end
        || end > decoded.text.len()
        || !decoded.text.is_char_boundary(start)
        || !decoded.text.is_char_boundary(end)
    {
        return None;
    }
    let source_form = &decoded.text[start..end];
    if source_form
        .chars()
        .any(|character| ('\u{e001}'..='\u{e004}').contains(&character))
    {
        return None;
    }

    let mut entry = Map::new();
    entry.insert("start".into(), json!(start));
    entry.insert("end".into(), json!(end));
    entry.insert(
        "construct_id".into(),
        json!(wire_construct(fact.construct_id)),
    );
    entry.insert("source_role".into(), json!(wire_role(fact.source_role)));
    entry.insert(
        "disposition".into(),
        json!(wire_disposition(fact.disposition)),
    );
    entry.insert(
        "evidence_class".into(),
        json!(wire_evidence(fact.evidence_class)),
    );
    entry.insert(
        "parser_evidence_code".into(),
        json!(format!(
            "classified-source:{}",
            wire_construct(fact.construct_id)
        )),
    );
    if matches!(
        fact.disposition,
        ClassifiedSourceDisposition::EmittedSemanticValue
            | ClassifiedSourceDisposition::PreservedSidecarValue
    ) {
        entry.insert("target_identity".into(), target_identity(parser));
    }
    if let Some(kind) = witness_kind(fact.construct_id) {
        entry.insert(
            "construct_witness".into(),
            json!({
                "construct_id": kind,
                "start": start,
                "end": end,
                "source_form": source_form
            }),
        );
    }
    Some(Value::Object(entry))
}

#[allow(
    clippy::too_many_arguments,
    reason = "the arguments are the complete closed normalization-proof protocol, not optional configuration"
)]
fn normalization_entry(
    construct_id: &str,
    inverse_rule: &str,
    role: &str,
    start: usize,
    end: usize,
    source_form: &str,
    normalized_form: &str,
    parser: &MemberIdentity,
) -> Value {
    json!({
        "start": start,
        "end": end,
        "construct_id": construct_id,
        "source_role": role,
        "disposition": "lossless_normalization",
        "evidence_class": "sanitizer_transform",
        "target_identity": target_identity(parser),
        "parser_evidence_code": format!("sanitize:{inverse_rule}"),
        "normalization_proof": {
            "source_form": source_form,
            "normalized_form": normalized_form,
            "source_bytes_hash": sha256(source_form.as_bytes()),
            "normalized_bytes_hash": sha256(normalized_form.as_bytes()),
            "inverse_rule": inverse_rule
        }
    })
}

fn sanitizer_entries(decoded: &DecodedSource, parser: &MemberIdentity) -> Vec<Value> {
    let mut entries = Vec::new();
    for (sanitized_start, character) in decoded.sanitized_text.char_indices() {
        if character != '\n' {
            continue;
        }
        let sanitized_end = sanitized_start + 1;
        let start = decoded.span_ctx.maps.to_source_offset(sanitized_start);
        let end = decoded.span_ctx.maps.to_source_end(sanitized_end);
        let Some(source_form) = decoded.text.get(start..end) else {
            continue;
        };
        let (construct, inverse) = match source_form {
            "\r\n" => ("crlf_normalization", "crlf"),
            "\r" => ("bare_cr_normalization", "bare_cr"),
            _ => continue,
        };
        entries.push(normalization_entry(
            construct,
            inverse,
            "structural_newline",
            start,
            end,
            source_form,
            "\n",
            parser,
        ));
    }

    for diagnostic in &decoded.sanitize_diagnostics {
        if diagnostic.code() != "aozora::lex::accent_decomposition_applied" {
            continue;
        }
        let span = diagnostic.span();
        let sanitized_start = span.start as usize;
        let sanitized_end = span.end as usize;
        let start = decoded.span_ctx.maps.to_source_offset(sanitized_start);
        let end = decoded.span_ctx.maps.to_source_end(sanitized_end);
        let (Some(source_form), Some(normalized_form)) = (
            decoded.text.get(start..end),
            decoded.sanitized_text.get(sanitized_start..sanitized_end),
        ) else {
            continue;
        };
        if source_form == normalized_form || source_form.is_empty() || normalized_form.is_empty() {
            continue;
        }
        entries.push(normalization_entry(
            "accent_normalization",
            "accent_decomposition",
            "visible_text",
            start,
            end,
            source_form,
            normalized_form,
            parser,
        ));
    }
    entries
}

fn reconcile_accent_edit_facts(
    mut facts: Vec<ClassifiedSourceFact>,
    decoded: &DecodedSource,
) -> Vec<ClassifiedSourceFact> {
    const DELIMITER_BYTES: u32 = 3;

    for diagnostic in &decoded.sanitize_diagnostics {
        if diagnostic.code() != "aozora::lex::accent_decomposition_applied" {
            continue;
        }
        let edit = diagnostic.span();
        let Ok(body_offset) = u32::try_from(decoded.span_ctx.body_offset) else {
            continue;
        };
        let Ok(body_len) = u32::try_from(decoded.span_text.len()) else {
            continue;
        };
        let Some(body_end) = body_offset.checked_add(body_len) else {
            continue;
        };
        if edit.start < body_offset
            || edit.end > body_end
            || edit.end - edit.start < DELIMITER_BYTES * 2
        {
            continue;
        }
        let owned = Span::new(edit.start - body_offset, edit.end - body_offset);
        let open = Span::new(owned.start, owned.start + DELIMITER_BYTES);
        let close = Span::new(owned.end - DELIMITER_BYTES, owned.end);
        let is_recovery =
            |fact: &&ClassifiedSourceFact| fact.construct_id == ConstructId::RecoveredVerbatim;
        let open_count = facts
            .iter()
            .filter(is_recovery)
            .filter(|fact| fact.source_span == open)
            .count();
        let close_count = facts
            .iter()
            .filter(is_recovery)
            .filter(|fact| fact.source_span == close)
            .count();
        if open_count != 1 || close_count != 1 {
            continue;
        }

        // The unique delimiter pair correlates the classifier stream with
        // this sanitizer edit. Only the pair's accepted text/newline payload
        // is folded into its whole-form recovery claim. Typed constructs and
        // additional recovery observations remain independent evidence.
        facts.retain(|fact| {
            if fact.source_span == open || fact.source_span == close {
                return fact.construct_id != ConstructId::RecoveredVerbatim;
            }
            let inside = owned.start <= fact.source_span.start && fact.source_span.end <= owned.end;
            !inside
                || !matches!(
                    fact.construct_id,
                    ConstructId::PlainText | ConstructId::Newline
                )
        });
        facts.push(ClassifiedSourceFact {
            source_span: owned,
            construct_id: ConstructId::RecoveredVerbatim,
            source_role: ClassifiedSourceRole::UnrecognizedSourceForm,
            disposition: ClassifiedSourceDisposition::PreservedOpaque,
            evidence_class: ClassifiedSourceEvidenceClass::RecoveredVerbatim,
        });
    }
    facts
}

fn build_ledger(bytes: &[u8], decoded: &DecodedSource, parser_output: &[u8]) -> Result<Value> {
    ensure!(
        decoded.encoding != "windows-31j-lossy",
        "lossy source decoding"
    );
    let parser = member_identity("json", parser_output);
    let output = lex(&decoded.span_text);
    let mut entries = reconcile_accent_edit_facts(output.classified_source_facts, decoded)
        .into_iter()
        .filter_map(|fact| fact_entry(fact, decoded, &parser))
        .collect::<Vec<_>>();
    entries.extend(sanitizer_entries(decoded, &parser));
    entries.sort_unstable_by_key(|entry| {
        let mut canonical = String::new();
        canonical_value(entry, &mut canonical);
        canonical
    });
    if let Some([entry, _]) = entries.windows(2).find(|pair| pair[0] == pair[1]) {
        anyhow::bail!("duplicate classified-source entry: {entry}");
    }

    let decoded_member = member_identity("txt", decoded.text.as_bytes());
    Ok(json!({
        "schema_version": LEDGER_VERSION,
        "ledger_schema_hash": ledger_schema_hash()?,
        "qualification_identity_ref": qualification_identity_ref(),
        "parser": "ab-aozora",
        "instrument_version": adapter_version(),
        "original_source": {
            "artifact_ref": format!("source/{}", decoded.source_hash),
            "value_hash": sha256(bytes)
        },
        "decoded_source": {
            "artifact_ref": decoded_member.artifact_ref,
            "value_hash": decoded_member.value_hash,
            "encoding": decoded.encoding,
            "bytes": decoded.text.len()
        },
        "policy_id": POLICY_ID,
        "policy_hash": policy_hash()?,
        "coordinate_system": "decoded_utf8",
        "parser_status": "complete",
        "entries": entries,
        "errors": []
    }))
}

fn qualification_identity_ref() -> String {
    sha256(b"ab-aozora-aat:parser-rq-classified-source-v1")
}

/// Produce canonical ledger bytes for one source value.
///
/// # Errors
///
/// Returns an error for lossy decoding, parser/capture failure, policy drift,
/// invalid facts, or JSON serialization failure.
pub fn classified_source_ledger_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let parser_output = aat_json_from_bytes(bytes)?;
    Ok(canonical_json(&build_ledger(
        bytes,
        &decoded,
        &parser_output,
    )?))
}

/// Capture all authenticated member bytes and their closed manifest.
///
/// # Errors
///
/// Returns an error when any member cannot be produced or the resulting
/// generation fails its closed authentication checks.
pub fn capture_generation_from_bytes(bytes: &[u8]) -> Result<CaptureGeneration> {
    let decoded = decode_source_bytes(bytes)?;
    let decoded_source = decoded.text.as_bytes().to_vec();
    let parser_output = aat_json_from_bytes(bytes)?;
    let raw_diagnostics = diagnostics_json_from_bytes(bytes)?;
    let classified_source_ledger = canonical_json(&build_ledger(bytes, &decoded, &parser_output)?);
    let decoded_identity = member_identity("txt", &decoded_source);
    let parser_identity = member_identity("json", &parser_output);
    let diagnostics_identity = member_identity("json", &raw_diagnostics);
    let ledger_identity = member_identity("json", &classified_source_ledger);
    let mut manifest = json!({
        "schema_version": GENERATION_VERSION,
        "qualification_identity_ref": qualification_identity_ref(),
        "work_id": decoded.source_hash,
        "members": {
            "decoded_source": member_json(&decoded_identity),
            "parser_output": member_json(&parser_identity),
            "raw_diagnostics": member_json(&diagnostics_identity),
            "classified_source_ledger": member_json(&ledger_identity)
        }
    });
    let generation_ref = canonical_hash_without(&manifest, "generation_ref")?;
    manifest
        .as_object_mut()
        .context("constructed generation manifest is not an object")?
        .insert("generation_ref".into(), json!(generation_ref));
    let generation = CaptureGeneration {
        decoded_source,
        parser_output,
        raw_diagnostics,
        classified_source_ledger,
        manifest: canonical_json(&manifest),
    };
    verify_capture_generation(&generation)?;
    Ok(generation)
}

fn verify_ledger_contract(generation: &CaptureGeneration, manifest: &Value) -> Result<()> {
    let ledger: Value = serde_json::from_slice(&generation.classified_source_ledger)?;
    let ledger_schema: Value = serde_json::from_slice(LEDGER_SCHEMA_BYTES)?;
    validate_schema(&ledger_schema, &ledger, "classified-source ledger")?;
    ensure!(
        ledger["qualification_identity_ref"] == manifest["qualification_identity_ref"],
        "ledger qualification identity mismatch"
    );
    ensure!(ledger["parser"] == "ab-aozora", "ledger parser mismatch");
    ensure!(
        ledger["coordinate_system"] == "decoded_utf8",
        "ledger coordinate system mismatch"
    );
    ensure!(ledger["policy_id"] == POLICY_ID, "policy ID mismatch");
    ensure!(
        ledger["policy_hash"] == policy_hash()?,
        "policy hash mismatch"
    );
    ensure!(
        ledger["ledger_schema_hash"] == ledger_schema_hash()?,
        "ledger schema hash mismatch"
    );
    ensure!(
        ledger["decoded_source"]
            == json!({
                "artifact_ref": member_identity("txt", &generation.decoded_source).artifact_ref,
                "value_hash": sha256(&generation.decoded_source),
                "encoding": ledger["decoded_source"]["encoding"],
                "bytes": generation.decoded_source.len()
            }),
        "decoded ledger identity mismatch"
    );
    ensure!(
        ledger["original_source"]["value_hash"] == manifest["work_id"],
        "original source and manifest work identity mismatch"
    );
    ensure!(
        ledger["original_source"]["artifact_ref"]
            == format!(
                "source/{}",
                manifest["work_id"].as_str().unwrap_or_default()
            ),
        "original source artifact relation mismatch"
    );
    let parser = member_identity("json", &generation.parser_output);
    for entry in ledger["entries"]
        .as_array()
        .context("ledger entries absent")?
    {
        if let Some(target) = entry.get("target_identity") {
            ensure!(
                target == &target_identity(&parser),
                "entry target is outside parser-output member"
            );
        }
    }
    Ok(())
}

/// Authenticate a generation's exact member tuple and acyclic reference.
///
/// # Errors
///
/// Returns an error for malformed JSON, identity drift, member replacement,
/// cross-generation mixing, or policy/schema drift.
pub fn verify_capture_generation(generation: &CaptureGeneration) -> Result<()> {
    let manifest: Value = serde_json::from_slice(&generation.manifest)?;
    validate_schema(
        &authenticate_generation_schema()?,
        &manifest,
        "capture generation",
    )?;
    ensure!(
        manifest["schema_version"] == GENERATION_VERSION,
        "manifest version mismatch"
    );
    ensure!(
        manifest["qualification_identity_ref"] == qualification_identity_ref(),
        "qualification identity mismatch"
    );
    let asserted_ref = manifest["generation_ref"]
        .as_str()
        .context("generation_ref is absent")?;
    ensure!(
        asserted_ref == canonical_hash_without(&manifest, "generation_ref")?,
        "generation reference mismatch"
    );
    let members = manifest["members"]
        .as_object()
        .context("manifest members are absent")?;
    let expected = [
        ("decoded_source", "txt", &generation.decoded_source),
        ("parser_output", "json", &generation.parser_output),
        ("raw_diagnostics", "json", &generation.raw_diagnostics),
        (
            "classified_source_ledger",
            "json",
            &generation.classified_source_ledger,
        ),
    ];
    for (name, extension, bytes) in expected {
        let identity = member_identity(extension, bytes);
        ensure!(
            members.get(name) == Some(&member_json(&identity)),
            "{name} member mismatch"
        );
        ensure!(
            !String::from_utf8_lossy(bytes).contains(asserted_ref),
            "member embeds generation reference"
        );
    }
    verify_ledger_contract(generation, &manifest)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn live_facts(source: &str) -> (DecodedSource, Vec<ClassifiedSourceFact>) {
        let decoded = decode_source_bytes(source.as_bytes()).unwrap();
        let parsed = lex(&decoded.span_text);
        let facts = reconcile_accent_edit_facts(parsed.classified_source_facts, &decoded);
        (decoded, facts)
    }

    fn live_entries(source: &str) -> Vec<Value> {
        let (decoded, facts) = live_facts(source);
        let parser = member_identity("json", b"{}");
        facts
            .into_iter()
            .filter_map(|fact| fact_entry(fact, &decoded, &parser))
            .collect()
    }

    fn recovered(entries: &[Value]) -> Vec<&Value> {
        entries
            .iter()
            .filter(|entry| entry["construct_id"] == "recovered_verbatim")
            .collect()
    }

    #[test]
    fn abc_authority_rejects_stale_identity_after_content_drift() {
        for (section, bytes) in [
            ("policy", POLICY_BYTES),
            ("ledger_schema", LEDGER_SCHEMA_BYTES),
            ("generation_schema", GENERATION_SCHEMA_BYTES),
        ] {
            authenticate_authority_bytes(section, bytes).unwrap();
            let mut changed = bytes.to_vec();
            changed.push(b' ');
            assert!(
                authenticate_authority_bytes(section, &changed)
                    .unwrap_err()
                    .to_string()
                    .contains("drift"),
                "{section}"
            );
        }
    }

    #[test]
    fn abc_authority_rejects_generation_algorithm_or_projection_drift() {
        let authority: Value = serde_json::from_slice(AUTHORITY_BYTES).unwrap();
        validate_generation_ref_contract(&authority, "generation_ref").unwrap();
        for (field, changed) in [
            ("algorithm_id", "sha256-legacy-json-v0"),
            ("projection", "closed-manifest-all-fields"),
            ("value_domain", "all-json-values"),
        ] {
            let mut mutated = authority.clone();
            mutated["generation_ref_contract"][field] = json!(changed);
            assert!(
                validate_generation_ref_contract(&mutated, "generation_ref")
                    .unwrap_err()
                    .to_string()
                    .contains("unsupported"),
                "{field}"
            );
        }
        assert!(
            validate_generation_ref_contract(&authority, "policy_hash")
                .unwrap_err()
                .to_string()
                .contains("unauthorized")
        );
    }

    #[test]
    fn live_plain_accent_edit_owns_one_recovery_candidate() {
        let source = "〔cafe'〕";
        let entries = live_entries(source);
        let recovered = recovered(&entries);
        assert_eq!(recovered.len(), 1);
        assert_eq!(recovered[0]["start"], 0);
        assert_eq!(recovered[0]["end"], source.len() as u64);
    }

    #[test]
    fn live_accent_edit_correlates_across_newline_and_nested_child() {
        for source in ["〔a`\nb〕", "〔a`｜青梅《おうめ》〕"] {
            let entries = live_entries(source);
            let recovered = recovered(&entries);
            assert_eq!(recovered.len(), 1, "{source}");
            assert_eq!(recovered[0]["start"], 0, "{source}");
            assert_eq!(recovered[0]["end"], source.len() as u64, "{source}");
            if source.contains('《') {
                assert!(entries.iter().any(|entry| entry["construct_id"] == "ruby"));
            }
        }
    }

    #[test]
    fn unrelated_recovery_inside_accent_edit_survives() {
        let entries = live_entries("〔a`｜〕");
        assert_eq!(recovered(&entries).len(), 2);
    }

    #[test]
    fn literal_tortoise_has_no_accent_edit_owner() {
        let entries = live_entries("〔literal〕");
        let recovered = recovered(&entries);
        assert_eq!(recovered.len(), 2);
        assert_eq!(
            (recovered[0]["start"].as_u64(), recovered[0]["end"].as_u64()),
            (Some(0), Some(3))
        );
        assert_eq!(
            (recovered[1]["start"].as_u64(), recovered[1]["end"].as_u64()),
            (Some(10), Some(13))
        );
    }

    #[test]
    fn ambiguous_duplicate_boundary_fact_is_not_consolidated() {
        let source = "〔cafe'〕";
        let decoded = decode_source_bytes(source.as_bytes()).unwrap();
        let mut facts = lex(&decoded.span_text).classified_source_facts;
        facts.push(facts[0]);
        let reconciled = reconcile_accent_edit_facts(facts, &decoded);
        assert_eq!(
            reconciled
                .iter()
                .filter(|fact| fact.source_span == Span::new(0, 3))
                .count(),
            2
        );
        assert!(!reconciled.iter().any(|fact| fact.source_span
            == Span::new(0, u32::try_from(decoded.span_text.len()).unwrap())));
    }
}
