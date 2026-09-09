//! Authenticated classified-source capture at the decoded-coordinate boundary.

use std::collections::BTreeMap;
use std::path::Path;

use ab_aozora_pipeline::{
    ClassifiedSourceDisposition, ClassifiedSourceEvidenceClass, ClassifiedSourceFact,
    ClassifiedSourceRole, ConstructId, lex,
};
use ab_artifact_store::{PublishedBlob, publish_blob};
use anyhow::{Context, Result, ensure};
use serde_json::{Map, Value, json};
use sha2::{Digest, Sha256};

use ab_aat::{
    DecodedSource, aat_json_from_bytes, adapter_version, decode_source_bytes,
    diagnostics_json_from_bytes,
};

const POLICY_BYTES: &[u8] =
    include_bytes!("../../../research/data/parser-rq-ab-aozora-classified-source-v1.json");
const LEDGER_SCHEMA_BYTES: &[u8] =
    include_bytes!("../../../research/schemas/parser-rq-classified-source-ledger.schema.json");
const GENERATION_SCHEMA_BYTES: &[u8] =
    include_bytes!("../../../research/schemas/parser-rq-capture-generation.schema.json");
const AUTHORITY_BYTES: &[u8] =
    include_bytes!("../../../research/data/parser-rq-classified-source-authority-v1.json");
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
        ConstructId::Kunten => "kunten",
        ConstructId::AngleQuote => "angle_quote",
        ConstructId::Emphasis => "emphasis",
        ConstructId::MarginNote => "margin_note",
        ConstructId::ContainerOpen => "container_open",
        ConstructId::ContainerClose => "container_close",
        ConstructId::Sic => "sic",
        ConstructId::BaseTextVariant => "base_text_variant",
        ConstructId::InvalidRubySpan => "invalid_ruby_span",
        ConstructId::WarichuClose => "warichu_close",
        ConstructId::EmptyDirective => "empty_directive",
        ConstructId::EditorNote => "editor_note",
        ConstructId::TranscriptionNote => "transcription_note",
        ConstructId::OmissionNote => "omission_note",
        ConstructId::IncompletenessNote => "incompleteness_note",
        ConstructId::ExplanationNote => "explanation_note",
        ConstructId::ExternalTableReference => "external_table_reference",
        ConstructId::RubyAttached => "ruby_attached",
        ConstructId::RubyRetarget => "ruby_retarget",
        ConstructId::RubyPairOpen => "ruby_pair_open",
        ConstructId::RubyPairClose => "ruby_pair_close",
        ConstructId::MarginNotePairOpen => "margin_note_pair_open",
        ConstructId::MarginNotePairClose => "margin_note_pair_close",
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
        ConstructId::WarichuClose => Some("warichu_close"),
        ConstructId::InvalidRubySpan => Some("invalid_ruby_span"),
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

/// Classified facts for the colophon, which is the TAIL region only.
///
/// The body is lexed by the parser; the metadata regions are not, so without a
/// producer of their own they carry no fact at all except the line-ending
/// normalizations, which only exist because the sanitizer walks the whole text.
/// That left the packaging unattributed rather than attributed-and-clear.
///
/// Three tail forms are classified here. The `key：value` line is what the
/// Aozora colophon is built from -- `底本：`, `入力：`, `校正：`, `初出：` and
/// the rest. Under a field sits its **continuation run**: indented lines that
/// carry the rest of what the field states and have no `key：` of their own,
/// `　　　1993（平成5）年2月10日第1刷発行` under a `底本：`, or the
/// per-piece citations under an `初出：`. Last comes the archive's own
/// **distribution notice**, which closes almost every file.
///
/// **The tail is what bounds this producer, not the shape of the line.**
/// `is_colophon_field` is a shape test and a loose one: the standard notation
/// legend `《》：ルビ` is a fullwidth-colon `key：value` line by that test and
/// nothing about its shape distinguishes it from `底本：`. The legend sits in
/// the HEADER, so scanning the tail alone is what keeps it out. Widening this
/// scan back to `regions.metadata()` would silently reclassify the legend
/// block as publication metadata.
///
/// **A continuation is bounded by the run it sits in, not by what it says.**
/// It must be indented and it must reach a field line above it without crossing
/// a blank line. The tail's other unindented lines (the transcriber's `※`
/// remarks, the distribution notice, and the file's own dating lines) are not
/// continuations of anything and stay unattributed. Testing the text instead
/// would be worse in both directions. One continuation in the 597-work sample
/// is a citation whose title opens with a `※［＃...］` gaiji annotation, so a
/// producer that declined `※` lines by shape would decline a real one.
///
/// The distribution notice must be tested before the colophon field because
/// one notice line in the corpus writes its URL with a fullwidth `http：//`.
/// That fullwidth colon causes the line to match `is_colophon_field` (a
/// non-empty key, a colon, and a value). Testing the notice first prevents
/// misclassifying that notice line as a colophon field.
///
/// The header's editorial legend block and bibliographic block have producers
/// of their own, `legend_entries` and `bibliographic_entries`.
///
/// **Nothing here is claimed by position alone.** Every construct below needs
/// an opener that was measured over the whole corpus, and a line that opens no
/// run and continues none stays unattributed. Blanket-claiming every metadata
/// byte would make the measure reach 1.0 by construction and therefore mean
/// nothing; the check that this has not happened is that it does not reach 1.0
/// -- no work of the 597-work sample does, because line terminators remain.
///
/// The span excludes the line terminator, which already carries its own
/// normalization or newline fact. Two producers must not claim the same byte.
fn metadata_entries(decoded: &DecodedSource, parser: &MemberIdentity) -> Vec<Value> {
    let Ok(regions) = decoded.source_regions() else {
        return Vec::new();
    };
    let tail = regions.tail();
    let Some(text) = decoded.text.get(tail.clone()) else {
        return Vec::new();
    };
    let mut entries = Vec::new();
    let mut run = TailRun::None;
    for line in region_lines(text, tail.start) {
        let content = line.content.as_str();
        if content.is_empty() {
            run = TailRun::None;
            continue;
        }
        // Openers are tested before continuations, and among themselves in the
        // sequence below. Precedence affects classification and each ordering has a test.
        let opened = if is_distribution_notice(content) {
            Some((
                TailRun::None,
                "distribution_notice_line",
                "distribution_notice",
            ))
        } else if !line.indented && is_licence_statement(content) {
            Some((TailRun::Licence, "licence_statement_line", "licence_terms"))
        } else if !line.indented && is_transcriber_remark(content) {
            Some((TailRun::Remark, "editorial_remark_line", "editorial_remark"))
        } else if is_colophon_field(content) {
            Some((
                TailRun::Field,
                "publication_metadata_line",
                "publication_metadata",
            ))
        } else if !line.indented && is_file_dating_line(content) {
            Some((TailRun::None, "file_dating_line", "file_provenance"))
        } else {
            None
        };
        if let Some((next, construct, role)) = opened {
            run = next;
            entries.push(region_fact(&line, construct, role));
            continue;
        }
        let continued = match run {
            TailRun::Field if line.indented => {
                Some(("publication_metadata_continuation", "publication_metadata"))
            }
            // A field name whose value opens with its own quoted title rather
            // than a colon -- see `is_titled_colophon_field`.
            TailRun::Field if is_titled_colophon_field(content) => {
                Some(("publication_metadata_line", "publication_metadata"))
            }
            TailRun::Remark => Some(("editorial_remark_continuation", "editorial_remark")),
            TailRun::Licence => Some(("licence_statement_continuation", "licence_terms")),
            _ => None,
        };
        let Some((construct, role)) = continued else {
            run = TailRun::None;
            continue;
        };
        entries.push(region_fact(&line, construct, role));
    }
    let _ = parser;
    entries
}

/// Which block of the colophon the scan is currently inside.
///
/// The tail is a line-structured record of blocks, and a line's construct
/// depends on the block above it as much as on its own text. `None` is not
/// "unknown" but "no block is open": the next line must open one itself or go
/// unattributed.
#[derive(Clone, Copy, PartialEq, Eq)]
enum TailRun {
    None,
    Field,
    Remark,
    Licence,
}

/// The transcriber's own remark about the transcription.
///
/// `※「旧字、旧仮名で書かれた作品を、現代表記にあらためる際の作業指針」に基づ
/// いて、底本の表記をあらためました。` is the commonest, and the rest say what
/// was normalized, what was left alone and what was checked against which
/// edition.
///
/// **`※` at column zero is the whole test, and both halves of it are
/// measured.** Across the pinned corpus's 17,718 colophon-bearing works, `※`
/// is the ONLY marker any tail line opens a remark with -- 12,526 lines, and
/// neither `＊` nor `●` ever appears in that position, though both do inside
/// the header's legend block, which a different producer bounds.
///
/// The column matters because indentation already means something else here.
/// Exactly four tail lines in the corpus are indented and open with `※`, and
/// **three of them are not remarks**: they are colophon continuations whose
/// cited title begins with a gaiji annotation, as in `※［＃「糸＋條」、第4水準
/// 2-84-53］蟲「三田文学　第四巻第二号」三田文学会`. Reading `※` without
/// reading the column would reclassify those three citations as remarks. The
/// fourth is a genuine remark that happens to be indented, and it is read as a
/// continuation instead; under a field run that is the expected classification,
/// because indentation under a field defines a continuation.
///
/// Claiming this is not claiming to understand the sentence. What is claimed
/// is which act the line performs -- editorial commentary on the transcription,
/// not bibliographic data about the source -- exactly as
/// `bibliographic_header_line` claims block membership without claiming which
/// bibliographic item a line is.
fn is_transcriber_remark(line: &str) -> bool {
    line.starts_with('※')
}

/// The file's own dating line: `2007年4月2日作成`, and nothing else on it.
///
/// This is provenance about the FILE rather than about the publication, which
/// is why it carries `file_provenance` and not `publication_metadata`. The
/// `底本：` block says where the text came from; this says when this file was
/// made from it.
///
/// **The three suffixes are closed and measured.** Corpus-wide, tail lines
/// matching `YYYY年M月D日...` carry 19 distinct suffixes over 21,990 lines, and
/// `作成` (15,780), `修正` (4,240) and `公開` (1,944) are 99.9% of them. The
/// other 16 -- `初版第1刷発行`, `第14刷`, `改版` -- are publication dates of the
/// source edition that happen to be written unindented, so they are not file
/// dating lines and are excluded from the set. A looser test that accepted
/// any suffix would relabel a publication date as file provenance.
fn is_file_dating_line(line: &str) -> bool {
    let Some(rest) = line.strip_suffix("作成").or_else(|| {
        line.strip_suffix("修正")
            .or_else(|| line.strip_suffix("公開"))
    }) else {
        return false;
    };
    let Some(rest) = rest.strip_suffix('日') else {
        return false;
    };
    let Some((year, day)) = rest.split_once('年') else {
        return false;
    };
    let Some((month, day)) = day.split_once('月') else {
        return false;
    };
    year.len() == 4
        && [year, month, day]
            .iter()
            .all(|part| !part.is_empty() && part.chars().all(|ch| ch.is_ascii_digit()))
}

/// A licence statement: the terms the file is offered under.
///
/// A different act from the distribution notice, which says how the file came
/// to exist. These say what a reader may do with it, so they carry their own
/// role: a consumer that wants provenance and a consumer that wants terms are
/// not asking the same question, and one role answering both would make the
/// distinction unrecoverable from the ledger.
///
/// **The anchor is the licence's own name.** Corpus-wide these are 216 lines in
/// 12 distinct head forms, and every one of them contains
/// `クリエイティブ・コモンズ` -- with and without the interpunct, with and
/// without the bracket, opened with `※` and opened plainly. The 72 lines
/// reading `上記のライセンスに従って、訳者に断りなく…` carry no such name and
/// are not tested for: they always follow one of the 72 `※この翻訳は「クリエイ
/// ティブ・コモンズ…` lines, so the run continues onto them.
///
/// Tested BEFORE the remark, because 89 of the 216 open with `※` and would
/// otherwise be read as remarks. Both claims attribute, so the ordering does
/// not move the measure by a byte -- it decides only whether the ledger says
/// "terms" or "commentary", which is the part worth getting right.
fn is_licence_statement(line: &str) -> bool {
    line.contains("クリエイティブ・コモンズ")
}

/// A colophon field whose value opens with a quoted title instead of a colon:
/// `底本の親本「宮本百合子全集　第六巻」河出書房`.
///
/// **Recognized only inside an open field run, never on its own.** The shape
/// alone is far too loose -- `繰返し記号「ゝ」「ゞ」は、仮名に書き換えました。`
/// is a transcriber's sentence with the same shape, and it is read correctly
/// because it sits in a remark run rather than a field one. Position is again
/// what separates two identical shapes.
///
/// Corpus-wide this catches 56 lines in 6 forms, and every one is a real field
/// name: `底本の親本` (47), `初出` and its punctuated variants (7),
/// `底本の親本一、` (1), `初出時の表題は` (1). Without it those lines also cost
/// the indented continuations beneath them, which lose the run they hang from.
fn is_titled_colophon_field(line: &str) -> bool {
    let Some((name, _title)) = line.split_once('「') else {
        return false;
    };
    !name.is_empty() && name.chars().all(|ch| !ch.is_control())
}

/// The archive's own distribution notice: how this file came to exist.
///
/// `このファイルは、インターネットの図書館、青空文庫（http://www.aozora.gr.jp/）
/// で作られました。入力、校正、制作にあたったのは、ボランティアの皆さんです。`
/// is the sentence, and a smaller family says the rights holder deposited the
/// file directly rather than volunteers transcribing it. It is generated by
/// Aozora Bunko rather than written by a transcriber, and it states in prose
/// what the `入力：` and `校正：` fields state as fields.
///
/// **This is the one place a sentence is recognized, and it is recognized as a
/// constant rather than read.** A closed set of literals was measured and
/// rejected: across all 17,876 works of the pinned corpus the notice appears in
/// 29 distinct forms, and the variation is not only the URL. It is
/// `インターネツト` for `インターネット`, `あたつた` for `あたった`,
/// `みんなさん` for `皆さん`, `www.aozora.gr.p` for `.jp`, a fullwidth `：` in
/// the scheme, halfwidth `()` for `（）`, and truncations that drop the final
/// `。`. Enumerating those would be enumerating typos, and the thirtieth would
/// arrive with the next corpus revision.
///
/// So the test is the two fixed anchors the sentence opens with and names: it
/// begins `このファイルは、` and it names `青空文庫`. Measured over the whole
/// corpus, **every one of the 17,680 tail lines beginning `このファイルは、` is
/// this notice** and not one of them lacks `青空文庫` -- so the second anchor
/// currently rejects nothing and is kept as the guard against a transcriber
/// remark that happens to open the same way.
///
/// Claiming this is not claiming to understand a sentence, which is why the `※`
/// remarks beside it stay unattributed. This sentence is a constant the archive
/// emits; those are prose a person wrote, each saying something different.
fn is_distribution_notice(line: &str) -> bool {
    line.starts_with("このファイルは、") && line.contains("青空文庫")
}

/// A colophon field line: a non-empty key, a fullwidth colon, and a value.
///
/// The key must not itself contain a colon, so a line that merely mentions one
/// mid-sentence is not a field. An empty value is still a field -- `初出：`
/// with the citation on the following lines is a real shape.
///
/// This is a shape test only, and shape does not separate a colophon field
/// from the header's notation legend: `《》：ルビ` passes it. Only the caller's
/// tail-region scope makes that distinction, so this must not be reused
/// against text from any other region.
fn is_colophon_field(line: &str) -> bool {
    let Some((key, _value)) = line.split_once('：') else {
        return false;
    };
    !key.is_empty() && !key.contains('：') && key.chars().all(|ch| !ch.is_control())
}

/// One line of a metadata region: its absolute span, the text as written, and
/// the trimmed text the form tests read.
struct RegionLine {
    /// The whole line, its terminator excluded. Layout whitespace INCLUDED --
    /// see `region_lines`.
    start: usize,
    end: usize,
    /// The line as written between those offsets: what the fact witnesses.
    source_form: String,
    /// The same line with layout whitespace trimmed off both ends: what the
    /// form tests read, so that a stray trailing space cannot decide whether a
    /// line is recognized.
    content: String,
    /// Whether the line was written with leading layout whitespace. Kept
    /// because the trimmed content cannot say so, and a colophon continuation
    /// is recognized by its indentation under a field rather than by its text.
    indented: bool,
}

/// A metadata region's lines, terminators excluded and layout whitespace kept.
///
/// **A line fact covers its line.** An earlier version of this trimmed the
/// indentation off the span as well as off the text, on the reasoning that a
/// claim should cover content and not the space in front of it. That was
/// wrong, and the producer below is what shows it: a colophon continuation is
/// recognized BY its indentation. A fact that claims the continuation while
/// declining to cover the whitespace declines to cover its own discriminator,
/// and leaves 16,470 bytes of a 597-work sample unattributable by
/// construction. Whitespace is not content, but it is evidence, and the span a
/// fact asserts over should be the span it read.
///
/// The trimmed `content` still drives every form test, and the symmetry there
/// is the point. Stripping only the indentation left a line's classification
/// dependent on whether the transcriber happened to leave a trailing space.
/// Measured across a 597-work sample, exactly one legend line carried trailing
/// whitespace, falling through every form test to unattributed because `）`
/// was no longer the last character.
///
/// `trim` here is Unicode whitespace, so the fullwidth `　` that Aozora indents
/// colophon continuations with is treated exactly as an ASCII space would be.
fn region_lines(text: &str, region_start: usize) -> Vec<RegionLine> {
    let mut lines = Vec::new();
    let mut offset = region_start;
    for raw in text.split_inclusive('\n') {
        let stripped = raw.trim_end_matches(['\n', '\r']);
        lines.push(RegionLine {
            start: offset,
            end: offset + stripped.len(),
            source_form: stripped.to_owned(),
            content: stripped.trim().to_owned(),
            indented: stripped.starts_with(char::is_whitespace),
        });
        offset += raw.len();
    }
    lines
}

/// One classified line of a metadata region.
///
/// Every metadata producer emits a consistent record structure comprising
/// a structural-control claim over the line's span, witnessed by the line as
/// written. The witness is what lets a reader of the ledger check the claim
/// against the source without re-deriving the region.
fn region_fact(line: &RegionLine, construct: &str, role: &str) -> Value {
    json!({
        "start": line.start,
        "end": line.end,
        "construct_id": construct,
        "source_role": role,
        "disposition": "structural_control",
        "evidence_class": "structural_token",
        "parser_evidence_code": format!("metadata:{construct}"),
        "construct_witness": {
            "construct_id": construct,
            "start": line.start,
            "end": line.end,
            "source_form": line.source_form.clone()
        }
    })
}

/// A separator rule: a run of ASCII hyphens alone on its line.
///
/// Measured over a 597-work sample of the pinned corpus: every fence of every
/// legend block is a run of `-` with no indentation and no other character,
/// 53 to 55 of them. The bound is permissive on length and strict on
/// composition, because length varies by transcriber style whereas composition does not.
fn is_separator_rule(line: &str) -> bool {
    line.len() >= 5 && line.bytes().all(|byte| byte == b'-')
}

/// Classified facts for the editorial legend block, which is the HEADER only.
///
/// Aozora headers carry a fenced block that explains the notation used in the
/// body: a pair of separator rules around a `【テキスト中に現れる記号について】`
/// heading, then the legend entries themselves (`《》：ルビ` and the rest),
/// worked examples, and occasional parenthetical notes.
///
/// **The fenced block is what bounds this producer, not the shape of a line.**
/// A legend entry is a fullwidth-colon `key：value` line and so is a colophon
/// field; the two are separated by which region and which block they sit in,
/// never by their own text. This mirrors `metadata_entries`, which is confined
/// to the tail for the same reason and in the opposite direction.
///
/// The block is identified by a closed pair of separator rules enclosing a
/// `【...】` heading. All three are required. Over a 597-work sample of the
/// pinned corpus, 557 headers carry such a block, every one of them is fenced
/// by hyphen runs, every one contains exactly one heading, and no fenced pair
/// was found without one -- so a fence pair with no heading is a shape this
/// producer has never seen and declines to interpret.
///
/// Lines inside the block that match none of the four forms are claimed as
/// `editorial_legend_line`: block membership, with no claim about which form
/// the line takes. They are `＊濁点付きの二倍の踊り字は…`, `※底本では…` notes,
/// `アクセント分解についての詳細は下記URLを参照してください` and the bare URL
/// under it -- 70 of 3,631 non-blank block lines in the sample.
///
/// **The fence is what makes that claim safe, and it is why the same catch-all
/// would not be safe anywhere else.** The block is entered only on a matched
/// pair of separator rules enclosing a `【...】` heading; a header carrying no
/// such pair yields no facts at all rather than a catch-all over its whole
/// text. What is claimed of these lines is that they are notation-legend
/// material -- not body, not bibliography, not colophon -- which is the
/// question the metadata measure asks, and is weaker than reading them.
///
/// Blank lines inside the block are left alone: their only bytes are the
/// terminator, which already carries a line-ending fact.
fn legend_entries(decoded: &DecodedSource, parser: &MemberIdentity) -> Vec<Value> {
    let Ok(regions) = decoded.source_regions() else {
        return Vec::new();
    };
    let header = regions.header();
    let Some(text) = decoded.text.get(header.clone()) else {
        return Vec::new();
    };
    let lines = region_lines(text, header.start);
    let fences = lines
        .iter()
        .enumerate()
        .filter(|(_, line)| is_separator_rule(&line.content))
        .map(|(index, _)| index)
        .collect::<Vec<_>>();
    let (Some(&open), Some(&close)) = (fences.first(), fences.get(1)) else {
        return Vec::new();
    };
    let inner = &lines[open + 1..close];
    if !inner.iter().any(|line| is_legend_heading(&line.content)) {
        return Vec::new();
    }

    let mut entries = Vec::new();
    let mut emit = |line: &RegionLine, construct: &str| {
        entries.push(region_fact(line, construct, "editorial_legend"));
    };
    emit(&lines[open], "editorial_separator_rule");
    emit(&lines[close], "editorial_separator_rule");
    for line in inner {
        let content = line.content.as_str();
        if content.is_empty() {
            continue;
        }
        let construct = if is_legend_heading(content) {
            "editorial_legend_heading"
        } else if content.starts_with("（例）") {
            "editorial_legend_example"
        } else if content.starts_with('（') && content.ends_with('）') {
            "editorial_legend_note"
        } else if is_legend_entry(content) {
            "editorial_legend_entry"
        } else {
            "editorial_legend_line"
        };
        emit(line, construct);
    }
    let _ = parser;
    entries
}

/// The legend block's heading: a `【...】` line and nothing else.
fn is_legend_heading(line: &str) -> bool {
    line.starts_with('【') && line.ends_with('】') && line.chars().count() > 2
}

/// Classified facts for the bibliographic block, which is the HEADER only.
///
/// An Aozora header opens with the work's own identification, one item to a
/// line and nothing else on it: title, then any original title, subtitle or
/// volume line, then the author, then any translator or editor.
/// `早すぎる埋葬` / `THE PREMATURE BURIAL` / `エドガー・アラン・ポー Edgar
/// Allan Poe` / `佐々木直次郎訳` is the full shape. What follows is the
/// notation legend, and then the body.
///
/// **The block is what bounds this producer, not the shape of a line.** The
/// block is the run of lines from the start of the header to the first blank
/// line, separator rule or bracketed editorial heading, whichever comes first.
/// Across a 597-work sample of the pinned corpus that run is 1 to 6 lines
/// (437 works have exactly the two lines of title and author), and no header
/// carries a further non-blank line between the blank line and the legend fence,
/// so the blank line is a genuine terminator. The 40 works with no run at all
/// have a zero-byte header, with nothing to classify.
///
/// No claim is made about which specific bibliographic role a line represents.
/// Line 1 is usually the title and the last is usually a translator, but
/// subtitles and original titles vary the ordering, making positional inference
/// unreliable. What is claimed is block membership: these bytes constitute
/// the work's front matter, rather than body text, notation legend, or
/// transcriber prose.
///
/// Shape is not consulted inside the block, and one work in the sample shows
/// why. Its title is `※［＃「氓のへん／（虫＋虫）」、第3水準1-91-58］の囁き`
/// (a title whose first character is a gaiji annotation). A producer that
/// declined `※` lines as transcriber remarks by shape would have declined a
/// title. The two bracketed-heading forms are excluded as terminators of the
/// block rather than as forms within it, because `【テキスト中に現れる記号に
/// ついて】` and `［表記について］` are where the legend starts.
fn bibliographic_entries(decoded: &DecodedSource, parser: &MemberIdentity) -> Vec<Value> {
    let Ok(regions) = decoded.source_regions() else {
        return Vec::new();
    };
    let header = regions.header();
    let Some(text) = decoded.text.get(header.clone()) else {
        return Vec::new();
    };
    let mut entries = Vec::new();
    for line in region_lines(text, header.start) {
        let content = line.content.as_str();
        if content.is_empty() || is_separator_rule(content) || is_bracketed_heading(content) {
            break;
        }
        entries.push(region_fact(
            &line,
            "bibliographic_header_line",
            "bibliographic",
        ));
    }
    let _ = parser;
    entries
}

/// A bracketed editorial heading: `【...】` or `［...］` alone on its line.
///
/// Looser than `is_legend_heading` on purpose. That one runs INSIDE a fenced
/// block and only has to recognize the legend's own heading; this one has to
/// stop the bibliographic block before ANY editorial heading, including the
/// `［表記について］` variant header that carries no fence around it.
fn is_bracketed_heading(line: &str) -> bool {
    let bracketed = (line.starts_with('【') && line.ends_with('】'))
        || (line.starts_with('［') && line.ends_with('］'));
    bracketed && line.chars().count() > 2
}

/// A legend entry: a notation symbol, a fullwidth colon, then its explanation.
///
/// Although syntactically identical to `is_colophon_field`, this is kept as
/// a separate function. The two share a syntax pattern but carry different
/// semantics in different regions. Combining them would allow unintended
/// reuse where the region distinction is lost.
fn is_legend_entry(line: &str) -> bool {
    let Some((symbol, _explanation)) = line.split_once('：') else {
        return false;
    };
    !symbol.is_empty() && !symbol.contains('：') && symbol.chars().all(|ch| !ch.is_control())
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

fn build_ledger(
    bytes: &[u8],
    decoded: &DecodedSource,
    parser_output: &[u8],
    identity_ref: &str,
) -> Result<Value> {
    ensure!(
        decoded.encoding != "windows-31j-lossy",
        "lossy source decoding"
    );
    let parser = member_identity("json", parser_output);
    let output = lex(&decoded.span_text);
    // Accent-rewritten `〔…〕` spans require no fact reconciliation. The sanitize
    // offset map records one edit per digraph site, allowing every classifier fact
    // inside a rewritten span to rebase to its exact source range. Delimiters
    // remain individual recoveries and interiors remain typed, matching the behavior
    // of spans untouched by the sanitizer. The former whole-span reconciliation
    // existed to compensate for an earlier offset edit that collapsed all interior
    // facts onto the span start.
    let mut entries = output
        .classified_source_facts
        .into_iter()
        .filter_map(|fact| fact_entry(fact, decoded, &parser))
        .collect::<Vec<_>>();
    entries.extend(sanitizer_entries(decoded, &parser));
    entries.extend(metadata_entries(decoded, &parser));
    entries.extend(legend_entries(decoded, &parser));
    entries.extend(bibliographic_entries(decoded, &parser));
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
        "qualification_identity_ref": identity_ref,
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
    // Frozen instrument identity, not a code reference: it is hashed into every
    // published capture and into the committed fixture, so it does not track the
    // crate's name. The crate was renamed from ab-aozora-aat, whose prefix
    // wrongly implied the lifted-from-upstream lineage; changing this string
    // would silently mint a second identity claiming the same v1.
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
        &qualification_identity_ref(),
    )?))
}

/// Capture all authenticated member bytes and their closed manifest.
///
/// # Errors
///
/// Returns an error when any member cannot be produced or the resulting
/// generation fails its closed authentication checks.
pub fn capture_generation_from_bytes(bytes: &[u8]) -> Result<CaptureGeneration> {
    capture_generation_from_bytes_for_identity(bytes, &qualification_identity_ref())
}

/// Capture all member bytes under an explicit qualification identity.
///
/// # Errors
///
/// Returns an error for a malformed identity reference or any capture failure.
pub fn capture_generation_from_bytes_for_identity(
    bytes: &[u8],
    identity_ref: &str,
) -> Result<CaptureGeneration> {
    capture_generation_from_bytes_with_identity(bytes, identity_ref, None)
}

/// Capture all member bytes under explicit qualification and work identities.
///
/// # Errors
///
/// Returns an error for an empty work identity, malformed qualification
/// identity, or any capture failure.
pub fn capture_generation_from_bytes_for_identity_and_work(
    bytes: &[u8],
    identity_ref: &str,
    work_id: &str,
) -> Result<CaptureGeneration> {
    ensure!(!work_id.is_empty(), "work identity must not be empty");
    capture_generation_from_bytes_with_identity(bytes, identity_ref, Some(work_id))
}

fn capture_generation_from_bytes_with_identity(
    bytes: &[u8],
    identity_ref: &str,
    work_id: Option<&str>,
) -> Result<CaptureGeneration> {
    ensure!(
        identity_ref.strip_prefix("sha256:").is_some_and(|digest| {
            digest.len() == 64
                && digest
                    .bytes()
                    .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
        }),
        "qualification identity must be a lowercase SHA-256 reference"
    );
    let decoded = decode_source_bytes(bytes)?;
    let decoded_source = decoded.text.as_bytes().to_vec();
    let parser_output = aat_json_from_bytes(bytes)?;
    let raw_diagnostics = diagnostics_json_from_bytes(bytes)?;
    let classified_source_ledger = canonical_json(&build_ledger(
        bytes,
        &decoded,
        &parser_output,
        identity_ref,
    )?);
    let decoded_identity = member_identity("txt", &decoded_source);
    let parser_identity = member_identity("json", &parser_output);
    let diagnostics_identity = member_identity("json", &raw_diagnostics);
    let ledger_identity = member_identity("json", &classified_source_ledger);
    let mut manifest = json!({
        "schema_version": GENERATION_VERSION,
        "qualification_identity_ref": identity_ref,
        "work_id": work_id.unwrap_or(&decoded.source_hash),
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
    let original_source_hash = ledger["original_source"]["value_hash"]
        .as_str()
        .context("original source hash is absent")?;
    ensure!(
        ledger["original_source"]["artifact_ref"] == format!("source/{original_source_hash}"),
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
        manifest["qualification_identity_ref"]
            .as_str()
            .is_some_and(|value| value.strip_prefix("sha256:").is_some_and(|digest| {
                digest.len() == 64
                    && digest
                        .bytes()
                        .all(|byte| byte.is_ascii_digit() || (b'a'..=b'f').contains(&byte))
            })),
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
        (decoded, parsed.classified_source_facts)
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
    fn explicit_capture_work_identity_is_preserved() {
        let generation = capture_generation_from_bytes_for_identity_and_work(
            "本文\n".as_bytes(),
            &qualification_identity_ref(),
            "000001_1",
        )
        .unwrap();
        let manifest: Value = serde_json::from_slice(&generation.manifest).unwrap();

        assert_eq!(manifest["work_id"], "000001_1");
        verify_capture_generation(&generation).unwrap();
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
    fn rewritten_accent_scope_is_not_literal_bracket_recovery() {
        let source = "〔cafe'〕";
        let entries = live_entries(source);
        assert!(recovered(&entries).is_empty());
        assert!(
            entries
                .iter()
                .any(|entry| entry["construct_id"] == "plain_text"
                    && entry["start"] == 3
                    && entry["end"] == 8)
        );
    }

    #[test]
    fn constructs_inside_a_rewritten_span_keep_their_own_source_spans() {
        // The duplicate-entry capture failures: every fact inside a rewritten
        // span once rebased to the whole bracketed range, so two same-typed
        // constructs became byte-identical entries. Ruby beside a digraph
        // must map to exactly its own source bytes.
        let source = "〔a`｜青梅《おうめ》〕";
        let entries = live_entries(source);
        let ruby: Vec<&Value> = entries
            .iter()
            .filter(|entry| entry["construct_id"] == "ruby")
            .collect();
        assert_eq!(ruby.len(), 1);
        let expected_start = source.find('｜').unwrap() as u64;
        let expected_end = source.rfind('》').unwrap() as u64 + '》'.len_utf8() as u64;
        assert_eq!(ruby[0]["start"], expected_start);
        assert_eq!(ruby[0]["end"], expected_end);
    }

    #[test]
    fn two_same_typed_constructs_inside_one_rewritten_span_capture_validly() {
        // The corpus shape that failed closed: two ruby constructs inside one
        // accent-rewritten span. Under the whole-span collapse both rebased
        // to identical entries and the ledger (correctly) refused the
        // duplicate; with per-site edits each keeps its own span.
        let source = "〔Henri《アンリイ》 De《ド》 Re'gnier《レニエ》〕";
        let generation = capture_generation_from_bytes_for_identity_and_work(
            source.as_bytes(),
            &qualification_identity_ref(),
            "ruby-inside-accent-span",
        )
        .unwrap();
        verify_capture_generation(&generation).unwrap();

        let ledger: Value = serde_json::from_slice(&generation.classified_source_ledger).unwrap();
        let ruby_spans: Vec<(u64, u64)> = ledger["entries"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|entry| entry["construct_id"] == "ruby")
            .map(|entry| {
                (
                    entry["start"].as_u64().unwrap(),
                    entry["end"].as_u64().unwrap(),
                )
            })
            .collect();
        assert_eq!(ruby_spans.len(), 3);
        assert!(
            ruby_spans.windows(2).all(|pair| pair[0].1 <= pair[1].0),
            "ruby spans must be distinct and ordered: {ruby_spans:?}"
        );
    }

    #[test]
    fn nested_accent_bracket_capture_produces_a_valid_generation() {
        let source = "〔George Innes, 1825―1894.〔Albert Biersta`dt〕";
        let generation = capture_generation_from_bytes_for_identity_and_work(
            source.as_bytes(),
            &qualification_identity_ref(),
            "nested-accent-bracket",
        )
        .unwrap();

        verify_capture_generation(&generation).unwrap();
    }

    #[test]
    fn unrelated_recovery_inside_accent_edit_survives() {
        let entries = live_entries("〔a`｜〕");
        let recovered = recovered(&entries);
        assert_eq!(recovered.len(), 1, "{entries:#?}");
        assert_eq!(
            (recovered[0]["start"].as_u64(), recovered[0]["end"].as_u64()),
            (Some(5), Some(8))
        );
        assert_eq!(recovered[0]["construct_witness"]["source_form"], "｜");
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
}
