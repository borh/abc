use std::collections::HashSet;
use std::fs::{self, File};
use std::io::{BufReader, Read};
use std::path::{Component, Path};

use anyhow::Result;
use sha2::{Digest, Sha256};

use crate::interval::{Interval, intersect, normalize, subtract, total_len};
use crate::{
    AggregateRecord, AggregateSchemaVersion, CoordinateSystem, CorpusEntry, QualificationIdentity,
    RecordIndex, TaxonomyIdentity, WorkCompleteness, WorkInterval, WorkRecord, WorkStatus,
    canonical_json,
};

fn digest(bytes: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(bytes))
}

fn stream_identity(path: &Path) -> Result<(String, u64, Vec<u8>)> {
    let mut reader = BufReader::new(File::open(path)?);
    let mut hasher = Sha256::new();
    let mut bytes = Vec::new();
    let mut buffer = [0_u8; 64 * 1024];
    let mut length = 0_u64;
    loop {
        let read = reader.read(&mut buffer)?;
        if read == 0 {
            break;
        }
        hasher.update(&buffer[..read]);
        bytes.extend_from_slice(&buffer[..read]);
        length = length
            .checked_add(read as u64)
            .ok_or_else(|| anyhow::anyhow!("record byte length overflow"))?;
    }
    Ok((format!("sha256:{:x}", hasher.finalize()), length, bytes))
}

fn intervals(wire: &[crate::WireInterval], bound: u64) -> Result<Vec<Interval>> {
    wire.iter()
        .map(|span| {
            let start = usize::try_from(span.start)?;
            let end = usize::try_from(span.end)?;
            let bound = usize::try_from(bound)?;
            Interval::new(start, end, bound)
        })
        .collect()
}

fn valid_record(
    record: &WorkRecord,
    corpus: &CorpusEntry,
    identity_ref: &str,
    taxonomy: &TaxonomyIdentity,
) -> Result<Vec<WorkInterval>> {
    anyhow::ensure!(record.status == WorkStatus::Ok, "work unavailable");
    anyhow::ensure!(record.errors.is_empty(), "ok work has errors");
    anyhow::ensure!(record.work_id == corpus.work_id, "record work ID mismatch");
    anyhow::ensure!(
        record.original_source.sha256 == corpus.original_sha256,
        "source hash mismatch"
    );
    anyhow::ensure!(
        record.identity_ref == identity_ref,
        "qualification identity mismatch"
    );
    anyhow::ensure!(
        record.taxonomy_version == taxonomy.taxonomy_version
            && record.taxonomy_hash == taxonomy.taxonomy_hash,
        "taxonomy identity mismatch"
    );
    anyhow::ensure!(
        record.coordinate_system == CoordinateSystem::DecodedUtf8,
        "coordinate system mismatch"
    );

    let ignored = normalize(intervals(&record.ignored, record.decoded_source_bytes)?);
    let eligible = normalize(intervals(&record.eligible, record.decoded_source_bytes)?);
    let covered = normalize(intervals(
        &record.covered_eligible,
        record.decoded_source_bytes,
    )?);
    let uncovered = normalize(intervals(
        &record.uncovered_eligible,
        record.decoded_source_bytes,
    )?);
    let ignored_bytes = total_len(&ignored)?;
    let eligible_bytes = total_len(&eligible)?;
    let covered_bytes = total_len(&covered)?;
    let uncovered_bytes = total_len(&uncovered)?;
    anyhow::ensure!(
        record.decoded_source.bytes == record.decoded_source_bytes,
        "decoded byte identity mismatch"
    );
    anyhow::ensure!(
        record.ignored_bytes == ignored_bytes
            && record.eligible_bytes == eligible_bytes
            && record.covered_eligible_bytes == covered_bytes
            && record.uncovered_eligible_bytes == uncovered_bytes,
        "supplied totals mismatch intervals"
    );
    anyhow::ensure!(
        ignored_bytes.checked_add(eligible_bytes) == Some(record.decoded_source_bytes),
        "decoded conservation failure"
    );
    anyhow::ensure!(
        covered_bytes.checked_add(uncovered_bytes) == Some(eligible_bytes),
        "eligible conservation failure"
    );
    anyhow::ensure!(
        intersect(&covered, &uncovered).is_empty(),
        "covered and uncovered overlap"
    );
    anyhow::ensure!(
        normalize([covered.clone(), uncovered.clone()].concat()) == eligible,
        "covered partition differs from eligible"
    );
    anyhow::ensure!(
        intersect(&ignored, &eligible).is_empty(),
        "ignored and eligible overlap"
    );
    anyhow::ensure!(
        subtract(&eligible, &normalize([covered, uncovered.clone()].concat())).is_empty(),
        "eligible bytes omitted"
    );
    Ok(uncovered
        .into_iter()
        .map(|span| WorkInterval {
            work_id: record.work_id.clone(),
            start: span.start() as u64,
            end: span.end() as u64,
        })
        .collect())
}

pub fn aggregate(
    corpus: &[CorpusEntry],
    index: &RecordIndex,
    records_root: &Path,
    identity: &QualificationIdentity,
    taxonomy: &TaxonomyIdentity,
) -> Result<AggregateRecord> {
    let expected = corpus.len() as u64;
    let observed = index.records.len() as u64;
    let corpus_ids: HashSet<_> = corpus.iter().map(|entry| entry.work_id.as_str()).collect();
    let index_ids: HashSet<_> = index
        .records
        .iter()
        .map(|entry| entry.work_id.as_str())
        .collect();
    let complete = corpus_ids == index_ids
        && corpus_ids.len() == corpus.len()
        && index_ids.len() == index.records.len()
        && expected == observed;
    let identity_ref = digest(canonical_json(identity)?.as_bytes());
    let mut errors = Vec::new();
    if !complete {
        errors.push("work-set-mismatch".to_owned());
    }
    if index.expected_work_count != expected || index.record_count != observed {
        errors.push("index-count-mismatch".to_owned());
    }
    if index.status != WorkStatus::Ok || !index.errors.is_empty() {
        errors.push("index-unavailable".to_owned());
    }
    if index.identity_ref != identity_ref {
        errors.push("index-identity-mismatch".to_owned());
    }
    if index.taxonomy_version != taxonomy.taxonomy_version
        || index.taxonomy_hash != taxonomy.taxonomy_hash
    {
        errors.push("index-taxonomy-mismatch".to_owned());
    }
    if digest(&taxonomy.taxonomy_jcs_bytes) != taxonomy.taxonomy_hash {
        errors.push("taxonomy-blob-mismatch".to_owned());
    }

    let root = match fs::canonicalize(records_root) {
        Ok(root) => Some(root),
        Err(_) => {
            errors.push("records-root-unavailable".to_owned());
            None
        }
    };
    let mut records = Vec::new();
    for entry in &index.records {
        let relative = Path::new(&entry.locator);
        if relative.is_absolute()
            || relative
                .components()
                .any(|part| matches!(part, Component::ParentDir))
        {
            errors.push(format!("record-locator-invalid:{}", entry.work_id));
            continue;
        }
        let Some(root) = &root else { continue };
        let path = match fs::canonicalize(root.join(relative)) {
            Ok(path) if path.starts_with(root) => path,
            _ => {
                errors.push(format!("record-locator-unavailable:{}", entry.work_id));
                continue;
            }
        };
        let (hash, length, bytes) = match stream_identity(&path) {
            Ok(value) => value,
            Err(_) => {
                errors.push(format!("record-read-failed:{}", entry.work_id));
                continue;
            }
        };
        if hash != entry.sha256 || length != entry.bytes {
            errors.push(format!("record-blob-mismatch:{}", entry.work_id));
            continue;
        }
        match serde_json::from_slice::<WorkRecord>(&bytes) {
            Ok(record) if record.work_id == entry.work_id => records.push(record),
            Ok(_) => errors.push(format!("record-index-work-id-mismatch:{}", entry.work_id)),
            Err(_) => errors.push(format!("record-decode-failed:{}", entry.work_id)),
        }
    }

    let corpus_by_id = corpus
        .iter()
        .map(|entry| (entry.work_id.as_str(), entry))
        .collect::<std::collections::HashMap<_, _>>();
    let mut eligible = 0_u64;
    let mut covered = 0_u64;
    let mut uncovered_bytes = 0_u64;
    let mut uncovered = Vec::new();
    for record in &records {
        let Some(corpus_entry) = corpus_by_id.get(record.work_id.as_str()) else {
            errors.push(format!("unexpected-record:{}", record.work_id));
            continue;
        };
        match valid_record(record, corpus_entry, &identity_ref, taxonomy) {
            Ok(witnesses) => {
                if let (Some(a), Some(b), Some(c)) = (
                    eligible.checked_add(record.eligible_bytes),
                    covered.checked_add(record.covered_eligible_bytes),
                    uncovered_bytes.checked_add(record.uncovered_eligible_bytes),
                ) {
                    eligible = a;
                    covered = b;
                    uncovered_bytes = c;
                    uncovered.extend(witnesses);
                } else {
                    errors.push("aggregate-total-overflow".to_owned());
                }
            }
            Err(error) => errors.push(format!("invalid-work:{}:{error}", record.work_id)),
        }
    }
    if eligible == 0 {
        errors.push("zero-eligible-byte-denominator".to_owned());
    }
    if covered.checked_add(uncovered_bytes) != Some(eligible) {
        errors.push("aggregate-conservation-failure".to_owned());
    }
    let work_completeness = WorkCompleteness {
        expected,
        observed,
        complete,
    };
    if !errors.is_empty() {
        return Ok(AggregateRecord {
            schema_version: AggregateSchemaVersion::V1,
            identity_ref,
            taxonomy_version: taxonomy.taxonomy_version,
            taxonomy_hash: taxonomy.taxonomy_hash.clone(),
            coordinate_system: CoordinateSystem::DecodedUtf8,
            status: WorkStatus::Unavailable,
            work_completeness,
            eligible_bytes: None,
            covered_eligible_bytes: None,
            uncovered_eligible_bytes: None,
            uncovered: None,
            errors: Some(errors),
        });
    }
    uncovered.sort_by(|a, b| (&a.work_id, a.start, a.end).cmp(&(&b.work_id, b.start, b.end)));
    Ok(AggregateRecord {
        schema_version: AggregateSchemaVersion::V1,
        identity_ref,
        taxonomy_version: taxonomy.taxonomy_version,
        taxonomy_hash: taxonomy.taxonomy_hash.clone(),
        coordinate_system: CoordinateSystem::DecodedUtf8,
        status: WorkStatus::Ok,
        work_completeness,
        eligible_bytes: Some(eligible),
        covered_eligible_bytes: Some(covered),
        uncovered_eligible_bytes: Some(uncovered_bytes),
        uncovered: Some(uncovered),
        errors: None,
    })
}
