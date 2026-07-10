use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use ab_diff_utils::{
    FirstDifference, first_difference, hash_bytes, hash_json, hash_string_sequence,
};
use anyhow::{Context, Result};
use rayon::prelude::*;
use rustc_hash::FxHashMap;
use serde::{Deserialize, Serialize};
use serde_json::Value;
use walkdir::WalkDir;

type CountMap = FxHashMap<String, usize>;

#[derive(Debug, Serialize)]
pub struct AatCompareSummary {
    pub common_aat: usize,
    pub only_a: usize,
    pub only_b: usize,
    pub structural_difference_count: usize,
    pub visible_text_difference_count: usize,
    pub normalized_visible_text_difference_count: usize,
    pub same_visible_structural_difference_count: usize,
    pub semantic_hash_difference_counts: BTreeMap<String, usize>,
    pub semantic_summary_hash_difference_counts: BTreeMap<String, usize>,
    pub normalized_visible_difference_buckets: BTreeMap<String, usize>,
    pub a_semantic_totals: BTreeMap<String, usize>,
    pub b_semantic_totals: BTreeMap<String, usize>,
    pub structural_differences: Vec<AatStructuralDifference>,
    pub coverage_only_difference_count: usize,
    pub coverage_differences: Vec<AatStructuralDifference>,
    pub coverage_metrics_missing: usize,
}

#[derive(Debug, Serialize)]
pub struct AatStructuralDifference {
    pub work_id: String,
    pub visible_text_differs: bool,
    pub normalized_visible_text_differs: bool,
    pub a_structure_hash: String,
    pub b_structure_hash: String,
    pub a_visible_hash: String,
    pub b_visible_hash: String,
    pub a_normalized_visible_hash: String,
    pub b_normalized_visible_hash: String,
    pub a_block_kinds: BTreeMap<String, usize>,
    pub b_block_kinds: BTreeMap<String, usize>,
    pub a_inline_kinds: BTreeMap<String, usize>,
    pub b_inline_kinds: BTreeMap<String, usize>,
    pub a_semantic_counts: BTreeMap<String, usize>,
    pub b_semantic_counts: BTreeMap<String, usize>,
    pub a_semantic_hashes: BTreeMap<String, String>,
    pub b_semantic_hashes: BTreeMap<String, String>,
    pub semantic_hashes_differ: BTreeMap<String, bool>,
    pub a_semantic_summary_hashes: BTreeMap<String, String>,
    pub b_semantic_summary_hashes: BTreeMap<String, String>,
    pub semantic_summary_hashes_differ: BTreeMap<String, bool>,
    pub normalized_visible_difference_bucket: Option<String>,
    pub normalized_visible_first_difference: Option<VisibleTextDifference>,
    pub coverage_mismatch: Option<CoverageDelta>,
}

#[derive(Debug, Serialize)]
pub struct CoverageDelta {
    pub a_had_fallback: bool,
    pub b_had_fallback: bool,
    pub a_fallback_reason: Option<String>,
    pub b_fallback_reason: Option<String>,
    pub a_source_bytes: Option<usize>,
    pub b_source_bytes: Option<usize>,
}

#[derive(Debug, Serialize)]
pub struct VisibleTextDifference {
    pub char_index: usize,
    pub a_snippet: String,
    pub b_snippet: String,
}

impl From<FirstDifference> for VisibleTextDifference {
    fn from(diff: FirstDifference) -> Self {
        Self {
            char_index: diff.char_index,
            a_snippet: diff.left_snippet,
            b_snippet: diff.right_snippet,
        }
    }
}

#[derive(Debug, Deserialize)]
struct AatRoot {
    work_id: String,
    blocks: Value,
    meta: Option<Value>,
}

#[derive(Debug)]
struct AatSummary {
    work_id: String,
    structure_hash: String,
    visible_hash: String,
    normalized_visible_hash: String,
    normalized_visible: String,
    block_kinds: CountMap,
    inline_kinds: CountMap,
    semantic_totals: CountMap,
    semantic_counts: CountMap,
    semantic_hashes: BTreeMap<String, String>,
    semantic_summary_hashes: BTreeMap<String, String>,
    fallback_used: Option<bool>,
    fallback_reason: Option<String>,
    source_bytes: Option<usize>,
}

#[derive(Debug)]
struct PathSummary {
    path: std::path::PathBuf,
    work_id: String,
    summary: AatSummary,
}

#[cfg(test)]
static LIVE_ROOTS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
#[cfg(test)]
static MAX_LIVE_ROOTS: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);

#[cfg(test)]
struct LiveRootGuard;

#[cfg(test)]
impl LiveRootGuard {
    fn new() -> Self {
        use std::sync::atomic::Ordering;
        let live = LIVE_ROOTS.fetch_add(1, Ordering::SeqCst) + 1;
        MAX_LIVE_ROOTS.fetch_max(live, Ordering::SeqCst);
        Self
    }
}

#[cfg(test)]
impl Drop for LiveRootGuard {
    fn drop(&mut self) {
        LIVE_ROOTS.fetch_sub(1, std::sync::atomic::Ordering::SeqCst);
    }
}

/// Compare AAT summary trees from two directories.
///
/// # Errors
///
/// Returns an error if either directory cannot be traversed or any input JSON
/// file cannot be read or parsed.
pub fn compare_aat_dirs(a: &Path, b: &Path) -> Result<AatCompareSummary> {
    compare_aat_dirs_with_limit(a, b, None)
}

/// Compare AAT summary trees with an optional difference output limit.
///
/// # Errors
///
/// Returns an error if either directory cannot be traversed or any input JSON
/// file cannot be read or parsed.
pub fn compare_aat_dirs_with_limit(
    a: &Path,
    b: &Path,
    difference_limit: Option<usize>,
) -> Result<AatCompareSummary> {
    let a = read_aat_summaries(a)?;
    let b = read_aat_summaries(b)?;
    let keys_a = a.keys().cloned().collect::<BTreeSet<_>>();
    let keys_b = b.keys().cloned().collect::<BTreeSet<_>>();
    let common = keys_a.intersection(&keys_b).cloned().collect::<Vec<_>>();
    let a_semantic_totals = semantic_totals(a.values());
    let b_semantic_totals = semantic_totals(b.values());
    let mut structural_differences = Vec::new();
    let mut structural_difference_count = 0usize;
    let mut visible_text_difference_count = 0usize;
    let mut normalized_visible_text_difference_count = 0usize;
    let mut same_visible_structural_difference_count = 0usize;
    let mut coverage_only_difference_count = 0usize;
    let mut coverage_differences = Vec::new();
    let mut coverage_metrics_missing = 0usize;
    let mut semantic_hash_difference_counts = BTreeMap::new();
    let mut semantic_summary_hash_difference_counts = BTreeMap::new();
    let mut normalized_visible_difference_buckets = BTreeMap::new();

    for key in &common {
        let left = &a[key];
        let right = &b[key];
        let semantic_hashes_differ = semantic_hash_differences(left, right);
        let semantic_summary_hashes_differ = semantic_summary_hash_differences(left, right);
        for (name, differs) in &semantic_hashes_differ {
            if *differs {
                increment_ordered(&mut semantic_hash_difference_counts, name.clone());
            }
        }
        for (name, differs) in &semantic_summary_hashes_differ {
            if *differs {
                increment_ordered(&mut semantic_summary_hash_difference_counts, name.clone());
            }
        }
        let coverage_mismatch = if left.fallback_used.is_some()
            && right.fallback_used.is_some()
            && left.fallback_used != right.fallback_used
        {
            Some(CoverageDelta {
                a_had_fallback: left.fallback_used.unwrap_or(false),
                b_had_fallback: right.fallback_used.unwrap_or(false),
                a_fallback_reason: left.fallback_reason.clone(),
                b_fallback_reason: right.fallback_reason.clone(),
                a_source_bytes: left.source_bytes,
                b_source_bytes: right.source_bytes,
            })
        } else {
            if left.fallback_used.is_none() || right.fallback_used.is_none() {
                coverage_metrics_missing += 1;
            }
            None
        };
        let has_hash_difference = left.structure_hash != right.structure_hash
            || left.visible_hash != right.visible_hash
            || semantic_summary_hashes_differ
                .values()
                .any(|differs| *differs);
        if has_hash_difference {
            structural_difference_count += 1;
            if left.visible_hash != right.visible_hash {
                visible_text_difference_count += 1;
            } else {
                same_visible_structural_difference_count += 1;
            }
            let normalized_visible_difference_bucket =
                if left.normalized_visible_hash != right.normalized_visible_hash {
                    normalized_visible_text_difference_count += 1;
                    let bucket = normalized_visible_difference_bucket(&semantic_hashes_differ);
                    increment_ordered(&mut normalized_visible_difference_buckets, bucket.clone());
                    Some(bucket)
                } else {
                    None
                };
            let normalized_visible_first_difference =
                normalized_visible_difference_bucket.as_ref().and_then(|_| {
                    first_difference(&left.normalized_visible, &right.normalized_visible)
                        .map(VisibleTextDifference::from)
                });
            if difference_limit.is_some_and(|limit| {
                structural_differences.len() + coverage_differences.len() >= limit
            }) {
                continue;
            }
            structural_differences.push(AatStructuralDifference {
                work_id: left.work_id.clone(),
                visible_text_differs: left.visible_hash != right.visible_hash,
                normalized_visible_text_differs: left.normalized_visible_hash
                    != right.normalized_visible_hash,
                a_structure_hash: left.structure_hash.clone(),
                b_structure_hash: right.structure_hash.clone(),
                a_visible_hash: left.visible_hash.clone(),
                b_visible_hash: right.visible_hash.clone(),
                a_normalized_visible_hash: left.normalized_visible_hash.clone(),
                b_normalized_visible_hash: right.normalized_visible_hash.clone(),
                a_block_kinds: sorted_counts(&left.block_kinds),
                b_block_kinds: sorted_counts(&right.block_kinds),
                a_inline_kinds: sorted_counts(&left.inline_kinds),
                b_inline_kinds: sorted_counts(&right.inline_kinds),
                a_semantic_counts: sorted_counts(&left.semantic_counts),
                b_semantic_counts: sorted_counts(&right.semantic_counts),
                a_semantic_hashes: left.semantic_hashes.clone(),
                b_semantic_hashes: right.semantic_hashes.clone(),
                semantic_hashes_differ,
                a_semantic_summary_hashes: left.semantic_summary_hashes.clone(),
                b_semantic_summary_hashes: right.semantic_summary_hashes.clone(),
                semantic_summary_hashes_differ,
                normalized_visible_difference_bucket,
                normalized_visible_first_difference,
                coverage_mismatch,
            });
        } else if coverage_mismatch.is_some() {
            coverage_only_difference_count += 1;
            if difference_limit.is_some_and(|limit| {
                structural_differences.len() + coverage_differences.len() >= limit
            }) {
                continue;
            }
            coverage_differences.push(AatStructuralDifference {
                work_id: left.work_id.clone(),
                visible_text_differs: false,
                normalized_visible_text_differs: false,
                a_structure_hash: left.structure_hash.clone(),
                b_structure_hash: right.structure_hash.clone(),
                a_visible_hash: left.visible_hash.clone(),
                b_visible_hash: right.visible_hash.clone(),
                a_normalized_visible_hash: left.normalized_visible_hash.clone(),
                b_normalized_visible_hash: right.normalized_visible_hash.clone(),
                a_block_kinds: sorted_counts(&left.block_kinds),
                b_block_kinds: sorted_counts(&right.block_kinds),
                a_inline_kinds: sorted_counts(&left.inline_kinds),
                b_inline_kinds: sorted_counts(&right.inline_kinds),
                a_semantic_counts: sorted_counts(&left.semantic_counts),
                b_semantic_counts: sorted_counts(&right.semantic_counts),
                a_semantic_hashes: left.semantic_hashes.clone(),
                b_semantic_hashes: right.semantic_hashes.clone(),
                semantic_hashes_differ,
                a_semantic_summary_hashes: left.semantic_summary_hashes.clone(),
                b_semantic_summary_hashes: right.semantic_summary_hashes.clone(),
                semantic_summary_hashes_differ,
                normalized_visible_difference_bucket: None,
                normalized_visible_first_difference: None,
                coverage_mismatch,
            });
        }
    }

    Ok(AatCompareSummary {
        common_aat: common.len(),
        only_a: keys_a.difference(&keys_b).count(),
        only_b: keys_b.difference(&keys_a).count(),
        structural_difference_count,
        visible_text_difference_count,
        normalized_visible_text_difference_count,
        same_visible_structural_difference_count,
        semantic_hash_difference_counts,
        semantic_summary_hash_difference_counts,
        normalized_visible_difference_buckets,
        a_semantic_totals,
        b_semantic_totals,
        structural_differences,
        coverage_only_difference_count,
        coverage_differences,
        coverage_metrics_missing,
    })
}

fn read_aat_summaries(root: &Path) -> Result<BTreeMap<String, AatSummary>> {
    let entries: Vec<std::path::PathBuf> = WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| {
            entry.file_type().is_file()
                && entry
                    .path()
                    .extension()
                    .is_some_and(|extension| extension == "json")
        })
        .map(|entry| entry.path().to_owned())
        .collect();

    let loaded: Vec<PathSummary> = entries
        .par_iter()
        .map(|path| {
            let bytes =
                fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
            let root: AatRoot = serde_json::from_slice(&bytes)
                .with_context(|| format!("failed to parse {}", path.display()))?;
            #[cfg(test)]
            let _live_root = LiveRootGuard::new();
            let work_id = root.work_id.clone();
            let summary = summarize(root)?;
            Ok::<_, anyhow::Error>(PathSummary {
                path: path.to_owned(),
                work_id,
                summary,
            })
        })
        .collect::<Result<_>>()?;

    let mut work_id_counts = BTreeMap::new();
    for loaded in &loaded {
        *work_id_counts
            .entry(loaded.work_id.clone())
            .or_insert(0usize) += 1;
    }

    let summaries = loaded
        .into_iter()
        .map(|loaded| {
            let key = aat_key(&work_id_counts, &loaded.path, &loaded.work_id);
            (key, loaded.summary)
        })
        .collect::<Vec<_>>();

    let mut out = BTreeMap::new();
    for (key, summary) in summaries {
        out.insert(key, summary);
    }
    Ok(out)
}

fn aat_key(work_id_counts: &BTreeMap<String, usize>, path: &Path, work_id: &str) -> String {
    if work_id_counts.get(work_id).copied().unwrap_or_default() <= 1 {
        return work_id.to_owned();
    }
    let filename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("duplicate");
    format!("{work_id}::{filename}")
}

fn summarize(root: AatRoot) -> Result<AatSummary> {
    let structure_hash = hash_json(&root.blocks)?;
    let mut block_kinds = CountMap::default();
    let mut inline_kinds = CountMap::default();
    let mut semantic_totals = CountMap::default();
    let mut semantic_counts = CountMap::default();
    let mut semantic_sequences = SemanticSequences::default();
    let mut visible = String::new();
    collect_blocks(
        &root.blocks,
        &mut block_kinds,
        &mut inline_kinds,
        &mut semantic_totals,
        &mut semantic_counts,
        &mut semantic_sequences,
        &mut visible,
    );
    let normalized_visible = normalize_visible(&visible);
    let (fallback_used, fallback_reason, source_bytes) = root
        .meta
        .as_ref()
        .and_then(|meta| meta.get("metrics"))
        .map(|metrics| {
            (
                Some(
                    metrics
                        .get("fallback_used")
                        .and_then(Value::as_bool)
                        .unwrap_or(false),
                ),
                metrics
                    .get("fallback_reason")
                    .and_then(Value::as_str)
                    .map(str::to_owned),
                metrics
                    .get("source_bytes")
                    .and_then(Value::as_u64)
                    .map(|n| n as usize),
            )
        })
        .unwrap_or((None, None, None));
    Ok(AatSummary {
        work_id: root.work_id,
        structure_hash,
        visible_hash: hash_bytes(visible.as_bytes()),
        normalized_visible_hash: hash_bytes(normalized_visible.as_bytes()),
        normalized_visible,
        block_kinds,
        inline_kinds,
        semantic_totals,
        semantic_counts,
        semantic_hashes: semantic_sequences.into_hashes(),
        semantic_summary_hashes: semantic_summary_hashes(root.meta.as_ref())?,
        fallback_used,
        fallback_reason,
        source_bytes,
    })
}

fn collect_blocks(
    blocks: &Value,
    block_kinds: &mut CountMap,
    inline_kinds: &mut CountMap,
    semantic_totals: &mut CountMap,
    semantic_counts: &mut CountMap,
    semantic_sequences: &mut SemanticSequences,
    visible: &mut String,
) {
    let Some(blocks) = blocks.as_array() else {
        return;
    };

    for (idx, block) in blocks.iter().enumerate() {
        if idx > 0 {
            visible.push('\n');
        }
        if let Some(kind) = kind(block) {
            *block_kinds.entry(kind.to_owned()).or_insert(0) += 1;
            count_both(semantic_totals, semantic_counts, format!("block:{kind}"));
            if kind == "heading"
                && let Some(level) = block.get("level").and_then(Value::as_u64)
            {
                count_both(
                    semantic_totals,
                    semantic_counts,
                    format!("heading_level:{level}"),
                );
            }
        }
        collect_inline_containers(
            block,
            inline_kinds,
            semantic_totals,
            semantic_counts,
            semantic_sequences,
            visible,
        );
    }
}

fn collect_inline_containers(
    value: &Value,
    inline_kinds: &mut CountMap,
    semantic_totals: &mut CountMap,
    semantic_counts: &mut CountMap,
    semantic_sequences: &mut SemanticSequences,
    visible: &mut String,
) {
    for field in ["content", "children", "upper", "lower"] {
        let Some(nodes) = value.get(field).and_then(Value::as_array) else {
            continue;
        };
        for node in nodes {
            collect_inline_node(
                node,
                inline_kinds,
                semantic_totals,
                semantic_counts,
                semantic_sequences,
                visible,
            );
        }
    }
}

fn collect_inline_node(
    node: &Value,
    inline_kinds: &mut CountMap,
    semantic_totals: &mut CountMap,
    semantic_counts: &mut CountMap,
    semantic_sequences: &mut SemanticSequences,
    visible: &mut String,
) {
    let Some(kind) = kind(node) else {
        return;
    };
    *inline_kinds.entry(kind.to_owned()).or_insert(0) += 1;
    count_both(semantic_totals, semantic_counts, format!("inline:{kind}"));
    if let Some(provenance) = node.get("x-provenance").and_then(Value::as_str) {
        count_both(
            semantic_totals,
            semantic_counts,
            format!("provenance:{provenance}"),
        );
    }

    match kind {
        "text" => {
            if let Some(value) = node.get("value").and_then(Value::as_str) {
                visible.push_str(value);
            }
        }
        "ruby" => {
            if let Some(base) = node.get("base").and_then(Value::as_str) {
                visible.push_str(base);
            }
            if let Some(reading) = node.get("reading").and_then(Value::as_str) {
                semantic_sequences.ruby_readings.push(reading.to_owned());
            }
        }
        "gaiji" => {
            let resolved = node.get("resolved").and_then(Value::as_str).unwrap_or("");
            if resolved.is_empty() {
                count_both(
                    semantic_totals,
                    semantic_counts,
                    "gaiji_unresolved".to_owned(),
                );
                if let Some(description) = node.get("description").and_then(Value::as_str) {
                    semantic_sequences
                        .gaiji_descriptions
                        .push(description.to_owned());
                }
            } else {
                count_both(
                    semantic_totals,
                    semantic_counts,
                    "gaiji_resolved".to_owned(),
                );
                visible.push_str(resolved);
            }
        }
        _ => {}
    }
    collect_inline_containers(
        node,
        inline_kinds,
        semantic_totals,
        semantic_counts,
        semantic_sequences,
        visible,
    );
}

#[derive(Debug, Default)]
struct SemanticSequences {
    ruby_readings: Vec<String>,
    gaiji_descriptions: Vec<String>,
}

impl SemanticSequences {
    fn into_hashes(self) -> BTreeMap<String, String> {
        BTreeMap::from([
            (
                "ruby_readings".to_owned(),
                hash_string_sequence(&self.ruby_readings),
            ),
            (
                "gaiji_descriptions".to_owned(),
                hash_string_sequence(&self.gaiji_descriptions),
            ),
        ])
    }
}

fn semantic_totals<'a>(
    summaries: impl IntoIterator<Item = &'a AatSummary>,
) -> BTreeMap<String, usize> {
    let mut out = CountMap::default();
    for summary in summaries {
        for (key, value) in &summary.semantic_totals {
            *out.entry(key.clone()).or_insert(0) += value;
        }
    }
    sorted_counts(&out)
}

fn count_both(totals: &mut CountMap, details: &mut CountMap, key: String) {
    increment(totals, key.clone());
    increment(details, key);
}

fn increment(counts: &mut CountMap, key: String) {
    *counts.entry(key).or_insert(0) += 1;
}

fn increment_ordered(counts: &mut BTreeMap<String, usize>, key: String) {
    *counts.entry(key).or_insert(0) += 1;
}

fn sorted_counts(counts: &CountMap) -> BTreeMap<String, usize> {
    counts
        .iter()
        .map(|(key, value)| (key.clone(), *value))
        .collect()
}

fn semantic_hash_differences(left: &AatSummary, right: &AatSummary) -> BTreeMap<String, bool> {
    let keys = left
        .semantic_hashes
        .keys()
        .chain(right.semantic_hashes.keys())
        .cloned()
        .collect::<BTreeSet<_>>();
    keys.into_iter()
        .map(|key| {
            let differs = left.semantic_hashes.get(&key) != right.semantic_hashes.get(&key);
            (key, differs)
        })
        .collect()
}

fn semantic_summary_hash_differences(
    left: &AatSummary,
    right: &AatSummary,
) -> BTreeMap<String, bool> {
    let keys = left
        .semantic_summary_hashes
        .keys()
        .chain(right.semantic_summary_hashes.keys())
        .cloned()
        .collect::<BTreeSet<_>>();
    keys.into_iter()
        .map(|key| {
            let differs =
                left.semantic_summary_hashes.get(&key) != right.semantic_summary_hashes.get(&key);
            (key, differs)
        })
        .collect()
}

fn semantic_summary_hashes(meta: Option<&Value>) -> Result<BTreeMap<String, String>> {
    let Some(syntax) = meta
        .and_then(|meta| meta.get("semantic_summary"))
        .and_then(|summary| summary.get("syntax"))
        .and_then(Value::as_object)
    else {
        return Ok(BTreeMap::new());
    };

    let mut out = BTreeMap::new();
    for (syntax_id, value) in syntax {
        let bytes = serde_json::to_vec(value)?;
        out.insert(format!("summary:{syntax_id}"), hash_bytes(&bytes));
    }
    Ok(out)
}

fn normalized_visible_difference_bucket(semantic_hashes_differ: &BTreeMap<String, bool>) -> String {
    let differing = semantic_hashes_differ
        .iter()
        .filter_map(|(name, differs)| differs.then_some(name.as_str()))
        .collect::<Vec<_>>();
    if differing.is_empty() {
        "visible_only".to_owned()
    } else {
        format!("visible_and_{}", differing.join("+"))
    }
}

fn kind(value: &Value) -> Option<&str> {
    value.get("kind").and_then(Value::as_str)
}

fn normalize_visible(value: &str) -> String {
    value.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[cfg(test)]
mod tests {
    use std::sync::atomic::Ordering;

    use super::*;

    #[test]
    fn summarization_does_not_retain_the_corpus_of_parsed_roots() {
        let dir = tempfile::tempdir().unwrap();
        for index in 0..200 {
            let aat = serde_json::json!({
                "work_id": format!("work-{index:03}"),
                "blocks": [{"kind": "paragraph", "content": [
                    {"kind": "text", "value": "本文"}
                ]}],
                "meta": {}
            });
            fs::write(
                dir.path().join(format!("{index:03}.json")),
                serde_json::to_vec(&aat).unwrap(),
            )
            .unwrap();
        }
        LIVE_ROOTS.store(0, Ordering::SeqCst);
        MAX_LIVE_ROOTS.store(0, Ordering::SeqCst);

        let summaries = read_aat_summaries(dir.path()).unwrap();

        assert_eq!(summaries.len(), 200);
        assert!(MAX_LIVE_ROOTS.load(Ordering::SeqCst) < 200);
        assert_eq!(LIVE_ROOTS.load(Ordering::SeqCst), 0);
    }
}
