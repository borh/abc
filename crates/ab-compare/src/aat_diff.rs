use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::Path,
};

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};
use walkdir::WalkDir;

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
}

#[derive(Debug, Serialize)]
pub struct VisibleTextDifference {
    pub char_index: usize,
    pub a_snippet: String,
    pub b_snippet: String,
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
    block_kinds: BTreeMap<String, usize>,
    inline_kinds: BTreeMap<String, usize>,
    semantic_totals: BTreeMap<String, usize>,
    semantic_counts: BTreeMap<String, usize>,
    semantic_hashes: BTreeMap<String, String>,
    semantic_summary_hashes: BTreeMap<String, String>,
}

pub fn compare_aat_dirs(a: &Path, b: &Path) -> Result<AatCompareSummary> {
    compare_aat_dirs_with_limit(a, b, None)
}

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
                increment(&mut semantic_hash_difference_counts, name.clone());
            }
        }
        for (name, differs) in &semantic_summary_hashes_differ {
            if *differs {
                increment(&mut semantic_summary_hash_difference_counts, name.clone());
            }
        }
        if left.structure_hash != right.structure_hash
            || left.visible_hash != right.visible_hash
            || semantic_summary_hashes_differ
                .values()
                .any(|differs| *differs)
        {
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
                    increment(&mut normalized_visible_difference_buckets, bucket.clone());
                    Some(bucket)
                } else {
                    None
                };
            let normalized_visible_first_difference =
                normalized_visible_difference_bucket.as_ref().and_then(|_| {
                    first_visible_difference(&left.normalized_visible, &right.normalized_visible)
                });
            if difference_limit.is_some_and(|limit| structural_differences.len() >= limit) {
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
                a_block_kinds: left.block_kinds.clone(),
                b_block_kinds: right.block_kinds.clone(),
                a_inline_kinds: left.inline_kinds.clone(),
                b_inline_kinds: right.inline_kinds.clone(),
                a_semantic_counts: left.semantic_counts.clone(),
                b_semantic_counts: right.semantic_counts.clone(),
                a_semantic_hashes: left.semantic_hashes.clone(),
                b_semantic_hashes: right.semantic_hashes.clone(),
                semantic_hashes_differ,
                a_semantic_summary_hashes: left.semantic_summary_hashes.clone(),
                b_semantic_summary_hashes: right.semantic_summary_hashes.clone(),
                semantic_summary_hashes_differ,
                normalized_visible_difference_bucket,
                normalized_visible_first_difference,
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
    })
}

fn read_aat_summaries(root: &Path) -> Result<BTreeMap<String, AatSummary>> {
    let mut loaded = Vec::new();
    for entry in WalkDir::new(root) {
        let entry = entry?;
        if !entry.file_type().is_file()
            || entry
                .path()
                .extension()
                .is_none_or(|extension| extension != "json")
        {
            continue;
        }
        let bytes = fs::read(entry.path())
            .with_context(|| format!("failed to read {}", entry.path().display()))?;
        let root: AatRoot = serde_json::from_slice(&bytes)
            .with_context(|| format!("failed to parse {}", entry.path().display()))?;
        loaded.push((entry.path().to_owned(), root));
    }

    let mut work_id_counts = BTreeMap::new();
    for (_, root) in &loaded {
        *work_id_counts.entry(root.work_id.clone()).or_insert(0usize) += 1;
    }

    let mut out = BTreeMap::new();
    for (path, root) in loaded {
        let key = aat_key(&work_id_counts, &path, &root);
        out.insert(key, summarize(root)?);
    }
    Ok(out)
}

fn aat_key(work_id_counts: &BTreeMap<String, usize>, path: &Path, root: &AatRoot) -> String {
    if work_id_counts
        .get(&root.work_id)
        .copied()
        .unwrap_or_default()
        <= 1
    {
        return root.work_id.clone();
    }
    let filename = path
        .file_name()
        .and_then(|name| name.to_str())
        .unwrap_or("duplicate");
    format!("{}::{filename}", root.work_id)
}

fn summarize(root: AatRoot) -> Result<AatSummary> {
    let structure_hash = hash_json(&root.blocks)?;
    let mut block_kinds = BTreeMap::new();
    let mut inline_kinds = BTreeMap::new();
    let mut semantic_totals = BTreeMap::new();
    let mut semantic_counts = BTreeMap::new();
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
    })
}

fn collect_blocks(
    blocks: &Value,
    block_kinds: &mut BTreeMap<String, usize>,
    inline_kinds: &mut BTreeMap<String, usize>,
    semantic_totals: &mut BTreeMap<String, usize>,
    semantic_counts: &mut BTreeMap<String, usize>,
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
    inline_kinds: &mut BTreeMap<String, usize>,
    semantic_totals: &mut BTreeMap<String, usize>,
    semantic_counts: &mut BTreeMap<String, usize>,
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
    inline_kinds: &mut BTreeMap<String, usize>,
    semantic_totals: &mut BTreeMap<String, usize>,
    semantic_counts: &mut BTreeMap<String, usize>,
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
    let mut out = BTreeMap::new();
    for summary in summaries {
        for (key, value) in &summary.semantic_totals {
            *out.entry(key.clone()).or_insert(0) += value;
        }
    }
    out
}

fn count_both(
    totals: &mut BTreeMap<String, usize>,
    details: &mut BTreeMap<String, usize>,
    key: String,
) {
    increment(totals, key.clone());
    increment(details, key);
}

fn increment(counts: &mut BTreeMap<String, usize>, key: String) {
    *counts.entry(key).or_insert(0) += 1;
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

fn first_visible_difference(left: &str, right: &str) -> Option<VisibleTextDifference> {
    let left_chars = left.chars().collect::<Vec<_>>();
    let right_chars = right.chars().collect::<Vec<_>>();
    let max_common = left_chars.len().min(right_chars.len());
    let char_index = (0..max_common)
        .find(|idx| left_chars[*idx] != right_chars[*idx])
        .or_else(|| (left_chars.len() != right_chars.len()).then_some(max_common))?;
    Some(VisibleTextDifference {
        char_index,
        a_snippet: snippet(&left_chars, char_index),
        b_snippet: snippet(&right_chars, char_index),
    })
}

fn snippet(chars: &[char], center: usize) -> String {
    let start = center.saturating_sub(24);
    let end = chars.len().min(center + 24);
    chars[start..end].iter().collect()
}

fn kind(value: &Value) -> Option<&str> {
    value.get("kind").and_then(Value::as_str)
}

fn hash_json(value: &Value) -> Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

fn hash_string_sequence(values: &[String]) -> String {
    let mut hasher = Sha256::new();
    for value in values {
        hasher.update(value.as_bytes());
        hasher.update([0]);
    }
    format!("sha256:{:x}", hasher.finalize())
}

fn normalize_visible(value: &str) -> String {
    value.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}
