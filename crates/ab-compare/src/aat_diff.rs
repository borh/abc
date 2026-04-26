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
    pub structural_differences: Vec<AatStructuralDifference>,
}

#[derive(Debug, Serialize)]
pub struct AatStructuralDifference {
    pub work_id: String,
    pub visible_text_differs: bool,
    pub a_structure_hash: String,
    pub b_structure_hash: String,
    pub a_visible_hash: String,
    pub b_visible_hash: String,
    pub a_block_kinds: BTreeMap<String, usize>,
    pub b_block_kinds: BTreeMap<String, usize>,
    pub a_inline_kinds: BTreeMap<String, usize>,
    pub b_inline_kinds: BTreeMap<String, usize>,
}

#[derive(Debug, Deserialize)]
struct AatRoot {
    work_id: String,
    blocks: Value,
}

#[derive(Debug)]
struct AatSummary {
    work_id: String,
    structure_hash: String,
    visible_hash: String,
    block_kinds: BTreeMap<String, usize>,
    inline_kinds: BTreeMap<String, usize>,
}

pub fn compare_aat_dirs(a: &Path, b: &Path) -> Result<AatCompareSummary> {
    let a = read_aat_summaries(a)?;
    let b = read_aat_summaries(b)?;
    let keys_a = a.keys().cloned().collect::<BTreeSet<_>>();
    let keys_b = b.keys().cloned().collect::<BTreeSet<_>>();
    let common = keys_a.intersection(&keys_b).cloned().collect::<Vec<_>>();
    let mut structural_differences = Vec::new();

    for key in &common {
        let left = &a[key];
        let right = &b[key];
        if left.structure_hash != right.structure_hash || left.visible_hash != right.visible_hash {
            structural_differences.push(AatStructuralDifference {
                work_id: left.work_id.clone(),
                visible_text_differs: left.visible_hash != right.visible_hash,
                a_structure_hash: left.structure_hash.clone(),
                b_structure_hash: right.structure_hash.clone(),
                a_visible_hash: left.visible_hash.clone(),
                b_visible_hash: right.visible_hash.clone(),
                a_block_kinds: left.block_kinds.clone(),
                b_block_kinds: right.block_kinds.clone(),
                a_inline_kinds: left.inline_kinds.clone(),
                b_inline_kinds: right.inline_kinds.clone(),
            });
        }
    }

    Ok(AatCompareSummary {
        common_aat: common.len(),
        only_a: keys_a.difference(&keys_b).count(),
        only_b: keys_b.difference(&keys_a).count(),
        structural_differences,
    })
}

fn read_aat_summaries(root: &Path) -> Result<BTreeMap<String, AatSummary>> {
    let mut out = BTreeMap::new();
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
        out.insert(root.work_id.clone(), summarize(root)?);
    }
    Ok(out)
}

fn summarize(root: AatRoot) -> Result<AatSummary> {
    let structure_hash = hash_json(&root.blocks)?;
    let mut block_kinds = BTreeMap::new();
    let mut inline_kinds = BTreeMap::new();
    let mut visible = String::new();
    collect_blocks(
        &root.blocks,
        &mut block_kinds,
        &mut inline_kinds,
        &mut visible,
    );
    Ok(AatSummary {
        work_id: root.work_id,
        structure_hash,
        visible_hash: hash_bytes(visible.as_bytes()),
        block_kinds,
        inline_kinds,
    })
}

fn collect_blocks(
    blocks: &Value,
    block_kinds: &mut BTreeMap<String, usize>,
    inline_kinds: &mut BTreeMap<String, usize>,
    visible: &mut String,
) {
    let Some(blocks) = blocks.as_array() else {
        return;
    };

    for block in blocks {
        if let Some(kind) = kind(block) {
            *block_kinds.entry(kind.to_owned()).or_insert(0) += 1;
        }
        collect_inline_containers(block, inline_kinds, visible);
    }
}

fn collect_inline_containers(
    value: &Value,
    inline_kinds: &mut BTreeMap<String, usize>,
    visible: &mut String,
) {
    for field in ["content", "children", "upper", "lower"] {
        let Some(nodes) = value.get(field).and_then(Value::as_array) else {
            continue;
        };
        for node in nodes {
            collect_inline_node(node, inline_kinds, visible);
        }
    }
}

fn collect_inline_node(
    node: &Value,
    inline_kinds: &mut BTreeMap<String, usize>,
    visible: &mut String,
) {
    let Some(kind) = kind(node) else {
        return;
    };
    *inline_kinds.entry(kind.to_owned()).or_insert(0) += 1;

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
        }
        "gaiji" => {
            let resolved = node.get("resolved").and_then(Value::as_str).unwrap_or("");
            if resolved.is_empty() {
                if let Some(description) = node.get("description").and_then(Value::as_str) {
                    visible.push_str(description);
                }
            } else {
                visible.push_str(resolved);
            }
        }
        _ => {}
    }
    collect_inline_containers(node, inline_kinds, visible);
}

fn kind(value: &Value) -> Option<&str> {
    value.get("kind").and_then(Value::as_str)
}

fn hash_json(value: &Value) -> Result<String> {
    let bytes = serde_json::to_vec(value)?;
    Ok(hash_bytes(&bytes))
}

fn hash_bytes(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("sha256:{:x}", hasher.finalize())
}
