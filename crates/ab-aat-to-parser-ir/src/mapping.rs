use std::{collections::BTreeMap, fs, path::Path, sync::OnceLock};

use anyhow::{Context, Result, bail};
use regex::Regex;
use serde::{Deserialize, Serialize};
use serde_json::Value;

use crate::schema::{SchemaSet, schema_hash, validate_value};

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MappingDocument {
    pub mapping_id: String,
    pub mapping_version: String,
    pub mapping_schema_hash: String,
    pub source_aat_version: u64,
    pub target_parser_ir_schema_id: String,
    pub target_parser_ir_schema_hash: String,
    pub transform_rule_descriptions: Vec<MappingRule>,
    pub loss_taxonomy: BTreeMap<String, LossTaxonomyEntry>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct MappingRule {
    pub rule_id: String,
    pub category: String,
    pub aat_pointer: Option<String>,
    pub parser_ir_pointer: Option<String>,
    pub action: String,
    pub description: String,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
pub struct LossTaxonomyEntry {
    pub description: String,
    pub default_action: String,
    pub records_sidecar: bool,
}

#[derive(Debug, Clone)]
pub struct MappingIndex {
    rules: BTreeMap<(String, Option<String>, Option<String>), MappingRule>,
}

impl MappingDocument {
    pub fn from_path(path: &Path) -> Result<Self> {
        let text = fs::read_to_string(path)
            .with_context(|| format!("failed to read {}", path.display()))?;
        serde_json::from_str(&text).with_context(|| format!("failed to parse {}", path.display()))
    }

    pub fn preflight(&self, schemas: &SchemaSet) -> Result<MappingIndex> {
        let value = serde_json::to_value(self)?;
        validate_value(&schemas.mapping_schema, &value, "AAT parser-IR mapping")?;

        let mapping_schema_hash = schema_hash(&schemas.mapping_schema)?;
        if self.mapping_schema_hash != mapping_schema_hash {
            bail!(
                "mapping schema hash mismatch: document={} computed={mapping_schema_hash}",
                self.mapping_schema_hash
            );
        }

        let parser_ir_schema_hash = schema_hash(&schemas.parser_ir_schema)?;
        if self.target_parser_ir_schema_hash != parser_ir_schema_hash {
            bail!(
                "target parser-IR schema hash mismatch: document={} computed={parser_ir_schema_hash}",
                self.target_parser_ir_schema_hash
            );
        }

        if self.source_aat_version != 1 {
            bail!("unsupported source_aat_version {}", self.source_aat_version);
        }

        for category in [
            "LOSS",
            "INVENTION",
            "AMBIGUITY",
            "UNSUPPORTED",
            "STRUCTURAL",
        ] {
            if !self.loss_taxonomy.contains_key(category) {
                bail!("loss taxonomy missing {category}");
            }
        }

        let mut rules = BTreeMap::new();
        for rule in &self.transform_rule_descriptions {
            if !self.loss_taxonomy.contains_key(&rule.category) {
                bail!(
                    "{} uses category without taxonomy entry: {}",
                    rule.rule_id,
                    rule.category
                );
            }
            let folded_aat_pointer = rule.aat_pointer.as_deref().map(fold_aat_pointer);
            if let Some(pointer) = &folded_aat_pointer {
                if !aat_pointer_exists(&schemas.aat_schema, pointer) {
                    bail!("{} has non-schema AAT pointer {pointer}", rule.rule_id);
                }
            }
            let key = (
                rule.category.clone(),
                folded_aat_pointer,
                rule.parser_ir_pointer.clone(),
            );
            if rules.insert(key, rule.clone()).is_some() {
                bail!("duplicate folded mapping rule {}", rule.rule_id);
            }
        }
        Ok(MappingIndex { rules })
    }
}

impl MappingIndex {
    pub fn has_rule(
        &self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
    ) -> bool {
        let key = (
            category.to_owned(),
            aat_pointer.map(fold_aat_pointer),
            parser_ir_pointer.map(ToOwned::to_owned),
        );
        self.rules.contains_key(&key)
    }

    pub fn require_rule(
        &self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
    ) -> Result<&MappingRule> {
        let key = (
            category.to_owned(),
            aat_pointer.map(fold_aat_pointer),
            parser_ir_pointer.map(ToOwned::to_owned),
        );
        self.rules.get(&key).ok_or_else(|| {
            anyhow::anyhow!(
                "unmeasured divergence: category={category} aat_pointer={} parser_ir_pointer={}",
                aat_pointer
                    .map(fold_aat_pointer)
                    .unwrap_or_else(|| "null".to_owned()),
                parser_ir_pointer.unwrap_or("null")
            )
        })
    }
}

pub fn fold_aat_pointer(pointer: &str) -> String {
    static INDEX_RE: OnceLock<Regex> = OnceLock::new();
    let index_re = INDEX_RE.get_or_init(|| Regex::new(r"\[[0-9]+\]").expect("valid index regex"));
    let folded = index_re
        .replace_all(pointer.trim_start_matches("$."), "[]")
        .to_string();
    folded
        .split_once('=')
        .map(|(path, _)| path.to_owned())
        .unwrap_or(folded)
}

fn aat_pointer_exists(schema: &Value, pointer: &str) -> bool {
    if pointer.starts_with('(') {
        return true;
    }
    let segments: Vec<&str> = pointer.split('.').collect();
    path_exists(schema, schema, &segments, 0, 0)
}

fn path_exists(
    schema: &Value,
    node: &Value,
    segments: &[&str],
    index: usize,
    depth: usize,
) -> bool {
    if index == segments.len() {
        return true;
    }
    if depth > 64 {
        return false;
    }
    let node = deref(schema, node);
    if let Some(branches) = node.get("oneOf").and_then(Value::as_array) {
        let segment = segments[index].trim_end_matches("[]");
        for branch in branches {
            let resolved = deref(schema, branch);
            for kind in kind_values(resolved) {
                if segment == kind && path_exists(schema, resolved, segments, index + 1, depth + 1)
                {
                    return true;
                }
            }
            if path_exists(schema, resolved, segments, index, depth + 1) {
                return true;
            }
        }
        return false;
    }
    for keyword in ["allOf", "anyOf"] {
        if let Some(branches) = node.get(keyword).and_then(Value::as_array) {
            for branch in branches {
                if path_exists(schema, branch, segments, index, depth + 1) {
                    return true;
                }
            }
            return false;
        }
    }
    if node.get("type").and_then(Value::as_str) == Some("array") {
        return node
            .get("items")
            .is_some_and(|items| path_exists(schema, items, segments, index, depth + 1));
    }
    if node.get("type").and_then(Value::as_str) == Some("object")
        || node.get("properties").is_some()
    {
        let segment = segments[index];
        let is_array = segment.ends_with("[]");
        let name = segment.trim_end_matches("[]");
        if let Some(child) = node.pointer(&format!("/properties/{name}")) {
            let child = deref(schema, child);
            if is_array {
                return child.get("items").is_some_and(|items| {
                    path_exists(schema, items, segments, index + 1, depth + 1)
                });
            }
            return path_exists(schema, child, segments, index + 1, depth + 1);
        }
    }
    false
}

fn deref<'a>(schema: &'a Value, node: &'a Value) -> &'a Value {
    let Some(reference) = node.get("$ref").and_then(Value::as_str) else {
        return node;
    };
    let prefix = "#/$defs/";
    if let Some(name) = reference.strip_prefix(prefix) {
        &schema["$defs"][name]
    } else {
        node
    }
}

fn kind_values(node: &Value) -> Vec<String> {
    let Some(kind) = node.pointer("/properties/kind") else {
        return Vec::new();
    };
    if let Some(value) = kind.get("const").and_then(Value::as_str) {
        return vec![value.to_owned()];
    }
    kind.get("enum")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
        .map(ToOwned::to_owned)
        .collect()
}
