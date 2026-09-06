use std::{fs, path::Path};

use anyhow::{Context, Result, bail};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sha2::{Digest, Sha256};

#[derive(Debug, Clone)]
pub struct SchemaSet {
    pub aat_schema: Value,
    pub mapping_schema: Value,
    pub parser_ir_schema: Value,
    pub abc_divergence_record_schema: Value,
    pub bundle_schema: Value,
}

impl SchemaSet {
    /// Use the compiled schema set unless research explicitly supplies another root.
    pub fn for_aat_version(
        repo_root: &Path,
        research_root: Option<&Path>,
        aat_version: u64,
    ) -> Result<Self> {
        if let Some(root) = research_root {
            return Self::load_for_aat_version(repo_root, root, aat_version);
        }
        let aat = match aat_version {
            1 => include_bytes!("../../../data/aat-schema-v1.json").as_slice(),
            2 => include_bytes!("../../../data/aat-schema.json").as_slice(),
            other => bail!("unsupported AAT schema version {other} (known: 1, 2)"),
        };
        Ok(Self {
            aat_schema: parse_json_value(aat)?,
            mapping_schema: parse_json_value(include_bytes!(
                "../../../research/schemas/aat-parser-ir-mapping.schema.json"
            ))?,
            parser_ir_schema: parse_json_value(include_bytes!(
                "../../../research/schemas/parser-ir.schema.json"
            ))?,
            abc_divergence_record_schema: parse_json_value(include_bytes!(
                "../../../research/schemas/aat-parser-ir-divergence.schema.json"
            ))?,
            bundle_schema: parse_json_value(include_bytes!(
                "../../../data/aat-parser-ir-divergence-bundle-v1.schema.json"
            ))?,
        })
    }

    /// Load the (AAT schema, research schema) tuple for a given AAT schema
    /// `version`. The AAT schema file is selected by version — 1 loads the
    /// frozen `data/aat-schema-v1.json`, 2 loads the current
    /// `data/aat-schema.json` — while the Research-owned schemas (mapping,
    /// parser-IR, divergence record, bundle) are shared across versions.
    pub fn load_for_aat_version(
        repo_root: &Path,
        research_root: &Path,
        aat_version: u64,
    ) -> Result<Self> {
        let aat_schema_path = match aat_version {
            1 => repo_root.join("data/aat-schema-v1.json"),
            2 => repo_root.join("data/aat-schema.json"),
            other => bail!("unsupported AAT schema version {other} (known: 1, 2)"),
        };
        Ok(Self {
            aat_schema: read_json(&aat_schema_path)?,
            mapping_schema: read_json(
                &research_root.join("schemas/aat-parser-ir-mapping.schema.json"),
            )?,
            parser_ir_schema: read_json(&research_root.join("schemas/parser-ir.schema.json"))?,
            abc_divergence_record_schema: read_json(
                &research_root.join("schemas/aat-parser-ir-divergence.schema.json"),
            )?,
            bundle_schema: read_json(
                &repo_root.join("data/aat-parser-ir-divergence-bundle-v1.schema.json"),
            )?,
        })
    }
}

pub fn read_json(path: &Path) -> Result<Value> {
    let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
    parse_json_value(&bytes).with_context(|| format!("failed to parse {}", path.display()))
}

pub fn parse_json_value(bytes: &[u8]) -> Result<Value> {
    let mut deserializer = serde_json::Deserializer::from_slice(bytes);
    deserializer.disable_recursion_limit();
    let deserializer = serde_stacker::Deserializer::new(&mut deserializer);
    Value::deserialize(deserializer).map_err(Into::into)
}

/// Validators compiled once from a [`SchemaSet`] so per-document (and
/// per-record) validation skips schema recompilation.
#[derive(Debug)]
pub struct SchemaValidators {
    pub aat: jsonschema::Validator,
    pub parser_ir: jsonschema::Validator,
    pub bundle: jsonschema::Validator,
    pub abc_divergence_record: jsonschema::Validator,
}

impl SchemaValidators {
    pub fn compile(schemas: &SchemaSet) -> Result<Self> {
        Ok(Self {
            aat: compile_validator(&schemas.aat_schema, "AAT")?,
            parser_ir: compile_validator(&schemas.parser_ir_schema, "parser-IR")?,
            bundle: compile_validator(&schemas.bundle_schema, "AAT parser-IR divergence bundle")?,
            abc_divergence_record: compile_validator(
                &schemas.abc_divergence_record_schema,
                "ABC divergence record",
            )?,
        })
    }
}

pub fn compile_validator(schema: &Value, label: &str) -> Result<jsonschema::Validator> {
    jsonschema::validator_for(schema).with_context(|| format!("failed to compile {label} schema"))
}

pub fn validate_compiled(
    validator: &jsonschema::Validator,
    value: &Value,
    label: &str,
) -> Result<()> {
    match validation_errors(validator, value, label)
        .into_iter()
        .next()
    {
        Some(error) => Err(anyhow::anyhow!(error)),
        None => Ok(()),
    }
}

pub fn validation_errors(
    validator: &jsonschema::Validator,
    value: &Value,
    label: &str,
) -> Vec<String> {
    let mut errors: Vec<String> = validator
        .iter_errors(value)
        .map(|error| {
            format!(
                "{label} validation failed at {}: {error}",
                error.instance_path()
            )
        })
        .collect();
    errors.sort();
    errors.dedup();
    errors
}

pub fn validate_value(schema: &Value, value: &Value, label: &str) -> Result<()> {
    let validator = compile_validator(schema, label)?;
    validate_compiled(&validator, value, label)
}

pub fn abc_legacy_json_c14n_v0(value: &Value) -> Result<Vec<u8>> {
    let text = sorted_json_text(value)?;
    Ok(text.replace('/', "\\/").into_bytes())
}

fn sorted_json_text(value: &Value) -> Result<String> {
    // Explicit key sort, independent of whichever `serde_json::Map` backend
    // (`BTreeMap` default, `IndexMap` under `preserve_order`) the compiling
    // workspace's unified feature graph happens to select — see
    // `crate::canonical_json` for why this can't be left to the map type.
    let sorted = crate::canonical_json::sort_keys_deep(value.clone());
    let mut bytes = Vec::new();
    let formatter = serde_json::ser::CompactFormatter;
    let mut serializer = serde_json::Serializer::with_formatter(&mut bytes, formatter);
    sorted.serialize(&mut serializer)?;
    Ok(String::from_utf8(bytes)?)
}

pub fn abc_legacy_json_hash(value: &Value) -> Result<String> {
    let payload = abc_legacy_json_c14n_v0(value)?;
    let mut hasher = Sha256::new();
    hasher.update(payload);
    Ok(format!("sha256:{:x}", hasher.finalize()))
}

pub fn schema_hash(value: &Value) -> Result<String> {
    abc_legacy_json_hash(value)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn embedded_schemas_match_owned_files_and_explicit_roots_fail_closed() {
        let root = Path::new(env!("CARGO_MANIFEST_DIR")).join("../..");
        for version in [1, 2] {
            let embedded =
                SchemaSet::for_aat_version(Path::new("/nonexistent"), None, version).unwrap();
            let files =
                SchemaSet::for_aat_version(&root, Some(&root.join("research")), version).unwrap();
            assert_eq!(embedded.aat_schema, files.aat_schema);
            assert_eq!(embedded.mapping_schema, files.mapping_schema);
            assert_eq!(embedded.parser_ir_schema, files.parser_ir_schema);
            assert_eq!(
                embedded.abc_divergence_record_schema,
                files.abc_divergence_record_schema
            );
            assert_eq!(embedded.bundle_schema, files.bundle_schema);
        }
        assert!(SchemaSet::for_aat_version(&root, Some(Path::new("/nonexistent")), 2).is_err());
        assert!(SchemaSet::for_aat_version(&root, None, 3).is_err());
    }
}
