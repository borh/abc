use std::{fs, path::Path};

use anyhow::{Context, Result};
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
    pub fn load(repo_root: &Path, abc_root: &Path) -> Result<Self> {
        Ok(Self {
            aat_schema: read_json(&repo_root.join("data/aat-schema.json"))?,
            mapping_schema: read_json(&abc_root.join("schemas/aat-parser-ir-mapping.schema.json"))?,
            parser_ir_schema: read_json(&abc_root.join("schemas/parser-ir.schema.json"))?,
            abc_divergence_record_schema: read_json(
                &abc_root.join("schemas/aat-parser-ir-divergence.schema.json"),
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
    pub parser_ir: jsonschema::Validator,
    pub bundle: jsonschema::Validator,
    pub abc_divergence_record: jsonschema::Validator,
}

impl SchemaValidators {
    pub fn compile(schemas: &SchemaSet) -> Result<Self> {
        Ok(Self {
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
    validator.validate(value).map_err(|error| {
        anyhow::anyhow!(
            "{label} validation failed at {}: {error}",
            error.instance_path()
        )
    })
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
    let mut bytes = Vec::new();
    let formatter = serde_json::ser::CompactFormatter;
    let mut serializer = serde_json::Serializer::with_formatter(&mut bytes, formatter);
    value.serialize(&mut serializer)?;
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
