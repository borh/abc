use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use anyhow::Result;
use serde_json::{Value, json};

#[cfg(test)]
use std::sync::atomic::{AtomicUsize, Ordering};

use crate::{
    mapping::{MappingDocument, MappingIndex, MappingRule},
    schema::{SchemaValidators, validate_compiled},
};

#[cfg(test)]
static BUNDLE_INVOCATIONS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
pub(crate) fn reset_bundle_invocation_count() {
    BUNDLE_INVOCATIONS.store(0, Ordering::SeqCst);
}

#[cfg(test)]
pub(crate) fn bundle_invocation_count() -> usize {
    BUNDLE_INVOCATIONS.load(Ordering::SeqCst)
}

#[derive(Debug, Clone)]
pub struct AatMeta {
    pub work_id: String,
    pub version: u64,
    pub adapter: String,
    pub adapter_version: String,
    pub source_hash: String,
    pub parse_complete: bool,
    pub metrics: Value,
    pub semantic_summary: Value,
}

#[derive(Debug, Clone)]
struct AggregatedRecord {
    rule: MappingRule,
    count: u64,
    first_path: Option<String>,
    source_value: Option<Value>,
    target_value: Option<Value>,
}

#[derive(Debug, Clone)]
pub struct DivergenceRecorder {
    index: Arc<MappingIndex>,
    records: BTreeMap<String, AggregatedRecord>,
}

impl DivergenceRecorder {
    pub fn new(index: impl Into<Arc<MappingIndex>>) -> Self {
        Self {
            index: index.into(),
            records: BTreeMap::new(),
        }
    }

    pub fn has_rule(
        &self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
    ) -> bool {
        self.index
            .has_rule(category, aat_pointer, parser_ir_pointer)
    }

    pub fn record(
        &mut self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
        source_value: Option<Value>,
        target_value: Option<Value>,
    ) -> Result<()> {
        let rule = self
            .index
            .require_rule(category, aat_pointer, parser_ir_pointer)?;
        aggregate_record(
            &mut self.records,
            rule,
            aat_pointer,
            source_value,
            target_value,
        );
        Ok(())
    }

    /// Records the divergence when a matching rule is measured; returns
    /// whether a rule matched. Collapses the `has_rule` + `record` pattern
    /// into a single index lookup.
    pub fn record_if_measured(
        &mut self,
        category: &str,
        aat_pointer: Option<&str>,
        parser_ir_pointer: Option<&str>,
        source_value: Option<Value>,
        target_value: Option<Value>,
    ) -> bool {
        let Some(rule) = self
            .index
            .get_rule(category, aat_pointer, parser_ir_pointer)
        else {
            return false;
        };
        aggregate_record(
            &mut self.records,
            rule,
            aat_pointer,
            source_value,
            target_value,
        );
        true
    }

    pub fn emitted_rule_ids(&self) -> BTreeSet<String> {
        self.records.keys().cloned().collect()
    }

    pub fn bundle(
        self,
        meta: AatMeta,
        validators: &SchemaValidators,
        mapping: &MappingDocument,
    ) -> Result<Value> {
        #[cfg(test)]
        BUNDLE_INVOCATIONS.fetch_add(1, Ordering::SeqCst);

        let mut summary = BTreeMap::from([
            ("LOSS", 0_u64),
            ("INVENTION", 0_u64),
            ("AMBIGUITY", 0_u64),
            ("UNSUPPORTED", 0_u64),
            ("STRUCTURAL", 0_u64),
        ]);

        let records: Vec<Value> = self
            .records
            .into_values()
            .map(|record| {
                *summary
                    .get_mut(record.rule.category.as_str())
                    .expect("known category") += record.count;
                json!({
                    "rule_id": record.rule.rule_id,
                    "category": record.rule.category,
                    "aat_pointer": record.rule.aat_pointer,
                    "parser_ir_pointer": record.rule.parser_ir_pointer,
                    "source_value": record.source_value,
                    "target_value": record.target_value,
                    "message": rule_message(&record.rule.description),
                    "count": record.count,
                    "first_path": record.first_path,
                })
            })
            .collect();

        let bundle = json!({
            "schema_id": "https://abc.local/schemas/aat-parser-ir-divergence-bundle-v1.json",
            "schema_version": "0.1.0",
            "work_id": meta.work_id,
            "mapping": {
                "mapping_id": mapping.mapping_id,
                "mapping_version": mapping.mapping_version,
                "mapping_schema_hash": mapping.mapping_schema_hash,
            },
            "target": {
                "parser_ir_schema_id": mapping.target_parser_ir_schema_id,
                "parser_ir_schema_hash": mapping.target_parser_ir_schema_hash,
            },
            "aat": {
                "version": meta.version,
                "adapter": meta.adapter,
                "adapter_version": meta.adapter_version,
                "source_hash": meta.source_hash,
                "parse_complete": meta.parse_complete,
            },
            "preserved_aat_meta": {
                "metrics": meta.metrics,
                "semantic_summary": meta.semantic_summary,
            },
            "summary": summary,
            "records": records,
        });

        validate_compiled(
            &validators.bundle,
            &bundle,
            "AAT parser-IR divergence bundle",
        )?;
        for record in bundle["records"].as_array().into_iter().flatten() {
            validate_compiled(
                &validators.abc_divergence_record,
                record,
                "ABC divergence record",
            )?;
        }
        Ok(bundle)
    }
}

fn aggregate_record(
    records: &mut BTreeMap<String, AggregatedRecord>,
    rule: &MappingRule,
    aat_pointer: Option<&str>,
    source_value: Option<Value>,
    target_value: Option<Value>,
) {
    if let Some(entry) = records.get_mut(rule.rule_id.as_str()) {
        entry.count += 1;
    } else {
        records.insert(
            rule.rule_id.clone(),
            AggregatedRecord {
                rule: rule.clone(),
                count: 1,
                first_path: aat_pointer.map(ToOwned::to_owned),
                source_value,
                target_value,
            },
        );
    }
}

fn rule_message(description: &str) -> String {
    description
        .split_once(". ")
        .map(|(_, rest)| rest.to_owned())
        .unwrap_or_else(|| description.to_owned())
}
