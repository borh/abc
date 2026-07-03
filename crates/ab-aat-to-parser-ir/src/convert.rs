use std::collections::BTreeSet;

use anyhow::{Result, bail};
use serde_json::Value;

use crate::{mapping::MappingDocument, schema::SchemaSet};

#[derive(Debug, Clone)]
pub struct ConversionRequest {
    pub aat: Value,
    pub mapping: MappingDocument,
    pub schemas: SchemaSet,
    pub options: ConversionOptions,
}

#[derive(Debug, Clone)]
pub struct ConversionOptions {
    pub validate_input_aat: bool,
    pub validate_output_parser_ir: bool,
    pub on_unmeasured_divergence: UnmeasuredDivergencePolicy,
}

impl Default for ConversionOptions {
    fn default() -> Self {
        Self {
            validate_input_aat: true,
            validate_output_parser_ir: true,
            on_unmeasured_divergence: UnmeasuredDivergencePolicy::Refuse,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnmeasuredDivergencePolicy {
    Refuse,
    RecordExploratory,
}

#[derive(Debug, Clone)]
pub struct ConversionOutput {
    pub parser_ir: Value,
    pub divergence_bundle: Value,
    pub emitted_rule_ids: BTreeSet<String>,
}

pub fn convert(_request: ConversionRequest) -> Result<ConversionOutput> {
    bail!("converter traversal is not implemented yet")
}
