use crate::model::{AtBlock, SourceDerivedContext};
use serde_json::{json, Value};

pub fn apply_source_derived_recovery(
    blocks: &mut Vec<AtBlock>,
    source_text: &str,
    ctx: &mut SourceDerivedContext,
) {
    // Phase A placeholder: mirror parser structure without mutation.
    let _ = source_text;
    // The full implementation will reconstitute source-derived gaiji, ruby,
    // indentation, and caption notes while preserving parser order.
    let mut synthetic_summary: Vec<Value> = Vec::new();
    if blocks.is_empty() {
        synthetic_summary.push(json!({
            "kind":"note",
            "value":"no-blocks",
            "provenance":"parser",
        }));
        ctx.summary.push_syntax("projection.warning", json!({"kind":"projection.warning","value":"no_blocks"}));
    }
    for warning in synthetic_summary {
        ctx.warnings.push(warning);
    }
}

pub fn attach_following_captions(blocks: Vec<AtBlock>, _context: &mut SourceDerivedContext) -> Vec<AtBlock> {
    let mut out = Vec::new();
    let mut index = 0usize;

    while index < blocks.len() {
        out.push(blocks[index].clone());
        index += 1;
    }

    out
}
