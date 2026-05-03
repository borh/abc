use ab_ir::aat_view::AatDocument;
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CaseEvaluation {
    pub case_id: String,
    pub schema_status: String,
    pub upstream_status: String,
    pub oracle_status: String,
    pub failures: Vec<String>,
}

pub fn schema_status(aat: &Value) -> String {
    if ab_check::check::validate_aat_value(aat).is_ok() {
        "pass".to_owned()
    } else {
        "fail".to_owned()
    }
}

pub fn visible_text(document: &AatDocument) -> String {
    document.visible_text()
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn schema_status_uses_ab_check() {
        assert_eq!(schema_status(&json!({"version": 1})), "fail");
    }

    #[test]
    fn visible_text_uses_ab_ir_aat_view() {
        let aat = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{"kind": "paragraph", "content": [{"kind": "text", "value": "本文"}]}],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        });
        let doc = AatDocument::from_value(aat);

        assert_eq!(visible_text(&doc), "本文");
    }
}
