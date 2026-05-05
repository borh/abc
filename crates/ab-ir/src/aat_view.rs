use serde_json::Value;

#[derive(Debug, Clone)]
pub struct AatDocument {
    root: Value,
}

impl AatDocument {
    #[must_use]
    pub fn from_value(root: Value) -> Self {
        Self { root }
    }

    #[must_use]
    pub fn root(&self) -> &Value {
        &self.root
    }

    #[must_use]
    pub fn visible_text(&self) -> String {
        visible_text_from_value(&self.root)
    }

    /// Selects nodes from the AAT document by a dot-separated selector.
    ///
    /// # Errors
    ///
    /// Returns an error when:
    /// - the selector is empty;
    /// - `**` is used with additional selector segments;
    /// - `*` is applied to a non-array value; or
    /// - the selector resolves to scalar terminal values.
    pub fn select(&self, selector: &str) -> Result<Vec<&Value>, SelectorError> {
        select(&self.root, selector)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectorError {
    pub selector: String,
    pub message: String,
}

#[must_use]
pub fn visible_text_from_value(value: &Value) -> String {
    ab_plaintext::visible_text_projection(value)
}

/// Selects nodes from the AAT JSON root by a dot-separated selector.
///
/// # Errors
///
/// Returns an error when:
/// - the selector is empty;
/// - `**` is used with additional selector segments;
/// - `*` is applied to a non-array value; or
/// - the selector resolves to scalar terminal values.
pub fn select<'a>(root: &'a Value, selector: &str) -> Result<Vec<&'a Value>, SelectorError> {
    if selector.is_empty() {
        return Err(SelectorError {
            selector: selector.to_owned(),
            message: "selector must not be empty".to_owned(),
        });
    }

    if selector == "**" {
        let mut out = Vec::new();
        collect_recursive_objects(root, &mut out);
        return Ok(out);
    }

    if selector.split('.').any(|segment| segment == "**") {
        return Err(SelectorError {
            selector: selector.to_owned(),
            message: "'**' is only supported as the entire selector".to_owned(),
        });
    }

    let mut current = vec![root];
    for segment in selector.split('.') {
        let mut next = Vec::new();
        for value in current {
            match segment {
                "*" => {
                    let Some(array) = value.as_array() else {
                        return Err(SelectorError {
                            selector: selector.to_owned(),
                            message: "'*' segment applied to non-array".to_owned(),
                        });
                    };
                    next.extend(array);
                }
                key => {
                    if let Some(child) = value.get(key) {
                        next.push(child);
                    }
                }
            }
        }
        current = next;
    }

    if current.iter().any(|value| {
        value.is_boolean() || value.is_number() || value.is_string() || value.is_null()
    }) {
        return Err(SelectorError {
            selector: selector.to_owned(),
            message:
                "selector returned scalar value; terminal scalar-property selectors are unsupported"
                    .to_owned(),
        });
    }

    Ok(current)
}

fn collect_recursive_objects<'a>(value: &'a Value, out: &mut Vec<&'a Value>) {
    if value.is_object() {
        out.push(value);
    }

    match value {
        Value::Array(values) => {
            for value in values {
                collect_recursive_objects(value, out);
            }
        }
        Value::Object(object) => {
            for value in object.values() {
                collect_recursive_objects(value, out);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn fixture_document() -> AatDocument {
        AatDocument::from_value(json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [
                {
                    "kind": "paragraph",
                    "content": [
                        {"kind": "text", "value": "foo"},
                        {
                            "kind": "ruby",
                            "base": "bar",
                            "reading": "baz",
                            "base_content": [{"kind": "text", "value": "bar"}]
                        }
                    ]
                },
                {
                    "kind": "caption_block",
                    "children": [
                        {
                            "kind": "paragraph",
                            "content": [{"kind": "text", "value": "nested"}]
                        }
                    ]
                }
            ],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        }))
    }

    #[test]
    fn selector_counts_direct_content_children_without_recursing_into_ruby_base_content() {
        let doc = fixture_document();

        let nodes = doc.select("blocks.*.content.*").unwrap();

        assert_eq!(nodes.len(), 2);
        assert_eq!(
            nodes
                .iter()
                .filter(|node| node.get("kind").and_then(Value::as_str) == Some("text"))
                .count(),
            1
        );
        assert_eq!(
            doc.select("**")
                .unwrap()
                .iter()
                .filter(|node| node.get("kind").and_then(Value::as_str) == Some("text"))
                .count(),
            3
        );
    }

    #[test]
    fn selector_allows_explicit_deeper_paths() {
        let doc = fixture_document();

        let nodes = doc.select("blocks.*.children.*.content.*").unwrap();

        assert_eq!(nodes.len(), 1);
        assert_eq!(nodes[0]["value"], "nested");
    }

    #[test]
    fn selector_skips_missing_fields_but_rejects_star_on_non_array() {
        let doc = fixture_document();

        let content_arrays = doc.select("blocks.*.content").unwrap();
        assert_eq!(content_arrays.len(), 1);

        let error = doc.select("blocks.*.content.*.*").unwrap_err();
        assert_eq!(error.selector, "blocks.*.content.*.*");
        assert!(error.message.contains("non-array"));
    }

    #[test]
    fn selector_rejects_terminal_scalar_property() {
        let doc = fixture_document();

        let error = doc.select("blocks.*.kind").unwrap_err();

        assert_eq!(error.selector, "blocks.*.kind");
        assert!(error.message.contains("scalar"));
    }

    #[test]
    fn visible_text_delegates_to_plaintext_projection() {
        let value = json!({
            "version": 1,
            "work_id": "fixture",
            "blocks": [{
                "kind": "paragraph",
                "content": [
                    {"kind": "text", "value": "A"},
                    {"kind": "gaiji", "description": "desc", "resolved": "", "unresolved_reason": "unresolved"},
                    {"kind": "text", "value": "B"}
                ]
            }],
            "meta": {
                "adapter": "fixture",
                "adapter_version": "fixture",
                "source_encoding": "utf-8",
                "source_hash": "sha256:0000000000000000000000000000000000000000000000000000000000000000",
                "parse_complete": true,
                "warnings": []
            }
        });
        let doc = AatDocument::from_value(value.clone());

        assert_eq!(
            doc.visible_text(),
            ab_plaintext::visible_text_projection(&value)
        );
    }
}
