use serde_json::Value;

#[derive(Debug, Clone)]
pub enum VisibleFragment<'a> {
    Text {
        value: String,
        path: String,
        node: &'a Value,
    },
    Gaiji {
        resolved: Option<String>,
        description: String,
        path: String,
        node: &'a Value,
    },
}

pub fn visible_text_projection(aat: &Value) -> String {
    visible_text_fragments(aat)
        .into_iter()
        .map(|fragment| match fragment {
            VisibleFragment::Text { value, .. } => value,
            VisibleFragment::Gaiji {
                resolved,
                description,
                ..
            } => resolved.unwrap_or(description),
        })
        .collect()
}

pub fn visible_text_fragments<'a>(aat: &'a Value) -> Vec<VisibleFragment<'a>> {
    let mut fragments = Vec::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for (idx, block) in blocks.iter().enumerate() {
            collect_block(block, &format!("$.blocks[{idx}]"), &mut fragments);
        }
    }
    fragments
}

pub fn inline_nodes_by_kind<'a>(aat: &'a Value, kind: &str) -> Vec<(String, &'a Value)> {
    let mut nodes = Vec::new();
    if let Some(blocks) = aat.get("blocks").and_then(Value::as_array) {
        for (idx, block) in blocks.iter().enumerate() {
            collect_inline_kind(block, &format!("$.blocks[{idx}]"), kind, &mut nodes);
        }
    }
    nodes
}

pub fn node_line(node: &Value) -> Option<usize> {
    node.get("span")?
        .get("line_start")?
        .as_u64()
        .and_then(|line| usize::try_from(line).ok())
}

fn collect_block<'a>(node: &'a Value, path: &str, fragments: &mut Vec<VisibleFragment<'a>>) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for (idx, inline) in content.iter().enumerate() {
            collect_inline(inline, &format!("{path}.content[{idx}]"), fragments);
        }
    }
    if let Some(children) = node.get("children").and_then(Value::as_array) {
        for (idx, child) in children.iter().enumerate() {
            collect_block(child, &format!("{path}.children[{idx}]"), fragments);
        }
    }
}

fn collect_inline<'a>(node: &'a Value, path: &str, fragments: &mut Vec<VisibleFragment<'a>>) {
    let kind = node.get("kind").and_then(Value::as_str).unwrap_or("");
    match kind {
        "text" => fragments.push(VisibleFragment::Text {
            value: node
                .get("value")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "ruby" => fragments.push(VisibleFragment::Text {
            value: node
                .get("base")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "gaiji" => fragments.push(VisibleFragment::Gaiji {
            resolved: node
                .get("resolved")
                .and_then(Value::as_str)
                .map(ToOwned::to_owned),
            description: node
                .get("description")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "raw" => fragments.push(VisibleFragment::Text {
            value: node
                .get("source")
                .and_then(Value::as_str)
                .unwrap_or_default()
                .to_owned(),
            path: path.to_owned(),
            node,
        }),
        "warigaki" => {
            for key in ["upper", "lower"] {
                if let Some(content) = node.get(key).and_then(Value::as_array) {
                    for (idx, inline) in content.iter().enumerate() {
                        collect_inline(inline, &format!("{path}.{key}[{idx}]"), fragments);
                    }
                }
            }
        }
        _ => {
            if let Some(content) = node.get("content").and_then(Value::as_array) {
                for (idx, inline) in content.iter().enumerate() {
                    collect_inline(inline, &format!("{path}.content[{idx}]"), fragments);
                }
            }
        }
    }
}

fn collect_inline_kind<'a>(
    node: &'a Value,
    path: &str,
    kind: &str,
    nodes: &mut Vec<(String, &'a Value)>,
) {
    if let Some(content) = node.get("content").and_then(Value::as_array) {
        for (idx, inline) in content.iter().enumerate() {
            let inline_path = format!("{path}.content[{idx}]");
            if inline.get("kind").and_then(Value::as_str) == Some(kind) {
                nodes.push((inline_path.clone(), inline));
            }
            collect_inline_kind(inline, &inline_path, kind, nodes);
        }
    }
    for key in ["children", "upper", "lower"] {
        if let Some(children) = node.get(key).and_then(Value::as_array) {
            for (idx, child) in children.iter().enumerate() {
                collect_inline_kind(child, &format!("{path}.{key}[{idx}]"), kind, nodes);
            }
        }
    }
}
