use crate::model::{AtBlock, MappingError};
use roxmltree::{Document, Node};
use serde_json::{json, Value};

fn class_contains(node: Node<'_, '_>, token: &str) -> bool {
    node.attribute("class")
        .is_some_and(|class| class.split_ascii_whitespace().any(|it| it == token))
}

fn inline_text(node: Node<'_, '_>) -> Vec<AtBlock> {
    node.descendants()
        .filter_map(|n| n.text())
        .map(|text| json!({"kind":"text","value":text.to_string()}))
        .collect()
}

fn map_inline_node(node: Node<'_, '_>) -> Vec<AtBlock> {
    match node.node_type() {
        roxmltree::NodeType::Text => {
            if let Some(text) = node.text() {
                vec![json!({"kind":"text","value": text})]
            } else {
                Vec::new()
            }
        }
        _ => {
            let name = node.tag_name().name();
            match name {
                "br" => vec![json!({"kind":"raw","source":"<br/>"})],
                "ruby" => {
                    let content: String = node.text().unwrap_or("").to_string();
                    let mut split = content.split('《');
                    let base = split.next().unwrap_or("");
                    let reading = split
                        .next()
                        .and_then(|right| right.split('》').next())
                        .unwrap_or("");
                    if base.is_empty() {
                        if reading.is_empty() {
                            vec![json!({"kind":"raw","source":content})]
                        } else {
                            vec![json!({"kind":"text","value":reading})]
                        }
                    } else {
                        vec![json!({"kind":"ruby","base":base,"reading":reading})]
                    }
                }
                "img" => {
                    let alt = node.attribute("alt").unwrap_or("");
                    if alt.is_empty() {
                        vec![json!({"kind":"figure","filename":node.attribute("src").unwrap_or(""),"css_class":"unknown"})]
                    } else {
                        vec![json!({"kind":"figure","filename":node.attribute("src").unwrap_or(""),"alt":alt})]
                    }
                }
                _ => {
                    if class_contains(node, "caption")
                        && node.text().is_some()
                    {
                        vec![json!({"kind":"caption","content": inline_text(node)})]
                    } else {
                        let mut out = Vec::new();
                        for child in node.children().filter(|c| c.is_text() || c.is_element()) {
                            out.extend(map_inline_node(child));
                        }
                        if out.is_empty() && node.text().is_some() {
                            out.push(json!({"kind":"text","value": node.text().unwrap_or("")}));
                        }
                        out
                    }
                }
            }
        }
    }
}

fn paragraph_from_block_node(block_node: Node<'_, '_>) -> Option<AtBlock> {
    let mut content = Vec::new();
    for child in block_node.children() {
        if child.is_element() || child.is_text() {
            content.extend(map_inline_node(child));
        }
    }

    let text_only = content
        .iter()
        .filter_map(|node| node.get("value").and_then(Value::as_str))
        .map(ToString::to_string)
        .collect::<String>();
    if content.is_empty() || text_only.is_empty() {
        return None;
    }

    let block_kind = match block_node.tag_name().name() {
        "h1" => "heading",
        "h2" => "heading",
        "h3" => "heading",
        "div" if class_contains(block_node, "midashi") => "heading",
        _ => "paragraph",
    };

    match block_kind {
        "heading" => {
            let level = if block_node.tag_name().name() == "h1" { 1 } else if block_node.tag_name().name() == "h2" { 2 } else { 3 };
            Some(json!({
                "kind":"heading",
                "level": level,
                "style":"default",
                "content": content
            }))
        }
        _ => Some(json!({
            "kind":"paragraph",
            "content": content
        })),
    }
}

pub fn map_blocks_from_xhtml_bytes(
    xhtml: &[u8],
    warnings: &mut Vec<Value>,
) -> Result<Vec<AtBlock>, MappingError> {
    let text = std::str::from_utf8(xhtml)
        .map_err(|err| MappingError::parse_error(format!("invalid XHTML bytes: {err}"), vec![], false))?;
    let doc = Document::parse(text)
        .map_err(|err| MappingError::parse_error(format!("invalid XHTML: {err}"), vec![], false))?;

    let root = doc.root_element();
    let main_text = root
        .descendants()
        .find(|node| node.is_element() && node.tag_name().name() == "div" && class_contains(*node, "main_text"))
        .unwrap_or(root);

    let mut blocks = Vec::new();
    for node in main_text.children() {
        if !node.is_element() {
            continue;
        }
        let name = node.tag_name().name();
        if name == "p" || name == "div" || name == "h1" || name == "h2" || name == "h3" {
            if let Some(block) = paragraph_from_block_node(node) {
                blocks.push(block);
            }
            continue;
        }
        if name == "img" && node.attribute("alt").is_some() {
            blocks.push(json!({
                "kind":"paragraph",
                "content":[
                    {"kind":"figure","filename":node.attribute("src").unwrap_or(""),"alt":node.attribute("alt").unwrap_or(""),"x-provenance":"parser"}
                ]
            }));
            continue;
        }
        if node.is_element() {
            let inline = map_inline_node(node);
            if !inline.is_empty() {
                blocks.push(json!({"kind":"paragraph","content":inline}));
            } else {
                warnings.push(json!({"message": format!("unmapped XHTML element <{}>", name), "path": format!("/blocks/.../{}", name)}));
            }
        }
    }

    if blocks.is_empty() {
        let fallback_content = inline_text(root);
        if !fallback_content.is_empty() {
            blocks.push(json!({"kind":"paragraph","content":fallback_content}));
        }
    }
    Ok(blocks)
}
