use anyhow::{Result, anyhow, bail};
use aozora_rs_core::{Break, Deco, Retokenized, parse_meta, retokenize, scopenize, tokenize};
use encoding_rs::SHIFT_JIS;
use serde_json::json;
use sha2::{Digest, Sha256};
use winnow::LocatingSlice;

pub const VERSION: &str = "aozora-rs-adapter 0.1.0 dd380ee639ca317ac9092ef2ba554acdf70e3c8d";

#[derive(Debug)]
pub struct DecodedSource {
    pub text: String,
    pub encoding: &'static str,
    pub source_hash: String,
}

#[derive(Debug)]
struct ParsedSource<'a> {
    retokenized: Vec<Retokenized<'a>>,
    warnings: Vec<String>,
}

pub fn decode_source_bytes(bytes: &[u8]) -> Result<DecodedSource> {
    let source_hash = format!("sha256:{}", hex_sha256(bytes));
    if bytes.starts_with(&[0xef, 0xbb, 0xbf]) {
        return Ok(DecodedSource {
            text: std::str::from_utf8(&bytes[3..])?.to_owned(),
            encoding: "utf-8-bom",
            source_hash,
        });
    }
    if let Ok(text) = std::str::from_utf8(bytes) {
        return Ok(DecodedSource {
            text: text.to_owned(),
            encoding: "utf-8",
            source_hash,
        });
    }
    let (cow, _, had_errors) = SHIFT_JIS.decode(bytes);
    Ok(DecodedSource {
        text: cow.into_owned(),
        encoding: if had_errors {
            "windows-31j-lossy"
        } else {
            "windows-31j"
        },
        source_hash,
    })
}

pub fn aat_json_from_bytes(bytes: &[u8]) -> Result<Vec<u8>> {
    let decoded = decode_source_bytes(bytes)?;
    let parsed = parse_with_aozora_rs(&decoded.text)?;
    let aat = build_aat(&decoded, &parsed);
    let mut out = Vec::new();
    serde_json::to_writer(&mut out, &aat)?;
    out.push(b'\n');
    Ok(out)
}

pub fn html_from_bytes(_bytes: &[u8]) -> Result<String> {
    bail!("aozora-rs-adapter --mode html is intentionally deferred to the render-diff phase")
}

fn parse_with_aozora_rs(text: &str) -> Result<ParsedSource<'_>> {
    let mut body = text;
    let mut warnings = Vec::new();
    if let Err(error) = parse_meta(&mut body) {
        warnings.push(format!("meta parse warning: {error}"));
    }
    body = trim_colophon(body);

    let mut input = LocatingSlice::new(body);
    let tokenized = tokenize(&mut input).map_err(|()| anyhow!("aozora-rs-core tokenize failed"))?;
    let ((scopenized, flat_tokens), scopenize_errors) = scopenize(tokenized).into_tuple();
    let (retokenized, retokenize_errors) = retokenize(flat_tokens, scopenized).into_tuple();
    warnings.extend(
        scopenize_errors
            .into_iter()
            .map(|error| format!("{error:?}")),
    );
    warnings.extend(retokenize_errors.into_iter().map(|error| error.to_string()));

    Ok(ParsedSource {
        retokenized,
        warnings,
    })
}

fn trim_colophon(body: &str) -> &str {
    let body_end = body
        .char_indices()
        .find_map(|(offset, _)| {
            let rest = &body[offset..];
            if rest.starts_with("底本：") || rest.starts_with("底本:") {
                Some(offset)
            } else {
                None
            }
        })
        .unwrap_or(body.len());
    &body[..body_end]
}

fn build_aat(decoded: &DecodedSource, parsed: &ParsedSource<'_>) -> serde_json::Value {
    json!({
        "version": 1,
        "work_id": "stdin",
        "blocks": retokenized_to_aat_blocks(&parsed.retokenized),
        "meta": {
            "adapter": "aozora-rs",
            "adapter_version": VERSION,
            "source_encoding": decoded.encoding,
            "source_hash": decoded.source_hash,
            "parse_complete": true,
            "warnings": parsed.warnings.iter().map(|message| json!({ "message": message })).collect::<Vec<_>>()
        }
    })
}

fn retokenized_to_aat_blocks(tokens: &[Retokenized<'_>]) -> Vec<serde_json::Value> {
    let mut blocks = Vec::new();
    let mut content = Vec::new();
    let mut idx = 0;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => push_text(&mut content, text),
            Retokenized::Odoriji(odoriji) => push_text(&mut content, &odoriji.to_string()),
            Retokenized::Kunten(kunten) => push_text(&mut content, kunten),
            Retokenized::Okurigana(okurigana) => push_text(&mut content, okurigana),
            Retokenized::Break(Break::BreakLine) => flush_paragraph(&mut blocks, &mut content),
            Retokenized::Break(_) => flush_paragraph(&mut blocks, &mut content),
            Retokenized::Figure(figure) => content.push(json!({
                "kind": "gaiji",
                "x-description-format": "aozora-rs-core Figure Display output; original gaiji notation is not preserved by Figure",
                "description": figure.to_string(),
                "resolved": "",
                "jis_code": null,
                "unresolved_reason": null
            })),
            Retokenized::DecoBegin(Deco::Ruby(reading)) => {
                let (base, next_idx) = collect_decorated_visible_text(tokens, idx + 1, |deco| {
                    matches!(deco, Deco::Ruby(_))
                });
                content.push(json!({
                    "kind": "ruby",
                    "base": base,
                    "reading": reading
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(Deco::AHead | Deco::BHead | Deco::CHead) => {
                flush_paragraph(&mut blocks, &mut content);
                let deco = match &tokens[idx] {
                    Retokenized::DecoBegin(deco) => deco,
                    _ => unreachable!(),
                };
                let (value, next_idx) =
                    collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                        matches!(
                            (deco, candidate),
                            (Deco::AHead, Deco::AHead)
                                | (Deco::BHead, Deco::BHead)
                                | (Deco::CHead, Deco::CHead)
                        )
                    });
                let level = match deco {
                    Deco::AHead => 1,
                    Deco::BHead => 2,
                    Deco::CHead => 3,
                    _ => unreachable!(),
                };
                blocks.push(json!({
                    "kind": "heading",
                    "level": level,
                    "style": stable_style_type(deco),
                    "content": [{"kind": "text", "value": value}]
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoBegin(deco) => {
                let (value, next_idx) =
                    collect_decorated_visible_text(tokens, idx + 1, |candidate| {
                        same_deco_kind(candidate, deco)
                    });
                content.push(json!({
                    "kind": "style",
                    "style_type": stable_style_type(deco),
                    "content": [{"kind": "text", "value": value}]
                }));
                idx = next_idx;
                continue;
            }
            Retokenized::DecoEnd(_) => {}
        }
        idx += 1;
    }
    flush_paragraph(&mut blocks, &mut content);
    if blocks.is_empty() {
        blocks.push(json!({ "kind": "paragraph", "content": [] }));
    }
    blocks
}

fn collect_decorated_visible_text(
    tokens: &[Retokenized<'_>],
    mut idx: usize,
    is_matching_end: impl Fn(&Deco<'_>) -> bool,
) -> (String, usize) {
    let mut value = String::new();
    let mut depth = 1;
    while idx < tokens.len() {
        match &tokens[idx] {
            Retokenized::Text(text) => value.push_str(text),
            Retokenized::Odoriji(odoriji) => value.push_str(&odoriji.to_string()),
            Retokenized::Kunten(kunten) => value.push_str(kunten),
            Retokenized::Okurigana(okurigana) => value.push_str(okurigana),
            Retokenized::Break(_) => value.push('\n'),
            Retokenized::Figure(figure) => value.push_str(&figure.to_string()),
            Retokenized::DecoBegin(_) => depth += 1,
            Retokenized::DecoEnd(deco) if depth == 1 && is_matching_end(deco) => {
                return (value, idx + 1);
            }
            Retokenized::DecoEnd(_) => depth -= 1,
        }
        idx += 1;
    }
    (value, idx)
}

fn flush_paragraph(blocks: &mut Vec<serde_json::Value>, content: &mut Vec<serde_json::Value>) {
    if content.is_empty() {
        return;
    }
    blocks.push(json!({
        "kind": "paragraph",
        "content": std::mem::take(content)
    }));
}

fn same_deco_kind(a: &Deco<'_>, b: &Deco<'_>) -> bool {
    matches!(
        (a, b),
        (Deco::Bold, Deco::Bold)
            | (Deco::Italic, Deco::Italic)
            | (Deco::Bosen(_), Deco::Bosen(_))
            | (Deco::Boten(_), Deco::Boten(_))
            | (Deco::Indent(_), Deco::Indent(_))
            | (Deco::Hanging(_), Deco::Hanging(_))
            | (Deco::Grounded, Deco::Grounded)
            | (Deco::LowFlying(_), Deco::LowFlying(_))
            | (Deco::HinV, Deco::HinV)
            | (Deco::Mama, Deco::Mama)
            | (Deco::Smaller(_), Deco::Smaller(_))
            | (Deco::Bigger(_), Deco::Bigger(_))
            | (Deco::VHCentre, Deco::VHCentre)
            | (Deco::Warichu, Deco::Warichu)
            | (Deco::HorizontalLayout, Deco::HorizontalLayout)
            | (Deco::Kerning(_), Deco::Kerning(_))
            | (Deco::Sub, Deco::Sub)
            | (Deco::Sup, Deco::Sup)
    )
}

fn stable_style_type(deco: &Deco<'_>) -> &'static str {
    match deco {
        Deco::Bold => "bold",
        Deco::Italic => "italic",
        Deco::Bosen(_) => "bosen",
        Deco::Boten(_) => "boten",
        Deco::Indent(_) => "indent",
        Deco::Hanging(_) => "hanging",
        Deco::Grounded => "grounded",
        Deco::LowFlying(_) => "low_flying",
        Deco::HinV => "tcy",
        Deco::Mama => "mama",
        Deco::Smaller(_) => "smaller",
        Deco::Bigger(_) => "bigger",
        Deco::VHCentre => "vh_centre",
        Deco::Warichu => "warichu",
        Deco::HorizontalLayout => "horizontal_layout",
        Deco::Kerning(_) => "kerning",
        Deco::Sub => "sub",
        Deco::Sup => "sup",
        Deco::Ruby(_) | Deco::AHead | Deco::BHead | Deco::CHead => "handled_elsewhere",
    }
}

fn push_text(content: &mut Vec<serde_json::Value>, value: &str) {
    if value.is_empty() {
        return;
    }
    content.push(json!({ "kind": "text", "value": value }));
}

fn hex_sha256(bytes: &[u8]) -> String {
    let mut hasher = Sha256::new();
    hasher.update(bytes);
    format!("{:x}", hasher.finalize())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn emits_schema_shaped_aat_for_ruby_and_gaiji() {
        let input = "\
タイトル
著者
-------------------------------------------------------
凡例
-------------------------------------------------------
吾輩《わがはい》は※［＃「口＋世」、U+546D］である。"
            .as_bytes();
        let out = aat_json_from_bytes(input).unwrap();
        let value: serde_json::Value = serde_json::from_slice(&out).unwrap();

        assert_eq!(value["work_id"], "stdin");
        assert_eq!(value["meta"]["adapter"], "aozora-rs");
        assert_eq!(value["meta"]["source_encoding"], "utf-8");
        assert!(value["meta"]["parse_complete"].as_bool().unwrap());
        assert_eq!(value["blocks"][0]["kind"], "paragraph");
        assert!(
            value["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .any(|node| node["kind"] == "ruby")
        );
    }
}
