//! Source-authority occurrences independent of any parser interpretation.

use ab_encoding::{decode_source_bytes, hex_sha256};
use ab_source_syntax::{SourceMarker, SourceMarkerKind, aozora_body_range, source_markers};
use serde_json::{Value, json};

use crate::source_inventory::{
    CompiledSourceInventoryPattern, SourceInventoryPattern, SourceMarkerRegion,
    append_composite_matching_rows, compile_patterns, inventory_document_observed, matching_rows,
};

/// Record lexical evidence without treating recognition as semantic support.
/// Spans address the decoded UTF-8 text, whose hash is distinct from source bytes.
#[must_use]
pub fn source_accountability(
    bytes: &[u8],
    matrix_bytes: &[u8],
    patterns: &[SourceInventoryPattern],
) -> Value {
    let mut report = json!({
        "schema": "aozora-source-accountability/2",
        "source_sha256": format!("sha256:{}", hex_sha256(bytes)),
        "matrix_sha256": format!("sha256:{}", hex_sha256(matrix_bytes)),
        "coordinate_system": "decoded_utf8",
        "scope": "lexical-source-markers",
        "semantic_coverage": "not-assessed",
        "occurrences": [],
    });
    let decoded = match decode_source_bytes(bytes) {
        Ok(decoded) => decoded,
        Err(error) => {
            report["decode_outcome"] = json!("failed");
            report["decode_error"] = json!(error.to_string());
            return report;
        }
    };
    report["encoding"] = json!(decoded.encoding);
    report["decode_outcome"] = json!(if decoded.encoding == "windows-31j-lossy" {
        "lossy"
    } else {
        "lossless"
    });
    report["decoded_sha256"] = json!(format!("sha256:{}", hex_sha256(decoded.text.as_bytes())));
    let (body, _) = aozora_body_range(&decoded.text);
    report["lexical_body_range"] = json!({"start": body.start, "end": body.end});
    let mut occurrences = Vec::new();
    let component_patterns: Vec<_> = patterns
        .iter()
        .filter(|pattern| {
            matches!(
                pattern.row_id.as_str(),
                "kunten.kaeriten"
                    | "kunten.okurigana"
                    | "ruby.basic"
                    | "accent.dotted_letter"
                    | "glyph.variant_note"
                    | "annotation.chuuki"
                    | "iteration.kunoji"
                    | "gaiji_ruby.inline_base"
                    | "gaiji.marker"
                    | "gaiji.jis_code"
                    | "gaiji.unicode_codepoint"
            )
        })
        .cloned()
        .collect();
    let compiled_patterns = compile_patterns(&component_patterns);
    inventory_document_observed(
        "",
        &decoded.text,
        patterns,
        |marker, classified, families| {
            let region = match classified {
                Some(SourceMarkerRegion::FrontMatter) => "front-matter",
                Some(SourceMarkerRegion::BodyEndBoundary) => "body-end-boundary",
                None if marker.span.start >= body.end => "back-matter",
                None if marker.span.start >= body.start && marker.span.end <= body.end => "body",
                None => "unknown-region",
            };
            let mut families = families.to_vec();
            families.sort();
            occurrences.push(json!({
                "source_span": {"start": marker.span.start, "end": marker.span.end,
                                "line": marker.span.line, "coordinate_system": "decoded_utf8"},
                "kind": format!("{:?}", marker.kind),
                "raw": marker.raw,
                "region": region,
                "families": families,
                "components": nested_components(marker, &compiled_patterns),
            }));
        },
    );
    report["occurrences"] = json!(occurrences);
    report
}

fn nested_components(
    marker: &SourceMarker<'_>,
    patterns: &[CompiledSourceInventoryPattern],
) -> Vec<Value> {
    let mut components = Vec::new();
    let mut pending = vec![marker.clone()];
    while let Some(parent) = pending.pop() {
        if !matches!(
            parent.kind,
            SourceMarkerKind::RubyExplicit
                | SourceMarkerKind::GaijiFullwidth
                | SourceMarkerKind::GaijiAscii
                | SourceMarkerKind::RubyImplicit
                | SourceMarkerKind::BracketNote
                | SourceMarkerKind::CommandFullwidth
                | SourceMarkerKind::CommandAscii
                | SourceMarkerKind::EditorialNoteBottomTextCorrection
        ) {
            continue;
        }
        let offset = parent.span.start + parent.body.as_ptr().addr() - parent.raw.as_ptr().addr();
        for mut child in source_markers(parent.body) {
            child.span.start += offset;
            child.span.end += offset;
            child.span.line += parent.span.line - 1;
            if child.span.start <= parent.span.start && child.span.end >= parent.span.end {
                continue;
            }
            // The terminal reading delimiters belong to the enclosing explicit ruby.
            let same_ruby = parent.kind == SourceMarkerKind::RubyExplicit
                && child.kind == SourceMarkerKind::RubyImplicit
                && child.span.end == parent.span.end;
            let eligible_families: &[&str] = match child.kind {
                SourceMarkerKind::RubyExplicit => &["ruby.basic", "gaiji_ruby.inline_base"],
                SourceMarkerKind::RubyImplicit => &["ruby.basic"],
                SourceMarkerKind::IterationNotation => &["iteration.kunoji"],
                SourceMarkerKind::CommandFullwidth | SourceMarkerKind::CommandAscii => &[
                    "kunten.kaeriten",
                    "kunten.okurigana",
                    "accent.dotted_letter",
                    "glyph.variant_note",
                    "annotation.chuuki",
                ],
                SourceMarkerKind::GaijiFullwidth | SourceMarkerKind::GaijiAscii => {
                    &["gaiji.marker", "gaiji.jis_code", "gaiji.unicode_codepoint"]
                }
                _ => &[],
            };
            if !same_ruby && !eligible_families.is_empty() {
                let mut families = matching_rows(child.raw, patterns);
                families.retain(|family| eligible_families.contains(&family.as_str()));
                families.sort();
                if !families.is_empty() {
                    components.push(json!({
                        "source_span": {"start": child.span.start, "end": child.span.end,
                                        "line": child.span.line, "coordinate_system": "decoded_utf8"},
                        "kind": format!("{:?}", child.kind), "raw": child.raw, "families": families,
                    }));
                }
            }
            pending.push(child);
        }
    }
    components.sort_by_key(|component| component["source_span"]["start"].as_u64());
    // Composite identity follows source adjacency, independently of quotation nesting.
    for index in 0..components.len().saturating_sub(1) {
        let left = &components[index];
        let right = &components[index + 1];
        if left["kind"] != "GaijiFullwidth" && left["kind"] != "GaijiAscii" {
            continue;
        }
        if right["kind"] != "RubyImplicit"
            || left["source_span"]["end"] != right["source_span"]["start"]
        {
            continue;
        }
        let start = usize::try_from(
            left["source_span"]["start"]
                .as_u64()
                .expect("component start"),
        )
        .expect("source index");
        let end = usize::try_from(right["source_span"]["end"].as_u64().expect("component end"))
            .expect("source index");
        let mut families = Vec::new();
        append_composite_matching_rows(
            &mut families,
            &marker.raw[start - marker.span.start..end - marker.span.start],
            left["raw"].as_str().expect("component spelling"),
            right["raw"].as_str().expect("component spelling"),
            patterns,
        );
        if families
            .iter()
            .any(|family| family == "gaiji_ruby.inline_base")
        {
            let families = components[index]["families"]
                .as_array_mut()
                .expect("component families");
            families.push(json!("gaiji_ruby.inline_base"));
            families.sort_by(|a, b| a.as_str().cmp(&b.as_str()));
        }
    }
    components
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_glyph_components_keep_independent_source_spans() {
        let source = "前※［＃「姉」の正字、「女＋※［＃第3水準1-85-57］のつくり」、252-下-27］後";
        let patterns = vec![SourceInventoryPattern {
            row_id: "gaiji.marker".into(),
            source_patterns: vec!["※［＃".into()],
        }];
        let marker = source_markers(source).remove(0);
        let components = nested_components(&marker, &compile_patterns(&patterns));
        assert_eq!(components.len(), 1);
        assert_eq!(components[0]["raw"], "※［＃第3水準1-85-57］");
        let start =
            usize::try_from(components[0]["source_span"]["start"].as_u64().unwrap()).unwrap();
        let end = usize::try_from(components[0]["source_span"]["end"].as_u64().unwrap()).unwrap();
        assert_eq!(&source[start..end], "※［＃第3水準1-85-57］");
        assert!(marker.span.start < start && end < marker.span.end);
    }

    #[test]
    fn nested_gaiji_ruby_uses_the_same_adjacent_marker_identity_as_top_level() {
        let patterns = vec![
            SourceInventoryPattern {
                row_id: "gaiji.marker".into(),
                source_patterns: vec![r"※［＃[^］]+］".into()],
            },
            SourceInventoryPattern {
                row_id: "ruby.basic".into(),
                source_patterns: vec![r"《[^》]+》".into()],
            },
            SourceInventoryPattern {
                row_id: "gaiji_ruby.inline_base".into(),
                source_patterns: vec![r"※［＃[^］]+］《[^》]+》".into()],
            },
        ];
        for (fragment, associated) in [
            ("※［＃字］《じ》", true),
            ("※［＃字］別《べつ》", false),
            ("※［＃字］ 《じ》", false),
            ("※［＃字］《じ", false),
        ] {
            let source = format!("［＃「{fragment}」は底本では「別」］");
            let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
            let components = report["occurrences"][0]["components"].as_array().unwrap();
            let gaiji = components
                .iter()
                .find(|item| item["kind"] == "GaijiFullwidth")
                .unwrap();
            assert_eq!(
                gaiji["families"]
                    .as_array()
                    .unwrap()
                    .iter()
                    .any(|family| family == "gaiji_ruby.inline_base"),
                associated
            );
            assert_eq!(gaiji["raw"], "※［＃字］");
            let start = usize::try_from(gaiji["source_span"]["start"].as_u64().unwrap()).unwrap();
            let end = usize::try_from(gaiji["source_span"]["end"].as_u64().unwrap()).unwrap();
            assert_eq!(&source[start..end], "※［＃字］");
            assert!(
                components
                    .iter()
                    .filter(|item| item["kind"] == "RubyImplicit")
                    .all(|item| item["families"] == json!(["ruby.basic"]))
            );
        }
        let source = "［＃「※［＃字］《じ》」は底本では「※［＃字］《じ》」］";
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let gaiji = report["occurrences"][0]["components"]
            .as_array()
            .unwrap()
            .iter()
            .filter(|component| component["kind"] == "GaijiFullwidth")
            .collect::<Vec<_>>();
        assert_eq!(gaiji.len(), 2);
        assert!(gaiji.iter().all(|component| {
            component["families"]
                .as_array()
                .unwrap()
                .contains(&json!("gaiji_ruby.inline_base"))
        }));
    }

    #[test]
    fn nested_explicit_ruby_keeps_its_whole_association_extent() {
        let patterns = vec![
            SourceInventoryPattern {
                row_id: "gaiji.marker".into(),
                source_patterns: vec![r"※［＃[^］]+］".into()],
            },
            SourceInventoryPattern {
                row_id: "ruby.basic".into(),
                source_patterns: vec![r"《[^》]+》".into()],
            },
            SourceInventoryPattern {
                row_id: "gaiji_ruby.inline_base".into(),
                source_patterns: vec![r"※［＃[^］]+］《[^》]+》".into()],
            },
        ];
        let source = "［＃「｜前※［＃字］《ぜんじ》」は底本では「別」］";
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let components = report["occurrences"][0]["components"].as_array().unwrap();
        assert_eq!(components.len(), 2);
        assert_eq!(components[0]["kind"], "RubyExplicit");
        assert_eq!(components[0]["raw"], "｜前※［＃字］《ぜんじ》");
        assert_eq!(
            components[0]["families"],
            json!(["gaiji_ruby.inline_base", "ruby.basic"])
        );
        assert_eq!(components[1]["kind"], "GaijiFullwidth");
        assert_eq!(components[1]["families"], json!(["gaiji.marker"]));
    }

    #[test]
    fn edition_notes_inside_brackets_keep_their_own_source_extent() {
        let patterns = vec![SourceInventoryPattern {
            row_id: "annotation.chuuki".into(),
            source_patterns: vec!["」は底本では「".into()],
        }];
        for (base, command) in [
            ("Hu:lshoff", "［＃「Hu:lshoff」は底本では「Hu:lshoffs」］"),
            (
                "schla:gt",
                "［＃「〔schla:gt〕」は底本では「〔scha:gt〕」］",
            ),
        ] {
            let source = format!("題\n作者\n\n〔前{base}{command}後〕\n");
            let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
            let components = report["occurrences"][0]["components"].as_array().unwrap();
            let component = components
                .iter()
                .find(|node| node["raw"] == command)
                .unwrap();
            assert_eq!(component["kind"], "CommandFullwidth");
            assert_eq!(component["families"], json!(["annotation.chuuki"]));
            let start = component["source_span"]["start"].as_u64().unwrap() as usize;
            let end = component["source_span"]["end"].as_u64().unwrap() as usize;
            assert_eq!(&source[start..end], command);
            assert_eq!(component["source_span"]["line"], 4);
        }
    }

    #[test]
    fn iteration_components_keep_original_coordinates_without_expansion() {
        let source = "題\n作者\n\nフゴ／＼。｜時／″＼《とき／＼》。／゛＼〳〵\n";
        let patterns = vec![
            SourceInventoryPattern {
                row_id: "iteration.kunoji".into(),
                source_patterns: vec!["／＼".into(), "／″＼".into()],
            },
            SourceInventoryPattern {
                row_id: "ruby.basic".into(),
                source_patterns: vec!["《[^》]+》".into()],
            },
        ];
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let occurrences = report["occurrences"].as_array().unwrap();
        assert_eq!(occurrences.len(), 2);
        assert_eq!(occurrences[0]["kind"], "IterationNotation");
        let components = occurrences[1]["components"].as_array().unwrap();
        assert_eq!(components.len(), 2);
        for event in std::iter::once(&occurrences[0]).chain(components) {
            let span = &event["source_span"];
            let start = span["start"].as_u64().unwrap() as usize;
            let end = span["end"].as_u64().unwrap() as usize;
            assert_eq!(&source[start..end], event["raw"].as_str().unwrap());
            assert_eq!(event["families"], json!(["iteration.kunoji"]));
        }
    }

    #[test]
    fn quoted_command_components_are_lexical_evidence_with_exact_spelling() {
        for (open, close) in [("［＃", "］"), ("[#", "]")] {
            let source =
                format!("題\n作者\n\n{open}ルビの「わざ／＼」は底本では「わさ／″＼」{close}\n");
            let patterns = vec![SourceInventoryPattern {
                row_id: "iteration.kunoji".into(),
                source_patterns: vec!["／＼".into(), "／″＼".into()],
            }];
            let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
            let occurrences = report["occurrences"].as_array().unwrap();
            assert_eq!(occurrences.len(), 1);
            let components = occurrences[0]["components"].as_array().unwrap();
            assert_eq!(components.len(), 2, "{report}");
            for (component, expected) in components.iter().zip(["／＼", "／″＼"]) {
                let span = &component["source_span"];
                let start = span["start"].as_u64().unwrap() as usize;
                let end = span["end"].as_u64().unwrap() as usize;
                assert_eq!(&source[start..end], expected);
                assert_eq!(component["raw"], expected);
                assert_eq!(component["families"], json!(["iteration.kunoji"]));
                assert!(component.get("claims").is_none());
            }
        }
    }

    #[test]
    fn quoted_ruby_and_its_nested_marks_remain_distinct_components() {
        let source = "題\n作者\n\n［＃「漢《かん／＼》」は底本では「字《じ》」］\n";
        let patterns = vec![
            SourceInventoryPattern {
                row_id: "ruby.basic".into(),
                source_patterns: vec!["《[^》]+》".into()],
            },
            SourceInventoryPattern {
                row_id: "iteration.kunoji".into(),
                source_patterns: vec!["／＼".into()],
            },
        ];
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let occurrences = report["occurrences"].as_array().unwrap();
        assert_eq!(occurrences.len(), 1);
        let components = occurrences[0]["components"].as_array().unwrap();
        assert_eq!(components.len(), 3);
        for (component, (raw, family)) in components.iter().zip([
            ("《かん／＼》", "ruby.basic"),
            ("／＼", "iteration.kunoji"),
            ("《じ》", "ruby.basic"),
        ]) {
            let span = &component["source_span"];
            assert_eq!(
                &source[span["start"].as_u64().unwrap() as usize
                    ..span["end"].as_u64().unwrap() as usize],
                raw
            );
            assert_eq!(component["raw"], raw);
            assert_eq!(component["families"], json!([family]));
        }

        let image = "［＃「漢《かん》」のキャプション付きの図（fig1.png、横20×縦30）入る］";
        let report = source_accountability(image.as_bytes(), b"matrix", &patterns);
        let components = report["occurrences"][0]["components"].as_array().unwrap();
        assert_eq!(components.len(), 1);
        assert_eq!(components[0]["raw"], "《かん》");
        assert_eq!(components[0]["kind"], "RubyImplicit");
        assert_eq!(components[0]["families"], json!(["ruby.basic"]));
        let span = &components[0]["source_span"];
        assert_eq!(
            &image
                [span["start"].as_u64().unwrap() as usize..span["end"].as_u64().unwrap() as usize],
            "《かん》"
        );
    }

    #[test]
    fn evidence_has_exact_decoded_spans_and_no_semantic_claim() {
        let source = "題名\n著者\n----------\n【テキスト中に現れる記号について】\n［＃］：入力者注\n----------\n漢字《かんじ》［＃］\n［＃本文終わり］\n底本：例\n";
        let patterns = vec![SourceInventoryPattern {
            row_id: "ruby.basic".into(),
            source_patterns: vec![],
        }];
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let events = report["occurrences"].as_array().unwrap();
        assert_eq!(events.len(), 4);
        assert_eq!(events[0]["region"], "front-matter");
        assert_eq!(events[1]["families"], json!(["ruby.basic"]));
        assert_eq!(events[2]["region"], "body");
        assert_eq!(events[2]["families"], json!([]));
        assert_eq!(events[3]["region"], "body-end-boundary");
        for event in events {
            let start = event["source_span"]["start"].as_u64().unwrap() as usize;
            let end = event["source_span"]["end"].as_u64().unwrap() as usize;
            assert_eq!(&source[start..end], event["raw"].as_str().unwrap());
        }
        assert_eq!(report["semantic_coverage"], "not-assessed");
    }

    #[test]
    fn composite_kunten_keep_one_occurrence_and_exact_component_extents() {
        let source = "題\n作者\n\n｜遊［＃二］松島［＃一］記《まつしまにあそぶき》\n";
        let patterns = vec![SourceInventoryPattern {
            row_id: "kunten.kaeriten".into(),
            source_patterns: vec!["［＃[一二]］".into()],
        }];
        let report = source_accountability(source.as_bytes(), b"matrix", &patterns);
        let occurrences = report["occurrences"].as_array().unwrap();
        assert_eq!(occurrences.len(), 1);
        let components = occurrences[0]["components"].as_array().unwrap();
        assert_eq!(components.len(), 2);
        for (component, raw) in components.iter().zip(["［＃二］", "［＃一］"]) {
            assert_eq!(component["raw"], raw);
            let span = &component["source_span"];
            assert_eq!(
                &source[span["start"].as_u64().unwrap() as usize
                    ..span["end"].as_u64().unwrap() as usize],
                raw
            );
            assert_eq!(component["families"], json!(["kunten.kaeriten"]));
        }
    }

    #[test]
    fn decoding_failures_and_loss_are_explicit() {
        assert_eq!(
            source_accountability(&[0xef, 0xbb, 0xbf, 0xff], b"", &[])["decode_outcome"],
            "failed"
        );
        assert_eq!(
            source_accountability(&[0x81], b"", &[])["decode_outcome"],
            "lossy"
        );
        let plain = source_accountability("本文".as_bytes(), b"", &[]);
        let bom = source_accountability("\u{feff}本文".as_bytes(), b"", &[]);
        assert_eq!(plain["decoded_sha256"], bom["decoded_sha256"]);
        assert_ne!(plain["source_sha256"], bom["source_sha256"]);
    }
}
