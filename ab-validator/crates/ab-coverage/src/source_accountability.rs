//! Source-authority occurrences independent of any parser interpretation.

use ab_encoding::{decode_source_bytes, hex_sha256};
use ab_source_syntax::aozora_body_range;
use serde_json::{Value, json};

use crate::source_inventory::{
    SourceInventoryPattern, SourceMarkerRegion, inventory_document_observed,
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
        "schema": "aozora-source-accountability/1",
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
            }));
        },
    );
    report["occurrences"] = json!(occurrences);
    report
}

#[cfg(test)]
mod tests {
    use super::*;

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
