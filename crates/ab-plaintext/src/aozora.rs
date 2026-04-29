use ab_source_syntax::comparison_lossy_body;
use encoding_rs::SHIFT_JIS;

use crate::{PlainTextDocument, SourceFormat, canonicalize_line_endings};

pub fn from_aozora_honbun_bytes(text_id: impl Into<String>, bytes: &[u8]) -> PlainTextDocument {
    let decoded = decode_source_bytes(bytes);
    let body = select_body(&decoded);
    let text = canonicalize_line_endings(comparison_lossy_body(&body).into_owned());

    PlainTextDocument {
        text_id: text_id.into(),
        source_format: SourceFormat::AozoraHonbun,
        text,
    }
}

fn decode_source_bytes(bytes: &[u8]) -> String {
    if bytes.starts_with(&[0xEF, 0xBB, 0xBF]) {
        return String::from_utf8_lossy(&bytes[3..]).into_owned();
    }

    if let Ok(text) = std::str::from_utf8(bytes) {
        return text.to_owned();
    }

    let (decoded, _, _) = SHIFT_JIS.decode(bytes);
    decoded.into_owned()
}

fn select_body(text: &str) -> String {
    let mut separator_count = 0usize;
    let mut body_start = 0usize;

    for line in text.split_inclusive('\n') {
        let trimmed = line.trim_matches(|ch| ch == '\r' || ch == '\n');
        let is_separator = trimmed.len() >= 20 && trimmed.chars().all(|ch| ch == '-');
        body_start += line.len();

        if is_separator {
            separator_count += 1;
            if separator_count == 2 {
                break;
            }
        }
    }

    let body = if separator_count >= 2 {
        &text[body_start..]
    } else {
        text
    };

    trim_colophon(body).to_owned()
}

fn trim_colophon(body: &str) -> &str {
    let japanese = body.find("底本：");
    let ascii = body.find("底本:");

    match (japanese, ascii) {
        (Some(a), Some(b)) => &body[..a.min(b)],
        (Some(index), None) | (None, Some(index)) => &body[..index],
        (None, None) => body,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn extracts_text_after_second_separator() {
        let source =
            "title\n--------------------\nmeta\n--------------------\n本文です。\n底本：x\n";
        let doc = from_aozora_honbun_bytes("t1", source.as_bytes());
        assert_eq!(doc.text_id, "t1");
        assert_eq!(doc.source_format, SourceFormat::AozoraHonbun);
        assert_eq!(doc.text.trim(), "本文です。");
    }

    #[test]
    fn falls_back_to_whole_text_without_two_separators() {
        let source = "本文だけです。";
        let doc = from_aozora_honbun_bytes("t2", source.as_bytes());
        assert_eq!(doc.text, "本文だけです。");
    }

    #[test]
    fn removes_aozora_ruby_to_visible_plaintext() {
        let source = "｜吾輩《わがはい》は猫である。";
        let doc = from_aozora_honbun_bytes("t3", source.as_bytes());
        assert_eq!(doc.text, "吾輩は猫である。");
    }

    #[test]
    fn canonicalizes_line_endings_after_projection() {
        let source = "A\r\nB\rC\nD";
        let doc = from_aozora_honbun_bytes("line-endings", source.as_bytes());
        assert_eq!(doc.text, "A\nB\nC\nD");
    }

    #[test]
    fn decodes_shift_jis() {
        let (bytes, _, _) = SHIFT_JIS.encode("本文です。");
        let doc = from_aozora_honbun_bytes("t4", &bytes);
        assert_eq!(doc.text, "本文です。");
    }

    #[test]
    fn empty_projected_body_is_valid_document() {
        let doc = from_aozora_honbun_bytes("empty", "底本：x".as_bytes());
        assert_eq!(doc.text_id, "empty");
        assert_eq!(doc.text, "");
    }
}
