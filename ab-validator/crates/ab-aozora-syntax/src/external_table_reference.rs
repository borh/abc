//! Source-owned table-file instructions, without resource resolution.

/// Extract the literal ASCII filename from a complete supplied table-file instruction.
#[must_use]
pub fn external_table_filename(body: &str) -> Option<&str> {
    let filename = body
        .strip_prefix("ここに表組入る、別ファイル（")?
        .strip_suffix("）参照")?;
    (!filename.is_empty()
        && !matches!(filename, "." | "..")
        && filename
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'-' | b'.')))
    .then_some(filename)
}

#[cfg(test)]
mod tests {
    use super::external_table_filename;

    #[test]
    fn complete_instruction_retains_the_literal_filename() {
        assert_eq!(
            external_table_filename("ここに表組入る、別ファイル（densyanokonzatsu_table.txt）参照"),
            Some("densyanokonzatsu_table.txt")
        );
    }

    #[test]
    fn incomplete_or_ambiguous_instructions_are_not_references() {
        for body in [
            "ここに表組入る、別ファイル（）参照",
            "ここに表組入る、別ファイル（../table.txt）参照",
            "ここに表組入る、別ファイル（table.txt）参照か",
            "ここに表組入る、別ファイル（table.txt",
            "ここに表組入る、別ファイル（「table.txt」）参照",
            "ここに図入る、別ファイル（table.txt）参照",
        ] {
            assert_eq!(external_table_filename(body), None, "{body}");
        }
    }
}
