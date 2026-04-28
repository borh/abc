use std::path::Path;

use anyhow::{Result, bail};

pub fn run_analyze_aat(
    aat: Option<&Path>,
    aat_dir: Option<&Path>,
    analyzer_ids: &[String],
    _analyses_output: &Path,
    _comparisons_output: Option<&Path>,
) -> Result<()> {
    if aat.is_none() == aat_dir.is_none() {
        bail!("provide exactly one of --aat or --aat-dir");
    }
    if analyzer_ids.is_empty() {
        bail!("provide at least one --analyzer");
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use super::*;

    #[test]
    fn rejects_missing_input() {
        let err = run_analyze_aat(
            None,
            None,
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_both_input_modes() {
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            Some(Path::new("aat")),
            &["vibrato".to_owned()],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("exactly one"));
    }

    #[test]
    fn rejects_empty_analyzer_list() {
        let err = run_analyze_aat(
            Some(Path::new("a.json")),
            None,
            &[],
            Path::new("out.jsonl"),
            None,
        )
        .unwrap_err();
        assert!(err.to_string().contains("at least one"));
    }
}
