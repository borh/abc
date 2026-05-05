use std::{
    io::Write,
    path::PathBuf,
    process::{Command, Stdio},
};

use anyhow::{Context, Result, bail};
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdapterSpec {
    pub id: String,
    pub command: PathBuf,
}

pub fn parse_adapter_spec(spec: &str) -> AdapterSpec {
    if let Some((id, command)) = spec.split_once('=') {
        return AdapterSpec {
            id: id.to_owned(),
            command: PathBuf::from(command),
        };
    }

    let command = PathBuf::from(spec);
    let id = command
        .file_name()
        .and_then(|name| name.to_str())
        .map(adapter_id_from_binary_name)
        .unwrap_or_else(|| spec.to_owned());
    AdapterSpec { id, command }
}

/// Invoke an adapter binary in AAT mode and parse its JSON output.
///
/// # Errors
///
/// Returns an error when the adapter cannot be started, when write/IO fails,
/// when process exit status is non-zero, or when output is not valid JSON.
pub fn run_adapter_aat(adapter: &AdapterSpec, source_utf8: &str, work_id: &str) -> Result<Value> {
    let mut child = Command::new(&adapter.command)
        .arg("--mode")
        .arg("aat")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .with_context(|| format!("failed to spawn adapter {}", adapter.command.display()))?;

    {
        let mut stdin = child.stdin.take().context("adapter stdin was not piped")?;
        stdin
            .write_all(source_utf8.as_bytes())
            .context("failed to write source to adapter")?;
    }

    let output = child
        .wait_with_output()
        .context("failed to wait for adapter")?;
    if !output.status.success() {
        bail!(
            "adapter {} failed: {}",
            adapter.command.display(),
            String::from_utf8_lossy(&output.stderr)
        );
    }

    let mut aat: Value = serde_json::from_slice(&output.stdout).with_context(|| {
        format!(
            "adapter {} did not emit valid JSON",
            adapter.command.display()
        )
    })?;
    if let Some(root) = aat.as_object_mut() {
        root.insert("work_id".to_owned(), Value::String(work_id.to_owned()));
    }
    Ok(aat)
}

fn adapter_id_from_binary_name(name: &str) -> String {
    name.strip_suffix("-adapter").unwrap_or(name).to_owned()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_adapter_id_from_binary_name() {
        let temp = std::env::temp_dir().join("aozora2-adapter");
        let temp_path = temp.to_string_lossy().to_string();
        let spec = parse_adapter_spec(&temp_path);

        assert_eq!(spec.id, "aozora2");
        assert_eq!(spec.command, temp);
    }

    #[test]
    fn parses_explicit_adapter_id() {
        let parser = std::env::temp_dir().join("parser");
        let parser_path = parser.to_string_lossy().to_string();
        let spec = parse_adapter_spec(&format!("custom={parser_path}"));

        assert_eq!(spec.id, "custom");
        assert_eq!(spec.command, parser);
    }
}
