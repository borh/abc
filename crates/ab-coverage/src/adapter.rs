//! Spawn an adapter binary on a single work, capture its AAT JSON output.
//!
//! Adapters take raw work bytes (Shift-JIS or UTF-8) on stdin and emit AAT
//! JSON on stdout. Exit code 0 or 2 are both treated as success (matching
//! `crates/ab-check/src/check.rs`).

use std::{
    io::{Read, Write},
    path::{Path, PathBuf},
    process::{Command, Stdio},
    thread,
    time::{Duration, Instant},
};

use anyhow::{Context, Result, bail};

#[derive(Debug, Clone)]
pub struct AdapterBinary {
    pub parser_id: String,
    pub binary: PathBuf,
}

impl AdapterBinary {
    /// Builds an adapter launcher for a known parser ID.
    ///
    /// # Errors
    ///
    /// Returns an error if the parser ID is unknown.
    pub fn for_parser(repo_root: &Path, parser_id: &str) -> Result<Self> {
        let binary = match parser_id {
            "aozora2" => repo_root.join("adapters/aozora2/target/release/aozora2-adapter"),
            "aozora-rs" => repo_root.join("adapters/aozora-rs/target/release/aozora-rs-adapter"),
            "aozora2html" => repo_root.join("adapters/aozora2html/aozora2html-adapter"),
            "aozora-epub3" => repo_root.join("adapters/aozora-epub3/aozora-epub3-adapter"),
            other => bail!("unknown parser id: {other}"),
        };
        Ok(Self {
            parser_id: parser_id.to_string(),
            binary,
        })
    }

    /// Runs the configured adapter and returns captured AAT JSON output bytes.
    ///
    /// # Errors
    ///
    /// Returns an error if spawning the adapter fails, reading I/O fails, the
    /// adapter times out, or it exits with an unexpected status code.
    pub fn invoke(&self, source_bytes: &[u8], timeout: Duration) -> Result<Vec<u8>> {
        if !self.binary.exists() {
            bail!(
                "adapter binary missing for {}: {}",
                self.parser_id,
                self.binary.display()
            );
        }
        let mut child = Command::new(&self.binary)
            .arg("--mode")
            .arg("aat")
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
            .with_context(|| format!("spawn {}", self.binary.display()))?;

        let mut stdout = child
            .stdout
            .take()
            .context("adapter stdout was not piped")?;
        let mut stderr = child
            .stderr
            .take()
            .context("adapter stderr was not piped")?;
        let stdout_reader = thread::spawn(move || {
            let mut bytes = Vec::new();
            stdout.read_to_end(&mut bytes).map(|_| bytes)
        });
        let stderr_reader = thread::spawn(move || {
            let mut bytes = Vec::new();
            stderr.read_to_end(&mut bytes).map(|_| bytes)
        });

        {
            let mut stdin = child.stdin.take().context("adapter stdin was not piped")?;
            stdin
                .write_all(source_bytes)
                .context("write adapter stdin")?;
        }

        let start = Instant::now();
        loop {
            if let Some(status) = child.try_wait().context("wait adapter")? {
                let stdout = stdout_reader
                    .join()
                    .map_err(|_| anyhow::anyhow!("stdout reader panicked"))??;
                let _stderr = stderr_reader
                    .join()
                    .map_err(|_| anyhow::anyhow!("stderr reader panicked"))??;
                return match status.code() {
                    Some(0 | 2) => Ok(stdout),
                    other => bail!("adapter {} exited with status {:?}", self.parser_id, other),
                };
            }
            if start.elapsed() > timeout {
                let _ = child.kill();
                bail!("adapter {} timed out after {:?}", self.parser_id, timeout);
            }
            std::thread::sleep(Duration::from_millis(20));
        }
    }
}
