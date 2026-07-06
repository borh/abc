use std::io::Write;
use std::path::Path;
use std::process::Command;

use anyhow::{Context, Result, bail};

use super::{
    NwayPatternKind, WAREHOUSE_CORE_FEATURE_KEYS, WarehouseFeatureProfile,
    WarehousePatternExampleOptions, WarehousePatternOptions,
};

/// Writes N-way pattern summaries from DuckDB output.
///
/// # Errors
///
/// Returns an error when input rows cannot be serialized or the writer cannot be
/// flushed.
pub fn write_warehouse_nway_patterns_duckdb_tsv<W: Write>(
    run_dir: &Path,
    options: &WarehousePatternOptions,
    mut writer: W,
) -> Result<bool> {
    if super::warehouse::warehouse_feature_pattern_counts_available(run_dir, options) {
        let sql = super::warehouse::warehouse_feature_pattern_counts_duckdb_sql(run_dir, options);
        return run_duckdb_tsv(
            run_dir,
            &sql,
            &mut writer,
            "warehouse feature pattern counts",
        );
    }
    if options.kind == NwayPatternKind::Feature
        && options.feature_profile == WarehouseFeatureProfile::Core
        && options.feature_key.is_none()
    {
        return write_warehouse_core_patterns_duckdb_tsv(run_dir, options, &mut writer);
    }
    let sql = super::warehouse::warehouse_pattern_duckdb_sql(run_dir, options);
    run_duckdb_tsv(run_dir, &sql, &mut writer, "warehouse pattern summary")
}

fn write_warehouse_core_patterns_duckdb_tsv<W: Write>(
    run_dir: &Path,
    options: &WarehousePatternOptions,
    writer: &mut W,
) -> Result<bool> {
    let mut outputs = Vec::new();
    for feature_key in WAREHOUSE_CORE_FEATURE_KEYS {
        let mut key_options = options.clone();
        key_options.feature_key = Some((*feature_key).to_owned());
        let sql = super::warehouse::warehouse_pattern_duckdb_sql(run_dir, &key_options);
        let mut output = Vec::new();
        if !run_duckdb_tsv(run_dir, &sql, &mut output, "warehouse core pattern summary")? {
            return Ok(false);
        }
        outputs.push(String::from_utf8(output).context("duckdb emitted non-UTF8 TSV")?);
    }
    write_merged_pattern_tsv(outputs.iter().map(String::as_str), options.limit, writer)
        .context("failed to merge core feature pattern summaries")?;
    Ok(true)
}

/// Writes warehouse region-level rows to DuckDB TSV output.
///
/// # Errors
///
/// Returns an error when warehouse region facts cannot be queried.
pub fn write_warehouse_regions_duckdb_tsv<W: Write>(
    run_dir: &Path,
    options: &super::WarehouseRegionOptions,
    mut writer: W,
) -> Result<bool> {
    let sql = super::warehouse::warehouse_region_examples_duckdb_sql(run_dir, options);
    run_duckdb_tsv(run_dir, &sql, &mut writer, "warehouse region examples")
}

/// Writes pattern example summaries from DuckDB output.
///
/// # Errors
///
/// Returns an error when the warehouse region patterns cannot be queried.
pub fn write_warehouse_pattern_examples_duckdb_tsv<W: Write>(
    run_dir: &Path,
    options: &WarehousePatternExampleOptions,
    mut writer: W,
) -> Result<bool> {
    let sql = super::warehouse::warehouse_pattern_examples_duckdb_sql(run_dir, options);
    run_duckdb_tsv(run_dir, &sql, &mut writer, "warehouse pattern examples")
}

/// Merges sorted TSV chunks produced by DuckDB pattern queries.
pub(crate) fn write_merged_pattern_tsv<'a, W: Write>(
    chunks: impl IntoIterator<Item = &'a str>,
    limit: usize,
    writer: &mut W,
) -> Result<()> {
    let mut header = None::<&str>;
    let mut rows = Vec::<(u64, &str)>::new();
    for chunk in chunks {
        let mut lines = chunk.lines();
        if header.is_none() {
            header = lines.next();
        } else {
            let _ = lines.next();
        }
        for line in lines {
            if line.trim().is_empty() {
                continue;
            }
            let examples = line
                .split('\t')
                .nth(1)
                .and_then(|value| value.parse::<u64>().ok())
                .unwrap_or(0);
            rows.push((examples, line));
        }
    }
    rows.sort_by(|left, right| right.0.cmp(&left.0).then_with(|| left.1.cmp(right.1)));
    if let Some(header) = header {
        writeln!(writer, "{header}")?;
    }
    for (_, row) in rows.into_iter().take(limit) {
        writeln!(writer, "{row}")?;
    }
    Ok(())
}

pub(crate) fn run_duckdb_tsv<W: Write>(
    run_dir: &Path,
    sql: &str,
    writer: &mut W,
    context: &str,
) -> Result<bool> {
    let duckdb_bin = std::env::var("AB_DUCKDB_BIN")
        .unwrap_or_else(|_| std::env::var("DUCKDB").unwrap_or_else(|_| String::from("duckdb")));

    std::fs::create_dir_all(super::warehouse::duckdb_temp_dir(run_dir)).with_context(|| {
        format!(
            "failed to create DuckDB temp directory for {}",
            run_dir.display()
        )
    })?;
    let output = match Command::new(&duckdb_bin).arg("-c").arg(sql).output() {
        Ok(output) => output,
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => return Ok(false),
        Err(error) => return Err(error).context("failed to run duckdb"),
    };
    if !output.status.success() {
        bail!(
            "duckdb {context} failed with status {}: stderr={} stdout={}",
            output.status,
            String::from_utf8_lossy(&output.stderr),
            String::from_utf8_lossy(&output.stdout)
        );
    }
    writer
        .write_all(&output.stdout)
        .with_context(|| format!("failed to write duckdb {context}"))?;
    Ok(true)
}
