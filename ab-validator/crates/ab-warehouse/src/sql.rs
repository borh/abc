use std::fs;
use std::path::Path;

use anyhow::{Context, Result};

use crate::schema::WarehouseTable;

pub const SCHEMA_SQL: &str = include_str!("../sql/schema.sql");
pub const MORPH_VIEWS_SQL_TEMPLATE: &str = include_str!("../sql/morph_views.sql");

/// # Errors
///
/// Returns an error if the schema file cannot be written.
pub fn write_schema_sql(warehouse_dir: &Path) -> Result<()> {
    let path = warehouse_dir.join("schema.sql");
    fs::write(&path, SCHEMA_SQL).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

/// Writes a rendered SQL view definition file for a run.
///
/// # Errors
///
/// Returns an error if canonicalization or SQL rendering fails, or if the output
/// file cannot be written.
pub fn write_run_views_sql(output_run_dir: &Path, final_run_dir: &Path) -> Result<()> {
    let final_run_dir = final_run_dir.canonicalize().or_else(|_| {
        if final_run_dir.is_absolute() {
            Ok(final_run_dir.to_path_buf())
        } else {
            std::env::current_dir().map(|cwd| cwd.join(final_run_dir))
        }
    })?;
    let final_run_dir = final_run_dir
        .to_string_lossy()
        .replace('\\', "\\\\")
        .replace('\'', "''");
    let mut views = MORPH_VIEWS_SQL_TEMPLATE.replace("__RUN_DIR__", &final_run_dir);
    if !output_run_dir
        .join(WarehouseTable::NwayFeatureDiffs.file_name())
        .exists()
    {
        views = remove_marked_sql_sections(
            &views,
            "-- __RAW_FEATURE_DIFFS_BEGIN__",
            "-- __RAW_FEATURE_DIFFS_END__",
        );
    }
    if !output_run_dir
        .join(WarehouseTable::ProjectionSpans.file_name())
        .exists()
    {
        views = remove_marked_sql_sections(
            &views,
            "-- __PROJECTION_SPANS_BEGIN__",
            "-- __PROJECTION_SPANS_END__",
        );
    }
    if !output_run_dir
        .join(WarehouseTable::NwayRegionOracleEvidence.file_name())
        .exists()
    {
        views = remove_marked_sql_sections(
            &views,
            "-- __ORACLE_EVIDENCE_BEGIN__",
            "-- __ORACLE_EVIDENCE_END__",
        );
    }
    views = views
        .replace("-- __RAW_FEATURE_DIFFS_BEGIN__\n", "")
        .replace("-- __RAW_FEATURE_DIFFS_END__\n", "")
        .replace("-- __PROJECTION_SPANS_BEGIN__\n", "")
        .replace("-- __PROJECTION_SPANS_END__\n", "")
        .replace("-- __ORACLE_EVIDENCE_BEGIN__\n", "")
        .replace("-- __ORACLE_EVIDENCE_END__\n", "");
    let path = output_run_dir.join("views.sql");
    fs::write(&path, views).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

fn remove_marked_sql_sections(sql: &str, begin: &str, end: &str) -> String {
    let mut remaining = sql;
    let mut output = String::new();
    while let Some(begin_index) = remaining.find(begin) {
        output.push_str(&remaining[..begin_index]);
        let after_begin = &remaining[begin_index + begin.len()..];
        let Some(end_index) = after_begin.find(end) else {
            output.push_str(&remaining[begin_index..]);
            return output;
        };
        let after_end = &after_begin[end_index + end.len()..];
        remaining = after_end.strip_prefix('\n').unwrap_or(after_end);
    }
    output.push_str(remaining);
    output
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sql_mentions_parquet_not_jsonl_and_has_version_policy() {
        assert!(SCHEMA_SQL.contains(&format!(
            "Morph warehouse schema version {}.",
            crate::schema::SCHEMA_VERSION
        )));
        assert!(SCHEMA_SQL.contains(
            "Readers must reject runs.schema_version values greater than the reader's supported maximum (3)."
        ));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains(&format!(
            "WHERE schema_version <= {}",
            crate::schema::SCHEMA_VERSION
        )));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("__RUN_DIR__/projection_spans.parquet"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("__RUN_DIR__/nway_regions.parquet"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("has_segmentation_disagreement"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("struct_pack"));
        assert!(!MORPH_VIEWS_SQL_TEMPLATE.contains("jsonl"));
    }

    #[test]
    fn schema_sql_columns_match_documented_parquet_columns() {
        for table in crate::schema::WarehouseTable::ALL {
            let table_name = table
                .file_name()
                .strip_suffix(".parquet")
                .expect("warehouse table file has parquet suffix");
            assert_eq!(
                schema_sql_columns(table_name),
                table.column_names(),
                "schema.sql column mismatch for {table:?}"
            );
        }
    }

    #[test]
    fn views_sql_drops_projection_spans_section_when_table_absent() {
        let dir = std::env::temp_dir().join(format!("views-spans-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(!views.contains("warehouse_projection_spans"));
        assert!(!views.contains("__PROJECTION_SPANS_BEGIN__"));

        fs::write(
            dir.join(WarehouseTable::ProjectionSpans.file_name()),
            b"stub",
        )
        .unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(views.contains("warehouse_projection_spans"));
        assert!(!views.contains("__PROJECTION_SPANS_BEGIN__"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn views_sql_drops_oracle_evidence_section_when_table_absent() {
        let dir = std::env::temp_dir().join(format!("views-oracle-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(!views.contains("warehouse_nway_region_oracle_evidence"));
        assert!(!views.contains("__ORACLE_EVIDENCE_BEGIN__"));

        fs::write(
            dir.join(WarehouseTable::NwayRegionOracleEvidence.file_name()),
            b"stub",
        )
        .unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(views.contains("warehouse_nway_region_oracle_evidence"));
        assert!(!views.contains("__ORACLE_EVIDENCE_BEGIN__"));
        let _ = fs::remove_dir_all(dir);
    }

    /// End-to-end check that the canonical `warehouse_nway_feature_diffs`
    /// view  re-expands a collapsed `analyzers` list column
    /// back into one row per analyzer with a scalar `analyzer_id`, matching
    /// the pre-collapse (schema v2) per-analyzer row shape byte-for-byte.
    /// Skips (rather than fails) if the `duckdb` binary is unavailable in
    /// this environment, mirroring `ab-morph-run`'s `run_duckdb_statement`.
    #[test]
    fn warehouse_nway_feature_diffs_view_unnests_collapsed_analyzers() {
        use crate::schema::{NwayFeatureDiffRow, NwayRegionRow, RunRow, WarehousePaths};
        use crate::writer::WarehouseWriter;

        let root = std::env::temp_dir().join(format!(
            "ab-warehouse-view-unnest-{}-{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_nanos()
        ));
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_runs(&[RunRow {
                schema_version: crate::schema::SCHEMA_VERSION,
                run_id: "run-a".to_owned(),
                created_at_utc: "2026-05-01T00:00:00Z".to_owned(),
                input_mode: "aat_dir".to_owned(),
                input_path: "scratch/aats".to_owned(),
                source_count: 1,
                analyzer_count: 2,
                error_count: 0,
                ortho_detect_mode: "off".to_owned(),
                input_normalization_detector_id: None,
                input_normalization_policy_hash: "sha256:identity".to_owned(),
            }])
            .unwrap();
        writer
            .append_nway_regions(&[NwayRegionRow {
                run_id: "run-a".into(),
                source_id: "source-a".into(),
                text_id: "work-a".into(),
                region_index: 0,
                byte_start: 0,
                byte_end: 6,
                char_start: 0,
                char_end: 2,
                is_nonempty_whitespace: false,
                is_agreement: false,
                has_coverage_mismatch: false,
                has_segmentation_disagreement: false,
                has_feature_disagreement: true,
            }])
            .unwrap();
        writer
            .append_nway_feature_diffs(&[NwayFeatureDiffRow {
                run_id: "run-a".into(),
                source_id: "source-a".into(),
                text_id: "work-a".into(),
                region_index: 0,
                feature_key: "pos1".into(),
                scope_type: "whole_region".into(),
                scope_position: None,
                scope_surface: None,
                feature_value: Some("名詞".into()),
                analyzers: vec!["sudachi-c".into(), "vibrato".into()],
            }])
            .unwrap();
        writer.finalize().unwrap();

        // `finalize` already renders `views.sql` (via `write_run_views_sql`)
        // into the finalized run directory.
        let views_sql = fs::read_to_string(paths.final_dir.join("views.sql")).unwrap();
        let query = format!(
            "{views_sql}\nCOPY (SELECT feature_value, analyzer_id FROM warehouse_nway_feature_diffs ORDER BY analyzer_id) TO STDOUT (HEADER, DELIMITER '\\t');"
        );
        let output = match std::process::Command::new("duckdb")
            .arg("-c")
            .arg(&query)
            .output()
        {
            Ok(output) => output,
            Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                let _ = fs::remove_dir_all(&root);
                eprintln!("skipping: duckdb binary not found");
                return;
            }
            Err(error) => panic!("failed to run duckdb: {error}"),
        };
        assert!(
            output.status.success(),
            "duckdb failed: stderr={}",
            String::from_utf8_lossy(&output.stderr)
        );
        let stdout = String::from_utf8_lossy(&output.stdout);
        let lines: Vec<&str> = stdout.lines().collect();
        assert_eq!(lines[0], "feature_value\tanalyzer_id");
        assert_eq!(&lines[1..], &["名詞\tsudachi-c", "名詞\tvibrato"]);

        let _ = fs::remove_dir_all(&root);
    }

    fn schema_sql_columns(table_name: &str) -> Vec<&str> {
        let start = format!("CREATE TABLE {table_name} (");
        let (_, rest) = SCHEMA_SQL
            .split_once(&start)
            .unwrap_or_else(|| panic!("schema.sql is missing CREATE TABLE {table_name}"));
        let (body, _) = rest
            .split_once("\n);")
            .unwrap_or_else(|| panic!("schema.sql table {table_name} has no closing );"));
        body.lines()
            .map(str::trim)
            .filter(|line| !line.is_empty())
            .map(|line| {
                line.trim_end_matches(',')
                    .split_whitespace()
                    .next()
                    .expect("column line has a name")
            })
            .collect()
    }
}
