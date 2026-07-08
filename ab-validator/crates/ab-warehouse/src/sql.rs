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
