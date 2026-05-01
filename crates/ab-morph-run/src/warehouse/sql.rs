use std::fs;
use std::path::Path;

use anyhow::{Context, Result};

pub(crate) const SCHEMA_SQL: &str = include_str!("../../sql/schema.sql");
pub(crate) const MORPH_VIEWS_SQL_TEMPLATE: &str = include_str!("../../sql/morph_views.sql");

pub(crate) fn write_schema_sql(warehouse_dir: &Path) -> Result<()> {
    let path = warehouse_dir.join("schema.sql");
    fs::write(&path, SCHEMA_SQL).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

pub(crate) fn write_run_views_sql(output_run_dir: &Path, final_run_dir: &Path) -> Result<()> {
    let final_run_dir = final_run_dir.canonicalize().unwrap_or_else(|_| {
        if final_run_dir.is_absolute() {
            final_run_dir.to_path_buf()
        } else {
            std::env::current_dir()
                .expect("current directory is available")
                .join(final_run_dir)
        }
    });
    let final_run_dir = final_run_dir
        .to_string_lossy()
        .replace('\\', "\\\\")
        .replace('\'', "''");
    let views = MORPH_VIEWS_SQL_TEMPLATE.replace("__RUN_DIR__", &final_run_dir);
    let path = output_run_dir.join("views.sql");
    fs::write(&path, views).with_context(|| format!("failed to write {}", path.display()))?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn sql_mentions_parquet_not_jsonl_and_has_version_policy() {
        assert!(SCHEMA_SQL.contains("Readers must reject runs.schema_version values other than 1"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("__RUN_DIR__/nway_regions.parquet"));
        assert!(MORPH_VIEWS_SQL_TEMPLATE.contains("struct_pack"));
        assert!(!MORPH_VIEWS_SQL_TEMPLATE.contains("jsonl"));
    }

    #[test]
    fn schema_sql_mentions_every_documented_column() {
        for table in crate::warehouse::schema::WarehouseTable::ALL {
            for column in table.column_names() {
                assert!(
                    SCHEMA_SQL.contains(column),
                    "schema.sql does not mention {:?}.{}",
                    table,
                    column
                );
            }
        }
    }
}
