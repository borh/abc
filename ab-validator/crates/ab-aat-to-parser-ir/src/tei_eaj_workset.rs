use std::path::{Path, PathBuf};

use anyhow::{Context, Result};
use serde::Deserialize;

use crate::schema::read_json;

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajWorksetExport {
    pub(crate) schema_version: String,
    pub(crate) summary: TeiEajExportSummary,
    #[serde(default)]
    pub(crate) tei_eaj_source: Option<TeiEajSourceExport>,
    #[serde(default)]
    pub(crate) candidate_work_ids: Vec<String>,
    #[serde(default)]
    pub(crate) missing_abc_counterpart_work_ids: Vec<String>,
    #[serde(default)]
    pub(crate) no_work_id_files: Vec<String>,
    #[serde(default)]
    pub(crate) files: Vec<TeiEajFileExport>,
}

#[derive(Debug, Clone, Default, Deserialize)]
pub(crate) struct TeiEajExportSummary {
    #[serde(default)]
    pub(crate) tei_eaj_file_count: u64,
    #[serde(default)]
    pub(crate) tei_eaj_work_id_count: u64,
    #[serde(default)]
    pub(crate) compared_file_count: u64,
    #[serde(default)]
    pub(crate) missing_counterpart_count: u64,
    #[serde(default)]
    pub(crate) no_work_id_count: u64,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajSourceExport {
    pub(crate) revision: Option<String>,
    pub(crate) root: Option<String>,
}

#[derive(Debug, Clone, Deserialize)]
pub(crate) struct TeiEajFileExport {
    pub(crate) work_id: Option<String>,
    pub(crate) title: Option<String>,
    pub(crate) tei_eaj_file: String,
    pub(crate) level: Option<String>,
    pub(crate) state: Option<String>,
    pub(crate) comparison_status: String,
    pub(crate) abc_tei: Option<String>,
    pub(crate) tei_eaj_p_count: Option<u64>,
    pub(crate) tei_eaj_note_count: Option<u64>,
    pub(crate) abc_p_count: Option<u64>,
    pub(crate) abc_note_count: Option<u64>,
    pub(crate) base_text_equal: Option<bool>,
}

pub(crate) fn read_tei_eaj_workset(path: &Path) -> Result<TeiEajWorksetExport> {
    let workset_value = read_json(path)
        .with_context(|| format!("failed to read TEI-EAJ workset {}", path.display()))?;
    serde_json::from_value(workset_value)
        .with_context(|| format!("failed to parse TEI-EAJ workset {}", path.display()))
}

pub(crate) fn resolve_tei_eaj_path(workset: &TeiEajWorksetExport, relative: &str) -> PathBuf {
    let path = PathBuf::from(relative);
    if path.is_absolute() {
        return path;
    }
    workset
        .tei_eaj_source
        .as_ref()
        .and_then(|source| source.root.as_deref())
        .map_or(path.clone(), |root| Path::new(root).join(path))
}
