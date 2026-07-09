//! Work-metadata resolution: joins the warehouse `aozora_works` sidecar
//! (Task 3's `WorkRow`) against an optional ABC catalog export
//! (`works/<work_id>.json`, `persons/<person_id>.json`) to recover author
//! names and richer work fields (`first_published`, `ndc`, `card_url`, …).
//! Every field degrades independently: no catalog configured is not an
//! error, but a configured catalog missing a record is a nonfatal error
//! reported in the returned `Vec<String>`.

use std::path::Path;

use serde::Serialize;

use crate::hydrate::tables::WorkRow;

/// One contributor to a work (author, translator, …), resolved against
/// `persons/<person_id>.json` when possible.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Contributor {
    pub person_id: String,
    pub role: String,
    pub family_name: Option<String>,
    pub given_name: Option<String>,
}

/// Resolved work metadata: warehouse sidecar fields (always present when
/// `Some`) overlaid with ABC catalog fields when the catalog and its
/// records are available.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct WorkMeta {
    pub work_id: Option<String>,
    pub title: Option<String>,
    pub publication_year: Option<i32>,
    pub orthographic_style: Option<String>,
    pub first_published: Option<String>,
    pub ndc: Option<String>,
    pub card_url: Option<String>,
    pub contributors: Vec<Contributor>,
}

impl WorkMeta {
    /// Joined `family_name given_name` of every `role == "著者"`
    /// contributor, `、`-separated; falls back to `person:<id>` when both
    /// names are unresolved; empty string when there is no author
    /// contributor.
    #[allow(dead_code)]
    pub fn display_author(&self) -> String {
        self.contributors
            .iter()
            .filter(|contributor| contributor.role == "著者")
            .map(|contributor| {
                if contributor.family_name.is_none() && contributor.given_name.is_none() {
                    format!("person:{}", contributor.person_id)
                } else {
                    format!(
                        "{}{}",
                        contributor.family_name.as_deref().unwrap_or_default(),
                        contributor.given_name.as_deref().unwrap_or_default()
                    )
                }
            })
            .collect::<Vec<_>>()
            .join("、")
    }
}

/// Reads and parses `path` as JSON, returning `None` on any failure
/// (missing file, unreadable, or malformed JSON) — callers turn that into
/// the appropriate `*-missing` error entry.
fn load_json(path: &Path) -> Option<serde_json::Value> {
    let bytes = std::fs::read(path).ok()?;
    serde_json::from_slice(&bytes).ok()
}

fn str_field(value: &serde_json::Value, field: &str) -> Option<String> {
    value.get(field)?.as_str().map(str::to_owned)
}

/// Resolves work metadata for `work_row` (the warehouse `aozora_works`
/// sidecar row, if any), overlaying fields and contributors from the
/// optional ABC catalog export at `abc_catalog`.
///
/// - `work_row` is `None` ⇒ the sidecar has no row for this source: returns
///   `(None, ["works-sidecar-missing"])`.
/// - `abc_catalog` is `None` ⇒ no catalog configured: not an error; the
///   result is warehouse-only, with a synthetic single-author contributor
///   (bare `person:<id>`) when `author_person_id` is set.
/// - `abc_catalog` is `Some` but a `works/<id>.json` or `persons/<id>.json`
///   record is missing/unparseable ⇒ nonfatal error entry
///   (`work-record-missing: works/<id>.json` /
///   `person-record-missing: persons/<id>.json`); resolution degrades
///   field-by-field.
pub fn resolve_work_meta(
    work_row: Option<&WorkRow>,
    abc_catalog: Option<&Path>,
) -> (Option<WorkMeta>, Vec<String>) {
    let Some(work_row) = work_row else {
        return (None, vec!["works-sidecar-missing".to_owned()]);
    };

    let mut errors = Vec::new();
    let mut meta = WorkMeta {
        work_id: Some(work_row.work_id.clone()),
        title: Some(work_row.title.clone()),
        publication_year: work_row.publication_year,
        orthographic_style: work_row.orthographic_style.clone(),
        first_published: None,
        ndc: None,
        card_url: None,
        contributors: Vec::new(),
    };

    let mut record_available = false;
    let mut contributor_pairs: Vec<(String, String)> = Vec::new();

    if let Some(catalog) = abc_catalog {
        let work_path = catalog
            .join("works")
            .join(format!("{}.json", work_row.work_id));
        match load_json(&work_path) {
            Some(record) => {
                record_available = true;
                if let Some(work) = record.get("work") {
                    if let Some(title) = str_field(work, "title") {
                        meta.title = Some(title);
                    }
                    if let Some(first_published) = str_field(work, "first_published") {
                        meta.first_published = Some(first_published);
                    }
                    if let Some(ndc) = str_field(work, "ndc") {
                        meta.ndc = Some(ndc);
                    }
                    if let Some(card_url) = str_field(work, "card_url") {
                        meta.card_url = Some(card_url);
                    }
                    if let Some(orthographic_style) = str_field(work, "orthographic_style") {
                        meta.orthographic_style = Some(orthographic_style);
                    }
                }
                if let Some(contributors) = record.get("contributors").and_then(|v| v.as_array()) {
                    for contributor in contributors {
                        let person_id = str_field(contributor, "person_id");
                        let role = str_field(contributor, "relation_to_work");
                        if let (Some(person_id), Some(role)) = (person_id, role) {
                            contributor_pairs.push((person_id, role));
                        }
                    }
                }
            }
            None => {
                errors.push(format!(
                    "work-record-missing: works/{}.json",
                    work_row.work_id
                ));
            }
        }
    }

    if contributor_pairs.is_empty()
        && !record_available
        && let Some(author_person_id) = &work_row.author_person_id
    {
        contributor_pairs.push((author_person_id.clone(), "著者".to_owned()));
    }

    for (person_id, role) in contributor_pairs {
        let (family_name, given_name) = match abc_catalog {
            Some(catalog) => {
                let person_path = catalog.join("persons").join(format!("{person_id}.json"));
                match load_json(&person_path) {
                    Some(record) => (
                        str_field(&record, "family_name"),
                        str_field(&record, "given_name"),
                    ),
                    None => {
                        errors.push(format!("person-record-missing: persons/{person_id}.json"));
                        (None, None)
                    }
                }
            }
            None => (None, None),
        };
        meta.contributors.push(Contributor {
            person_id,
            role,
            family_name,
            given_name,
        });
    }

    (Some(meta), errors)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hydrate::tables::WorkRow;

    fn write_catalog(dir: &std::path::Path) {
        std::fs::create_dir_all(dir.join("works")).unwrap();
        std::fs::create_dir_all(dir.join("persons")).unwrap();
        std::fs::write(dir.join("works/000080.json"), serde_json::json!({
            "work": {"work_id": "000080", "title": "煙管", "first_published": "1916",
                     "ndc": "NDC 913", "card_url": "https://www.aozora.gr.jp/cards/000879/card80.html",
                     "orthographic_style": "新字新仮名"},
            "contributors": [{"person_id": "000879", "relation_to_work": "著者"}]
        }).to_string()).unwrap();
        std::fs::write(
            dir.join("persons/000879.json"),
            serde_json::json!({
                "person_id": "000879", "family_name": "芥川", "given_name": "竜之介"
            })
            .to_string(),
        )
        .unwrap();
    }

    fn work_row() -> WorkRow {
        WorkRow {
            work_id: "000080".to_owned(),
            title: "煙管".to_owned(),
            author_person_id: Some("000879".to_owned()),
            publication_year: Some(1916),
            orthographic_style: Some("新字新仮名".to_owned()),
        }
    }

    #[test]
    fn resolves_names_and_work_fields_from_catalog() {
        let dir = tempfile::tempdir().unwrap();
        write_catalog(dir.path());
        let (meta, errors) = resolve_work_meta(Some(&work_row()), Some(dir.path()));
        assert!(errors.is_empty());
        let meta = meta.unwrap();
        assert_eq!(meta.title.as_deref(), Some("煙管"));
        assert_eq!(
            meta.card_url.as_deref(),
            Some("https://www.aozora.gr.jp/cards/000879/card80.html")
        );
        assert_eq!(meta.contributors[0].family_name.as_deref(), Some("芥川"));
        assert_eq!(meta.display_author(), "芥川竜之介");
    }

    #[test]
    fn degrades_without_catalog_and_without_sidecar() {
        let (meta, errors) = resolve_work_meta(Some(&work_row()), None);
        let meta = meta.unwrap();
        // Warehouse-only: title/year/style survive; author is the bare id.
        assert_eq!(meta.display_author(), "person:000879");
        assert!(errors.is_empty()); // no catalog configured is not an error

        let (meta, errors) = resolve_work_meta(None, None);
        assert!(meta.is_none());
        assert_eq!(errors, vec!["works-sidecar-missing".to_owned()]);
    }

    #[test]
    fn missing_records_are_reported_but_nonfatal() {
        let dir = tempfile::tempdir().unwrap();
        std::fs::create_dir_all(dir.path().join("works")).unwrap();
        std::fs::create_dir_all(dir.path().join("persons")).unwrap();
        let (meta, errors) = resolve_work_meta(Some(&work_row()), Some(dir.path()));
        assert!(meta.is_some());
        assert!(errors.iter().any(|e| e.starts_with("work-record-missing")));
        assert!(
            errors
                .iter()
                .any(|e| e.starts_with("person-record-missing"))
        );
    }
}
