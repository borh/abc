use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::{Path, PathBuf};

use anyhow::{Context, Result, bail};

use crate::compact::source_id_from_aat_path;

pub fn resolve_source_id_aat_paths(aat_dir: &Path, source_ids: &[String]) -> Result<Vec<PathBuf>> {
    if source_ids.is_empty() {
        bail!("provide at least one --source-id");
    }
    if !aat_dir.is_dir() {
        bail!(
            "--aat-dir must point to an existing directory: {}",
            aat_dir.display()
        );
    }

    let requested = source_ids.iter().cloned().collect::<BTreeSet<_>>();
    let mut discovered = BTreeMap::<String, PathBuf>::new();
    collect_matching_json(aat_dir, &requested, &mut discovered)?;

    let mut paths = Vec::new();
    for source_id in source_ids {
        let Some(path) = discovered.get(source_id) else {
            bail!("missing source_id `{source_id}` under {}", aat_dir.display());
        };
        paths.push(path.clone());
    }
    Ok(paths)
}

fn collect_matching_json(
    dir: &Path,
    requested: &BTreeSet<String>,
    discovered: &mut BTreeMap<String, PathBuf>,
) -> Result<()> {
    for entry in fs::read_dir(dir).with_context(|| format!("failed to read {}", dir.display()))? {
        let path = entry?.path();
        if path.is_dir() {
            collect_matching_json(&path, requested, discovered)?;
        } else if path.extension().and_then(|ext| ext.to_str()) == Some("json") {
            let source_id = source_id_from_aat_path(&path);
            if requested.contains(&source_id)
                && discovered.insert(source_id.clone(), path).is_some()
            {
                bail!("duplicate source_id `{source_id}` under {}", dir.display());
            }
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::fs;
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn resolves_source_ids_recursively_in_requested_order() {
        let dir = temp_dir("resolve");
        let nested = dir.join("aozora-rs-adapter");
        fs::create_dir_all(&nested).unwrap();
        fs::write(nested.join("src-b.json"), "{}").unwrap();
        fs::write(nested.join("src-a.json"), "{}").unwrap();

        let paths =
            resolve_source_id_aat_paths(&dir, &["src-a".to_owned(), "src-b".to_owned()]).unwrap();

        assert_eq!(paths.len(), 2);
        assert_eq!(source_id_from_aat_path(&paths[0]), "src-a");
        assert_eq!(source_id_from_aat_path(&paths[1]), "src-b");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn duplicate_source_id_is_an_error() {
        let dir = temp_dir("duplicate");
        let one = dir.join("one");
        let two = dir.join("two");
        fs::create_dir_all(&one).unwrap();
        fs::create_dir_all(&two).unwrap();
        fs::write(one.join("same.json"), "{}").unwrap();
        fs::write(two.join("same.json"), "{}").unwrap();

        let error = resolve_source_id_aat_paths(&dir, &["same".to_owned()])
            .unwrap_err()
            .to_string();

        assert!(error.contains("duplicate source_id"));
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn missing_source_id_is_an_error() {
        let dir = temp_dir("missing");
        fs::create_dir_all(&dir).unwrap();

        let error = resolve_source_id_aat_paths(&dir, &["missing".to_owned()])
            .unwrap_err()
            .to_string();

        assert!(error.contains("missing source_id"));
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-select-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
