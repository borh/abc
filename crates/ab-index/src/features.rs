use std::{
    collections::HashMap,
    fs,
    path::{Component, Path},
};

use anyhow::{Context, Result};
use regex::Regex;
use serde::Deserialize;

#[derive(Debug)]
pub struct FeatureDetector {
    features: Vec<FeaturePattern>,
}

#[derive(Debug)]
struct FeaturePattern {
    name: String,
    regex: Regex,
}

#[derive(Debug, Deserialize)]
struct FeatureFile {
    features: HashMap<String, FeatureSpec>,
}

#[derive(Debug, Deserialize)]
struct FeatureSpec {
    pattern: String,
    #[allow(dead_code)]
    description: String,
}

impl FeatureDetector {
    /// Creates a detector from a TOML feature pattern file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read, parsed, or if any regex in the
    /// file is invalid.
    pub fn from_toml(path: &Path) -> Result<Self> {
        let raw = fs::read_to_string(path)
            .with_context(|| format!("failed to read feature patterns {}", path.display()))?;
        let parsed: FeatureFile = toml::from_str(&raw)
            .with_context(|| format!("failed to parse feature patterns {}", path.display()))?;

        let mut features = parsed
            .features
            .into_iter()
            .map(|(name, spec)| {
                let regex = Regex::new(&spec.pattern)
                    .with_context(|| format!("invalid regex for feature {name}"))?;
                Ok(FeaturePattern { name, regex })
            })
            .collect::<Result<Vec<_>>>()?;
        features.sort_by(|a, b| a.name.cmp(&b.name));
        Ok(Self { features })
    }

    #[must_use]
    pub fn detect(&self, text: &str) -> HashMap<String, Vec<usize>> {
        let mut detected = HashMap::new();
        for feature in &self.features {
            let mut lines = Vec::new();
            for (idx, line) in text.lines().enumerate() {
                if feature.regex.is_match(line) {
                    lines.push(idx + 1);
                }
            }
            if !lines.is_empty() {
                detected.insert(feature.name.clone(), lines);
            }
        }
        detected
    }

    #[must_use]
    pub fn feature_names(&self) -> Vec<&str> {
        self.features
            .iter()
            .map(|feature| feature.name.as_str())
            .collect()
    }
}

#[must_use]
pub fn normalize_relative_path(path: &Path) -> String {
    path.components()
        .filter_map(|component| match component {
            Component::Normal(part) => Some(part.to_string_lossy().into_owned()),
            Component::CurDir => None,
            Component::ParentDir => Some("..".to_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/")
}

#[cfg(test)]
mod tests {
    use std::{fs, path::PathBuf};

    use super::*;

    #[test]
    fn detects_feature_lines() {
        let path = temp_file("feature-patterns-test.toml");
        fs::write(
            &path,
        "\n[features.ruby]\npattern = '《[^》]+》'\ndescription = 'Ruby'\n",
        )
        .unwrap();

        let detector = FeatureDetector::from_toml(&path).unwrap();
        let detected = detector.detect("no\n吾輩《わがはい》\n猫《ねこ》");
        assert_eq!(detected["ruby"], vec![2, 3]);
        let _ = fs::remove_file(path);
    }

    #[test]
    fn normalizes_to_posix_separators() {
        let path = Path::new("cards")
            .join("000001")
            .join("files")
            .join("x.txt");
        assert_eq!(normalize_relative_path(&path), "cards/000001/files/x.txt");
    }

    fn temp_file(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!("ab-index-{name}-{}", std::process::id()))
    }
}
