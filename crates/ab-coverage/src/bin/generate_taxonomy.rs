//! Taxonomy generator for Aozora Bunko parser features.
//!
//! Reads canonical sources (`chuki_tag.txt` and `annotation/*.html`) at runtime,
//! produces a generated feature-taxonomy table, and verifies it against the
//! hand-written §0 of `PARSER_REPORT.md`.

use anyhow::{Context, Result};
use clap::Parser;
use regex::Regex;
use std::collections::{HashMap, HashSet};
use std::fs::{self, File};
use std::io::{BufRead, BufReader};
use std::path::{Path, PathBuf};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Verdict {
    Verified,
    ChukiOnly,
    ManualOnly,
    Unverified,
    NoMarker,
}

impl std::fmt::Display for Verdict {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Verdict::Verified => "VERIFIED",
            Verdict::ChukiOnly => "CHUKI-ONLY",
            Verdict::ManualOnly => "MANUAL-ONLY",
            Verdict::Unverified => "UNVERIFIED",
            Verdict::NoMarker => "NO-MARKER",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
struct Feature {
    id: String,
    family: String,
    name: String,
    marker_examples: Vec<String>,
    sources: Vec<String>,
    verdict: Verdict,
}

struct FeatureBuilder {
    family: String,
    name: String,
    marker_examples: Vec<String>,
    chuki_lines: Vec<usize>,
    manual_pages: Vec<String>,
}

fn family_rank(family: &str) -> usize {
    match family {
        "Layout" => 0,
        "Headings" => 1,
        "Gaiji" => 2,
        "Kunten" => 3,
        "Emphasis" => 4,
        "Graphics" => 5,
        "Other" => 6,
        _ => 7,
    }
}

fn family_prefix(family: &str) -> &'static str {
    match family {
        "Layout" => "L",
        "Headings" => "H",
        "Gaiji" => "E",
        "Kunten" => "K",
        "Emphasis" => "M",
        "Graphics" => "G",
        "Other" => "O",
        _ => "X",
    }
}

fn section_title(family: &str) -> &str {
    match family {
        "Layout" => "Layout / レイアウト",
        "Headings" => "Headings / 見出し",
        "Gaiji" => "External Characters / 外字",
        "Kunten" => "Kunten / 訓点",
        "Emphasis" => "Emphasis / 強調",
        "Graphics" => "Graphics / 画像",
        "Other" => "Other / その他",
        _ => family,
    }
}

/// Normalize a marker string.
///
/// * Concrete quoted content (`「...」` or `"..."`) becomes `「○○」`.
/// * Runs of ASCII/fullwidth digits become the placeholder `N`.
/// * The fullwidth Latin `Ｎ` placeholder is collapsed to `N`.
fn normalize(s: &str) -> String {
    let trimmed = s.trim();

    // Mask concrete quoted content.
    let mut masked = String::with_capacity(trimmed.len());
    let mut chars = trimmed.chars();
    while let Some(c) = chars.next() {
        if c == '「' {
            masked.push('「');
            masked.push_str("○○");
            for x in chars.by_ref() {
                if x == '」' {
                    masked.push('」');
                    break;
                }
            }
        } else if c == '"' {
            masked.push_str("「○○」");
            for x in chars.by_ref() {
                if x == '"' {
                    break;
                }
            }
        } else {
            masked.push(c);
        }
    }

    let digit_re = Regex::new(r"[0-9０-９]+").expect("constant regex");
    let mut out = digit_re.replace_all(&masked, "N").to_string();
    out = out.replace('Ｎ', "N");
    out
}

fn extract_header_text(line: &str) -> Option<&str> {
    let start = line.find("####")?;
    let end = line.rfind("####")?;
    if end <= start + 3 {
        return None;
    }
    let text = line[start + 4..end].trim();
    if text.is_empty() {
        return None;
    }
    Some(text)
}

fn family_from_header(text: &str) -> String {
    let t = text.trim();
    if t.contains("改ページ") || t.contains("左右中央") || t.contains("字下げ") {
        "Layout".to_string()
    } else if t.contains("見出し") {
        "Headings".to_string()
    } else if t.contains("外字") {
        "Gaiji".to_string()
    } else if t.contains("訓点") || t.contains("返り点") {
        "Kunten".to_string()
    } else if t.contains("強調") {
        "Emphasis".to_string()
    } else if t.contains("画像") {
        "Graphics".to_string()
    } else if t.contains("その他") || t.contains("番外") {
        "Other".to_string()
    } else {
        "Other".to_string()
    }
}

/// Parse `chuki_tag.txt`.
///
/// Returns `(family, marker_name, line_number)` for each marker row, tracking
/// `#### ... ####` category headers as the current family.
fn parse_chuki_tag(path: impl AsRef<Path>) -> Result<Vec<(String, String, usize)>> {
    let file = File::open(path).context("opening chuki_tag.txt")?;
    let reader = BufReader::new(file);
    let mut family = "Other".to_string();
    let mut out = Vec::new();
    for (i, line) in reader.lines().enumerate() {
        let line = line.context("reading chuki_tag.txt")?;
        if line.trim().is_empty() {
            continue;
        }
        if let Some(header_text) = extract_header_text(&line) {
            family = family_from_header(header_text);
            continue;
        }
        if line.starts_with('#') {
            continue;
        }
        let name = line.split('\t').next().unwrap_or("").trim().to_string();
        if name.is_empty() {
            continue;
        }
        out.push((family.clone(), name, i + 1));
    }
    Ok(out)
}

fn page_family(page: &str) -> String {
    match page {
        "layout_1" | "layout_2" | "layout_3" => "Layout",
        "heading" => "Headings",
        "external_character" => "Gaiji",
        "kunten" => "Kunten",
        "emphasis" => "Emphasis",
        "graphics" => "Graphics",
        _ => "Other",
    }
    .to_string()
}

fn family_from_marker(marker: &str) -> String {
    // Prefer the semantics of the marker itself, falling back to the page family.
    if marker.contains("見出し") {
        "Headings".to_string()
    } else if marker.contains("外字") || marker.contains("二の字点") {
        "Gaiji".to_string()
    } else if marker.contains("訓点")
        || marker.contains("返り点")
        || marker.contains("送り仮名")
        || marker.contains("再読")
    {
        "Kunten".to_string()
    } else if marker.contains("傍点")
        || marker.contains("傍線")
        || marker.contains("取消線")
        || marker.contains("鎖線")
        || marker.contains("破線")
        || marker.contains("波線")
        || marker.contains("太字")
        || marker.contains("斜体")
        || marker.contains("ゴシック")
    {
        "Emphasis".to_string()
    } else if marker.contains("図") || marker.contains("キャプション") || marker.contains("挿絵")
    {
        "Graphics".to_string()
    } else if marker.contains("字下げ")
        || marker.contains("改ページ")
        || marker.contains("改丁")
        || marker.contains("改段")
        || marker.contains("改見開き")
        || marker.contains("左右中央")
        || marker.contains("地付")
        || marker.contains("字上げ")
        || marker.contains("字詰め")
        || marker.contains("段組")
        || marker.contains("中央")
        || marker.contains("横組")
        || marker.contains("横書")
    {
        "Layout".to_string()
    } else {
        "Other".to_string()
    }
}

/// Parse the annotation/`*.html` directory.
///
/// Returns a map from normalized marker to the list of page filenames where it
/// appears. HTML tags are stripped before marker extraction.
fn parse_manual_annotation(dir: impl AsRef<Path>) -> Result<HashMap<String, Vec<String>>> {
    let marker_re = Regex::new(r"［＃([^］]*)］").expect("constant regex");
    let html_re = Regex::new(r"<[^>]*>").expect("constant regex");

    let mut entries: Vec<_> = fs::read_dir(dir.as_ref())
        .context("reading annotation directory")?
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.file_name());

    let mut map: HashMap<String, Vec<String>> = HashMap::new();
    for entry in entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("html") {
            continue;
        }
        let page = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        let raw =
            fs::read_to_string(&path).with_context(|| format!("reading {}", path.display()))?;
        let text = html_re.replace_all(&raw, "");
        for cap in marker_re.captures_iter(&text) {
            let inner = &cap[1];
            let marker = format!("［＃{}］", inner);
            let norm = normalize(&marker);
            if norm.is_empty() {
                continue;
            }
            let pages = map.entry(norm).or_default();
            if !pages.contains(&page) {
                pages.push(page.clone());
            }
        }
    }
    Ok(map)
}

/// Build features from the union of chuki and manual markers.
fn build_features(
    chuki: Vec<(String, String, usize)>,
    manual: HashMap<String, Vec<String>>,
) -> Vec<Feature> {
    let mut by_key: HashMap<String, FeatureBuilder> = HashMap::new();
    let mut insertion_order: Vec<String> = Vec::new();

    // Chuki entries first, preserving file order.
    for (family, name, lineno) in chuki {
        let marker = format!("［＃{}］", name);
        let key = normalize(&marker);
        if key.is_empty() {
            continue;
        }
        by_key.entry(key.clone()).or_insert_with(|| {
            insertion_order.push(key.clone());
            FeatureBuilder {
                family: family.clone(),
                name: name.clone(),
                marker_examples: vec![key.clone()],
                chuki_lines: Vec::new(),
                manual_pages: Vec::new(),
            }
        });
        let fb = by_key.get_mut(&key).unwrap();
        fb.marker_examples[0] = key.clone();
        fb.chuki_lines.push(lineno);
        // If family was missing (shouldn't happen for chuki), update.
        if fb.family != family {
            fb.family = family;
        }
    }

    // Manual entries added after all chuki entries.
    let mut manual_keys: Vec<String> = manual.keys().cloned().collect();
    manual_keys.sort();
    for key in manual_keys {
        let pages = manual.get(&key).cloned().unwrap_or_default();
        by_key.entry(key.clone()).or_insert_with(|| {
            insertion_order.push(key.clone());
            let family = {
                let inferred = family_from_marker(&key);
                if inferred != "Other" {
                    inferred
                } else {
                    page_family(pages.first().map(String::as_str).unwrap_or("other"))
                }
            };
            let inner = key
                .strip_prefix('［')
                .and_then(|s| s.strip_suffix('］'))
                .map(|s| s.strip_prefix('＃').unwrap_or(s))
                .unwrap_or(&key);
            FeatureBuilder {
                family,
                name: inner.to_string(),
                marker_examples: vec![key.clone()],
                chuki_lines: Vec::new(),
                manual_pages: Vec::new(),
            }
        });
        let fb = by_key.get_mut(&key).unwrap();
        for page in pages {
            if !fb.manual_pages.contains(&page) {
                fb.manual_pages.push(page);
            }
        }
    }

    let mut builders: Vec<FeatureBuilder> = insertion_order
        .into_iter()
        .map(|k| by_key.remove(&k).expect("key in map"))
        .collect();

    // Assign deterministic, family-sequential IDs.
    builders.sort_by(|a, b| {
        let ra = family_rank(&a.family);
        let rb = family_rank(&b.family);
        ra.cmp(&rb).then_with(|| {
            let a_chuki = a.chuki_lines.iter().min().copied();
            let b_chuki = b.chuki_lines.iter().min().copied();
            match (a_chuki, b_chuki) {
                (Some(al), Some(bl)) => al.cmp(&bl),
                (Some(_), None) => std::cmp::Ordering::Less,
                (None, Some(_)) => std::cmp::Ordering::Greater,
                (None, None) => {
                    let ap = a.manual_pages.first().cloned().unwrap_or_default();
                    let bp = b.manual_pages.first().cloned().unwrap_or_default();
                    ap.cmp(&bp)
                }
            }
        })
    });

    let mut counters: HashMap<String, usize> = HashMap::new();
    let mut features = Vec::with_capacity(builders.len());
    for b in builders {
        let count = counters.entry(b.family.clone()).or_insert(1);
        let id = format!("{}{}", family_prefix(&b.family), *count);
        *count += 1;

        let verdict = if b.chuki_lines.is_empty() && b.manual_pages.is_empty() {
            Verdict::NoMarker
        } else if !b.chuki_lines.is_empty() && !b.manual_pages.is_empty() {
            Verdict::Verified
        } else if !b.chuki_lines.is_empty() {
            Verdict::ChukiOnly
        } else {
            Verdict::ManualOnly
        };

        let mut sources: Vec<String> = b
            .chuki_lines
            .iter()
            .map(|l| format!("chuki_tag.txt:{}", l))
            .collect();
        sources.extend(b.manual_pages);

        features.push(Feature {
            id,
            family: b.family,
            name: b.name,
            marker_examples: b.marker_examples,
            sources,
            verdict,
        });
    }

    features
}

fn emit_markdown(features: &[Feature]) -> String {
    let mut out = String::new();
    out.push_str("# Generated Aozora Bunko Feature Taxonomy\n\n");
    out.push_str("Derived from the canonical sources `chuki_tag.txt` and `annotation/*.html`.\n\n");

    let mut current_family = String::new();
    for f in features {
        if f.family != current_family {
            current_family = f.family.clone();
            out.push_str(&format!("### {}\n\n", section_title(&current_family)));
            out.push_str("| ID | Feature | Example | Sources | Verdict |\n");
            out.push_str("|----|---------|---------|---------|---------|\n");
        }
        let example = if f.marker_examples.is_empty() {
            "—".to_string()
        } else {
            format!("`{}`", f.marker_examples.join("`, `"))
        };
        let sources = if f.sources.is_empty() {
            "—".to_string()
        } else {
            f.sources.join("; ")
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} |\n",
            f.id, f.name, example, sources, f.verdict
        ));
    }
    out
}

#[derive(Parser, Debug)]
#[command(
    name = "generate_taxonomy",
    about = "Generate an Aozora Bunko feature taxonomy table"
)]
struct Args {
    #[arg(
        long,
        default_value = "references/parsers/AozoraEpub3-JDK21/chuki_tag.txt"
    )]
    chuki: PathBuf,

    #[arg(long, default_value = "/home/bor/Dependencies/aozorabunko/annotation")]
    annotation_dir: PathBuf,

    #[arg(long, default_value = "references/PARSER_REPORT.md")]
    reference: PathBuf,

    #[arg(long)]
    write: Option<PathBuf>,
}

#[derive(Debug, Clone)]
struct ReferenceFeature {
    id: String,
    name: String,
    markers: Vec<String>,
}

fn parse_section0(path: &Path) -> Result<Vec<ReferenceFeature>> {
    let content = fs::read_to_string(path).context("reading PARSER_REPORT.md")?;
    let lines: Vec<&str> = content.lines().collect();
    let start = lines
        .iter()
        .position(|l| l.starts_with("## 0."))
        .context("could not locate '## 0.' section in PARSER_REPORT.md")?;
    let end = lines[start + 1..]
        .iter()
        .position(|l| l.starts_with("## "))
        .map(|i| start + 1 + i)
        .unwrap_or(lines.len());

    let marker_re = Regex::new(r"［＃([^］]*)］").expect("constant regex");
    let id_re = Regex::new(r"^[A-Z][0-9]+$").expect("constant regex");
    let sep_re = Regex::new(r"^:?-+:?$").expect("constant regex");

    let mut out = Vec::new();
    for line in &lines[start..end] {
        let trimmed = line.trim();
        if !trimmed.starts_with('|') {
            continue;
        }
        let cells: Vec<&str> = trimmed
            .trim_matches('|')
            .split('|')
            .map(|c| c.trim())
            .collect();
        if cells.is_empty() {
            continue;
        }
        // Separator/header rows.
        if cells.iter().all(|c| c.is_empty() || sep_re.is_match(c)) {
            continue;
        }
        let id = cells[0];
        if !id_re.is_match(id) {
            continue;
        }
        let name = cells.get(1).unwrap_or(&"").to_string();
        let row_text = cells.join(" ");
        let markers: Vec<String> = marker_re
            .captures_iter(&row_text)
            .map(|cap| normalize(&format!("［＃{}］", &cap[1])))
            .filter(|m| !m.is_empty())
            .collect();
        out.push(ReferenceFeature {
            id: id.to_string(),
            name,
            markers,
        });
    }
    Ok(out)
}

fn matches_source(marker: &str, set: &HashSet<String>) -> bool {
    if marker.is_empty() {
        return false;
    }
    if set.contains(marker) {
        return true;
    }
    for s in set {
        if marker.contains(s) || s.contains(marker) {
            return true;
        }
    }
    false
}

fn verify_section0(
    refs: &[ReferenceFeature],
    chuki_set: &HashSet<String>,
    manual_set: &HashSet<String>,
) -> Vec<(String, String, Vec<String>)> {
    let mut counts: HashMap<Verdict, usize> = HashMap::new();
    let mut unverified: Vec<(String, String, Vec<String>)> = Vec::new();

    for rf in refs {
        let verdict = if rf.markers.is_empty() {
            Verdict::NoMarker
        } else {
            let in_chuki = rf.markers.iter().any(|m| matches_source(m, chuki_set));
            let in_manual = rf.markers.iter().any(|m| matches_source(m, manual_set));
            if in_chuki && in_manual {
                Verdict::Verified
            } else if in_chuki {
                Verdict::ChukiOnly
            } else if in_manual {
                Verdict::ManualOnly
            } else {
                Verdict::Unverified
            }
        };
        *counts.entry(verdict).or_default() += 1;
        if verdict == Verdict::Unverified {
            unverified.push((rf.id.clone(), rf.name.clone(), rf.markers.clone()));
        }
    }

    eprintln!("# PARSER_REPORT §0 verification\n");
    eprintln!("Canonical sources loaded:");
    eprintln!(
        "  chuki_tag.txt:        {} distinct normalized markers",
        chuki_set.len()
    );
    eprintln!(
        "  annotation/*.html:    {} distinct normalized markers",
        manual_set.len()
    );
    eprintln!(
        "  PARSER_REPORT §0:     {} feature rows parsed\n",
        refs.len()
    );

    eprintln!("## Summary counts (disjoint 5-way partition)\n");
    let total = refs.len();
    for v in [
        Verdict::Verified,
        Verdict::ChukiOnly,
        Verdict::ManualOnly,
        Verdict::Unverified,
        Verdict::NoMarker,
    ] {
        eprintln!(
            "  {:14}: {:3}",
            v.to_string(),
            counts.get(&v).copied().unwrap_or(0)
        );
    }
    eprintln!("  {:14}: {:3}", "TOTAL", total);

    eprintln!("\n## UNVERIFIED features\n");
    if unverified.is_empty() {
        eprintln!("  (none)");
    } else {
        for (id, name, markers) in &unverified {
            eprintln!(
                "  {} {} -> {}",
                id,
                name,
                if markers.is_empty() {
                    "—".to_string()
                } else {
                    markers.join(", ")
                }
            );
        }
    }

    unverified
}

fn main() -> Result<()> {
    let args = Args::parse();

    let chuki = parse_chuki_tag(&args.chuki)?;
    let manual = parse_manual_annotation(&args.annotation_dir)?;
    let features = build_features(chuki.clone(), manual.clone());

    let markdown = emit_markdown(&features);
    if let Some(out_path) = args.write {
        if let Some(parent) = out_path.parent() {
            fs::create_dir_all(parent).context("creating output directory")?;
        }
        fs::write(&out_path, markdown)
            .with_context(|| format!("writing {}", out_path.display()))?;
        eprintln!(
            "Wrote {} features to {}",
            features.len(),
            out_path.display()
        );
    } else {
        println!("{}", markdown);
    }

    // Build canonical source sets for verification.
    let chuki_set: HashSet<String> = chuki
        .iter()
        .map(|(_, name, _)| normalize(&format!("［＃{}］", name)))
        .collect();
    let manual_set: HashSet<String> = manual.keys().cloned().collect();

    if args.reference.exists() {
        let refs = parse_section0(&args.reference)?;
        verify_section0(&refs, &chuki_set, &manual_set);
    } else {
        eprintln!(
            "Skipping PARSER_REPORT §0 verification ({} not found)",
            args.reference.display()
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs::File;
    use std::io::Write;

    #[test]
    fn parses_chuki_header_and_rows() {
        let tmp = std::env::temp_dir().join(format!("chuki_test_{}.txt", std::process::id()));
        {
            let mut f = File::create(&tmp).unwrap();
            let content = "# comment\n#### 見出し ####\n大見出し\t<div class=\"chap1\">\t1\n# another comment\n中見出し\t<span>\t2\n";
            f.write_all(content.as_bytes()).unwrap();
        }
        let got = parse_chuki_tag(&tmp).unwrap();
        std::fs::remove_file(&tmp).ok();
        assert_eq!(
            got,
            vec![
                ("Headings".to_string(), "大見出し".to_string(), 3usize),
                ("Headings".to_string(), "中見出し".to_string(), 5usize),
            ]
        );
    }

    #[test]
    fn normalizes_markers() {
        assert_eq!(normalize("［＃5字下げ］"), "［＃N字下げ］");
        assert_eq!(normalize("［＃５字下げ］"), "［＃N字下げ］");
        assert_eq!(normalize("［＃10字下げ］"), "［＃N字下げ］");
        assert_eq!(
            normalize("［＃「文明論之概略　巻之一」は大見出し］"),
            "［＃「○○」は大見出し］"
        );
    }

    #[test]
    #[ignore]
    fn l13_dangumi_is_unverified() {
        // Requires the real (gitignored) canonical sources and PARSER_REPORT.md.
        let chuki = parse_chuki_tag("references/parsers/AozoraEpub3-JDK21/chuki_tag.txt").unwrap();
        let manual =
            parse_manual_annotation("/home/bor/Dependencies/aozorabunko/annotation").unwrap();
        let chuki_set: HashSet<String> = chuki
            .iter()
            .map(|(_, name, _)| normalize(&format!("［＃{}］", name)))
            .collect();
        let manual_set: HashSet<String> = manual.keys().cloned().collect();
        let refs = parse_section0(Path::new("references/PARSER_REPORT.md")).unwrap();
        let unverified = verify_section0(&refs, &chuki_set, &manual_set);
        assert!(
            unverified
                .iter()
                .any(|(id, name, _)| id == "L13" && name.contains("段組")),
            "L13 段組み should be UNVERIFIED"
        );
    }
}
