//! Taxonomy generator for Aozora Bunko parser features.
//!
//! Reads authoritative sources at runtime:
//!   - `annotation/*.html` from aozora.gr.jp (the spec)
//!   - real corpus `cards/*/files/*.zip` (zipped SHIFT_JIS text files)
//!
//! Produces a generated feature-taxonomy table and verifies it against the
//! hand-written §0 of `PARSER_REPORT.md`.
//!
//! Non-authoritative `chuki_tag.txt` is deliberately not read.

use anyhow::{Context, Result};
use clap::Parser;
use encoding_rs::SHIFT_JIS;
use regex::Regex;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs;
use std::io::Read;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;
use zip::ZipArchive;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum Status {
    Documented,
    Observed,
    DocumentedAndObserved,
    Deprecated,
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            Status::Documented => "DOCUMENTED",
            Status::Observed => "OBSERVED",
            Status::DocumentedAndObserved => "DOCUMENTED-AND-OBSERVED",
            Status::Deprecated => "DEPRECATED",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone)]
struct Feature {
    id: String,
    family: String,
    sub_family: String,
    name: String,
    marker_example: String,
    manual_pages: BTreeSet<String>,
    observed: bool,
    status: Status,
}

#[derive(Debug, Clone)]
struct FeatureBuilder {
    family: String,
    sub_family: String,
    name: String,
    marker_example: String,
    manual_pages: BTreeSet<String>,
    observed: bool,
    is_deprecated: bool,
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
        "Duplication" => 7,
        "Deprecation" => 8,
        _ => 9,
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
        "Duplication" => "Dp",
        "Deprecation" => "Hc",
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
        "Duplication" => "Duplication / 重複",
        "Deprecation" => "Deprecation / 変更点",
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

/// Map an annotation page filename stem to its top-level family.
fn page_family(page: &str) -> String {
    match page {
        "layout_1" | "layout_2" | "layout_3" => "Layout",
        "heading" => "Headings",
        "external_character" => "Gaiji",
        "kunten" => "Kunten",
        "emphasis" => "Emphasis",
        "graphics" => "Graphics",
        "duplication" => "Duplication",
        "henkoten" => "Deprecation",
        "etc" | "extra" | "index" => "Other",
        _ => "Other",
    }
    .to_string()
}

/// Infer a marker's family from its content.
fn family_from_marker(marker: &str) -> String {
    if marker.contains("見出し") {
        "Headings".to_string()
    } else if marker.contains("外字")
        || marker.contains("二の字点")
        || marker.contains("U+")
        || marker.contains("JIS")
        || marker.contains("水準")
        || marker.contains("に代えて")
    {
        "Gaiji".to_string()
    } else if marker.contains("訓点")
        || marker.contains("返り点")
        || marker.contains("送り仮名")
        || marker.contains("再読")
        || marker.contains("（ツ）")
        || marker.contains("（フ）")
        || marker.contains("（二）")
        || marker.contains("（テ）")
        || marker.contains("（レ）")
        || marker.contains("（ヘ）")
        || marker.contains("（カ）")
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
        || marker.contains("イタリック")
    {
        "Emphasis".to_string()
    } else if marker.contains("図")
        || marker.contains("キャプション")
        || marker.contains("挿絵")
        || marker.contains("写真")
    {
        "Graphics".to_string()
    } else if marker.contains("字下げ")
        || marker.contains("改ページ")
        || marker.contains("改頁")
        || marker.contains("改丁")
        || marker.contains("改段")
        || marker.contains("改見開き")
        || marker.contains("左右中央")
        || marker.contains("地付")
        || marker.contains("地寄せ")
        || marker.contains("字上げ")
        || marker.contains("字詰め")
        || marker.contains("段組")
        || marker.contains("中央")
        || marker.contains("横組")
        || marker.contains("横書")
        || marker.contains("同行")
        || marker.contains("窓見出し")
        || marker.contains("割り注")
        || marker.contains("行右小書き")
        || marker.contains("罫囲み")
        || marker.contains("文字サイズ")
    {
        "Layout".to_string()
    } else if marker.contains("重複") || marker.contains("重ねて") {
        "Duplication".to_string()
    } else {
        "Other".to_string()
    }
}

/// Resolve the family for a marker encountered on a given annotation page.
///
/// If a forced family is supplied (e.g. henkoten is always Deprecation), use
/// it; otherwise prefer content-based inference and fall back to the page
/// family.
fn resolve_family(marker: &str, page: &str, forced: Option<&str>) -> String {
    if let Some(f) = forced {
        return f.to_string();
    }
    let inferred = family_from_marker(marker);
    if inferred != "Other" {
        inferred
    } else {
        page_family(page)
    }
}

/// Strip simple HTML tags from a snippet.
fn strip_tags(html: &str) -> String {
    let tag_re = Regex::new(r"<[^>]*>").expect("constant regex");
    tag_re.replace_all(html, "").trim().to_string()
}

/// Parse the annotation/`*.html` directory.
///
/// Returns a map from normalized marker to a builder. HTML tags are stripped
/// for marker extraction, but headings are parsed first so each marker carries
/// the nearest preceding `<h2>`/`<h3>` as its sub-family. `index.html` is
/// skipped; `henkoten.html` is processed last and only contributes a
/// deprecation flag/source page without overriding a marker's family.
fn parse_manual_annotation(dir: impl AsRef<Path>) -> Result<BTreeMap<String, FeatureBuilder>> {
    // Match h1/h2/h3 headings explicitly (no backreferences; the regex crate
    // does not support them) and ［＃...］ markers, in document order.
    let heading_marker_re = Regex::new(
        r"(?is)<h1(?:\s[^>]*)?>(.*?)</h1>|<h2(?:\s[^>]*)?>(.*?)</h2>|<h3(?:\s[^>]*)?>(.*?)</h3>|［＃([^］]*)］",
    )
    .expect("constant regex");

    let mut entries: Vec<_> = fs::read_dir(dir.as_ref())
        .context("reading annotation directory")?
        .filter_map(|e| e.ok())
        .collect();
    entries.sort_by_key(|e| e.file_name());

    let mut builders: BTreeMap<String, FeatureBuilder> = BTreeMap::new();

    // First pass: every page except index and henkoten.
    for entry in &entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("html") {
            continue;
        }
        let page = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        if page == "index" || page == "henkoten" {
            continue;
        }
        process_annotation_page(&path, &page, &heading_marker_re, &mut builders, false, None)?;
    }

    // Second pass: henkoten contributes only deprecation status/source.
    for entry in &entries {
        let path = entry.path();
        if path.extension().and_then(|s| s.to_str()) != Some("html") {
            continue;
        }
        let page = path
            .file_stem()
            .and_then(|s| s.to_str())
            .unwrap_or("unknown")
            .to_string();
        if page != "henkoten" {
            continue;
        }
        process_annotation_page(
            &path,
            &page,
            &heading_marker_re,
            &mut builders,
            true,
            Some("Deprecation"),
        )?;
    }

    Ok(builders)
}

fn process_annotation_page(
    path: &Path,
    page: &str,
    re: &Regex,
    builders: &mut BTreeMap<String, FeatureBuilder>,
    is_deprecated: bool,
    forced_family: Option<&str>,
) -> Result<()> {
    let raw = fs::read_to_string(path).with_context(|| format!("reading {}", path.display()))?;
    let source_page = format!("annotation/{}.html", page);

    let mut current_sub = String::new();
    for cap in re.captures_iter(&raw) {
        // Heading groups 1/2/3 correspond to h1/h2/h3; marker is group 4.
        if cap.get(1).is_some() || cap.get(2).is_some() || cap.get(3).is_some() {
            // Track h2/h3 as sub-family scopes; h1 is the page title.
            let level = if cap.get(1).is_some() {
                1
            } else if cap.get(2).is_some() {
                2
            } else {
                3
            };
            if level == 2 || level == 3 {
                let text = strip_tags(cap.get(level).expect("matched heading group").as_str());
                if !text.is_empty() {
                    current_sub = text.split_whitespace().collect::<Vec<_>>().join(" ");
                }
            }
        } else if let Some(inner) = cap.get(4) {
            let marker = format!("［＃{}］", inner.as_str());
            let norm = normalize(&marker);
            if norm.is_empty() {
                continue;
            }
            let family = resolve_family(&norm, page, forced_family);
            let sub_family = if current_sub.is_empty() {
                "—".to_string()
            } else {
                current_sub.clone()
            };
            let name = norm
                .strip_prefix('［')
                .and_then(|s| s.strip_suffix('］'))
                .and_then(|s| s.strip_prefix('＃'))
                .unwrap_or(&norm)
                .to_string();

            let builder = builders
                .entry(norm.clone())
                .or_insert_with(|| FeatureBuilder {
                    family,
                    sub_family,
                    name,
                    marker_example: norm.clone(),
                    manual_pages: BTreeSet::new(),
                    observed: false,
                    is_deprecated: false,
                });
            builder.manual_pages.insert(source_page.clone());
            if is_deprecated {
                builder.is_deprecated = true;
            }
        }
    }
    Ok(())
}

/// Walk the real corpus and collect normalized marker forms observed in text.
///
/// The corpus is expected as `*.zip` files, each containing one or more
/// SHIFT_JIS-encoded `.txt` members. If `limit` is non-zero, processing stops
/// after that many zip files (useful for quick tests; full runs use 0).
fn parse_corpus(dir: impl AsRef<Path>, limit: usize) -> Result<BTreeSet<String>> {
    let marker_re = Regex::new(r"［＃([^］]*)］").expect("constant regex");
    let mut observed: BTreeSet<String> = BTreeSet::new();
    let mut files_read = 0usize;

    for entry in WalkDir::new(dir.as_ref())
        .sort_by_file_name()
        .into_iter()
        .filter_map(|e| e.ok())
    {
        if !entry.file_type().is_file() {
            continue;
        }
        let path = entry.path();
        if path
            .extension()
            .and_then(|s| s.to_str())
            .map(|s| s.eq_ignore_ascii_case("zip"))
            != Some(true)
        {
            continue;
        }

        let file = match fs::File::open(path) {
            Ok(f) => f,
            Err(_) => continue,
        };
        let mut archive = match ZipArchive::new(file) {
            Ok(a) => a,
            Err(_) => continue,
        };

        for i in 0..archive.len() {
            let mut zf = match archive.by_index(i) {
                Ok(z) => z,
                Err(_) => continue,
            };
            let name_raw = zf.name_raw();
            let (name, _, _) = SHIFT_JIS.decode(name_raw);
            if !name.to_lowercase().ends_with(".txt") {
                continue;
            }
            let mut bytes = Vec::new();
            if zf.read_to_end(&mut bytes).is_err() {
                continue;
            }
            let (text, _, _) = SHIFT_JIS.decode(&bytes);
            for cap in marker_re.captures_iter(&text) {
                let inner = &cap[1];
                let marker = format!("［＃{}］", inner);
                let norm = normalize(&marker);
                if !norm.is_empty() {
                    observed.insert(norm);
                }
            }
        }

        files_read += 1;
        if limit > 0 && files_read >= limit {
            break;
        }
    }

    Ok(observed)
}

/// Merge annotation builders with observed corpus markers and assign IDs.
fn build_features(
    mut builders: BTreeMap<String, FeatureBuilder>,
    observed: BTreeSet<String>,
) -> Vec<Feature> {
    for norm in observed {
        let builder = builders.entry(norm.clone()).or_insert_with(|| {
            let family = family_from_marker(&norm);
            let inner = norm
                .strip_prefix('［')
                .and_then(|s| s.strip_suffix('］'))
                .and_then(|s| s.strip_prefix('＃'))
                .unwrap_or(&norm);
            FeatureBuilder {
                family,
                sub_family: "—".to_string(),
                name: inner.to_string(),
                marker_example: norm.clone(),
                manual_pages: BTreeSet::new(),
                observed: false,
                is_deprecated: false,
            }
        });
        builder.observed = true;
    }

    let mut list: Vec<FeatureBuilder> = builders.into_values().collect();
    list.sort_by(|a, b| {
        family_rank(&a.family)
            .cmp(&family_rank(&b.family))
            .then(a.family.cmp(&b.family))
            .then(a.sub_family.cmp(&b.sub_family))
            .then(a.name.cmp(&b.name))
    });

    let mut counters: BTreeMap<String, usize> = BTreeMap::new();
    list.into_iter()
        .map(|b| {
            let count = counters.entry(b.family.clone()).or_insert(1);
            let id = format!("{}{}", family_prefix(&b.family), *count);
            *count += 1;

            let status = if b.is_deprecated {
                Status::Deprecated
            } else if !b.manual_pages.is_empty() && b.observed {
                Status::DocumentedAndObserved
            } else if b.observed {
                Status::Observed
            } else {
                Status::Documented
            };

            Feature {
                id,
                family: b.family,
                sub_family: b.sub_family,
                name: b.name,
                marker_example: b.marker_example,
                manual_pages: b.manual_pages,
                observed: b.observed,
                status,
            }
        })
        .collect()
}

fn emit_markdown(features: &[Feature]) -> String {
    let mut out = String::new();
    out.push_str("# Generated Aozora Bunko Feature Taxonomy\n\n");
    out.push_str(
        "Derived from `annotation/*.html` (aozora.gr.jp authoritative spec) and real corpus observation.\n",
    );
    out.push_str("Third-party converter tables are not used as sources.\n\n");

    let mut current_family = String::new();
    for f in features {
        if f.family != current_family {
            current_family = f.family.clone();
            out.push_str(&format!("### {}\n\n", section_title(&current_family)));
            out.push_str("| ID | Family | Sub-family | Feature | Example | Sources | Status |\n");
            out.push_str("|----|--------|------------|---------|---------|---------|--------|\n");
        }
        let example = format!("`{}`", f.marker_example);
        let mut sources: Vec<String> = f.manual_pages.iter().cloned().collect();
        if f.observed {
            sources.push("corpus".to_string());
        }
        let sources_str = if sources.is_empty() {
            "—".to_string()
        } else {
            sources.join("; ")
        };
        out.push_str(&format!(
            "| {} | {} | {} | {} | {} | {} | {} |\n",
            f.id, f.family, f.sub_family, f.name, example, sources_str, f.status
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
    #[arg(long, default_value = "/home/bor/Dependencies/aozorabunko/annotation")]
    annotation_dir: PathBuf,

    #[arg(long)]
    corpus_dir: Option<PathBuf>,

    /// Maximum number of corpus zip files to read (0 = unlimited).
    #[arg(long, default_value = "0")]
    corpus_limit: usize,

    #[arg(long)]
    reference: Option<PathBuf>,

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
    manual_set: &HashSet<String>,
    observed_set: &HashSet<String>,
) -> Vec<(String, String, Vec<String>)> {
    let mut counts: HashMap<Status, usize> = HashMap::new();
    let mut unverified: Vec<(String, String, Vec<String>)> = Vec::new();

    for rf in refs {
        let status = if rf.markers.is_empty() {
            Status::Documented
        } else {
            let in_manual = rf.markers.iter().any(|m| matches_source(m, manual_set));
            let in_observed = rf.markers.iter().any(|m| matches_source(m, observed_set));
            if in_manual && in_observed {
                Status::DocumentedAndObserved
            } else if in_manual {
                Status::Documented
            } else if in_observed {
                Status::Observed
            } else {
                Status::Deprecated
            }
        };
        *counts.entry(status).or_default() += 1;
        if status == Status::Deprecated {
            unverified.push((rf.id.clone(), rf.name.clone(), rf.markers.clone()));
        }
    }

    eprintln!("# PARSER_REPORT §0 verification\n");
    eprintln!("Sources loaded:");
    eprintln!(
        "  annotation/*.html:    {} distinct normalized markers",
        manual_set.len()
    );
    eprintln!(
        "  corpus:               {} distinct normalized markers",
        observed_set.len()
    );
    eprintln!(
        "  PARSER_REPORT §0:     {} feature rows parsed\n",
        refs.len()
    );

    eprintln!("## Summary counts\n");
    let total = refs.len();
    for s in [
        Status::Documented,
        Status::Observed,
        Status::DocumentedAndObserved,
        Status::Deprecated,
    ] {
        eprintln!(
            "  {:26}: {:3}",
            s.to_string(),
            counts.get(&s).copied().unwrap_or(0)
        );
    }
    eprintln!("  {:26}: {:3}", "TOTAL", total);

    eprintln!("\n## Unmatched reference rows\n");
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

    let annotation_builders = parse_manual_annotation(&args.annotation_dir)?;
    let manual_set: HashSet<String> = annotation_builders.keys().cloned().collect();

    let observed: BTreeSet<String> = if let Some(corpus_dir) = &args.corpus_dir {
        parse_corpus(corpus_dir, args.corpus_limit)?
    } else {
        BTreeSet::new()
    };

    let features = build_features(annotation_builders, observed.clone());
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

    if let Some(reference) = args.reference {
        if reference.exists() {
            let observed_set: HashSet<String> = observed.into_iter().collect();
            let refs = parse_section0(&reference)?;
            verify_section0(&refs, &manual_set, &observed_set);
        } else {
            eprintln!(
                "Skipping PARSER_REPORT §0 verification ({} not found)",
                reference.display()
            );
        }
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;

    fn tmp_path(name: &str) -> PathBuf {
        std::env::temp_dir().join(format!("{}_{}", name, std::process::id()))
    }

    #[test]
    fn parses_annotation_page_headings_and_markers() {
        let tmp = tmp_path("annotation_test");
        fs::create_dir_all(&tmp).unwrap();
        let html = tmp.join("test_emphasis.html");
        {
            let mut f = fs::File::create(&html).unwrap();
            let content = "<h2>強調</h2>\n<h3>傍点</h3>\n<p>例：○○［＃「テスト」に傍点］を使う。</p>\n<h3>傍線</h3>\n<p>［＃「サンプル」に傍線］</p>\n";
            f.write_all(content.as_bytes()).unwrap();
        }
        let got = parse_manual_annotation(&tmp).unwrap();
        std::fs::remove_dir_all(&tmp).ok();

        assert_eq!(got.len(), 2);
        let point = got
            .get("［＃「○○」に傍点］")
            .expect("傍 point marker present");
        assert_eq!(point.family, "Emphasis");
        assert_eq!(point.sub_family, "傍点");
        assert!(point.manual_pages.contains("annotation/test_emphasis.html"));

        let line = got.get("［＃「○○」に傍線］").expect("傍線 marker present");
        assert_eq!(line.sub_family, "傍線");
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
    fn status_display_values() {
        assert_eq!(format!("{}", Status::Documented), "DOCUMENTED");
        assert_eq!(format!("{}", Status::Observed), "OBSERVED");
        assert_eq!(
            format!("{}", Status::DocumentedAndObserved),
            "DOCUMENTED-AND-OBSERVED"
        );
        assert_eq!(format!("{}", Status::Deprecated), "DEPRECATED");
    }
}
