//! Markdown renderer for a [`HydratedBundle`] (spec §Output). Pure
//! presentation: every field it reads was already resolved (or recorded as
//! an error) by `hydrate::build_example`; this module never touches the
//! filesystem or the warehouse. Iteration follows bundle order only, so
//! output is deterministic byte-for-byte across identical inputs (mirrors
//! `examples.json`'s determinism guarantee).

use std::collections::BTreeMap;
use std::io::Write;

use anyhow::Result;

use crate::hydrate::analyses::AnalyzerAnalysis;
use crate::hydrate::metadata::WorkMeta;
use crate::hydrate::tables::Token;
use crate::hydrate::{HydratedBundle, HydratedExample};

/// Writes the Markdown rendering of `bundle` to `out` (spec §Output layout).
pub fn write_markdown(bundle: &HydratedBundle, out: &mut impl Write) -> Result<()> {
    let full_to_short: BTreeMap<&str, &str> = bundle
        .provenance
        .analyzer_legend
        .iter()
        .map(|(short, full)| (full.as_str(), short.as_str()))
        .collect();

    writeln!(out, "# Hydrated examples — {}", bundle.provenance.run_id)?;
    writeln!(out)?;
    writeln!(
        out,
        "- Ranking: `{}` (sha256:{})",
        bundle.provenance.interesting_path, bundle.provenance.interesting_sha256
    )?;
    let limit_display = bundle
        .provenance
        .limit
        .map_or_else(|| "all".to_owned(), |limit| limit.to_string());
    writeln!(
        out,
        "- Built: {} · context: {} chars · limit: {}",
        bundle.provenance.built_at_utc, bundle.provenance.context_chars, limit_display
    )?;
    let catalog_display = bundle.provenance.abc_catalog.as_deref().map_or_else(
        || "absent — authors shown as person ids".to_owned(),
        |path| format!("`{path}`"),
    );
    writeln!(out, "- ABC catalog: {catalog_display}")?;
    writeln!(out)?;
    writeln!(out, "Analyzer legend: {}", render_legend(bundle))?;

    for (index, row) in bundle.rows.iter().enumerate() {
        let rank = index + 1;
        writeln!(out)?;
        writeln!(out, "## #{rank} {} — {}", row.row.kind, row.row.pattern)?;
        writeln!(
            out,
            "rrf {:.6} · {} examples · pattern `{}`",
            row.row.rrf_score, row.row.examples, row.row.pattern_id
        )?;
        for example in &row.hydrated_examples {
            writeln!(out)?;
            render_heading(
                out,
                "",
                &example.work,
                &normal_paren(&example.work),
                example,
            )?;
            render_body(out, example, &full_to_short)?;
        }
    }

    writeln!(out)?;
    writeln!(out, "## Anomalies")?;
    if bundle.anomalies.is_empty() {
        writeln!(out)?;
        writeln!(out, "_(none)_")?;
    }
    for (index, anomaly) in bundle.anomalies.iter().enumerate() {
        let n = index + 1;
        writeln!(out)?;
        let paren = format!("（anomaly {:.2}）", anomaly.row.anomaly_score);
        render_heading(
            out,
            &format!("{n}. "),
            &anomaly.hydrated.work,
            &paren,
            &anomaly.hydrated,
        )?;
        render_body(out, &anomaly.hydrated, &full_to_short)?;
    }

    Ok(())
}

/// `**short** = full · **short** = full · …`, in `analyzer_legend`'s (sorted
/// by short id) order; `(none)` when no analyzer contributed to this bundle.
fn render_legend(bundle: &HydratedBundle) -> String {
    if bundle.provenance.analyzer_legend.is_empty() {
        return "(none)".to_owned();
    }
    bundle
        .provenance
        .analyzer_legend
        .iter()
        .map(|(short, full)| format!("**{short}** = {full}"))
        .collect::<Vec<_>>()
        .join(" · ")
}

/// `（{year}・{style}）`, `（{year}）`, `（{style}）`, or `""` — whichever
/// pieces of work metadata are present, gracefully omitting the rest.
fn normal_paren(work: &Option<WorkMeta>) -> String {
    let Some(work) = work else {
        return String::new();
    };
    let year = work.publication_year.map(|year| year.to_string());
    let style = work.orthographic_style.clone();
    match (year, style) {
        (Some(year), Some(style)) => format!("（{year}・{style}）"),
        (Some(year), None) => format!("（{year}）"),
        (None, Some(style)) => format!("（{style}）"),
        (None, None) => String::new(),
    }
}

/// The heading line (`### {prefix}『title』 author{paren} — text_id, region
/// n, chars a–b`) plus the `[card](url)` line when a card URL resolved.
/// Falls back to `### {prefix}text_id, region n, chars a–b` when `work` is
/// `None` (spec: missing work heading degradation).
fn render_heading(
    out: &mut impl Write,
    prefix: &str,
    work: &Option<WorkMeta>,
    paren: &str,
    example: &HydratedExample,
) -> Result<()> {
    match work {
        Some(work) => {
            let title = work.title.as_deref().unwrap_or_default();
            let author = work.display_author();
            writeln!(
                out,
                "### {prefix}『{title}』 {author}{paren} — {}, region {}, chars {}–{}",
                example.text_id, example.region_index, example.char_start, example.char_end
            )?;
            if let Some(card_url) = &work.card_url {
                writeln!(out, "[card]({card_url})")?;
            }
        }
        None => {
            writeln!(
                out,
                "### {prefix}{}, region {}, chars {}–{}",
                example.text_id, example.region_index, example.char_start, example.char_end
            )?;
        }
    }
    Ok(())
}

/// The shared body every example (row example or anomaly) renders
/// identically: snippet quote, analyzer table, Aozora markup fence, AAT
/// line. Each layer degrades independently to an italic `_… unavailable:
/// …_` line (snippet/markup) or is simply omitted (table/AAT) when there is
/// no data and no associated error to report.
fn render_body(
    out: &mut impl Write,
    example: &HydratedExample,
    full_to_short: &BTreeMap<&str, &str>,
) -> Result<()> {
    writeln!(out)?;
    match &example.snippet {
        Some(snippet) => writeln!(out, "> {}", snippet.marked())?,
        None => {
            let error = layer_error(&example.errors, &["snippet:"]);
            writeln!(out, "_snippet unavailable: {error}_")?;
        }
    }

    if !example.analyzer_analyses.is_empty() {
        writeln!(out)?;
        writeln!(out, "| analyzers | segmentation | pos |")?;
        writeln!(out, "| --- | --- | --- |")?;
        for analysis in &example.analyzer_analyses {
            writeln!(
                out,
                "| {} | {} | {} |",
                escape_cell(&render_analyzer_ids(analysis, full_to_short)),
                escape_cell(&render_segmentation(analysis)),
                escape_cell(&render_pos(analysis))
            )?;
        }
    }

    writeln!(out)?;
    match &example.aozora_markup {
        Some(markup) => {
            let header = if markup.approximate_pointers.is_empty() {
                "Aozora markup:"
            } else {
                "Aozora markup (approximate):"
            };
            writeln!(out, "{header}")?;
            writeln!(out, "```")?;
            writeln!(out, "{}", markup.text)?;
            writeln!(out, "```")?;
        }
        None => {
            let error = layer_error(&example.errors, &["markup-unreconstructable"]);
            writeln!(out, "_markup unavailable: {error}_")?;
        }
    }

    if !example.aat_nodes.is_empty() {
        let joined = example
            .aat_nodes
            .iter()
            .map(|node| format!("{} ({})", node.pointer, node.inline_kind))
            .collect::<Vec<_>>()
            .join(", ");
        writeln!(out, "AAT: {joined}")?;
    }

    Ok(())
}

/// The first error whose code matches one of `prefixes`, falling back to
/// the first error overall (a whole-source load failure — `aat-missing` or
/// `projection-mismatch` — takes down every layer at once, so it is the
/// right explanation even though its code doesn't name the layer), and
/// finally to a generic message when there is no error at all to show
/// (defensive: every `None` layer this is called for was pushed alongside
/// an error by `build_example`, but rendering must not panic if that
/// invariant ever slips).
fn layer_error<'a>(errors: &'a [String], prefixes: &[&str]) -> &'a str {
    errors
        .iter()
        .find(|error| prefixes.iter().any(|prefix| error.starts_with(prefix)))
        .or_else(|| errors.first())
        .map_or("unknown error", String::as_str)
}

fn render_analyzer_ids(
    analysis: &AnalyzerAnalysis,
    full_to_short: &BTreeMap<&str, &str>,
) -> String {
    analysis
        .analyzer_ids
        .iter()
        .map(|id| {
            full_to_short
                .get(id.as_str())
                .copied()
                .unwrap_or(id.as_str())
                .to_owned()
        })
        .collect::<Vec<_>>()
        .join(", ")
}

fn render_segmentation(analysis: &AnalyzerAnalysis) -> String {
    analysis
        .tokens
        .iter()
        .map(|token| token.surface.as_str())
        .collect::<Vec<_>>()
        .join("｜")
}

/// Per token: `pos1..pos4` joined with `-`, skipping empty/absent features.
/// Tokens joined with `｜`. When every token's pos string is empty (no
/// analyzer contributed any pos1..pos4 feature at all), the whole cell is
/// left empty rather than a run of bare `｜` separators.
fn render_pos(analysis: &AnalyzerAnalysis) -> String {
    let per_token: Vec<String> = analysis.tokens.iter().map(pos_string_for_token).collect();
    if per_token.iter().all(String::is_empty) {
        String::new()
    } else {
        per_token.join("｜")
    }
}

fn pos_string_for_token(token: &Token) -> String {
    ["pos1", "pos2", "pos3", "pos4"]
        .into_iter()
        .filter_map(|key| token.features.get(key))
        .filter(|value| !value.is_empty())
        .cloned()
        .collect::<Vec<_>>()
        .join("-")
}

/// Escapes `|` as `\|` so table cell content can never break the row.
fn escape_cell(cell: &str) -> String {
    cell.replace('|', "\\|")
}

#[cfg(test)]
mod tests {
    #[test]
    fn markdown_renders_all_layers() {
        let (_dir, opts) = crate::hydrate::tests::write_e2e_fixture();
        crate::hydrate::run_hydrate_interesting(&opts).unwrap();
        let md = std::fs::read_to_string(opts.output_dir.join("examples.md")).unwrap();
        assert!(md.starts_with("# Hydrated examples — run-h"));
        assert!(md.contains("Analyzer legend:"));
        assert!(md.contains("## #1 feature — pattern-p-ruby"));
        assert!(md.contains("『煙管』 芥川竜之介"));
        assert!(md.contains("【仏蘭西】"));
        assert!(md.contains("| analyzers | segmentation | pos |"));
        assert!(md.contains("｜仏蘭西《フランス》"));
        assert!(md.contains("AAT: /blocks/0/content/1"));
        assert!(!md.contains('\t'), "markdown must not contain raw tabs");
    }
}
