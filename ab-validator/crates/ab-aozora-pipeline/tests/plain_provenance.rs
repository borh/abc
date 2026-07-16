//! Characterizes the public parse projections while exposing the classifier's
//! binary plain-source provenance split.

use ab_aozora_pipeline::lex;
use ab_aozora_pipeline::lexer::{PlainProvenance, SpanKind, classify, pair, sanitize, tokenize};
use ab_aozora_render::{render_html, serialize};
use ab_aozora_syntax::alloc::Allocator;

fn plain_provenance(source: &str) -> Vec<(u32, u32, PlainProvenance)> {
    let sanitized = sanitize(source);
    let tokens = tokenize(&sanitized.text);
    let pairs = pair(tokens);
    let mut alloc = Allocator::default();
    classify(pairs, &sanitized.text, &mut alloc)
        .filter_map(|span| match span.kind {
            SpanKind::Plain(plain) => Some((
                span.source_span.start,
                span.source_span.end,
                plain.provenance,
            )),
            _ => None,
        })
        .collect()
}

#[test]
fn accepted_plain_recovery_paths_are_distinct_from_text() {
    use PlainProvenance::{RecoveredVerbatim as R, Text as T};

    let cases = [
        ("plain", vec![(0, 5, T)]),
        ("｜", vec![(0, 3, R)]),
        ("＃", vec![(0, 3, R)]),
        ("｜＃", vec![(0, 6, R)]),
        ("※", vec![(0, 3, R)]),
        ("「literal」", vec![(0, 3, R), (3, 10, T), (10, 13, R)]),
        ("〔literal〕", vec![(0, 3, R), (3, 10, T), (10, 13, R)]),
        ("［＃tail", vec![(0, 6, R), (6, 10, T)]),
        ("stray］", vec![(0, 5, T), (5, 8, R)]),
        ("※［＃［＃nested］］", vec![(0, 3, R)]),
    ];

    for (source, expected) in cases {
        assert_eq!(plain_provenance(source), expected, "source={source:?}");
    }
}

#[test]
fn recovery_output_characterization() {
    let cases = [
        ("plain", "plain", "[]", "[]", "<p>plain</p>\n"),
        ("｜", "｜", "[]", "[]", "<p>｜</p>\n"),
        ("＃", "＃", "[]", "[]", "<p>＃</p>\n"),
        ("※", "※", "[]", "[]", "<p>※</p>\n"),
        (
            "「literal」",
            "「literal」",
            "[]",
            "[]",
            "<p>「literal」</p>\n",
        ),
        (
            "〔literal〕",
            "〔literal〕",
            "[]",
            "[]",
            "<p>〔literal〕</p>\n",
        ),
        (
            "［＃tail",
            "［＃tail",
            "[]",
            "[UnclosedBracket { at: SourceSpan { offset: SourceOffset(0), length: 3 }, kind: Bracket, span: Span { start: 0, end: 3 } }]",
            "<p>［＃tail</p>\n",
        ),
        (
            "stray］",
            "stray］",
            "[]",
            "[UnmatchedClose { at: SourceSpan { offset: SourceOffset(5), length: 3 }, kind: Bracket, span: Span { start: 5, end: 8 } }]",
            "<p>stray］</p>\n",
        ),
        (
            "※［＃［＃nested］］",
            "※\u{e001}",
            "[SourceNode { source_span: Span { start: 3, end: 27 }, node: Inline(Directive(Directive { raw: StrId(0), kind: Unknown })) }]",
            "[]",
            "<p>※<span class=\"aozora-directive\" hidden>［＃［＃nested］］</span></p>\n",
        ),
    ];

    for (source, normalized, ast_projection, diagnostics, html) in cases {
        let out = lex(source);
        assert_eq!(out.normalized, normalized, "normalized bytes: {source:?}");
        assert_eq!(
            format!("{:?}", out.source_nodes),
            ast_projection,
            "AST: {source:?}"
        );
        assert_eq!(
            format!("{:?}", out.diagnostics),
            diagnostics,
            "diagnostics: {source:?}"
        );
        assert_eq!(render_html(&out), html, "HTML: {source:?}");
        assert_eq!(serialize(&out), source, "verbatim source: {source:?}");
    }
}
