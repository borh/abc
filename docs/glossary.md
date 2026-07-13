# Invariant Glossary

Stable concept identifiers used as cross-file vocabulary. Each entry
records the invariant name, the issue number that named it, and a
one-paragraph definition.

## #228 — double-render invariant

Styled text must never be rendered twice. When a forward directive
(emphasis, bouten, etc.) references a text run, the styled copy is the
sole rendered occurrence; the unstyled literal must be reclaimed so no
duplicate appears in output.

## #435 — gothic/太字 distinction

ゴシック体 (gothic typeface) is a first-class construct distinct from
太字 (bold). The parser keeps them separate; non-canonical corpus
variants (ゴチック, etc.) decline to `Directive{Unknown}` with a lint
suggesting the canonical form.

## #333 — non-adjacent referent resolution

A forward directive (emphasis, bouten, dotted-letter) may target a
referent that is not the immediately preceding plain-text run. The
classifier resolves the interior target position and splices the styled
decoration at it.

## #78 — compound structural markers

Structural markers (改行, 改段, 改ページ) and compound indent/line-layout
directives (ここから{N}字下げ + 字組み, etc.) form a closed set of
self-contained structural leaves recognized by the lexer.

## #384 — ruby-base emphasis

A forward directive whose target resolves to a ruby base text. Since
the ruby base is owned by the ruby node, the emphasis cannot be pulled
into a text-only forward leaf; instead a render-only `base_emphasis`
flag is set on the ruby node.

## #331 — dotted-letter composition

ドット付き directives compose a combining dot onto an addressed Latin
letter (e.g., `mは上ドット付き` → ṁ). This is a separate facility from
accent decomposition and uses a selector grammar over the reclaimed run.
