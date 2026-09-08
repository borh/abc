# Parser and rendering invariants

Names for parser and rendering invariants shared across modules. These are
internal terms: they name behaviour the converter must preserve, and they are
used in code comments and commit messages so one invariant has one name.

Looking for the vocabulary of the published corpus — admission, assessment,
manifest, chain, fidelity, work and edition? That is the
[glossary for readers of the corpus](user-glossary.md).

## double-render invariant

Styled text must never be rendered twice. When a forward directive
(emphasis, bouten, etc.) references a text run, the styled copy is the
sole rendered occurrence; the unstyled literal must be reclaimed so no
duplicate appears in output.

## gothic/太字 distinction

ゴシック体 (gothic typeface) is a first-class construct distinct from
太字 (bold). The parser keeps them separate; non-canonical corpus
variants (ゴチック, etc.) decline to `Directive{Unknown}` with a lint
suggesting the canonical form.

## non-adjacent referent resolution

A forward directive (emphasis, bouten, dotted-letter) may target a
referent that is not the immediately preceding plain-text run. The
classifier resolves the interior target position and splices the styled
decoration at it.

## compound structural markers

Structural markers (改行, 改段, 改ページ) and compound indent/line-layout
directives (ここから{N}字下げ + 字組み, etc.) form a closed set of
self-contained structural leaves recognized by the lexer.

## ruby-base emphasis

A forward directive whose target resolves to a ruby base text. Since
the ruby base is owned by the ruby node, the emphasis cannot be pulled
into a text-only forward leaf; instead a render-only `base_emphasis`
flag is set on the ruby node.

## dotted-letter composition

ドット付き directives compose a combining dot onto an addressed Latin
letter (e.g., `mは上ドット付き` → ṁ). This is a separate facility from
accent decomposition and uses a selector grammar over the reclaimed run.
