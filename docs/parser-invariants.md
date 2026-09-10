# Parser and rendering invariants

Names for parser and rendering invariants shared across modules. These
internal terms name converter behaviors that must be preserved. They provide
consistent names for use in code comments and commit messages.

For the vocabulary of the published corpus (such as admission, assessment,
manifest, chain, fidelity, work, and edition), see the
[reader glossary](user-glossary.md).

## double-render invariant

Styled text must never be rendered twice. When a forward directive
(emphasis, bouten, etc.) references a text run, the styled copy is the
sole rendered occurrence; the unstyled literal must be reclaimed so no
duplicate appears in output.

## gothic/太字 distinction

ゴシック体 (gothic typeface) is a distinct syntax construct from
太字 (bold), and the parser keeps them separate. What a non-canonical
variant spelling does depends on the directive's form, not on the spelling
alone. A bare `［＃ゴチック］` declines to `DirectiveKind::Unknown` and the
lint catalogue suggests `ゴシック体`; the forward-referencing
`［＃「X」はゴチック］` carries typed gothic emphasis instead. The catalogue
holds that line by construction: its self-test requires every key to still
parse to Unknown and every suggestion to parse to something else.

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
