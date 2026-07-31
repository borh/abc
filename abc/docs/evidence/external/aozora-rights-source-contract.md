# Aozora Rights Source Contract

Assessment date: 2026-07-26
Review after: 2027-01-31 — a conservative Soranoha review interval, not a claim
about upstream behaviour. Aozora's reevaluation cadence is unresolved (Q6).
Decision owner: Soranoha project owner
Authority: **incomplete** — the census below is settled; the legal and
operational semantics are not. This document grants no publication authority in
its current state. See *Open Questions* and *Non-Authority*.

## Snapshot Binding

Every number here is a function of one catalog snapshot and nothing else.

| | |
|---|---|
| aozorabunko commit | `0e9ea3e586eb0aa34039fabfc85a407d2f98b165` |
| catalog archive | `index_pages/list_person_all_extended_utf8.zip` |
| archive sha256 | `sha256:5ea13273dd457f89af31de39f559ea9c6f5435d9bda1ae3d46d6a683b8bc3c92` |
| CSV sha256 | `sha256:c0ace54c7ac037e5aebd045922c7879b9f7dc01f85b8f7483c8d4ecc29569ef4` |
| rows (work × person) | 19,470 |
| distinct works | 17,810 |
| distinct persons | 1,334 |

Reproduce with `python3 tools/rights_source_census.py <aozorabunko-root>` from
the `abc` directory. The census reads the ZIP directly; it does not depend on
any ingested corpus.

## Purpose

`rights-assessment-and-external-statements` blocks release publication because
the Aozora `copyright_expired` Boolean carries no jurisdiction, effective date,
or assessment provenance, and mapping it to an external rights IRI asserts a
legal status the source does not support. This document is Task 2 of
`docs/superpowers/plans/2026-07-12-rights-assessment-remediation.md`: establish
what the source fields actually are before any schema is written against them.

It is deliberately split, and the split is enforced strictly. The **Observed
Facts** are *lexical* measurements of catalog cells under the pinned snapshot
hash: which values appear, how often, and how they co-occur. They carry no
claim about what any value means, legally or operationally. The **Open
Questions** are those meanings, and this project has not sourced them. No
downstream schema, mapping, or RDF emission may treat an Open Question as
settled, and no Observed Fact may be read as evidence about one.

Supplying the missing semantics from the author's general knowledge would
reproduce the exact fault this ADR exists to remove, one file further along.

## Field Inventory

The catalog is one denormalized row per (work, contributor). Four columns bear
on rights:

| Column | Position | Cell values as observed |
|---|---|---|
| `作品著作権フラグ` | 11 | Per-work flag, `あり` / `なし` |
| `役割フラグ` | 24 | `著者` `翻訳者` `編者` `校訂者` `その他` |
| `没年月日` | 26 | Per-person free-form date text |
| `人物著作権フラグ` | 27 | Per-person flag, `あり` / `なし` |

Nationality is **not** a catalog field. Neither is the basis for a flag, the
jurisdiction it applies to, the date it was assessed, nor who assessed it. That
absence is why the flag cannot be republished as a legal assertion unchanged,
and it also bounds this census: several distinctions the law turns on are not
expressible in the data at all.

## Observed Facts

Each is a statement about cell values under the snapshot hash above.

### F0 — The denormalized table is internally consistent

Several facts below collapse the one-row-per-(work, contributor) table by
taking a single representative row per work or per person. That is sound only
while each repeated cell agrees across a key's rows. Checked, not assumed:
`作品著作権フラグ`, `原題`, `テキストファイルURL`, and `作品名` agree across all
rows of every `作品ID`, and `人物著作権フラグ`, `没年月日`, `姓`, and `名` agree
across all rows of every `人物ID` — **zero conflicts in each of the eight
columns**.

The consequence is scoped to the representative-row measurements: F1's flag
counts, F3, F4, F5, F7, and the partition select one row per key and would
become functions of row order if a relevant column ever conflicted. The
measurements that consume every row are unaffected — F1's role counts, the
contributor-flag side of F2, and translator detection in F6, all of which
iterate the full row set.

The census reports conflicts and still exits zero, by design (see *Proposed
future checks*). A reported conflict does not invalidate the run's exit status;
it invalidates the representative-row measurements above, which must not be
read as facts about that snapshot.

### F1 — Flag distribution

| | `なし` | `あり` |
|---|---|---|
| works | 17,324 | 486 |
| persons | 1,123 | 211 |

Role cells across all rows: 著者 18,274 · 翻訳者 1,146 · 校訂者 28 · 編者 16 ·
その他 6.

### F2 — Work flag and contributor flags agree, in both directions

For all 17,810 works, `作品著作権フラグ = あり` if and only if at least one
contributor row carries `人物著作権フラグ = あり`. **Zero disagreements.**

The measurement is an equality between two fields, and on its own that is all
it is. The direction of derivation is no longer open, however: **S1 sources it
directly.** Aozora states that the work flag is set to `あり` when any
contributing person's flag is `あり`, so the work flag is computed and the
person flag is the maintained primitive. The census independently confirms the
stated rule holds across all 17,810 works.

### F3 — Observed death-year frontier among `なし` records

| flag | persons | with parseable CE year | min | max | no parseable year |
|---|---|---|---|---|---|
| `なし` | 1,123 | 1,104 | 297 | **1967** | 19 |
| `あり` | 211 | 51 | 1948 | 2023 | 160 |

Person counts by death year across the frontier:

| year | `なし` | `あり` |
|---|---|---|
| 1965 | 21 | 0 |
| 1966 | 25 | 0 |
| 1967 | **31** | 0 |
| 1968 | 0 | 0 |
| 1969 | 0 | 0 |
| 1970 | 0 | 1 |

The measurement establishes exactly three things: the maximum parseable death
year among `なし` persons is 1967; no `なし` person has a later parseable year;
and the next `あり` year outside the F4 exception set is 1970.

**It does not locate a boundary.** The catalog contains no deaths in 1968 or
1969, so the data is equally consistent with a threshold anywhere in 1967–1969.
Any sharper statement is two claims, and they belong to different questions:
what legal threshold is available to compare against is Q1; whether that
threshold produced or governs these flags is Q6.

### F4 — Four `あり` persons have a parseable death year at or below the frontier

| 没年月日 | person | 役割フラグ | 作品名 | 原題 |
|---|---|---|---|---|
| 1948-04-11 | ナワラット Ｍ・Ｒ・ニミットモンコン | 著者 | 幻想の国 | *Muang Nimit* |
| 1958 | シュヴァルツ エフゲーニイ | 著者 | 裸の王様 | *Golyi Korol* |
| 1962 | クロッサー マイラ・ジョー | 著者 | 門にて | *AT THE GATE* |
| 1963-07-07 | ニャット・リン | 著者 | 白い蝶 | *Bướm Trắng* |

All four works carry a non-empty `原題` and a `翻訳者` contributor also flagged
`あり`. Since nationality is not a catalog field, the census cannot classify
these records further.

### F5 — Text-file URL presence by flag

| | works | with non-empty `テキストファイルURL` |
|---|---|---|
| `なし` | 17,324 | 17,318 |
| `あり` | 486 | **337** |

What it signifies that a work flagged `あり` carries a text-file URL is
partially resolved — see S2 and F5b.

### F5b — Text-file URL host by flag

| flag | with URL | `www.aozora.gr.jp` | elsewhere |
|---|---|---|---|
| `なし` | 17,318 | 17,315 | 3, all `www.let.osaka-u.ac.jp` |
| `あり` | 337 | **280** | **57 across 19 hosts** |

Host is recorded because S2 makes it rights-relevant: Aozora's handling rules
turn on where a file sits and on a per-work permission notice, neither of which
URL presence alone captures. Largest non-Aozora hosts for `あり`:
`www.vesta.dti.ne.jp` (11), `attic.neophilia.co.jp` (8), `www1.bbiq.jp` (6),
`mega.nz` (5), `ryoji-kobo.jp` (5).

### F6 — Translation signals by flag

| flag | works | with a `翻訳者` contributor | with non-empty `原題` |
|---|---|---|---|
| `なし` | 17,324 | 742 | 262 |
| `あり` | 486 | 357 | 310 |

### F7 — 19 `なし` persons carry no CE year parseable by this census

Empty cells: 紫式部, 菅原孝標女, 陳玄祐, ホーマー, 文部省, 新青年編輯局,
婦人文化研究会, 日本童話研究会, ニューヨーク・サン紙, 慶應義塾, 稗田阿礼,
司馬遷, 日本国, 作者不詳, 日本経済新聞社, 長瀬金平. Non-empty but not
CE-year-parseable: ランド ハンス (`不詳`), サッフォ (`紀元前6世紀初`),
プラトン (`前347`).

Some of these name institutions and some name persons, and the parse failures
have several distinct causes. Which rule governs each, and whether the flag
means anything comparable across them, is Q3 and Q6. The census asserts only
that no CE year was extracted.

### F8 — Permission notices on the 337 `あり` works with a text URL

Reproduced by `tools/rights_permission_notice_census.py` against the same pinned
checkout. S2 names three places a permission may be recorded — the 図書カード,
the 作品ファイル, or a page about the author linked from the card — so this reads
the first two directly. None of the three is a catalog column, which is why F5b
could not settle it.

All **337** cards were read; all **337** display the `＊著作権存続＊` banner. Of
the work files, **280** are hosted on `www.aozora.gr.jp` and were read from the
snapshot; **57** are hosted elsewhere and were not inspected at all.

| Where the notice appears | Works |
|---|---|
| 図書カード | 12 |
| 作品ファイル | 46 |
| union of the two | 46 |
| card only | 0 |
| work file only | 34 |
| both, naming the same license | 12 |
| both, naming different licenses | 0 |

**The card alone is not sufficient.** Every notice found on a card is also in the
work file, while 34 works carry one *only* in the file — so for those 34 a
card-only reading reports no permission where a permission is recorded. This
extends F5b's conclusion rather than replacing it: the catalog cannot answer Q6,
and neither can the cards on their own.

License identities over the 46, normalized only for presentation (spacing,
hyphen forms, a trailing `・ライセンス` segment, and a parenthetical gloss —
Aozora writes the same license several ways):

| License as written | Works |
|---|---|
| 表示-非営利-改変禁止 2.1 日本 | 27 |
| 表示-非営利-改変禁止 4.0 国際 | 11 |
| 表示 2.1 日本 | 8 |

Two facts follow lexically and are worth stating separately, because they point
in opposite directions. **38 of the 46 name 改変禁止** — a no-derivatives term.
**8 name 表示 alone** — attribution with no non-commercial or no-derivatives
term. A permission notice existing is therefore not the same fact as a
permission covering a given use, and the 46 are not one population.

Whether any of these licenses authorizes what Soranoha's pipeline produces from a
work — parser-IR, plaintext, TEI — is a legal question about derivative works.
This census does not answer it, and per *Non-Authority* below ABC does not
answer it either.

**291 `あり` works carry a notice in neither place inspected.** That is not a
finding that no permission exists: it excludes the 57 off-host texts, the 2
archives below, and the linked author pages this census does not follow.

Two of the 280 archives cannot be read by Python's stock `zipfile` and are
reported as unread rather than as carrying no notice:
`058100` (`Bad magic number for central directory`) and `050710` (`Bad CRC-32
for file 'fushigino_kunino_alice_musical.txt'`). Both are already known from the
corpus evidence — the first is the trailing-decoy-EOCD archive that ABC's
admission path recovers, the second is the `declared_actual_size_mismatch` entry
in `data/source-bundle/aozorabunko-0e9ea3e-summary.json`. This census does not
reimplement that recovery, so its residual is an upper bound by at most two.

## What the census does and does not corroborate

Comparing the flag against a death-year threshold covers **1,155 of 1,334
persons** — the 1,104 `なし` and 51 `あり` records carrying a parseable CE year.
Within that population the threshold agrees with the flag for every person
except the four in F4.

The remaining **179 persons are outside the check's domain entirely**: 19 `なし`
and 160 `あり` records carry no parseable CE year, so the comparison returns
neither agreement nor disagreement for them. They are uncorroborated, not
corroborated.

## Sources

Aozora's own pages ship **inside the pinned checkout**, so these are cited by
path at commit `0e9ea3e`, not by live URL: content-addressed by the same
snapshot as the CSV, readable offline, and immune to the page changing under
the citation. Quotations below were read from those files directly.

Live-URL fetching was tried first and proved unreliable on exactly the passage
that mattered — two fetches of the same 2011 page returned different, partial
flag vocabularies and neither surfaced S1's sentence. Read the pinned files.

### S1 — `soramoyou/soramoyou2011.html`, entry dated 2011-01-13

> 「作品著作権フラグ」（K）は、著者の他、翻訳者等、その作品に関わった人物のうち、
> 一人でも「人物著作権フラグ」（Z）が「あり」であれば、「あり」になる。

Announces the extended CSV and documents the column semantics. Sources F2's
direction of derivation: the work flag is computed from the contributor flags
by disjunction. Note the entry describes a provisional release open for comment
until 2011-03-15, so it documents the format's introduction, not necessarily
its final state.

### S2 — `guide/kijyunn.html`, 青空文庫収録ファイルの取り扱い規準

Aozora's operative handling rules, and the richest Q6 material found so far.

- **Expired works** — 「ファイルは、有償・無償であるかを問わず、自由に複製・再配布・共有することができます」, with adaptation and translation likewise free and no permission or payment owed to Aozora.
- **Non-expired works** — reproduction only within 著作権法第三十条 private use; 「私的使用の範囲を越える利用および複製・再配布は、著作権者の許しがない限り、できません」.
- **Where permission lives** — 「図書カード・作品ファイル中、もしくは図書カードからリンクした作者にかかわるウェッブページに、著作権者による特別の許諾（クリエイティブ・コモンズ・ライセンス等）が明記されていれば、あなたはその範囲内で、利用および複製と再配布を行うことができます」.
- **Translations** — 「翻訳された作品では、書いた人に加え、訳した人にも著作権が生じます。書いた人の権利が切れていても、訳した人の権利が生きていれば、その作品は「著作権の切れていない作品」に該当します」.
- **The catalog's own licence** — the 書架情報（作家別作品一覧CSVファイル）, this census's input, is 「クリエイティブ・コモンズ 表示 4.0 国際 ライセンス」.

Three consequences matter more than the quotations.

**The permission basis is not in the catalog.** A special permission is
effective only where it is written on the card, in the file, or on a linked
author page. Nothing in the CSV records it. So the 337 `あり` works with text
URLs cannot be resolved from catalog data at any level of care — the question
is answerable only by inspecting cards, which this census has not done.

**Aozora's translation rule matches F4 and F6 exactly.** A work whose author's
rights have expired but whose translator's have not *is* a non-expired work by
their stated rule. That is precisely the shape of F4's four pre-frontier `あり`
persons, each paired with an `あり` translator.

**Reusing the catalog data carries an attribution obligation.** CC BY 4.0
governs our republication of the flag values themselves — a distinct question
from the rights status of the works, and one that applies regardless of how
Q1–Q8 resolve.

### S3 — `guide/aozora_bunko_faq.html`

Confirms the CSV's CC BY 4.0 licence independently. It does **not** contain a
statement that protected works hosted on Aozora's own server necessarily carry
a Creative Commons licence; the pinned copy's CC references concern the CSV
files and the Aozora logo. A live fetch appeared to show such a passage, which
is either a post-snapshot change or a summarizer artifact. Under S2 the
operative rule is per-card notice, not host, so nothing here licenses an
inference from F5b's 280 on-server works.

## Open Questions — unsourced, must not be treated as decided

Each needs an authoritative citation with a retrieval date before any mapping
depends on it. The parenthetical is the hypothesis to test, not a finding.

- **Q1 — What is the applicable Japanese term, and its transition rule?**
  (Hypothesis: a 50-year post-mortem term, with the 2018-12-30 extension to 70
  years not reviving already-expired terms.) Needs: 著作権法 art. 51, the TPP11
  implementing amendment and its effective date, and 文化庁 on non-revival.
  This asks only what the law is. Whether Aozora applies it, and whether the
  classification in F3 was produced by it at all, is editorial process and
  belongs to Q6 — sourced law cannot establish what a third party did.
- **Q2 — When within a year does a term expire?** (Hypothesis: 暦年主義, art. 57
  — the term runs to 31 December of the final year.) Note what this can and
  cannot settle: art. 57 fixes `status_effective_at` **only under an assessment
  ABC performs itself** from Q1–Q5. It says nothing about the effective date of
  an assertion someone else made. See *Three distinct times* below.
- **Q3 — What rule governs anonymous, pseudonymous, and corporate works?**
  (Hypothesis: arts. 52–53, a publication-based term.) Bears on F7.
- **Q4 — How is a foreign work's Japanese term determined?** (Hypothesis:
  comparison of terms under art. 58 plus 戦時加算 for nationals of Allied powers
  under the peace treaty.) Bears on F4 and the translated works in F6.
- **Q5 — How do an original author's and a translator's terms compose?**
  (Hypothesis: art. 28 — a translation is a derivative work and both terms must
  have run.) **Partly answered by S2**, which states the composition rule as
  Aozora's operating practice and is what F4 and F6 measure the effect of. What
  remains open is the statutory citation itself: S2 establishes what the source
  does, not what the law requires.
- **Q6 — What do Aozora's flags mean operationally?** **Partly answered.** S1
  settles which field is maintained versus computed. S2 settles what `あり`
  implies for reuse (private use only, absent a per-work permission notice) and
  where such a permission is recorded (the card, the file, or a linked page —
  never the catalog). **F8 settles the per-work survey**: of the 337 `あり` works
  with a text URL, 46 carry a permission notice in the card or the file, 291
  carry one in neither place inspected, and the notice is in the work file alone
  for 34 of the 46 — so a card-only reading is unsound. F8 also shows the 46 are
  not one population: 38 name a no-derivatives term, 8 name attribution only.
  Still open: who assesses a flag, against which jurisdiction, whether the Q1
  rule is the one applied, when a flag is re-evaluated, whether any of the
  licenses in F8 authorizes ABC's derived representations, and what is recorded
  for the 57 off-host texts and the linked author pages F8 did not inspect.
  **This remains the load-bearing question**: it decides not just how a mapping
  is built but whether one exists to build. See *What Q6 decides* below.
- **Q7 — Is a flag jurisdiction-scoped to Japan?** Any external IRI is read
  globally, so an unscoped assertion overclaims wherever the term differs.
  Asked per documented Q6 cell, not once for the corpus.
- **Q8 — Which external vocabulary term is correct**, given Q7, and may it be
  emitted without a jurisdiction qualifier? Also per cell: a cell Q6 leaves
  unresolved has no mapping for Q8 to evaluate.

## Non-normative census partition

Three counts of catalog records grouped by lexical properties. **No outcome is
attached to any row, and none is implied for any other row.** This is not the
shape of a proposed mapping; see *What Q6 decides* for why no such shape exists
yet.

| Partition | Works |
|---|---|
| `なし`, every contributor has a parseable CE year ≤ 1967, no `翻訳者` and no `原題` | 16,558 |
| `なし`, not date-checkable or carrying a translation signal | 766 |
| `あり` | 486 |
| total | 17,810 |

The 766 decompose as 657 with translation signals and otherwise-checkable
dates, 15 with an unparseable date only, and 94 with both.

## What Q6 decides

Q6 is not one global question about "the flag", and answering it once would
build the wrong thing. There are two flag fields and two lexical values, the
remediation design keeps work and person assessments separate, and Aozora may
document any cell without documenting its neighbours. Q6 is therefore a
decision matrix — **field × lexical value × documented scope → source semantics
or unresolved** — with every cell currently open:

| Field | Value | Documented semantics | Documented scope |
|---|---|---|---|
| `作品著作権フラグ` | `なし` | S2: free reuse, no permission owed | jurisdiction unresolved |
| `作品著作権フラグ` | `あり` | S2: private use only, unless a per-card notice grants more | per-work, **not in the catalog** |
| `人物著作権フラグ` | `なし` | unresolved — S1 makes it primitive, S2 speaks only of works | unresolved |
| `人物著作権フラグ` | `あり` | unresolved — S1 makes it primitive, S2 speaks only of works | unresolved |

The asymmetry S1 and S2 create is worth stating plainly: the **work** flag now
has documented reuse semantics, while the **person** flag — the maintained
primitive that the work flag is computed from — has none. Aozora documents the
consequence, not the criterion. And the remediation design keeps work and
person assessments separate, so the person cells cannot inherit the work cells'
answers.

The asymmetries are the point: work `なし` may be documented while person `なし`
is not; `なし` may carry an exact meaning while `あり` carries none, or the
reverse; and documented applicability may vary by record class within a single
cell. Q7 and Q8 then evaluate an external mapping **per documented cell**, not
once for the corpus — a cell with no documented semantics has no mapping to
evaluate, whatever its neighbours resolve to.

Within any single cell, three branches remain, and this census's partition
means something different in each:

- **If the cell is documented as the assertion the design needs**, the
  partition is irrelevant to its outcome — every record in that cell is covered
  and the date split does no work.
- **If it is not documented**, those records stay source-only. Death-year
  arithmetic cannot rescue them: an undocumented flag plus a computation of our
  own is two unsourced inferences, not one sourced one.
- **If the documentation bounds the cell's applicability**, that documented
  scope determines the partition, and this census's heuristic — parseable date
  below a frontier, no translation signal — is superseded by it.

Which branch holds for which cell is unknown, so no completeness cost, coverage
figure, or recovery plan can be stated yet. Any of them would presuppose an
answer.

### What a negative answer does not block

Source-only is a designed outcome, not a failure. The remediation design admits
`assessment_status` values `public-domain | in-copyright | undetermined |
not-evaluated`, and its SHACL contract admits source-only and undetermined
records. So if no cell resolves to an exact mapping:

- external rights IRIs are blocked, and
- release publication is **not**, once the assessment schema and migration are
  complete and every record carries a valid assessment — which
  `undetermined` and `not-evaluated` are.

An ABC-performed legal assessment is therefore an optional, separately
authorized route to *richer* rights statements. It is not the only route to an
admissible publication, and this document should not be read as making the
publication gate contingent on a positive Q6.

### Attribution is not semantic sufficiency

A flag can be *attributed* — a named body, a snapshot commit, an observation
date — and that is enough to preserve the lexical fact "Aozora supplied `なし`
for this work" as a provenance-bearing source assertion. It is not enough to
emit an external rights IRI. Attribution establishes who said what and when we
saw it; Q6–Q8 establish whether what they said has an exact mapping to a rights
statement, under which jurisdiction, in which vocabulary. Conflating the two is
how a citation becomes a legal claim, which is the fault this ADR exists to
remove.

Deriving a term ourselves is not the alternative: it substitutes one unsourced
legal inference for another. Whatever role the date comparison eventually plays
is a corroborating one, over the 1,155-person population it actually covers.

### Three distinct times

The snapshot binding establishes exactly one of them, and the record must keep
them apart rather than let a known value stand in for an unknown one:

| Field | Status |
|---|---|
| `retrieved_at` | **Known.** When Soranoha observed the cell: 2026-07-26. The commit and hashes bind *which content* was observed, not *when* — they are snapshot identity, so the date must be recorded separately and not inferred from them. |
| source `assessed_at` | **Unknown** unless Aozora documents when a flag was set or last reviewed (Q6). |
| `status_effective_at` | **Unknown** unless supplied by the source, or produced by an ABC assessment that has been explicitly authorized to make one. |

Only one authority may govern `assessment_status`, `status_effective_at`, and
the external statement for a given record. If Aozora's flag is that authority,
`status_effective_at` comes from documented source semantics or stays null —
art. 57 cannot supply it, because we would then be dating someone else's
assertion by our own computation. If ABC performs its own assessment, Q1–Q5
govern the date and the flag becomes corroborating input.

What is *not* exclusive is the lexical source assertion. "Aozora supplied
`なし` for this work, observed on this date" is preserved under either model and
sits alongside whichever assessment governs. Only the assessed fields have a
single authority; the record of what the source said always survives.

## Proposed future checks — not implemented

`tools/rights_source_census.py` reports and always exits zero. It is a census,
not a gate. Two invariants from this snapshot are candidates for a gate once a
policy exists for them to defend, which is Task 3 or 4, not this document:

- F2's biconditional across all works (currently 17,810 / 17,810).
- Flag-versus-threshold agreement across the parseable-year population
  (currently 1,151 of 1,155 agreeing, with F4 as the declared exception set).

Both would need a committed baseline and non-zero exit on violation. The 1967
constant in the census script is a reporting parameter carrying no authority.

## Governance

Under `subtractive-evidence-simplification`, this is ordinary evidence at a
repository path, not a registry entry: it lives under `docs/evidence/external/`,
git records its state, and there is no hash registration or evidence-kind
declaration. When Q1–Q8 close it becomes citable as the `:evidence` path on
claim `:c4` of `rights-assessment-and-external-statements`, whose statement is
the promotion condition that authoritative evidence define the Aozora source
fields and every enabled external mapping. That claim carries no evidence path
today, and this document does not yet satisfy it.

Task 2 of the remediation plan additionally directs registering this document
as `:external-authority` with retrieval and review dates. That instruction
predates `subtractive-evidence-simplification` (Accepted 2026-07-23) and no
longer describes the governance model; the path citation above replaces it.

## Assumptions

- The catalog CSV is the authoritative expression of Aozora's rights
  bookkeeping; the per-work HTML card is not separately consulted.

## Falsifiers

- Any work where `作品著作権フラグ` is not equal to the disjunction of its
  contributors' flags (F2 currently 17,810 / 17,810).
- Any `なし` person with a parseable death year later than 1967, or any `あり`
  person with a parseable year at or below 1967 outside the F4 exception set —
  which would falsify the 1967 threshold as a description of the catalog.
- Any conflicting repeated cell in the eight columns of F0, which would
  invalidate the representative-row measurements F0 enumerates.
- Aozora documentation under Q6 establishing that the flags are not assessed
  per the Q1 rule, which would falsify the corroboration policy — the threshold
  comparison would then be measuring against a rule the source does not use.
  Note that a mismatch between sourced law and the catalog is a fact about
  Aozora's process or about the threshold hypothesis, never a defect in the law.
- Aozora documentation under Q6 that contradicts this census's reading of any
  field, including the assumption that the CSV is authoritative.

## Non-Authority

This document establishes no rights status for any work. It grants no
publication authority, does not authorize an external `dcterms:rights`
statement, and does not satisfy the promotion conditions of
`rights-assessment-and-external-statements`. `data/publication-policy.edn`
remains `:blocked-pending-assessment-migration` and must not be changed on the
strength of this document alone. The Observed Facts may be relied on as lexical
measurements of the pinned snapshot; the Open Questions may not be relied on at
all.
