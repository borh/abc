# Verification memo: aozora2html bucket characterization & aozora-rs path

Date: 2026-07-03
Status: verification complete
Reviews: `docs/handoffs/aozora2html-buckets-and-aozora-rs-path.md`
Method: independent re-derivation of every quantitative claim from
`/db/ab-validator/aat-corpus/aozora2html-full-20260703T020301Z/` report JSONs,
the `triage/outputs/*.csv` files, and the in-repo source.

## Verdict

The handoff's **decision (Option 3) is sound and boundary-aligned**, and its
bucket-characterization structure is correct. The bucket sizes reproduce
**exactly as report-counts**. However the handoff carries one **numerically
impossible** sub-claim, one **non-reproducible** sub-claim, and a systematic
**"works" vs "reports" labeling error**. These do not change the decision but
should be corrected before this handoff is cited downstream.

## Claims that verify exactly

| Claim | Source | Reproduced |
|---|---|---|
| `adapter_timeout` = 196 | `adapter_errors.csv`, `property_failures.csv`, full report scan | yes (196 reports; 194 unique work_ids) |
| `adapter_protocol_error` = 1, `000125_1317`, exit 101, no AAT | `check-reports/.../000125_1317-*.json` | yes (report has only `adapter_protocol_error`; no `aat/...` file) |
| `parse_incomplete` (parse_completeness fails) = 105 | `property_failures.csv` failures col; report scan | yes (105 reports; 103 unique work_ids) |
| `report_failed_other_property` = 710 with breakdown 565 / 88 / 24 / 9 / 9 / 9 / 6 | full report scan, **per-report** counting | yes — reproduces **exactly** via per-report (per-file) counting, not per-work |
| per-property `visible_text_body_order` 668 / `gaiji_resolution` 190 / `ruby_completeness` 127 / `parse_completeness` 105 | `property_failures.csv` | yes, all four exact |
| `visible_text_body_order` = 662 affected_works | `property_failures.csv` `affected_works` | yes |
| `source_feature_without_aat_observation` = 32 (29 warigaki + 3 kunten) | policy-residual-triage report table | yes |
| warigaki/kunten residual splits 32+36 timeout, 16+151 report_failed | policy-residual-triage report | yes |
| harness: exit 0/2 → success, 1 → `fatal_error`, else → `adapter_protocol_error`, timeout → `adapter_timeout` | `crates/ab-check/src/check.rs::invoke_and_check` | yes |
| wrapper converts non-zero Ruby parser exit to `--parser-failed` (does not exit non-zero) | `adapters/aozora2html/aozora2html-adapter` | yes |
| aozora-rs call sites `aat.rs:464,642,753` call `ab_ir::block_content_mut` | grep | exact |
| `ab-ir` has no `fn block_content_mut`; `block_content` (immutable) is the only accessor | grep + standalone `cargo build` failure (3× E0425) | exact |
| `Block::Break { kind: BreakKind }` only — no `content` field | `crates/ab-ir/src/lib.rs:38-39` | exact |
| git history: `block_content_mut` added `983a343`, removed `60841e2` | `git log -S` | exact |
| `adapters/aozora-rs` excluded from workspace `Cargo.toml` | `Cargo.toml` exclude list | exact |
| `justfile` has no aozora-rs recipes; all aozora recipes target aozora2html | `justfile` | exact |
| boundary decision quotes (AAT JSON normative; ab-ir optional convenience) | `docs/superpowers/specs/2026-07-03-mapping-and-adapter-boundary-decision.md` | verbatim match |
| `000125_1317` is a Rust panic, not a parser-level error | exit 101 + no AAT written + wrapper `exec`s mapper last | supported (see caveat below on "before any parser-failed envelope" phrasing) |

## Defects found

### D1 — Systematic "works" vs "reports" label error (bucket sizes)

The corpus has **236 duplicate work_ids** (the same work measured across
different zip entries / txt_paths). Every bucket "count" in the handoff is a
**per-report** count, but the handoff labels them "works":

| Bucket | Handoff says | Actual reports | Actual unique works |
|---|---|---|---|
| adapter_timeout | "196 works" | 196 | **194** |
| parse_incomplete | "105 works" | 105 | **103** |
| report_failed_other_property | "710 works" | 710 | **~704** |

The numbers reproduce exactly as report-counts; only the noun is wrong. Worse,
§3.4 mixes per-report counts (the 565/88/…/710 table) with per-work counts
("668 failures / 662 works" taken from `property_failures.csv`'s `affected_works`)
in the same section without flagging that 662 and 565 are different units
(per-work vs per-report, and "all VTBO failures" vs "VTBO-only failures").

**Fix:** relabel "works" → "reports" throughout §3, or state the per-work
deduped count alongside the report count, and explicitly note the 236 duplicate
work_ids in §1 so readers know the two units diverge.

### D2 — `parse_incomplete` co-failure figure is impossible (§3.3)

Handoff: *"64 works fail all three of `gaiji_resolution`, `parse_completeness`,
`ruby_completeness`; the remainder fail two of the three."*

Re-derived from all 105 parse_incomplete reports:

| Pattern | Reports |
|---|---|
| fail all three (gaiji + parse + ruby) | **59** |
| fail two of three | **39** |
| fail only `parse_completeness` | **7** |

`64` is not just inaccurate, it is **arithmetically impossible**: among
parse_incomplete reports, `gaiji_resolution` fails in only **63** reports, so
"fail all three" cannot exceed 63. The "remainder fail two of the three" also
omits the 7 reports that fail only `parse_completeness`.

**Fix:** 64 → 59; add the 39-two-of-three and 7-only-parse rows.

### D3 — `parse_incomplete` root-cause split is non-reproducible (§3.3)

Handoff: *~48 Ruby structural / ~32 malformed XHTML or Ruby internal / ~25
other parser aborts.*

The 105 warnings are almost all distinct strings (most appear once), so any
3-way split is a hand classification. A clean regex reclassification yields
**56 structural / 47 XHTML-or-Ruby-internal / 2 other**. The handoff's "other
= 25" bucket corresponds to multi-byte-punctuation warnings followed by a
structural error — but those are **structural errors with a warning preamble**,
so counting them as "other" is poorly motivated. The qualitative shape
(~half parser-structural, ~half mapper/XHTML robustness) is supportable; the
**48 / 32 / 25 numbers are not**.

**Fix:** drop the precise counts (keep "~half / ~half" with examples) or publish
the exact classification rules so the split is reproducible.

### D4 — Wrapper structure mischaracterized (§3.1)

Handoff: *"the wrapper pipelines Ruby parser → Rust mapper inside a single
`nix develop` invocation."*

Only the **Ruby parser** runs inside `nix develop`; the Rust mapper runs
**directly** via `exec "$RUST_MAPPER_BIN"`. The harness still kills all stages
on timeout (it kills the wrapper child, which kills the `nix develop` subtree),
so the conclusion ("harness cannot tell which stage hung") holds. But "single
`nix develop` invocation" is wrong about the structure.

**Fix:** "the wrapper runs the Ruby parser under `nix develop` and then `exec`s
the Rust mapper; the harness times the whole wrapper, so the stage that hung is
not distinguishable."

### D5 — Timeout file-size numbers: minor drift (§3.1)

| Metric | Handoff | Verified |
|---|---|---|
| timeout median size | 372 KB | 367 KB (194 unique works) |
| timeout works > 500 KB | 51 | 50 |
| corpus works > 500 KB | 90 of 17 886 | 89 of 17 885 |
| corpus median | 11 KB | 10.6 KB |

Differences are 1–2 counts / 5 KB, attributable to per-report-vs-per-work dedup
plus one size unresolved. The characterization (strong size correlation, ~26 %
of timeouts > 500 KB vs ~0.5 % corpus-wide) is **solid**. Relabel "works" →
"reports" or restate as per-work, and the numbers are within noise.

### D6 — Drift-doc line number wrong

`docs/handoffs/aozora-rs-block-content-mut-drift.md` says
`crates/ab-ir/src/lib.rs:1159 defines pub fn block_content(...)`. Actual line is
**1153** (confirmed by grep and by the `cargo build` error message the compiler
itself prints). 6-line drift; trivial fix.

## Decision soundness — Option 3

**Supported and verified.** The load-bearing premises all check out:

- AAT JSON is the normative contract; `ab-ir` is explicitly optional
  in-workspace convenience — verbatim from the boundary spec.
- aozora-rs is excluded from `Cargo.toml`, has no `justfile` recipes, and the
  current full-corpus run uses `aozora2html-adapter` (`metadata.json`).
- `block_content_mut` cannot be cleanly restored: `Block::Break { kind }` has no
  `content: Vec<Inline>`, so a `_mut` accessor would need a static empty Vec or
  `panic!` arm. Option 2 is correctly rejected.
- Standalone build fails with exactly 3 E0425 — does **not** affect `cargo test
  --workspace` (aozora-rs is excluded). The "broken-with-notice" state is
  low-risk and reversible.

**One gap in the rationale:** the handoff argues Option 3 partly on "aozora-rs
is not a live measurement target," which is verified, but it does **not**
acknowledge that `adapters/aozora-rs/` had active commits as recently as
**2026-05-04** (`ed37b70` "Improve aozora-rs AAT oracle fidelity" + `2ef2dc5`,
`eacd5b3` over 2026-05-03/04) — only ~2 months before the boundary decision.
The drift doc itself flagged aozora-rs as "live." Someone was actively investing
in the typed path. The decision is still defensible (the boundary decision
postdates and supersedes that direction), but the README notice required by §4.5
should record the **last-active commit date and the re-evaluation trigger**
("revisit only if aozora-rs is revived as a JSON-emitting adapter"), so the
broken state is not silently forgotten if the May-era author returns.

**Counter-argument the handoff should at least name:** a 3-call-site migration
to `block_content` + replacement-block construction is small and would keep the
adapter buildable, preserving it as a typed reference. The handoff treats this
as "wasted work on a non-gated adapter" — a judgment call, not a verified fact.
It is a reasonable judgment given the boundary decision, but it should be
framed as a trade-off (lower maintenance burden now vs. preserved optionality),
not a factual conclusion.

## Bottom line

- **Decision: approve Option 3** as recorded, with the README notice + last-commit
  date + re-evaluation trigger added.
- **Characterization: structurally correct; correct the works-vs-reports labels
  (D1), the impossible 64 (D2), the non-reproducible 48/32/25 (D3), the
  nix-develop phrasing (D4), and the line number (D6) before this handoff is
  treated as authoritative.**
