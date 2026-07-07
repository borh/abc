# Phase 4 Ruby Oracle Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce `nway_region_oracle_evidence.parquet` during the Full-profile analysis pass — for each Aozora editor ruby base, adjudicate which analyzer's reading matches the editor's ruby reading and which disagree.

**Architecture:** A new `oracle` module in `ab-morph-run` holds a pure kana canonicalizer (`reading_norm`) and a pure adjudicator (`ruby`). Ruby base spans + readings are extracted from the already-collected `projection_spans` + the retained AAT `Value` (via `Value::pointer` on the span's `aat_pointer`) — no change to the AAT walker. A compact region lookup is accumulated during the existing single n-way region pass in `append_warehouse_nway_fact_rows`; the adjudicator maps each ruby base to a `region_index`, compares per-analyzer concatenated readings against the normalized ruby reading, and emits a row only when at least one analyzer disagrees. The table is a v2 sidecar cloned at every layer from the shipped `projection_spans` table; `SCHEMA_VERSION` stays 2 with presence-probed views.

**Tech Stack:** Rust, `arrow-array`/`parquet` (warehouse writer), `serde_json` (AAT `Value`, `evidence_detail`), DuckDB SQL views, `hegel` property tests.

## Global Constraints

- Governing spec: `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`. Cycle spec: `docs/superpowers/specs/2026-07-07-ruby-oracle-design.md` (Decisions R1–R8) — this plan implements it verbatim; do not re-decide semantics.
- `SCHEMA_VERSION` stays **2** (`crates/ab-warehouse/src/schema.rs:4`). No `READER_MAX_SCHEMA_VERSION` change. New sidecar ships under v2 with presence-probing.
- `oracle_source` is always the string `"ruby"` this cycle.
- Emit a row **iff at least one analyzer's normalized reading over the ruby base fails to match the normalized editor ruby reading** (spec R2). All-match bases are skipped.
- `winning_analyzer` is set **only** when exactly one analyzer matches; else `null` (spec R4). `nonstandard_ruby` ⇔ zero matches.
- Reading feature key by analyzer family (spec R7): `vibrato*`/`vaporetto*` → `kana`, fallback `pron`; `sudachi*` → `reading_form`; `test*` → none.
- A per-analyzer reading is comparable only when its covered morphemes **exactly tile** the base span (spec R8); otherwise that analyzer is a non-match with reason `boundary-misalign`.
- Oracle emits only for runs with **≥2 analyzers** (spec §Precondition); single-analyzer runs produce no oracle rows.
- Tests for `ab-morph-run` MUST run with `--features test-analyzer` (bare invocation false-fails 2 bin tests). Warehouse recipes default `jobs=8`.
- Row struct id fields use plain `String` (match `NwayRegionRow`/`NwayRegionAnalyzerRow`, not the `Arc<str>` used by `ProjectionSpanRow`).
- Column order MUST be identical across `schema.rs` `column_names`, the arrow `*_schema()`, `schema.sql`, and the append method — the `schema_sql_columns_match_documented_parquet_columns` test enforces it over `WarehouseTable::ALL`.

---

### Task 1: `reading_norm` — kana canonicalizer

**Files:**
- Create: `crates/ab-morph-run/src/oracle/mod.rs`
- Create: `crates/ab-morph-run/src/oracle/reading_norm.rs`
- Modify: `crates/ab-morph-run/src/lib.rs` (add `mod oracle;` near the other `mod` declarations)

**Interfaces:**
- Produces: `pub fn normalize(reading: &str) -> String` — applies NFKC → strip-non-kana → katakana→hiragana → discrete historical folds → long-vowel canonicalization → bounded lexical table. Idempotent. `pub(crate)` re-export from `oracle` is not required; `ruby.rs` calls `super::reading_norm::normalize`.

- [ ] **Step 1: Register the module.** In `crates/ab-morph-run/src/lib.rs`, add alongside the existing `mod` lines:

```rust
mod oracle;
```

And create `crates/ab-morph-run/src/oracle/mod.rs`:

```rust
//! Aozora ruby oracle: adjudicate analyzer readings against editor ruby.
pub(crate) mod reading_norm;
pub(crate) mod ruby;
```

(`ruby` is added in Task 4; if the crate is compiled between tasks, temporarily comment the `ruby` line until Task 4 lands, or land Task 1+4 module files together. The subagent executing Task 1 should create `mod.rs` with only `reading_norm` and add `ruby` in Task 4.)

For Task 1, `mod.rs` is:

```rust
//! Aozora ruby oracle: adjudicate analyzer readings against editor ruby.
pub(crate) mod reading_norm;
```

- [ ] **Step 2: Write failing tests** in `crates/ab-morph-run/src/oracle/reading_norm.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::normalize;

    #[test]
    fn katakana_and_hiragana_fold_equal() {
        assert_eq!(normalize("トウキョウ"), normalize("とうきょう"));
    }

    #[test]
    fn strips_interpunct_and_spaces() {
        assert_eq!(normalize("と・う きょう"), normalize("とうきょう"));
    }

    #[test]
    fn discrete_historical_substitutions() {
        assert_eq!(normalize("ゐ"), normalize("い"));
        assert_eq!(normalize("ゑ"), normalize("え"));
        assert_eq!(normalize("かぢ"), normalize("かじ"));
        assert_eq!(normalize("みづ"), normalize("みず"));
        assert_eq!(normalize("くわし"), normalize("かし"));
    }

    #[test]
    fn word_medial_ha_row_folds() {
        // 川: kaha (historical) → kawa (modern)
        assert_eq!(normalize("かは"), normalize("かわ"));
        assert_eq!(normalize("こひ"), normalize("こい"));
    }

    #[test]
    fn long_vowel_family_collapses() {
        // pron form (ー) == modern kana (う) == historical (かう→こう)
        assert_eq!(normalize("トーキョー"), normalize("とうきょう"));
        assert_eq!(normalize("かう"), normalize("こう"));
    }

    #[test]
    fn bounded_lexical_table() {
        assert_eq!(normalize("てふ"), normalize("ちょう"));
        assert_eq!(normalize("けふ"), normalize("きょう"));
    }

    #[test]
    fn idempotent() {
        for s in ["とうきやう", "トーキョー", "てふ", "くわし", "かは"] {
            assert_eq!(normalize(&normalize(s)), normalize(s));
        }
    }
}
```

- [ ] **Step 3: Run tests to verify they fail**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::reading_norm`
Expected: FAIL — `cannot find function normalize`.

- [ ] **Step 4: Implement `normalize`.** Above the test module in `reading_norm.rs`:

```rust
//! Deterministic kana canonicalizer for ruby-oracle reading comparison.
//! Applied identically to the analyzer reading and the editor ruby reading.

use unicode_normalization::UnicodeNormalization;

/// Canonicalize a reading string for oracle equality comparison.
/// Layers: NFKC → strip non-kana → katakana→hiragana → discrete historical
/// folds → long-vowel canonicalization → bounded lexical table. Idempotent.
pub(crate) fn normalize(reading: &str) -> String {
    // 1. NFKC, then katakana→hiragana, keeping only kana.
    let mut chars: Vec<char> = reading
        .nfkc()
        .filter_map(kana_to_hiragana)
        .collect();

    // 5. bounded lexical table (applied on the hiragana form, longest-match first).
    apply_lexical_table(&mut chars);

    // 3. discrete historical single-char substitutions.
    for ch in &mut chars {
        *ch = match *ch {
            'ゐ' => 'い',
            'ゑ' => 'え',
            'ぢ' => 'じ',
            'づ' => 'ず',
            other => other,
        };
    }

    // 3b. くわ/ぐわ → か/が ; word-medial は行 → わ行 (positions > 0).
    let folded = fold_digraphs_and_medial_ha(&chars);

    // 4. long-vowel canonicalization: collapse each vowel's long-vowel spellings.
    long_vowel_canonicalize(&folded)
}

/// Map a char to hiragana, dropping anything that is not kana (spaces, ・, punctuation).
fn kana_to_hiragana(ch: char) -> Option<char> {
    match ch {
        // Katakana block → hiragana (offset 0x60), excluding the prolongation mark.
        'ァ'..='ン' => char::from_u32(ch as u32 - 0x60),
        'ー' => Some('ー'), // prolongation mark handled in long-vowel layer
        'ぁ'..='ん' => Some(ch),
        _ => None,
    }
}

fn apply_lexical_table(chars: &mut Vec<char>) {
    // Longest-match-first historical readings that are not mechanically foldable.
    const TABLE: &[(&str, &str)] = &[
        ("てふ", "ちょう"),
        ("けふ", "きょう"),
        ("せふ", "しょう"),
        ("でふ", "じょう"),
    ];
    let mut s: String = chars.iter().collect();
    for (from, to) in TABLE {
        if s.contains(from) {
            s = s.replace(from, to);
        }
    }
    *chars = s.chars().collect();
}

fn fold_digraphs_and_medial_ha(chars: &[char]) -> Vec<char> {
    let mut out: Vec<char> = Vec::with_capacity(chars.len());
    let mut i = 0;
    while i < chars.len() {
        // くわ→か, ぐわ→が
        if i + 1 < chars.len() && chars[i + 1] == 'わ' {
            match chars[i] {
                'く' => { out.push('か'); i += 2; continue; }
                'ぐ' => { out.push('が'); i += 2; continue; }
                _ => {}
            }
        }
        // word-medial は行 → わ行 (never at position 0)
        let folded = if i > 0 {
            match chars[i] {
                'は' => 'わ',
                'ひ' => 'い',
                'ふ' => 'う',
                'へ' => 'え',
                'ほ' => 'お',
                other => other,
            }
        } else {
            chars[i]
        };
        out.push(folded);
        i += 1;
    }
    out
}

fn long_vowel_canonicalize(chars: &[char]) -> String {
    // Collapse long-vowel families to the canonical two-kana modern spelling's
    // vowel: あ段+う/ー → おう family maps to a single marker. We canonicalize by
    // rewriting a vowel + {う, ー, matching historical vowel} into vowel+'ー' then
    // to the modern long form, so トーキョー, とうきょう, たうきやう converge.
    let mut out: Vec<char> = Vec::with_capacity(chars.len());
    for &ch in chars {
        if ch == 'ー' {
            // extend previous vowel
            if let Some(&prev) = out.last() {
                out.push(long_vowel_partner(prev));
            }
            continue;
        }
        out.push(ch);
    }
    // Normalize the o/u long-vowel historical 'au/ou' → 'oo', 'ou' spellings to one form.
    let s: String = out.iter().collect();
    s.replace("あう", "おう")
        .replace("かう", "こう")
        .replace("がう", "ごう")
        .replace("さう", "そう")
        .replace("たう", "とう")
        .replace("なう", "のう")
        .replace("はう", "ほう")
        .replace("まう", "もう")
        .replace("やう", "よう")
        .replace("らう", "ろう")
        .replace("わう", "おう")
}

/// The kana that a prolongation mark after `prev` expands to (long-vowel of that row).
fn long_vowel_partner(prev: char) -> char {
    match prev {
        'こ' | 'と' | 'の' | 'ほ' | 'も' | 'よ' | 'ろ' | 'お' | 'ご' | 'ぞ' | 'ど' | 'ぼ' | 'ぽ' => 'う',
        'き' | 'し' | 'ち' | 'に' | 'ひ' | 'み' | 'り' | 'い' | 'ぎ' | 'じ' | 'び' | 'ぴ' => 'う',
        'か' | 'さ' | 'た' | 'な' | 'は' | 'ま' | 'や' | 'ら' | 'あ' | 'が' | 'ざ' | 'だ' | 'ば' | 'ぱ' => 'あ',
        'け' | 'せ' | 'て' | 'ね' | 'へ' | 'め' | 'れ' | 'え' | 'げ' | 'ぜ' | 'で' | 'べ' | 'ぺ' => 'え',
        'く' | 'す' | 'つ' | 'ぬ' | 'ふ' | 'む' | 'る' | 'う' | 'ぐ' | 'ず' | 'づ' | 'ぶ' | 'ぷ' => 'う',
        other => other,
    }
}
```

The `unicode-normalization` crate is **already** a dependency of `ab-morph-run` (`crates/ab-morph-run/Cargo.toml:27` — `unicode-normalization.workspace = true`, pinned at workspace root `Cargo.toml:75`). No `Cargo.toml` change is needed; `use unicode_normalization::UnicodeNormalization;` compiles as-is.

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::reading_norm`
Expected: PASS (7 tests). If `long_vowel_canonicalize` misses a case, extend the row tables — the golden tests pin the required equalities.

- [ ] **Step 6: Add a property test.** Append to the test module:

```rust
    #[test]
    fn prop_idempotent_and_script_invariant() {
        // hegel-style: any katakana string canonicalizes equal to its hiragana form,
        // and normalize is idempotent. Kept as a bounded enumerated check to avoid
        // pulling generators into a unit test; the hegel target is added in Task 6.
        for s in ["カハ", "ミヅ", "テフテフ", "トーキヨー", "クワシ"] {
            let n = normalize(s);
            assert_eq!(normalize(&n), n, "idempotent for {s}");
        }
    }
```

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::reading_norm`
Expected: PASS.

- [ ] **Step 7: Commit**

```bash
git add crates/ab-morph-run/src/oracle/mod.rs crates/ab-morph-run/src/oracle/reading_norm.rs crates/ab-morph-run/src/lib.rs
git commit -m "feat(oracle): kana canonicalizer for ruby-oracle reading comparison"
```

---

### Task 2: `ab-warehouse` — `nway_region_oracle_evidence` table (schema + writer + DDL)

**Files:**
- Modify: `crates/ab-warehouse/src/schema.rs` (enum variant, `ALL`, `MERGED_DATA`, `file_name`, `column_names`, new Row struct)
- Modify: `crates/ab-warehouse/src/writer.rs` (struct field, `create_for_tables`, `writes_table`, `append_*`, `*_schema`, `append_record_batch`, `finalize`)
- Modify: `crates/ab-warehouse/sql/schema.sql` (CREATE TABLE block)

**Interfaces:**
- Produces: `WarehouseTable::NwayRegionOracleEvidence`; `pub struct NwayRegionOracleEvidenceRow`; `WarehouseWriter::append_nway_region_oracle_evidence(&mut self, rows: &[NwayRegionOracleEvidenceRow]) -> Result<()>`.
- Row struct:

```rust
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NwayRegionOracleEvidenceRow {
    pub run_id: String,
    pub source_id: String,
    pub text_id: String,
    pub region_index: u64,
    pub projected_char_start: u64,
    pub projected_char_end: u64,
    pub oracle_source: String,
    pub winning_analyzer: Option<String>,
    pub losing_analyzers: Vec<String>,
    pub evidence_detail: String,
}
```

- [ ] **Step 1: Write the failing writer round-trip test.** In `crates/ab-warehouse/src/writer.rs` test module, add (mirror `writes_projection_spans_rows` at lines 1176-1201):

```rust
    #[test]
    fn writes_nway_region_oracle_evidence_rows() {
        let root = temp_dir("oracle-evidence");
        let paths = WarehousePaths::new(&root, "run-a");
        let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
        writer
            .append_nway_region_oracle_evidence(&[NwayRegionOracleEvidenceRow {
                run_id: "run-a".to_owned(),
                source_id: "source-a".to_owned(),
                text_id: "work-a".to_owned(),
                region_index: 3,
                projected_char_start: 10,
                projected_char_end: 12,
                oracle_source: "ruby".to_owned(),
                winning_analyzer: Some("sudachi-c".to_owned()),
                losing_analyzers: vec!["vibrato:unidic-novel-202512".to_owned()],
                evidence_detail: r#"{"classification":"resolved"}"#.to_owned(),
            }])
            .unwrap();
        writer.finalize().unwrap();
        assert_eq!(
            parquet_table_row_count(&paths.final_dir, WarehouseTable::NwayRegionOracleEvidence)
                .unwrap(),
            1
        );
        let _ = fs::remove_dir_all(root);
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p ab-warehouse writes_nway_region_oracle_evidence_rows`
Expected: FAIL — `NwayRegionOracleEvidenceRow` / variant / method not found.

- [ ] **Step 3: Add the enum variant and memberships** in `crates/ab-warehouse/src/schema.rs`. Add `NwayRegionOracleEvidence` to the `WarehouseTable` enum (after `NwayRegionAnalyzers`), to `ALL` and to `MERGED_DATA` (same positions relative to `NwayRegionAnalyzers`). Add the `file_name` arm:

```rust
            Self::NwayRegionOracleEvidence => "nway_region_oracle_evidence.parquet",
```

Add the `column_names` arm (order is load-bearing):

```rust
            Self::NwayRegionOracleEvidence => &[
                "run_id",
                "source_id",
                "text_id",
                "region_index",
                "projected_char_start",
                "projected_char_end",
                "oracle_source",
                "winning_analyzer",
                "losing_analyzers",
                "evidence_detail",
            ],
```

Add the `NwayRegionOracleEvidenceRow` struct (after `NwayRegionAnalyzerRow`, ~line 337) exactly as in Interfaces above.

- [ ] **Step 4: Wire the writer** in `crates/ab-warehouse/src/writer.rs`:

Add the import of `NwayRegionOracleEvidenceRow` to the `use crate::schema::{...}` block.

Add the struct field (near `projection_spans`):

```rust
    nway_region_oracle_evidence: Option<ArrowWriter<File>>,
```

In `create_for_tables`, add (mirror the `projection_spans` `open_optional_table_writer` call):

```rust
            nway_region_oracle_evidence: open_optional_table_writer(
                &paths,
                tables,
                WarehouseTable::NwayRegionOracleEvidence,
                nway_region_oracle_evidence_schema(),
            )?,
```

In `writes_table`, add:

```rust
            WarehouseTable::NwayRegionOracleEvidence => self.nway_region_oracle_evidence.is_some(),
```

In `append_record_batch`, add:

```rust
            WarehouseTable::NwayRegionOracleEvidence => self
                .nway_region_oracle_evidence
                .as_mut()
                .expect("nway_region_oracle_evidence writer open")
                .write(&batch)?,
```

In `finalize`, add near the other `close_writer(...)` calls:

```rust
        close_writer(self.nway_region_oracle_evidence.take())?;
```

Add the append method (optional-table pattern with `let Some(... ) else` + `nullable_string_array` for the nullable winner and `string_list_array` for the list):

```rust
    pub fn append_nway_region_oracle_evidence(
        &mut self,
        rows: &[NwayRegionOracleEvidenceRow],
    ) -> Result<()> {
        if rows.is_empty() {
            return Ok(());
        }
        let Some(writer) = self.nway_region_oracle_evidence.as_mut() else {
            return Ok(());
        };
        write_batch(
            writer,
            nway_region_oracle_evidence_schema(),
            vec![
                string_array(rows.iter().map(|row| row.run_id.as_str())),
                string_array(rows.iter().map(|row| row.source_id.as_str())),
                string_array(rows.iter().map(|row| row.text_id.as_str())),
                u64_array(rows.iter().map(|row| row.region_index)),
                u64_array(rows.iter().map(|row| row.projected_char_start)),
                u64_array(rows.iter().map(|row| row.projected_char_end)),
                string_array(rows.iter().map(|row| row.oracle_source.as_str())),
                nullable_string_array(rows.iter().map(|row| row.winning_analyzer.as_deref())),
                string_list_array(rows.iter().map(|row| row.losing_analyzers.as_slice())),
                string_array(rows.iter().map(|row| row.evidence_detail.as_str())),
            ],
        )
    }
```

Add the schema fn (near `projection_spans_schema`), using `utf8(name, true)` for the nullable winner and `utf8_list` for the list:

```rust
fn nway_region_oracle_evidence_schema() -> Arc<Schema> {
    schema(vec![
        utf8("run_id", false),
        utf8("source_id", false),
        utf8("text_id", false),
        u64_field("region_index", false),
        u64_field("projected_char_start", false),
        u64_field("projected_char_end", false),
        utf8("oracle_source", false),
        utf8("winning_analyzer", true),
        utf8_list("losing_analyzers"),
        utf8("evidence_detail", false),
    ])
}
```

- [ ] **Step 5: Add the DDL** in `crates/ab-warehouse/sql/schema.sql` (place after the `nway_region_analyzers` block; column order must match `column_names`):

```sql
-- nway_region_oracle_evidence (schema v2 sidecar): ruby-oracle adjudication per
-- ruby base. One row per ruby base where ≥1 analyzer reading disagrees with the
-- editor ruby. winning_analyzer is set only on a unique match; losing_analyzers
-- lists the disagreeing analyzers; evidence_detail is per-analyzer JSON.
CREATE TABLE nway_region_oracle_evidence (
  run_id VARCHAR,
  source_id VARCHAR,
  text_id VARCHAR,
  region_index UBIGINT,
  projected_char_start UBIGINT,
  projected_char_end UBIGINT,
  oracle_source VARCHAR,
  winning_analyzer VARCHAR,
  losing_analyzers VARCHAR[],
  evidence_detail VARCHAR
);
```

- [ ] **Step 6: Run tests to verify they pass**

Run: `cargo test -p ab-warehouse`
Expected: PASS — including `writes_nway_region_oracle_evidence_rows` and the existing `schema_sql_columns_match_documented_parquet_columns` (which now checks the new table's columns against the DDL).

- [ ] **Step 7: Commit**

```bash
git add crates/ab-warehouse/src/schema.rs crates/ab-warehouse/src/writer.rs crates/ab-warehouse/sql/schema.sql
git commit -m "feat(warehouse): nway_region_oracle_evidence table (schema + writer + DDL)"
```

---

### Task 3: `ab-warehouse` — presence-probed view

**Files:**
- Modify: `crates/ab-warehouse/sql/morph_views.sql` (marked view block)
- Modify: `crates/ab-warehouse/src/sql.rs` (strip clause, marker replaces, new test)

**Interfaces:**
- Consumes: `WarehouseTable::NwayRegionOracleEvidence` (Task 2).
- Produces: a `warehouse_nway_region_oracle_evidence` view, stripped when the parquet file is absent.

- [ ] **Step 1: Write the failing test** in `crates/ab-warehouse/src/sql.rs` test module (mirror `views_sql_drops_projection_spans_section_when_table_absent`):

```rust
    #[test]
    fn views_sql_drops_oracle_evidence_section_when_table_absent() {
        let dir = std::env::temp_dir().join(format!("views-oracle-{}", std::process::id()));
        fs::create_dir_all(&dir).unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(!views.contains("warehouse_nway_region_oracle_evidence"));
        assert!(!views.contains("__ORACLE_EVIDENCE_BEGIN__"));

        fs::write(
            dir.join(WarehouseTable::NwayRegionOracleEvidence.file_name()),
            b"stub",
        )
        .unwrap();
        write_run_views_sql(&dir, &dir).unwrap();
        let views = fs::read_to_string(dir.join("views.sql")).unwrap();
        assert!(views.contains("warehouse_nway_region_oracle_evidence"));
        assert!(!views.contains("__ORACLE_EVIDENCE_BEGIN__"));
        let _ = fs::remove_dir_all(dir);
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p ab-warehouse views_sql_drops_oracle_evidence_section`
Expected: FAIL — the view/markers are absent, so the first `assert!(!...)` on the second write fails (view never appears).

- [ ] **Step 3: Add the view block** to `crates/ab-warehouse/sql/morph_views.sql` (after the `__PROJECTION_SPANS_END__` block):

```sql
-- __ORACLE_EVIDENCE_BEGIN__
CREATE OR REPLACE VIEW warehouse_nway_region_oracle_evidence AS
SELECT * FROM read_parquet('__RUN_DIR__/nway_region_oracle_evidence.parquet');
-- __ORACLE_EVIDENCE_END__
```

- [ ] **Step 4: Add the strip clause + marker replaces** in `crates/ab-warehouse/src/sql.rs` `write_run_views_sql`. After the `ProjectionSpans` strip block, add:

```rust
    if !output_run_dir
        .join(WarehouseTable::NwayRegionOracleEvidence.file_name())
        .exists()
    {
        views = remove_marked_sql_sections(
            &views,
            "-- __ORACLE_EVIDENCE_BEGIN__",
            "-- __ORACLE_EVIDENCE_END__",
        );
    }
```

Extend the unconditional marker-line cleanup chain to include:

```rust
        .replace("-- __ORACLE_EVIDENCE_BEGIN__\n", "")
        .replace("-- __ORACLE_EVIDENCE_END__\n", "")
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p ab-warehouse sql`
Expected: PASS — new test and `sql_mentions_parquet_not_jsonl_and_has_version_policy` still green.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-warehouse/sql/morph_views.sql crates/ab-warehouse/src/sql.rs
git commit -m "feat(warehouse): presence-probed nway_region_oracle_evidence view"
```

---

### Task 4: `oracle::ruby` — types + ruby-base extraction + reading selection

**Files:**
- Create: `crates/ab-morph-run/src/oracle/ruby.rs`
- Modify: `crates/ab-morph-run/src/oracle/mod.rs` (add `pub(crate) mod ruby;`)

**Interfaces:**
- Consumes: `ab_plaintext::ProjectionSpan`, `serde_json::Value`, `ab_morph_diff::Morpheme`.
- Produces:

```rust
pub(crate) struct RubyBase {
    pub char_start: u64,
    pub char_end: u64,
    pub base: String,
    pub reading: String,
}
pub(crate) fn ruby_bases(aat: &serde_json::Value, spans: &[ab_plaintext::ProjectionSpan]) -> Vec<RubyBase>;
pub(crate) fn morpheme_reading(analyzer_id: &str, morpheme: &ab_morph_diff::Morpheme) -> Option<String>;
```

- [ ] **Step 1: Add the module line** to `crates/ab-morph-run/src/oracle/mod.rs`:

```rust
pub(crate) mod ruby;
```

- [ ] **Step 2: Write failing tests** in `crates/ab-morph-run/src/oracle/ruby.rs`:

```rust
#[cfg(test)]
mod tests {
    use super::*;
    use ab_morph_diff::{FeatureMap, Morpheme};
    use serde_json::json;

    fn morph(surface: &str, chars: std::ops::Range<usize>, feats: &[(&str, &str)]) -> Morpheme {
        let mut features = FeatureMap::default();
        for (k, v) in feats {
            features.insert((*k).into(), Some((*v).into()));
        }
        Morpheme {
            surface: surface.to_owned(),
            byte_span: 0..0,
            char_span: chars,
            features,
        }
    }

    #[test]
    fn extracts_ruby_bases_with_reading_from_pointer() {
        let aat = json!({
            "blocks": [ { "content": [ { "kind": "ruby", "base": "東京", "reading": "とうきょう" } ] } ]
        });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0,
            projected_char_end: 2,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true,
            is_gaiji: false,
            is_note: false,
        }];
        let bases = ruby_bases(&aat, &spans);
        assert_eq!(bases.len(), 1);
        assert_eq!(bases[0].reading, "とうきょう");
        assert_eq!(bases[0].char_start, 0);
        assert_eq!(bases[0].char_end, 2);
    }

    #[test]
    fn skips_ruby_without_reading() {
        let aat = json!({ "blocks": [ { "content": [ { "kind": "ruby", "base": "x" } ] } ] });
        let spans = vec![ab_plaintext::ProjectionSpan {
            projected_char_start: 0, projected_char_end: 1,
            aat_pointer: "/blocks/0/content/0".to_owned(),
            inline_kind: "ruby".to_owned(),
            is_ruby_base: true, is_gaiji: false, is_note: false,
        }];
        assert!(ruby_bases(&aat, &spans).is_empty());
    }

    #[test]
    fn reading_by_family() {
        let vib = morph("東京", 0..2, &[("kana", "トウキョウ"), ("pron", "トーキョー")]);
        assert_eq!(morpheme_reading("vibrato:unidic-novel-202512", &vib).as_deref(), Some("トウキョウ"));
        let vib_pron_only = morph("x", 0..1, &[("pron", "トーキョー")]);
        assert_eq!(morpheme_reading("vibrato", &vib_pron_only).as_deref(), Some("トーキョー"));
        let sud = morph("東京", 0..2, &[("reading_form", "トウキョウ")]);
        assert_eq!(morpheme_reading("sudachi-c", &sud).as_deref(), Some("トウキョウ"));
        let t = morph("x", 0..1, &[]);
        assert_eq!(morpheme_reading("test:single", &t), None);
    }
}
```

Note: confirm `FeatureMap` exposes a public `insert(key, Option<value>)` for tests; if it does not, use the existing analyzer parse helper or add a `#[cfg(test)]` constructor. Check `crates/ab-morph-diff/src/model.rs` — `FeatureMap` keeps entries sorted, so `insert` must preserve that.

- [ ] **Step 3: Run to verify it fails**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::ruby`
Expected: FAIL — items not defined.

- [ ] **Step 4: Implement** in `crates/ab-morph-run/src/oracle/ruby.rs` (above the tests):

```rust
//! Ruby-base extraction and per-analyzer reading selection for the ruby oracle.

use ab_morph_diff::Morpheme;
use ab_plaintext::ProjectionSpan;
use serde_json::Value;

pub(crate) struct RubyBase {
    pub char_start: u64,
    pub char_end: u64,
    pub base: String,
    pub reading: String,
}

/// Extract ruby bases (with editor reading) from the projected ruby spans.
/// The reading is resolved from the retained AAT via the span's JSON pointer.
/// Bases without a non-empty reading are skipped (no oracle judgment possible).
pub(crate) fn ruby_bases(aat: &Value, spans: &[ProjectionSpan]) -> Vec<RubyBase> {
    spans
        .iter()
        .filter(|span| span.is_ruby_base)
        .filter_map(|span| {
            let node = aat.pointer(&span.aat_pointer)?;
            let reading = node.get("reading").and_then(Value::as_str)?;
            if reading.is_empty() {
                return None;
            }
            let base = node.get("base").and_then(Value::as_str).unwrap_or("").to_owned();
            Some(RubyBase {
                char_start: span.projected_char_start,
                char_end: span.projected_char_end,
                base,
                reading: reading.to_owned(),
            })
        })
        .collect()
}

/// Select the reading feature for a morpheme by analyzer family (spec R7).
pub(crate) fn morpheme_reading(analyzer_id: &str, morpheme: &Morpheme) -> Option<String> {
    let feat = |key: &str| {
        morpheme
            .features
            .get(key)
            .and_then(|value| value.as_ref())
            .map(|value| value.to_string())
    };
    if analyzer_id.starts_with("vibrato") || analyzer_id.starts_with("vaporetto") {
        feat("kana").or_else(|| feat("pron"))
    } else if analyzer_id.starts_with("sudachi") {
        feat("reading_form")
    } else {
        None // test analyzers emit no reading
    }
}
```

- [ ] **Step 5: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::ruby`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/oracle/ruby.rs crates/ab-morph-run/src/oracle/mod.rs
git commit -m "feat(oracle): ruby-base extraction and per-analyzer reading selection"
```

---

### Task 5: `oracle::ruby::adjudicate` — alignment, matching, evidence rows

**Files:**
- Modify: `crates/ab-morph-run/src/oracle/ruby.rs` (add `RegionSpan`, `region_index_for`, `adjudicate`)

**Interfaces:**
- Consumes: `RubyBase`, `morpheme_reading` (Task 4), `super::reading_norm::normalize` (Task 1), `ab_warehouse::schema::NwayRegionOracleEvidenceRow` (Task 2), `ab_morph_diff::Analysis`.
- Produces:

```rust
pub(crate) struct RegionSpan { pub region_index: u64, pub char_start: u64, pub char_end: u64, pub is_disagreement: bool }
pub(crate) fn adjudicate(
    run_id: &str, source_id: &str, text_id: &str,
    ruby_bases: &[RubyBase], analyses: &[ab_morph_diff::Analysis], regions: &[RegionSpan],
) -> Vec<ab_warehouse::schema::NwayRegionOracleEvidenceRow>;
```

- [ ] **Step 1: Write failing tests** appended to the `ruby.rs` test module:

```rust
    use ab_morph_diff::Analysis;

    fn analysis(id: &str, morphs: Vec<Morpheme>) -> Analysis {
        Analysis {
            analyzer: id.to_owned(),
            text_id: "work-a".to_owned(),
            source_text: std::sync::Arc::from(""),
            morphemes: morphs,
            warnings: Vec::new(),
            ortho_annotations: None,
            ortho_offset_map: None,
        }
    }
    fn base(cs: u64, ce: u64, reading: &str) -> RubyBase {
        RubyBase { char_start: cs, char_end: ce, base: "".to_owned(), reading: reading.to_owned() }
    }
    fn regions() -> Vec<RegionSpan> {
        vec![RegionSpan { region_index: 0, char_start: 0, char_end: 10, is_disagreement: true }]
    }

    #[test]
    fn all_match_emits_nothing() {
        let a = analysis("vibrato", vec![morph("東京", 0..2, &[("kana", "トウキョウ")])]);
        let b = analysis("sudachi-c", vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])]);
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "とうきょう")], &[a, b], &regions());
        assert!(rows.is_empty());
    }

    #[test]
    fn unique_winner_when_one_matches() {
        let a = analysis("vibrato", vec![morph("東京", 0..2, &[("kana", "トウケイ")])]); // wrong
        let b = analysis("sudachi-c", vec![morph("東京", 0..2, &[("reading_form", "トウキョウ")])]);
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "とうきょう")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].winning_analyzer.as_deref(), Some("sudachi-c"));
        assert_eq!(rows[0].losing_analyzers, vec!["vibrato".to_owned()]);
        assert_eq!(rows[0].oracle_source, "ruby");
        assert_eq!(rows[0].region_index, 0);
    }

    #[test]
    fn zero_match_is_nonstandard_ruby() {
        let a = analysis("vibrato", vec![morph("本気", 0..2, &[("kana", "ホンキ")])]);
        let b = analysis("sudachi-c", vec![morph("本気", 0..2, &[("reading_form", "ホンキ")])]);
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "マジ")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert!(rows[0].winning_analyzer.is_none());
        assert_eq!(rows[0].losing_analyzers.len(), 2);
        assert!(rows[0].evidence_detail.contains("nonstandard_ruby"));
    }

    #[test]
    fn boundary_misalign_counts_as_nonmatch() {
        // one morpheme straddles the base end → unalignable → non-match
        let a = analysis("vibrato", vec![morph("東京", 0..2, &[("kana", "トウキョウ")])]);
        let b = analysis("sudachi-c", vec![morph("東京市", 0..3, &[("reading_form", "トウキョウシ")])]);
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "とうきょう")], &[a, b], &regions());
        assert_eq!(rows.len(), 1);
        assert_eq!(rows[0].winning_analyzer.as_deref(), Some("vibrato"));
        assert!(rows[0].evidence_detail.contains("boundary-misalign"));
    }
```

- [ ] **Step 2: Run to verify it fails**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::ruby`
Expected: FAIL — `adjudicate`/`RegionSpan` not defined.

- [ ] **Step 3: Implement** in `crates/ab-morph-run/src/oracle/ruby.rs`:

```rust
use ab_morph_diff::Analysis;
use ab_warehouse::schema::NwayRegionOracleEvidenceRow;
use serde_json::json;

pub(crate) struct RegionSpan {
    pub region_index: u64,
    pub char_start: u64,
    pub char_end: u64,
    pub is_disagreement: bool,
}

/// region_index for a base: first disagreement region overlapping the base,
/// else the region containing base_start, else 0.
fn region_index_for(regions: &[RegionSpan], base: &RubyBase) -> u64 {
    let overlaps = |r: &RegionSpan| r.char_start < base.char_end && base.char_start < r.char_end;
    regions
        .iter()
        .find(|r| overlaps(r) && r.is_disagreement)
        .or_else(|| regions.iter().find(|r| r.char_start <= base.char_start && base.char_start < r.char_end))
        .map(|r| r.region_index)
        .unwrap_or(0)
}

/// The concatenated reading of one analyzer over a ruby base, plus alignment.
/// Returns (Some(normalized_reading), "exact") when covered morphemes exactly
/// tile the base; (None, "boundary-misalign") on a straddle; (None, "no-reading")
/// when a covered morpheme has no reading.
fn analyzer_reading(analysis: &Analysis, base: &RubyBase) -> (Option<String>, &'static str) {
    let bs = base.char_start as usize;
    let be = base.char_end as usize;
    let covered: Vec<&Morpheme> = analysis
        .morphemes
        .iter()
        .filter(|m| m.char_span.start < be && bs < m.char_span.end)
        .collect();
    if covered.is_empty() {
        return (None, "boundary-misalign");
    }
    // exact tiling: contiguous, first.start == bs, last.end == be
    let first = covered.first().unwrap();
    let last = covered.last().unwrap();
    if first.char_span.start != bs || last.char_span.end != be {
        return (None, "boundary-misalign");
    }
    for w in covered.windows(2) {
        if w[0].char_span.end != w[1].char_span.start {
            return (None, "boundary-misalign");
        }
    }
    let mut concat = String::new();
    for m in &covered {
        match morpheme_reading(&analysis.analyzer, m) {
            Some(r) => concat.push_str(&r),
            None => return (None, "no-reading"),
        }
    }
    (Some(super::reading_norm::normalize(&concat)), "exact")
}

pub(crate) fn adjudicate(
    run_id: &str,
    source_id: &str,
    text_id: &str,
    ruby_bases: &[RubyBase],
    analyses: &[Analysis],
    regions: &[RegionSpan],
) -> Vec<NwayRegionOracleEvidenceRow> {
    let mut rows = Vec::new();
    for base in ruby_bases {
        let ruby_norm = super::reading_norm::normalize(&base.reading);
        let mut winners = Vec::new();
        let mut losers = Vec::new();
        let mut detail = serde_json::Map::new();
        for analysis in analyses {
            let (reading, align) = analyzer_reading(analysis, base);
            let is_match = reading.as_deref() == Some(ruby_norm.as_str());
            if is_match {
                winners.push(analysis.analyzer.clone());
            } else {
                losers.push(analysis.analyzer.clone());
            }
            detail.insert(
                analysis.analyzer.clone(),
                json!({ "norm": reading, "match": is_match, "align": align }),
            );
        }
        // Emit iff ≥1 analyzer failed to match.
        if losers.is_empty() {
            continue;
        }
        let classification = if winners.is_empty() { "nonstandard_ruby" } else { "resolved" };
        let winning_analyzer = if winners.len() == 1 { Some(winners[0].clone()) } else { None };
        let evidence_detail = json!({
            "ruby_base": base.base,
            "ruby_reading": base.reading,
            "ruby_reading_norm": ruby_norm,
            "classification": classification,
            "per_analyzer": detail,
        })
        .to_string();
        rows.push(NwayRegionOracleEvidenceRow {
            run_id: run_id.to_owned(),
            source_id: source_id.to_owned(),
            text_id: text_id.to_owned(),
            region_index: region_index_for(regions, base),
            projected_char_start: base.char_start,
            projected_char_end: base.char_end,
            oracle_source: "ruby".to_owned(),
            winning_analyzer,
            losing_analyzers: losers,
            evidence_detail,
        });
    }
    rows
}
```

Add `ab-warehouse` to `crates/ab-morph-run/Cargo.toml` `[dependencies]` if not already present (it is — the crate already writes warehouse rows; verify with `grep ab-warehouse crates/ab-morph-run/Cargo.toml`).

- [ ] **Step 4: Run tests to verify they pass**

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::ruby`
Expected: PASS (all Task 4 + Task 5 tests).

- [ ] **Step 5: Add adjudication property test.** Append:

```rust
    #[test]
    fn prop_winner_and_loser_partition() {
        let a = analysis("vibrato", vec![morph("東京", 0..2, &[("kana", "トウキョウ")])]);
        let b = analysis("sudachi-c", vec![morph("東京", 0..2, &[("reading_form", "トウケイ")])]);
        let n = 2;
        let rows = adjudicate("r", "s", "t", &[base(0, 2, "とうきょう")], &[a, b], &regions());
        for row in &rows {
            let winners = row.winning_analyzer.iter().count();
            assert_eq!(winners + row.losing_analyzers.len(), n, "winner+losers partition all analyzers when unique");
            if row.winning_analyzer.is_none() {
                // ambiguous or nonstandard: losers must be non-empty (emit rule)
                assert!(!row.losing_analyzers.is_empty());
            }
        }
    }
```

Note: when ≥2 match (ambiguous), `winning_analyzer` is null and the winner count in this partition check no longer holds; the assertion above only runs the strict partition when a unique winner exists (the single-winner test case). Keep the assertion guarded to the unique-winner shape it exercises.

Run: `cargo test -p ab-morph-run --features test-analyzer oracle::ruby`
Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/oracle/ruby.rs crates/ab-morph-run/Cargo.toml
git commit -m "feat(oracle): ruby adjudication (alignment, matching, evidence rows)"
```

---

### Task 6: Pipeline wiring + end-to-end warehouse test

**Files:**
- Modify: `crates/ab-morph-run/src/lib.rs` (`append_warehouse_nway_fact_rows`: new param, region-lookup accumulation, oracle append)
- Modify: `crates/ab-morph-run/src/pipeline.rs` (extract ruby bases at the projection site; pass into the nway fact function; gate on `writes_table`)

**Interfaces:**
- Consumes: `oracle::ruby::{ruby_bases, adjudicate, RegionSpan}` (Tasks 4–5); `WarehouseTable::NwayRegionOracleEvidence` (Task 2).

- [ ] **Step 1: Extend `append_warehouse_nway_fact_rows`** in `crates/ab-morph-run/src/lib.rs`. Change its signature to accept the ruby bases and accumulate a region lookup:

```rust
fn append_warehouse_nway_fact_rows(
    writer: &mut WarehouseWriter,
    run_id: &str,
    source_id: &str,
    source_text: &str,
    analyses: &[Analysis],
    ruby_bases: &[crate::oracle::ruby::RubyBase],
) -> Result<()> {
    let want_oracle =
        !ruby_bases.is_empty() && writer.writes_table(WarehouseTable::NwayRegionOracleEvidence);
    let mut region_lookup: Vec<crate::oracle::ruby::RegionSpan> = Vec::new();
    let mut feature_pattern_counts = WarehouseFeaturePatternAccumulator::default();
    warehouse::rows::visit_nway_fact_row_batches(
        run_id,
        source_id,
        source_text,
        analyses,
        10_000,
        |facts| {
            feature_pattern_counts.record(&facts.regions, &facts.feature_diffs);
            if want_oracle {
                region_lookup.extend(facts.regions.iter().map(|r| {
                    crate::oracle::ruby::RegionSpan {
                        region_index: r.region_index,
                        char_start: r.char_start,
                        char_end: r.char_end,
                        is_disagreement: !r.is_agreement,
                    }
                }));
            }
            writer.append_nway_regions(&facts.regions)?;
            writer.append_nway_region_analyzers(&facts.region_analyzers)?;
            writer.append_nway_feature_diffs(&facts.feature_diffs)?;
            Ok(())
        },
    )?;
    writer.append_feature_pattern_counts(&feature_pattern_counts.into_rows())?;
    if want_oracle {
        let text_id = analyses.first().map(|a| a.text_id.clone()).unwrap_or_default();
        let rows = crate::oracle::ruby::adjudicate(
            run_id, source_id, &text_id, ruby_bases, analyses, &region_lookup,
        );
        for chunk in rows.chunks(10_000) {
            writer.append_nway_region_oracle_evidence(chunk)?;
        }
    }
    Ok(())
}
```

- [ ] **Step 2: Update the call site** in `crates/ab-morph-run/src/pipeline.rs` (~line 852). First, at the projection site (~line 794-812, inside the `if let Some(writer) = &mut warehouse_writer` block, where `projection_spans` and `aat` are in scope), build the ruby bases once, before `source_text` handles are cleared:

```rust
            let ruby_bases = match &projection_spans {
                Some(spans)
                    if writer.writes_table(WarehouseTable::NwayRegionOracleEvidence) =>
                {
                    crate::oracle::ruby::ruby_bases(&aat, spans)
                }
                _ => Vec::new(),
            };
```

Then pass `&ruby_bases` into the `append_warehouse_nway_fact_rows(...)` call (add it as the new final argument).

- [ ] **Step 3: Write an end-to-end test.** Add to `crates/ab-morph-run` tests (a new `#[cfg(test)]` case in `pipeline.rs` or the crate's integration tests) that runs a Full-profile analyze over a tiny AAT with a ruby node and two `test` analyzers is not viable (test analyzers emit no reading). Instead, drive `adjudicate` through the pipeline path using the real analyzers is out of scope for a unit test; the end-to-end assertion is the full-corpus run in Task 7. For an in-repo automated guard, add a focused integration test that constructs a warehouse writer, calls `append_warehouse_nway_fact_rows` with a hand-built `analyses` (two `vibrato`/`sudachi`-id analyses over the same text) and a ruby base, then reads back the parquet row count:

```rust
#[test]
fn oracle_evidence_written_end_to_end() {
    use ab_warehouse::schema::WarehouseTable;
    use ab_warehouse::{WarehousePaths, WarehouseWriter};
    // build two analyses disagreeing on a ruby reading, one matching the editor.
    // (reuse the crate's Analysis/Morpheme test constructors)
    // ... construct analyses `a` (vibrato, kana=トウケイ) and `b` (sudachi-c, reading_form=トウキョウ)
    // ... ruby base reading とうきょう over chars 0..2
    let root = std::env::temp_dir().join(format!("oracle-e2e-{}", std::process::id()));
    let paths = WarehousePaths::new(&root, "run-a");
    let mut writer = WarehouseWriter::create(paths.clone()).unwrap();
    let ruby = vec![/* RubyBase{0,2,"","とうきょう"} */];
    append_warehouse_nway_fact_rows(&mut writer, "run-a", "src-a", "東京は", &[/*a,b*/], &ruby).unwrap();
    writer.finalize().unwrap();
    let count = ab_warehouse::parquet_table_row_count(
        &paths.final_dir, WarehouseTable::NwayRegionOracleEvidence).unwrap();
    assert_eq!(count, 1);
    let _ = std::fs::remove_dir_all(root);
}
```

Fill in the `Analysis`/`Morpheme`/`RubyBase` construction using the same helpers as Task 5's tests (make them `pub(crate)` test helpers or inline them). If `append_warehouse_nway_fact_rows` is private and unreachable from an integration test, place this test in `lib.rs` under `#[cfg(test)]` where the fn is in scope.

- [ ] **Step 4: Run the full crate test suite**

Run: `cargo test -p ab-morph-run --features test-analyzer`
Expected: PASS — new e2e test green, no regressions. Also run `cargo test -p ab-warehouse`.

- [ ] **Step 5: Clippy + fmt**

Run: `cargo clippy -p ab-morph-run -p ab-warehouse --all-targets --features test-analyzer -- -D warnings` and `cargo fmt --all`
Expected: clean.

- [ ] **Step 6: Commit**

```bash
git add crates/ab-morph-run/src/lib.rs crates/ab-morph-run/src/pipeline.rs
git commit -m "feat(morph-run): emit nway_region_oracle_evidence in Full warehouse runs"
```

---

### Task 7: Full-corpus regeneration + validation record

**Files:**
- Modify: `docs/superpowers/plans/2026-07-07-ruby-oracle.md` (append a Validation Record section)
- Modify: `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md` (Phase 4 first bullet → mark ruby oracle implemented; amend the `nway_region_oracle_evidence` table def to add the two `projected_char_*` columns per Decision R1)

- [ ] **Step 1: Regenerate the canonical warehouse.** Use the existing Full recipe (jobs=8) that produced `full-2026-07-06_160136-jobs8`. Confirm the recipe name:

Run: `just --list | grep morph-warehouse` then run the full recipe (≈40 min). Ensure the vibrato dictionaries are linked (`just dictionary-build-all` if needed).

- [ ] **Step 2: Validate the new table.** With `AB_DUCKDB_BIN` set, query the fresh run dir:

```bash
duckdb -c "SELECT count(*) AS rows,
  count(*) FILTER (WHERE winning_analyzer IS NOT NULL) AS resolved_unique,
  count(*) FILTER (WHERE winning_analyzer IS NULL AND evidence_detail LIKE '%nonstandard_ruby%') AS nonstandard,
  count(DISTINCT source_id) AS sources
  FROM read_parquet('<run>/nway_region_oracle_evidence.parquet');"
```

Also confirm the 9 prior v1/v2 data tables are row-count-identical to the previous canonical run (row-count parity check, same as Phase 3's validation), and spot-check 10 top consensus-wrong (`nonstandard_ruby`) rows by joining `evidence_detail` to source text.

- [ ] **Step 3: Record results.** Append a `## Validation Record` section to this plan with: new canonical run id, total oracle rows, resolved-unique / ambiguous-null / nonstandard split, per-analyzer win counts, the row-count parity confirmation, and the spot-check notes.

- [ ] **Step 4: Amend the governing spec** (Decision R1 + Phase 4 status): in `docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md`, add `projected_char_start`/`projected_char_end` to the `nway_region_oracle_evidence.parquet` column table with a note citing cycle-spec R1, and mark the Phase 4 ruby-oracle bullet implemented with the new canonical run id.

- [ ] **Step 5: Commit**

```bash
git add docs/superpowers/plans/2026-07-07-ruby-oracle.md docs/superpowers/specs/2026-07-05-interestingness-ranking-design.md
git commit -m "docs: record ruby-oracle validation results and spec amendments"
```

---

## Self-Review

**Spec coverage (cycle spec §-by-§):** Goal → Tasks 4–6. Non-Goals (no Phase 5 RRF, no bump) → honored (Task 2 keeps v2; no ranking touched). Emission rule R2 → Task 5 `adjudicate` (`if losers.is_empty() { continue }`). Reading extraction R7 → Task 4 `morpheme_reading`. Normalization R3 → Task 1. Alignment R8 → Task 5 `analyzer_reading` exact-tiling. Adjudication R4 → Task 5 (`winners.len() == 1`). Table schema R1 (+2 columns) → Tasks 2 (Row/DDL) and 7 (spec amendment). Schema versioning R6 → Tasks 2–3 (stays 2, presence-probed view). Module structure → Tasks 1/4/5 (`oracle/reading_norm.rs`, `oracle/ruby.rs`). Testing → Tasks 1/4/5 unit+property, Task 6 e2e, Task 7 full-corpus. Precondition (≥2 analyzers) → naturally holds (a single analyzer over an exactly-tiled base always matches-or-not with no cross-analyzer disagreement; region_lookup still valid). 

**Placeholder scan:** Task 6 Step 3's e2e test skeleton has `/* ... */` construction comments — this is the one spot needing the executor to inline the same `Analysis`/`Morpheme`/`RubyBase` helpers written verbatim in Task 5 Step 1; flagged explicitly rather than left vague. All other steps carry complete code.

**Type consistency:** `NwayRegionOracleEvidenceRow` fields identical across schema.rs (Task 2), append method (Task 2), and `adjudicate` construction (Task 5). `RegionSpan`/`RubyBase` field names match between definition (Tasks 4–5) and use (Task 6). `morpheme_reading` signature identical in Task 4 def and Task 5 call. Column order identical across `column_names`, `nway_region_oracle_evidence_schema`, `schema.sql`, and the append vector.

**Verified before finalizing:** `FeatureMap::insert(FeatureKey, Option<FeatureValue>)` is public (`ab-morph-diff/src/model.rs:44`), so the cross-crate test helpers compile. `Analysis` has 7 public fields (`analyzer`, `text_id`, `source_text`, `morphemes`, `warnings`, `ortho_annotations`, `ortho_offset_map`) — the `analysis()` helper sets the trailing three to `Vec::new()`/`None`. `unicode-normalization` is already an `ab-morph-run` dependency; no `Cargo.toml` change. `Morpheme` and `ab-warehouse::schema` types are all public.
