# Aozora Person Drift History Check

Date: 2026-04-30

## Question

Before designing an ABC person-drift detector, check whether Aozora Bunko's
local git history actually contains person identity split/merge cases in
`index_pages/list_person_all_extended_utf8.zip`.

The local upstream checkout examined was:

```text
/home/bor/Dependencies/aozorabunko
```

## Method

The file has 4,834 commits in local history. A full daily replay would be
expensive, so this first pass used deterministic checkpoints:

- year-end snapshots for every year in the local history;
- month-end snapshots for every month in the local history.

For each snapshot, the ZIP was read from the git object database with:

```bash
git -C /home/bor/Dependencies/aozorabunko show <commit>:index_pages/list_person_all_extended_utf8.zip
```

The contained CSV was parsed and compared as:

- `person_id -> identity tuple`:
  `(姓, 名, 姓読み, 名読み, 姓ローマ字, 名ローマ字, 生年月日, 没年月日)`
- `(作品ID, 役割フラグ) -> #{人物ID}` contributor edges.

The check looked for:

- removed person IDs;
- added person IDs;
- same identity tuple moving between person IDs;
- contributor edge changes, especially one-to-many, many-to-one, and disjoint
  replacements.

## Findings

### Person ID movement

No month-end or year-end checkpoint showed the same identity tuple moving from
one `人物ID` to another.

Removed IDs observed at month-end checkpoints:

```text
000070 000071 000103 000130 000143 000213 000901 000902 000943 001260
```

These removals correspond to works/persons disappearing from the corpus, not
to split/merge evidence in the remaining contributor graph.

### Contributor edge replacements

The only month-end disjoint contributor replacements occurred from
2013-03-31 to 2013-04-30:

```text
047971 著者: 001030 -> 000075
047957 著者: 001030 -> 001769
047896 著者: 001030 -> 000075
047959 著者: 001030 -> 000075
```

Inspection of the affected rows shows these are attribution corrections, not
identity drift:

- `001030` is 堀 辰雄 / Hori Tatsuo.
- `000075` is リルケ ライネル・マリア / Rainer Maria Rilke.
- `001769` is ゲラン シャルル / Charles Guerin.
- After the correction, Hori remains present as translator on the relevant
  Rilke/Guerin cards where appropriate.

This is a work-contributor role correction, not a split of Hori's person
identity or a merge of multiple identities.

### Contributor set expansions

Year-end snapshots surfaced a few one-to-many contributor-set changes, for
example:

```text
2018: work 002223 著者 changed from #{000020} to #{000020 000332}
2022: several works changed from #{001341} to #{001341 002098}
2025: several works added co-contributors around 000107 / 002401 / 000019
```

These are additions of contributors to works. They do not imply a person
identity split because the original person ID remains present on the same
work-role edge.

## Conclusion

This local-history check did not find evidence that Aozora has actually
performed persistent person identity splits or merges in the normalized
`list_person_all_extended_utf8.csv` history.

The observed changes are better classified as:

- ordinary corpus growth;
- work removal;
- person removal tied to removed works;
- contributor attribution correction;
- contributor-set expansion.

## Design Implication

A production `person-drift-detect` feature should not start by trying to
materialize ADR 0020/0021 drift events. The realistic next step is a
history-audit/report tool over generated ABC corpus snapshots:

- classify added/removed persons;
- classify changed contributor edges;
- flag split/merge candidates only when evidence survives conservative
  filtering;
- produce a report for inspection.

If future full-history or daily replay finds persistent split/merge evidence,
the existing drift-event schema and harness are ready to represent it. Until
then, automatic drift event emission would be premature.
