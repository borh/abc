# Aozora rights fields and source assertions

Aozora's work classification is an attributed upstream assertion. It does not
establish individual contributor rights, the complete rights-relevant contribution
set, or the date of an independent legal assessment. Soranoha's
[assessment evaluator](../assessment-evaluation.md) keeps edition-level reliance
separate from independent rights findings.

## Pinned source evidence

The passages below were inspected in the aozorabunko Git checkout at revision
`0e9ea3e586eb0aa34039fabfc85a407d2f98b165`. They describe that revision's
published guidance; current reliance also checks live official responses.

- [`soramoyou/soramoyou2011.html`, 2011-01-13 entry](https://github.com/aozorabunko/aozorabunko/blob/0e9ea3e586eb0aa34039fabfc85a407d2f98b165/soramoyou/soramoyou2011.html)
  introduces the extended CSV: the work flag is `あり` if any contributing
  person's flag is `あり`. The announcement describes a provisional format,
  with comments invited until 2011-03-15.
- [`guide/kijyunn.html`](https://github.com/aozorabunko/aozorabunko/blob/0e9ea3e586eb0aa34039fabfc85a407d2f98b165/guide/kijyunn.html)
  describes free copying, redistribution, adaptation and translation of works
  classified as expired, without permission or payment owed to Aozora. For works
  classified as protected, reuse beyond private use requires permission. Special
  permissions may appear on the card, in the work file, or on a linked author
  page; the CSV does not record their terms. A translation remains protected
  under these rules while the translator's rights remain, even if the original
  author's rights have expired.
- [`guide/aozora_bunko_faq.html`](https://github.com/aozorabunko/aozorabunko/blob/0e9ea3e586eb0aa34039fabfc85a407d2f98b165/guide/aozora_bunko_faq.html)
  and the handling rules identify the catalog CSV's licence as CC BY 4.0. That
  licence concerns catalog reuse; it does not grant a licence to every work
  hosted on the same server.

## Catalog field boundaries

The catalog repeats work values across contributor rows.

| Column | Observed vocabulary |
|---|---|
| `作品著作権フラグ` | Per-work `あり` / `なし` |
| `役割フラグ` | `著者`, `翻訳者`, `編者`, `校訂者`, `その他` |
| `没年月日` | Per-person free-form date text |
| `人物著作権フラグ` | Per-person `あり` / `なし` |

Nationality, jurisdiction, flag-assessment date, assessor and assessment basis
are absent. A flag's observation date therefore cannot stand for the source's
assessment date or a legal status's effective date. Catalog rows also omit some
input and proofreading contributors named in source texts; a catalog role does
not by itself establish that its holder owns copyright.

The pinned catalog contains 19,470 rows, 17,810 works and 1,334 persons. Its
archive is `index_pages/list_person_all_extended_utf8.zip`, SHA-256
`5ea13273dd457f89af31de39f559ea9c6f5435d9bda1ae3d46d6a683b8bc3c92`;
the CSV SHA-256 is
`c0ace54c7ac037e5aebd045922c7879b9f7dc01f85b8f7483c8d4ecc29569ef4`.
These counts describe catalog records, not an admitted publication population.
