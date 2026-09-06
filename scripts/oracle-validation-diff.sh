#!/usr/bin/env bash
set -euo pipefail

# Compare non-oracle table row counts and oracle rows keyed by source, text,
# region and projected character offsets. Older classifications come from
# evidence_detail; tables are directories of Parquet parts.
PRIOR="${1:?usage: oracle-validation-diff.sh PRIOR_RUN_DIR NEW_RUN_DIR}"
NEW="${2:?usage: oracle-validation-diff.sh PRIOR_RUN_DIR NEW_RUN_DIR}"
rp() { printf "read_parquet('%s/%s.parquet/*.parquet')" "$1" "$2"; }
ORACLE=nway_region_oracle_evidence

echo "PRIOR=$PRIOR"
echo "NEW=$NEW"
echo
echo "== Non-oracle table row-count parity =="
for t in sources projection_spans analyses morphemes morpheme_features \
         nway_regions nway_region_analyzers nway_feature_diffs \
         feature_pattern_counts errors; do
  duckdb -noheader -list -c "
    WITH a AS (SELECT COUNT(*) c FROM $(rp "$PRIOR" "$t")),
         b AS (SELECT COUNT(*) c FROM $(rp "$NEW" "$t"))
    SELECT '$t' || ':  prior=' || (SELECT c FROM a)
        || '  new=' || (SELECT c FROM b)
        || CASE WHEN (SELECT c FROM a)=(SELECT c FROM b) THEN '  OK'
                ELSE '  *** MISMATCH ***' END;"
done
echo
echo "== Oracle keyed 4-bucket diff =="
duckdb -c "
WITH p AS (SELECT source_id,text_id,region_index,projected_char_start,projected_char_end,
                  json_extract_string(evidence_detail,'\$.classification') AS cls
           FROM $(rp "$PRIOR" "$ORACLE")),
     n AS (SELECT source_id,text_id,region_index,projected_char_start,projected_char_end,
                  classification AS cls
           FROM $(rp "$NEW" "$ORACLE")),
     j AS (SELECT p.classification_present AS in_p, n.classification_present AS in_n,
                  p.cls AS pcls, n.cls AS ncls
           FROM (SELECT *, TRUE AS classification_present FROM p) p
           FULL OUTER JOIN (SELECT *, TRUE AS classification_present FROM n) n
             ON  p.source_id=n.source_id AND p.text_id=n.text_id
             AND p.region_index=n.region_index
             AND p.projected_char_start=n.projected_char_start
             AND p.projected_char_end=n.projected_char_end)
SELECT
  SUM(CASE WHEN in_p IS NOT NULL AND in_n IS NULL THEN 1 ELSE 0 END)                              AS dropped,
  SUM(CASE WHEN in_n IS NOT NULL AND in_p IS NULL THEN 1 ELSE 0 END)                              AS newly_emitted,
  SUM(CASE WHEN in_p IS NOT NULL AND in_n IS NOT NULL AND pcls IS DISTINCT FROM ncls THEN 1 ELSE 0 END)     AS classification_changed,
  SUM(CASE WHEN in_p IS NOT NULL AND in_n IS NOT NULL AND pcls IS NOT DISTINCT FROM ncls THEN 1 ELSE 0 END) AS unchanged,
  (SELECT COUNT(*) FROM p) AS prior_total,
  (SELECT COUNT(*) FROM n) AS new_total
FROM j;"
echo
echo "== classification transitions on shared keys (prior JSON -> new column) =="
duckdb -c "
WITH p AS (SELECT source_id,text_id,region_index,projected_char_start,projected_char_end,
                  json_extract_string(evidence_detail,'\$.classification') AS pcls
           FROM $(rp "$PRIOR" "$ORACLE")),
     n AS (SELECT source_id,text_id,region_index,projected_char_start,projected_char_end,
                  classification AS ncls
           FROM $(rp "$NEW" "$ORACLE"))
SELECT pcls AS prior_cls, ncls AS new_cls, COUNT(*) AS n
FROM p JOIN n USING(source_id,text_id,region_index,projected_char_start,projected_char_end)
GROUP BY 1,2 ORDER BY n DESC;"
echo
echo "== New classification breakdown =="
duckdb -c "
SELECT classification, COUNT(*) AS n,
       ROUND(100.0*COUNT(*)/SUM(COUNT(*)) OVER (),1) AS pct
FROM $(rp "$NEW" "$ORACLE") GROUP BY 1 ORDER BY n DESC;"
