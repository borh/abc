#!/usr/bin/env bash
# The real end-to-end integration proof for the sole-publication-producer
# migration: runs `build-publication` through the SAME flake-wrapped Soranoha
# program `apps.<system>.soranoha` exports (real ab-aozora + real
# ab-aat-to-parser-ir + the governed v2 mapping — no stubs, no env overrides
# here), over a committed, git-backed Aozora fixture.
#
# Usage: publication-build-real-wiring-smoke.sh <wrapped-soranoha-program> <work-dir>
#
# Deliberately sets NO adapter/mapping environment variables (the flake
# wrapper is solely responsible for those) and never invokes Clojure directly
# — the wrapped program is the only thing this script executes.
set -euo pipefail

if [ "$#" -ne 2 ]; then
  echo "usage: publication-build-real-wiring-smoke.sh <wrapped-soranoha-program> <work-dir>" >&2
  exit 2
fi

soranoha_program="$1"
work_dir="$2"

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"

rm -rf "$work_dir"
mkdir -p "$work_dir"

aozora_root="$work_dir/aozora"
output_root="$work_dir/output"
mkdir -p "$aozora_root/index_pages" "$aozora_root/cards/000879/files"

# ── Step 1+2: the committed 000127 catalog fixture (real Aozora metadata for
# 羅生門 by 芥川竜之介, work 000127 / person 000879) becomes the official
# catalog ZIP. Its テキストファイルURL basename is repointed at the synthetic
# card ZIP this script writes below, so real selection matching (which keys
# solely on that basename, per soranoha-build-publication's catalog-index)
# finds it.
csv_source="$repo_root/abc/examples/v0/example-work/aozora-csv/list_person_all_extended_utf8_127.csv"
csv_staging="$work_dir/csv-staging"
mkdir -p "$csv_staging"
csv_staged="$csv_staging/list_person_all_extended_utf8.csv"
cp "$csv_source" "$csv_staged"
sed -i 's/127_ruby_150\.zip/000001_ruby_fixture.zip/' "$csv_staged"
grep -q '000001_ruby_fixture.zip' "$csv_staged"

(cd "$csv_staging" && zip -q -X "$aozora_root/index_pages/list_person_all_extended_utf8.zip" \
  "$(basename "$csv_staged")")

# ── Step 3: one real, minimal, parseable Aozora-format card ZIP. The single
# ruby line is the same real fixture content used elsewhere in this monorepo
# to exercise the genuine ab-aozora binary (not a stub).
card_staging="$work_dir/card-staging"
mkdir -p "$card_staging"
printf '吾輩《わがはい》は猫である。\n' > "$card_staging/000001.txt"
(cd "$card_staging" && zip -q -X \
  "$aozora_root/cards/000879/files/000001_ruby_fixture.zip" "000001.txt")

# ── Step 4: git-back the fixture. official-git source trust requires a
# provable `git rev-parse HEAD` plus a clean `git status --porcelain -- cards
# index_pages`.
git -C "$aozora_root" init -q
git -C "$aozora_root" config user.name "publication-build-real-wiring-smoke"
git -C "$aozora_root" config user.email "smoke@soranoha.invalid"
git -C "$aozora_root" add -A
git -C "$aozora_root" commit -q -m "smoke: 000127 catalog + synthetic ruby fixture card"

# ── Step 5: best-effort config 0.2.0 — official-git source trust, the
# project-owned ab-aozora parser profile, and the decision-bound release parser
# identity record (the standalone parser is authenticated against
# data/release-parser-identity-v1.edn, which release-parser-identity-approval
# binds — no longer the parser-rq campaign candidate).
config_json="$work_dir/config.json"
cat > "$config_json" <<JSON
{
  "config_schema_id": "https://w3id.org/abc/schemas/soranoha-publication-build-config.schema.json",
  "config_schema_version": "0.2.0",
  "source_trust_mode": "official-git",
  "parser_profile": "ab-aozora",
  "release_parser_identity": "data/release-parser-identity-v1.edn",
  "publication_profile": "tei-profile-v0",
  "continue_on_failure": true,
  "materialization_scope": "smoke"
}
JSON

# ── Step 6: run build-publication through the wrapped Soranoha program only.
build_log="$work_dir/build-publication.log"
set +e
"$soranoha_program" build-publication \
  --aozora-root "$aozora_root" \
  --config "$config_json" \
  --snapshot-date "2024-01-01" \
  --output-root "$output_root" \
  > "$build_log" 2>&1
build_exit=$?
set -e

cat "$build_log"
echo "build_exit_code: $build_exit"

# ── Step 7: the exit code is only meaningful because it AGREES with the
# recomputed verifier result the build itself recorded.
report="$output_root/publications/publications-report.json"
if [ "$build_exit" -ne 0 ] && [ "$build_exit" -ne 1 ]; then
  echo "FAIL: build-publication exited $build_exit (expected 0 or 1)" >&2
  exit 1
fi
if [ ! -f "$report" ]; then
  echo "FAIL: publications-report.json missing at $report (build exited $build_exit before recording a verifier result)" >&2
  exit 1
fi

release_admissible="$(jq -r '.release_admissible' "$report")"
problem_count="$(jq '.release_problems | length' "$report")"

case "$release_admissible" in
  true)
    if [ "$build_exit" -ne 0 ]; then
      echo "FAIL: verifier says release_admissible=true but build exited $build_exit (expected 0)" >&2
      exit 1
    fi
    if [ "$problem_count" -ne 0 ]; then
      echo "FAIL: release_admissible=true but release_problems is non-empty" >&2
      jq '.release_problems' "$report" >&2
      exit 1
    fi
    ;;
  false)
    if [ "$build_exit" -ne 1 ]; then
      echo "FAIL: verifier says release_admissible=false but build exited $build_exit (expected 1)" >&2
      exit 1
    fi
    if [ "$problem_count" -eq 0 ]; then
      echo "FAIL: release_admissible=false but release_problems is empty" >&2
      exit 1
    fi
    ;;
  *)
    echo "FAIL: publications-report.json has non-boolean release_admissible: $release_admissible" >&2
    exit 1
    ;;
esac

# ── Step 9 (checked before step 8's positive assertions so a real defect is
# never masked by "the files happen to exist"): independently prove the ONLY
# problem this real build may surface is the rights gate. Any source, parser,
# mapping, schema, manifest, or closure problem is a genuine integration
# defect, not an expected outcome — this must NOT be papered over. Nor does
# this check hard-code "must be exit 1": if governance later flips the rights
# policy to the allowed state, release_problems is empty, release_admissible
# is true, and build_exit is 0 — the assertions above already cover that case.
non_rights_problem_count="$(jq '[.release_problems[] | select(.code != "release-rights-blocked")] | length' "$report")"
if [ "$non_rights_problem_count" -ne 0 ]; then
  echo "FAIL: real build surfaced non-rights release problems (a genuine integration defect):" >&2
  jq '[.release_problems[] | select(.code != "release-rights-blocked")]' "$report" >&2
  exit 1
fi
if [ "$problem_count" -gt 1 ]; then
  echo "FAIL: more than one release problem recorded (expected at most the sole rights problem):" >&2
  jq '.release_problems' "$report" >&2
  exit 1
fi

# ── Step 8: the real artifacts a closed candidate root must contain: parser-IR
# and TEI content, all four per-work manifest kinds, and a live snapshot-index
# 0.2.0.
work_slug="000127_000879_000879_000001_ruby_fixture"
pub_dir="$output_root/publications/$work_slug"

for required in \
  "$pub_dir/source.manifest.json" \
  "$pub_dir/parser-ir.manifest.json" \
  "$pub_dir/plaintext.manifest.json" \
  "$pub_dir/tei.manifest.json" \
  "$pub_dir/parser-ir.json" \
  "$pub_dir/plain.txt" \
  "$pub_dir/tei.xml"; do
  if [ ! -f "$required" ]; then
    echo "FAIL: expected release artifact missing: $required" >&2
    exit 1
  fi
done

snapshot_index="$output_root/snapshot-index.json"
if [ ! -f "$snapshot_index" ]; then
  echo "FAIL: snapshot-index.json missing at $snapshot_index" >&2
  exit 1
fi
snapshot_schema_version="$(jq -r '.schema_version' "$snapshot_index")"
if [ "$snapshot_schema_version" != "0.2.0" ]; then
  echo "FAIL: snapshot-index.json schema_version is $snapshot_schema_version, expected 0.2.0" >&2
  exit 1
fi

reference_kinds="$(jq -r '[.artifact_references[].artifact_kind] | sort | unique | join(",")' "$snapshot_index")"
if [ "$reference_kinds" != "parser-ir,plaintext,source,tei" ]; then
  echo "FAIL: snapshot-index artifact_references kinds were [$reference_kinds], expected exactly source,parser-ir,plaintext,tei" >&2
  exit 1
fi

failure_count="$(jq '.failures | length' "$snapshot_index")"
if [ "$failure_count" -ne 0 ]; then
  echo "FAIL: snapshot-index has $failure_count recorded failure(s); the real build should render this single fixture work cleanly" >&2
  exit 1
fi

echo "publication-build-real-wiring smoke ok (release_admissible=$release_admissible, sole-permitted-problem=release-rights-blocked, build_exit=$build_exit)"
