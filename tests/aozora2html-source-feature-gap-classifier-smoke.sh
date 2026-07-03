#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
out="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/aozora2html-source-feature-gap-classifier-smoke"
run="$out/run"
corpus="$out/corpus"
residual="$run/residual-worksets"
generated="$out/generated"
worksets="$out/worksets"

rm -rf "$out"
mkdir -p \
  "$corpus/cards/000001/files" \
  "$corpus/cards/000002/files" \
  "$corpus/cards/000003/files" \
  "$residual" \
  "$generated"

python3 - "$corpus" <<'PY'
import io
import sys
import zipfile
from pathlib import Path

corpus = Path(sys.argv[1])

def write_zip(path: Path, member: str, lines: list[str]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    payload = ("\n".join(lines) + "\n").encode("utf-8")
    with zipfile.ZipFile(path, "w") as zf:
        zf.writestr(member, payload)

write_zip(
    corpus / "cards/000001/files/warigaki_koko.zip",
    "warigaki_koko.txt",
    [
        "一行目",
        "［＃ここから割り注］割り注本文［＃改行］続き［＃ここで割り注終わり］",
        "三行目",
    ],
)
write_zip(
    corpus / "cards/000001/files/warigaki_compact.zip",
    "warigaki_compact.txt",
    [
        "前文",
        "［＃割り注］簡約割り注［＃割り注終わり］",
    ],
)
write_zip(
    corpus / "cards/000002/files/kunten_compact.zip",
    "kunten_compact.txt",
    [
        "漢文 ［＃レ］ ［＃一］",
    ],
)
(corpus / "cards/000002/files/okurigana_note.txt").write_text(
    "題は底本では別題である ［＃（ノ）］\n",
    encoding="utf-8",
)
(corpus / "cards/000003/files/unknown.txt").write_text(
    "本文に未知の検出ヒットがある\n",
    encoding="utf-8",
)
PY

cat > "$run/index.json" <<JSON
{
  "version": 1,
  "corpus_root": "$corpus",
  "corpus_hash": "sha256:smoke",
  "generated_at": "2026-07-03T00:00:00Z",
  "works_count": 5,
  "works": [
    {
      "id": "war-koko",
      "txt_path": "cards/000001/files/warigaki_koko.zip::warigaki_koko.txt",
      "html_path": "cards/000001/files/warigaki_koko.html",
      "features": ["warigaki"],
      "feature_lines": {"warigaki": [2]}
    },
    {
      "id": "war-compact",
      "txt_path": "cards/000001/files/warigaki_compact.zip::warigaki_compact.txt",
      "html_path": "cards/000001/files/warigaki_compact.html",
      "features": ["warigaki"],
      "feature_lines": {"warigaki": [2]}
    },
    {
      "id": "kun-compact",
      "txt_path": "cards/000002/files/kunten_compact.zip::kunten_compact.txt",
      "html_path": "cards/000002/files/kunten_compact.html",
      "features": ["kaeriten"],
      "feature_lines": {"kaeriten": [1]}
    },
    {
      "id": "oku-note",
      "txt_path": "cards/000002/files/okurigana_note.txt",
      "html_path": "cards/000002/files/okurigana_note.html",
      "features": ["okurigana"],
      "feature_lines": {"okurigana": [1]}
    },
    {
      "id": "unknown-hit",
      "txt_path": "cards/000003/files/unknown.txt",
      "html_path": "cards/000003/files/unknown.html",
      "features": ["warigaki"],
      "feature_lines": {"warigaki": [1]}
    }
  ],
  "by_feature": {
    "warigaki": ["war-koko", "war-compact", "unknown-hit"],
    "kaeriten": ["kun-compact"],
    "okurigana": ["oku-note"]
  }
}
JSON

cat > "$residual/warigaki-source_feature_without_aat_observation.json" <<'JSON'
[
  "unknown-hit",
  "war-compact",
  "war-koko"
]
JSON

cat > "$residual/kunten-source_feature_without_aat_observation.json" <<'JSON'
[
  "kun-compact",
  "oku-note"
]
JSON

cat > "$generated/residual.summary.json" <<JSON
{
  "run_dir": "$run",
  "worksets": {
    "warigaki.source_feature_without_aat_observation": "$residual/warigaki-source_feature_without_aat_observation.json",
    "kunten.source_feature_without_aat_observation": "$residual/kunten-source_feature_without_aat_observation.json"
  },
  "bucket_counts": {
    "warigaki": {
      "source_feature_without_aat_observation": 3
    },
    "kunten": {
      "source_feature_without_aat_observation": 2
    }
  },
  "verdict_inputs": {
    "has_source_feature_without_aat_observation": true
  }
}
JSON

python3 "$repo_root/reports/aat-fidelity/classify-aozora2html-source-feature-gaps.py" \
  --run-dir "$run" \
  --residual-summary "$generated/residual.summary.json" \
  --out-md "$generated/source-feature-gap-classification.md" \
  --summary-json "$generated/source-feature-gap-classification.summary.json" \
  --worksets-dir "$worksets"

jq -e '.marker_class_counts["warigaki.koko_start_end"] == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.marker_class_counts["warigaki.with_source_line_break"] == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.marker_class_counts["warigaki.compact_start_end"] == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.marker_class_counts["kunten.kaeriten.compact"] == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.marker_class_counts["kunten.okurigana.parenthesized"] == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.marker_class_counts.unknown == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.context_hint_counts.body_candidate == 4' "$generated/source-feature-gap-classification.summary.json"
jq -e '.context_hint_counts.base_text_note == 1' "$generated/source-feature-gap-classification.summary.json"
jq -e '.works["oku-note"].context_hints == ["base_text_note"]' "$generated/source-feature-gap-classification.summary.json"
jq -e '.worksets["warigaki.adapter_obligation_candidates"] == "'"$worksets"'/warigaki-adapter-obligation-candidates.json"' "$generated/source-feature-gap-classification.summary.json"
jq -e '.worksets["kunten.adapter_obligation_candidates"] == "'"$worksets"'/kunten-adapter-obligation-candidates.json"' "$generated/source-feature-gap-classification.summary.json"
jq -e '.worksets["adapter_obligation_union"] == "'"$worksets"'/source-feature-gap-adapter-obligation-union.json"' "$generated/source-feature-gap-classification.summary.json"
jq -e '.worksets["unknown_union"] == "'"$worksets"'/source-feature-gap-unknown-union.json"' "$generated/source-feature-gap-classification.summary.json"
jq -e '.worksets["source_index_only_candidates"] == "'"$worksets"'/source-feature-gap-source-index-only-candidates.json"' "$generated/source-feature-gap-classification.summary.json"
jq -e '. == ["war-compact", "war-koko"]' "$worksets/warigaki-adapter-obligation-candidates.json"
jq -e '. == ["kun-compact"]' "$worksets/kunten-adapter-obligation-candidates.json"
jq -e '. == ["kun-compact", "war-compact", "war-koko"]' "$worksets/source-feature-gap-adapter-obligation-union.json"
jq -e '. == ["oku-note"]' "$worksets/source-feature-gap-source-index-only-candidates.json"
jq -e '. == ["unknown-hit"]' "$worksets/source-feature-gap-unknown-union.json"
