#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

mkdir -p "$tmp/vectors/ruby_explicit" "$tmp/vectors/unsupported_shape"

cat > "$tmp/vectors/ruby_explicit/vector.json" <<'JSON'
{
  "name": "ruby_explicit",
  "meta": {
    "feature": "ruby",
    "level": "must",
    "spec_section": "6.1",
    "note": "[provenance:smoke] hand-authored fixture"
  },
  "source": "｜青梅《おうめ》",
  "expected": {
    "serialize": "｜青梅《おうめ》",
    "nodes": [{"kind": "ruby", "span": {"start": 0, "end": 24}}],
    "pairs": [],
    "diagnostics": []
  }
}
JSON

cat > "$tmp/vectors/unsupported_shape/vector.json" <<'JSON'
{
  "name": "unsupported_shape",
  "meta": {
    "feature": "unknown_future_feature",
    "level": "should",
    "spec_section": "9.9",
    "note": "[provenance:smoke] deliberately unsupported by fake adapter"
  },
  "source": "［＃未来機能］",
  "expected": {
    "serialize": "［＃未来機能］",
    "nodes": [{"kind": "futureNode", "span": {"start": 0, "end": 18}}],
    "pairs": [],
    "diagnostics": []
  }
}
JSON

cat > "$tmp/fake-aozora" <<'SH'
#!/usr/bin/env bash
set -euo pipefail
kind="$2"
input="$(cat)"
case "$kind:$input" in
  nodes:｜青梅*)
    printf '{"schemaVersion":1,"data":[{"kind":"ruby","span":{"start":0,"end":24}}]}\n'
    ;;
  nodes:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  pairs:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  diagnostics:*)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
  *)
    printf '{"schemaVersion":1,"data":[]}\n'
    ;;
esac
SH
chmod +x "$tmp/fake-aozora"

python3 "$repo_root/reports/parser-conformance/run-aozora-notation-spec.py" \
  --vectors-dir "$tmp/vectors" \
  --adapter "fake=$tmp/fake-aozora inspect" \
  --summary-json "$tmp/summary.json" \
  --report-md "$tmp/report.md"

jq -e '.totals.vectors == 2' "$tmp/summary.json"
jq -e '.totals.adapters == 1' "$tmp/summary.json"
jq -e '.totals.rows == 2' "$tmp/summary.json"
jq -e '.rows[] | select(.vector == "ruby_explicit" and .adapter == "fake") | .status == "pass"' "$tmp/summary.json"
jq -e '.rows[] | select(.vector == "unsupported_shape" and .adapter == "fake") | .status == "warning"' "$tmp/summary.json"
rg -n 'ruby_explicit' "$tmp/report.md"
rg -n 'unsupported_shape' "$tmp/report.md"

echo "aozora notation-spec comparator smoke ok"
