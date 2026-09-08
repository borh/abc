#!/usr/bin/env bash
set -euo pipefail

# Resolution must be deterministic, reject a wrong content hash, and record
# the correct hash. The fixture needs neither corpus storage nor network access.

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
resolve="$repo_root/ab-validator/reports/aat-fidelity/resolve-run-set.py"
lib="$repo_root/ab-validator/reports/lib"
tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

# The fixtures omit `expected.source` and `run_descriptor` on purpose: without a
# `source.flake_input` the validator never consults flake.lock, so the fixture is
# self-contained (no repo state, no /db); see validate_run_set / aat_runs.py.
unset AB_AOZORA_AAT_DIR

mkdir -p "$tmp/aat/aozora-adapter"
printf '{"blocks":[]}\n' > "$tmp/aat/aozora-adapter/000001_1-test.json"

# The dump's content identity, computed via the same hasher resolve uses.
good_hash="$(PYTHONPATH="$lib" python3 "$lib/aat_hash.py" "$tmp/aat/aozora-adapter")"
case "$good_hash" in
  sha256:*) ;;
  *) echo "fidelity-lock idempotency smoke: unexpected hash format: $good_hash" >&2; exit 1 ;;
esac

write_manifest() {
  # $1: out path   $2: pinned content_hash
  cat > "$1" <<JSON
{
  "schema_version": 1,
  "run_set_id": "fidelity-lock-smoke",
  "adapters": {
    "aozora": {
      "aat_dir": "$tmp/aat/aozora-adapter",
      "expected": {
        "adapter_id": "aozora",
        "content_hash": "$2"
      }
    }
  }
}
JSON
}

good_manifest="$tmp/run-set-good.json"
bad_manifest="$tmp/run-set-bad.json"
write_manifest "$good_manifest" "$good_hash"
write_manifest "$bad_manifest" "sha256:0000000000000000000000000000000000000000000000000000000000000000"

# 1. DETERMINISM (golden-lock): resolve the CORRECT manifest twice; the emitted
# locks must be byte-identical (resolve records no timestamp / no RNG).
PYTHONPATH="$lib" python3 "$resolve" "$good_manifest" --repo-root "$repo_root" --out "$tmp/lock-a.json"
PYTHONPATH="$lib" python3 "$resolve" "$good_manifest" --repo-root "$repo_root" --out "$tmp/lock-b.json"
if ! cmp -s "$tmp/lock-a.json" "$tmp/lock-b.json"; then
  echo "fidelity-lock idempotency smoke: locks are NOT byte-identical (non-deterministic resolve)" >&2
  diff "$tmp/lock-a.json" "$tmp/lock-b.json" >&2 || true
  exit 1
fi

# 2. FAIL-CLOSED: a manifest with the WRONG content_hash must make resolve exit
# non-zero and refuse to emit a lock.
set +e
PYTHONPATH="$lib" python3 "$resolve" "$bad_manifest" --repo-root "$repo_root" --out "$tmp/lock-bad.json" \
  > "$tmp/bad.out" 2> "$tmp/bad.err"
bad_status=$?
set -e
if [ "$bad_status" -eq 0 ]; then
  echo "fidelity-lock idempotency smoke: resolve accepted a mismatched content_hash (should fail closed)" >&2
  exit 1
fi
if [ -e "$tmp/lock-bad.json" ]; then
  echo "fidelity-lock idempotency smoke: resolve emitted a lock despite a content_hash mismatch" >&2
  exit 1
fi
if ! grep -q "content hash mismatch" "$tmp/bad.err"; then
  echo "fidelity-lock idempotency smoke: expected a content-hash-mismatch diagnostic, got:" >&2
  cat "$tmp/bad.err" >&2
  exit 1
fi

# 3. PASS: the CORRECT manifest resolves and the lock records that content_hash.
PYTHONPATH="$lib" python3 "$resolve" "$good_manifest" --repo-root "$repo_root" --out "$tmp/lock-good.json"
PYTHONPATH="$lib" python3 - "$tmp/lock-good.json" "$good_hash" <<'PY'
import sys

from fidelity_lock import load_lock, lock_adapters

lock = load_lock(sys.argv[1])
expected_hash = sys.argv[2]
adapters = lock_adapters(lock)
entry = adapters.get("aozora")
assert isinstance(entry, dict), lock
assert entry.get("content_hash") == expected_hash, (entry, expected_hash)
PY

echo "fidelity-lock idempotency smoke ok"
