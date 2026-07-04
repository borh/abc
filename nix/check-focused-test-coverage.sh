#!/usr/bin/env bash
# Guard: every test/**/*-test.clj file's namespace must be registered in
# nix/clj-nix-deps.edn's :abc/focused-test alias — in BOTH the require form AND
# the run-tests call. Without this guard, a contributor adding a test file and
# forgetting to edit the vendored allowlist ships a test that silently never
# runs in CI: the :abc/focused-test alias uses clojure.test/run-tests with an
# EXPLICIT namespace list (not kaocha auto-discovery), so an unregistered
# namespace is simply never loaded or run.
#
# This check parses the (ns ...) declaration of each test file and asserts the
# declared symbol appears in both lists in nix/clj-nix-deps.edn. Regression
# signal: add a new test file, forget to register it → this check fails.
set -euo pipefail

python3 - <<'PY'
import re, pathlib, sys

deps_path = pathlib.Path("nix/clj-nix-deps.edn")
deps = deps_path.read_text()

# Legacy allowlist: namespaces explicitly excluded from :abc/focused-test
# (transitive deps not on the pinned Nix classpath / external resources).
# Mirrors docs/adr/.acceptance-legacy-allowlist's ratchet pattern: existing
# exclusions are pinned here with reasons; any NEW unregistered test still fails.
allowlist_path = pathlib.Path("nix/.focused-test-legacy-allowlist")
allowed = set()
if allowlist_path.exists():
    for raw in allowlist_path.read_text().splitlines():
        line = raw.split("#", 1)[0].strip()
        if line:
            allowed.add(line.split()[0])

# The :abc/focused-test alias's main-opts embeds a single string of Clojure:
#   (require '... 'ns ... ) (do ...) (let [...] (test/run-tests 'ns ... 'ns)])
# Extract the require-list and the run-tests-list as substrings, then pull
# every quoted symbol from each.
req_match = re.search(r"\(require (.*?)\) \(do", deps)
rt_match  = re.search(r"\(test/run-tests (.*?)\)\]", deps)
if not req_match or not rt_match:
    print("FATAL: could not locate (require …) or (test/run-tests …) forms in "
          + str(deps_path), file=sys.stderr)
    sys.exit(2)

# Clojure symbol body chars (after the leading '): letters, digits, . - * _ + ! ?
sym = r"[A-Za-z][A-Za-z0-9.\-*_+!?]*"
req_symbols = set(re.findall(r"'" + "(" + sym + ")", req_match.group(1)))
rt_symbols  = set(re.findall(r"'" + "(" + sym + ")", rt_match.group(1)))

test_files = sorted(pathlib.Path("test").rglob("*_test.clj"))
if not test_files:
    print("FATAL: no test/**/*-test.clj files found", file=sys.stderr)
    sys.exit(2)

missing = []
for f in test_files:
    text = f.read_text()
    m = re.search(r"^\(ns\s+([^\s)]+)", text, re.MULTILINE)
    if not m:
        missing.append((str(f), "<no (ns …) declaration>", False, False))
        continue
    ns = m.group(1)
    in_req = ns in req_symbols
    in_rt  = ns in rt_symbols
    if in_req and in_rt:
        continue
    if ns in allowed:
        continue
    missing.append((str(f), ns, in_req, in_rt))

if missing:
    print("focused-test-coverage: some test namespaces are NOT registered in "
          "nix/clj-nix-deps.edn's :abc/focused-test alias AND are not in the "
          "legacy allowlist (nix/.focused-test-legacy-allowlist):", file=sys.stderr)
    print("(the alias uses clojure.test/run-tests with an EXPLICIT namespace "
          "list, not kaocha auto-discovery;", file=sys.stderr)
    print("an unregistered namespace is never loaded or run in CI.)", file=sys.stderr)
    print("", file=sys.stderr)
    for f, ns, in_req, in_rt in missing:
        where = []
        if not in_req: where.append("require")
        if not in_rt:  where.append("run-tests")
        print(f"  {f}: ns '{ns}' missing from {' + '.join(where)}", file=sys.stderr)
    print("", file=sys.stderr)
    print("Add the namespace to BOTH the require form AND the run-tests call "
          "in nix/clj-nix-deps.edn.", file=sys.stderr)
    sys.exit(1)

print(f"focused-test-coverage: all {len(test_files)} test namespaces either registered "
      f"in :abc/focused-test (require + run-tests) or listed in the legacy "
      f"allowlist ({len(allowed)} excluded).")
PY
