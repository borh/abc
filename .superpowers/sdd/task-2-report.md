## Task 2 Report — Add The Probe Smoke First

### What I changed
- Added `tests/parser-ir-plain-prose-source-delta-smoke.sh` with fixture inputs matching the plan.
- Set the smoke script executable (`chmod +x tests/parser-ir-plain-prose-source-delta-smoke.sh`).

### TDD RED evidence
- `bash -n tests/parser-ir-plain-prose-source-delta-smoke.sh`
  - Result: `bash -n ok`
- `./tests/parser-ir-plain-prose-source-delta-smoke.sh`
  - Result: non-zero exit code (expected at this stage).
- Reproducible failure transcript (`bash -x` tail):
```
+ python3 /home/bor/.../reports/parser-ir/plain-prose-source-delta.py ... (strict mode invocation)
+ strict_status=2
...
+ rg -n 'missing parser evidence' "$strict_stderr"
+ rm -rf "$out_dir"
exit:1
```

The strict invocation exits with status 2, as expected for a missing target script, and the later strict-stderr assertion path fails because the expected message is not present before Task 3 implementation.

### Files changed
- `tests/parser-ir-plain-prose-source-delta-smoke.sh`
- `.superpowers/sdd/task-2-report.md`

### Self-review
- Scope is limited to Task 2 artifacts only.
- The script matches existing smoke style and fixture shape conventions in this repository.
- No unrelated code paths or report logic were touched.

### Concerns
- The full assertion block after strict execution cannot be exercised until `reports/parser-ir/plain-prose-source-delta.py` exists (expected for Task 3).
