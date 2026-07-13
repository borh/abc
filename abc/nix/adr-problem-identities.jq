def stable_problem_identity:
  if .kind == "input-hash-mismatch" then
    {
      kind,
      "claim-id": .["claim-id"],
      "affected-claim-ids": .["affected-claim-ids"],
      "artifact-path": .["artifact-path"],
      "input-path": .["input-path"]
    }
  elif .kind == "missing-claim-header" then
    {kind, file, "criterion-index": .["criterion-index"]}
  elif .kind == "missing-release-authority"
    or .kind == "missing-validation-scope" then
    {kind, file}
  else
    del(
      .message,
      .actual,
      .expected,
      .detail,
      .errors,
      .["errors-humanized"],
      .line,
      .column
    )
  end;

[.problems[] | stable_problem_identity] | sort_by(tojson)
