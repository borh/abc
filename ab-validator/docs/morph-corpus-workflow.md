# Morph corpus workflow

Run corpus comparisons over checked AAT JSON using the Nix-backed recipes from
`ab-validator/`. Set runtime storage through the repository configuration and
pass the AAT directory explicitly. Large runs require local corpus storage.

```sh
just morph-warehouse-run triage /absolute/path/to/aats
just morph-warehouse-list-runs
just morph-warehouse-build-report /absolute/path/to/run
```

Warehouse mode writes sealed Parquet fact tables. The `triage` profile keeps
segmentation, N-way, pairwise, morpheme and core POS pattern facts while omitting
raw morpheme-feature and N-way feature-difference tables. Use `full` when the
analysis requires those raw tables.

The default analyzer set includes Vibrato with contemporary and novel UniDic
and Sudachi's short- and long-unit modes. `just morph-warehouse-run-suw` restricts
the comparison to short-unit analyzers. Dictionaries are resolved through Nix;
`jobs=0` lets the runtime choose a worker count from available memory and CPUs.

Runs are reused only when the recorded input identity matches AAT content,
dictionaries, analyzers, profile and schema. The explicit `force` argument to
`morph-warehouse-run-with-analyzers` requests recomputation.

`source_id` identifies an AAT record and distinguishes duplicate records in the
corpus index. `text_id` groups records for a logical work. Use `--group-by
source-id` for a concrete record and `--group-by text-id` for work-level reports.
The `ab-morph-run` CLI exposes targeted `rerun-full` and warehouse summary
commands; its `--help` describes their output and selection options.
