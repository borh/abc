set shell := ["bash", "-euo", "pipefail", "-c"]

repo_root := `pwd`
repo_storage_root := `git rev-parse --path-format=absolute --git-common-dir | xargs dirname`
ab_db_root := env_var_or_default("AB_DB_ROOT", "/db/ab-validator")
morph_warehouse_dir := env_var_or_default("AB_MORPH_WAREHOUSE_DIR", "/db/ab-validator/morph-warehouse")
morph_warehouse_aat_dir := env_var_or_default("AB_MORPH_WAREHOUSE_AAT_DIR", "/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter")
aozora_rs_full_aat_dir := env_var_or_default("AB_AOZORA_RS_AAT_DIR", repo_storage_root + "/scratch/morph-full-corpus/aats/aozora-rs-adapter")
aozora2_full_aat_dir := env_var_or_default("AB_AOZORA2_AAT_DIR", ab_db_root + "/aat-corpus/aozora2-full-20260704T132955Z/aat/aozora2-adapter")
aozora2html_full_aat_dir := env_var_or_default("AB_AOZORA2HTML_AAT_DIR", ab_db_root + "/aat-corpus/aozora2html-full-20260703T020301Z/aat/aozora2html-adapter")
aozora_epub3_full_aat_dir := env_var_or_default("AB_AOZORA_EPUB3_AAT_DIR", ab_db_root + "/aat-corpus/aozora-epub3-full-20260704T050652Z-300s/aat/aozora-epub3-adapter")
aozora_full_aat_dir := env_var_or_default("AB_AOZORA_AAT_DIR", ab_db_root + "/aat-corpus/aozora-full-20260705T000000Z/aat/aozora-adapter")
tei_eaj_workset := env_var_or_default("AB_TEI_EAJ_WORKSET", repo_storage_root + "/../abc/docs/handoffs/tei-eaj-aozora-workset-export.json")
aozora2html_flake := repo_root + "#aozora2html"
vibrato_dictionary_root := repo_root + "/dictionary"
vibrato_unidic_sources := vibrato_dictionary_root + "/unidic-sources"
vibrato_compiled_dir := vibrato_dictionary_root + "/compiled"
vibrato_optimized_dir := vibrato_dictionary_root + "/optimized"
vibrato_rkyv_dir := env_var_or_default(
    "VIBRATO_RKYV_DIR",
    repo_root + "/../vibrato-pipe/third-party/vibrato-rkyv",
)

default:
	@just --list

fmt:
	@cargo fmt --all

fmt-check:
	@cargo fmt --all -- --check

workspace-check:
	@cargo check --workspace --all-targets

clippy:
	@cargo clippy --workspace --all-targets --all-features

quality:
	@just fmt-check
	@just workspace-check
	@just clippy

flake-cargo *ARGS:
	@nix develop "{{repo_root}}#default" --command cargo {{ARGS}}

flake-python *ARGS:
	@nix develop "{{repo_root}}#default" --command python3 {{ARGS}}

flake-aozora2html-python *ARGS:
	@nix develop "{{repo_root}}#aozora2html" --command python3 {{ARGS}}

quality-fix:
	@just fmt
	@just workspace-check
	@just clippy

aozora2html-rust-build PROFILE="release":
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora2html/Cargo.toml" --{{PROFILE}}

aozora2html-rust-clean:
	@rm -rf "{{repo_root}}/adapters/aozora2html/target"

aozora2html-rust-test:
	@cargo test --manifest-path "{{repo_root}}/adapters/aozora2html/Cargo.toml"

aozora2html-rust-parity:
	@nix run "{{repo_root}}#aozora2html-rust-parity"

aozora-build PROFILE="release":
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml" --{{PROFILE}}

aozora-test:
	@AB_AOZORA_BIN="${AB_AOZORA_BIN:-aozora}" cargo test --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml"

aozora-smoke: aozora-build
	@bash "{{repo_root}}/tests/aozora-adapter-smoke.sh"

aozora-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.aozora-smoke" --print-build-logs

aozora-notation-spec-comparator-smoke:
	@bash "{{repo_root}}/tests/aozora-notation-spec-comparator-smoke.sh"

aozora-notation-spec-comparator-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.aozora-notation-spec-comparator-smoke" --print-build-logs

aozora-notation-spec-comparison VECTORS="" REPORT_MD="docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-aozora-notation-spec-comparison.summary.json":
	@vectors="{{VECTORS}}"; if [ -z "$vectors" ]; then vectors="$(nix build --no-link --print-out-paths '{{repo_root}}#reference-aozora-notation-spec')/conformance/vectors"; fi; \
	aozora_bin="$(nix build --no-link --print-out-paths '{{repo_root}}#reference-aozora')/bin/aozora"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml" --release; \
	python3 "{{repo_root}}/reports/parser-conformance/run-aozora-notation-spec.py" \
		--vectors-dir "$vectors" \
		--adapter "aozora=inspect:$aozora_bin inspect" \
		--adapter "ab-aozora=aat:{{repo_root}}/adapters/aozora/target/release/aozora-adapter --mode aat" \
		--adapter "aozora2=aat:{{repo_root}}/adapters/aozora2/target/release/aozora2-adapter --mode aat" \
		--adapter "aozora2html=aat:{{repo_root}}/adapters/aozora2html/aozora2html-adapter --mode aat" \
		--adapter "aozora-rs=aat:{{repo_root}}/adapters/aozora-rs/target/release/aozora-rs-adapter --mode aat" \
		--adapter "aozora-epub3=aat:{{repo_root}}/adapters/aozora-epub3/aozora-epub3-adapter --mode aat" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}"

aat-schema-smoke-suite:
	@nix run "{{repo_root}}#aat-oracle-data-schema-smoke"
	@nix run "{{repo_root}}#adapter-fidelity-notes-schema-smoke"

clean-db:
	@echo "cleaning generated artifacts under {{ab_db_root}}"
	@rm -rf \
		"{{ab_db_root}}/aat-fidelity" \
		"{{ab_db_root}}/target" \
		"{{ab_db_root}}/tmp" \
		"{{ab_db_root}}/stage" \
		"{{ab_db_root}}/results"
	@for d in {{ab_db_root}}/target-*; do rm -rf "$d"; done

clean-db-full:
	@echo "also cleaning residual corpora artifacts under {{ab_db_root}}"
	@just clean-db
	@rm -rf "{{ab_db_root}}/residuals" "{{ab_db_root}}/visible-gaiji"

clean-db-dry-run:
	@echo "candidate cleanup paths under {{ab_db_root}}:"
	@for d in \
	  "{{ab_db_root}}/aat-fidelity" \
	  "{{ab_db_root}}/target" \
	  "{{ab_db_root}}/tmp" \
	  "{{ab_db_root}}/stage" \
	  "{{ab_db_root}}/results" \
	  "{{ab_db_root}}/residuals" \
	  "{{ab_db_root}}/visible-gaiji"; do \
	  if [ -e "$d" ]; then echo "$d"; fi; \
	done
	@for d in {{ab_db_root}}/target-*; do \
	  if [ -e "$d" ]; then echo "$d"; fi; \
	done

fidelity-cross-summary-smoke:
	@bash tests/aat-fidelity-cross-summary-xhtml-smoke.sh

aat-batch-triage-smoke:
	@bash tests/aat-batch-triage-smoke.sh

aat-to-parser-ir-smoke:
	@bash tests/aat-to-parser-ir-cli-smoke.sh

aat-to-parser-ir-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.aat-to-parser-ir-smoke" --print-build-logs

parser-performance-smoke:
	@bash tests/parser-performance-measure-smoke.sh

parser-performance-all-parsers:
	@index="${INDEX:-}"; \
	corpus="${CORPUS:-{{repo_root}}/references/aozorabunko}"; \
	out_dir="${OUT_DIR:-{{ab_db_root}}/parser-performance/all-parsers-$(date -u +%Y%m%dT%H%M%SZ)}"; \
	work_ids="${WORK_IDS:-}"; \
	sample="${SAMPLE:-20}"; \
	limit_s="${LIMIT_S:-300}"; \
	stage_split_sample="${STAGE_SPLIT_SAMPLE:-5}"; \
	jobs="${JOBS:-24}"; \
	test -n "$index" || { echo "INDEX=/path/to/index.json is required" >&2; exit 2; }; \
	aozora_pkg="$(nix --option post-build-hook "" build --no-link --print-out-paths '{{repo_root}}#reference-aozora')"; \
	aozora2html_pkg="$(nix --option post-build-hook "" build --no-link --print-out-paths '{{repo_root}}#reference-aozora2html')"; \
	epub3_jar="${AB_AOZORAEPUB3_JAR:-{{repo_root}}/references/parsers/AozoraEpub3-JDK21/build/libs/AozoraEpub3.jar}"; \
	test -f "$epub3_jar" || { echo "AozoraEpub3.jar not found: $epub3_jar" >&2; echo "build it with: (cd {{repo_root}}/references/parsers/AozoraEpub3-JDK21 && ./gradlew jar), or set AB_AOZORAEPUB3_JAR" >&2; exit 2; }; \
	aozora_pkg_q="$(python3 -c 'import shlex, sys; print(shlex.quote(sys.argv[1]))' "$aozora_pkg")"; \
	epub3_jar_q="$(python3 -c 'import shlex, sys; print(shlex.quote(sys.argv[1]))' "$epub3_jar")"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora2/Cargo.toml" --release --jobs "$jobs"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora-rs/Cargo.toml" --release --jobs "$jobs"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora2html/Cargo.toml" --release --jobs "$jobs"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora-epub3/Cargo.toml" --release --jobs "$jobs"; \
	cargo build --manifest-path "{{repo_root}}/adapters/aozora/Cargo.toml" --release --jobs "$jobs"; \
	work_args=(); if [ -n "$work_ids" ]; then work_args=(--work-ids "$work_ids"); fi; \
	python3 "{{repo_root}}/reports/aat-fidelity/measure-parser-performance.py" \
		--index "$index" \
		--corpus "$corpus" \
		--out-dir "$out_dir" \
		"${work_args[@]}" \
		--sample "$sample" \
		--limit-s "$limit_s" \
		--adapter "aozora2={{repo_root}}/adapters/aozora2/target/release/aozora2-adapter --mode aat" \
		--adapter "aozora-rs={{repo_root}}/adapters/aozora-rs/target/release/aozora-rs-adapter --mode aat" \
		--adapter "aozora2html={{repo_root}}/adapters/aozora2html/aozora2html-adapter --mode aat" \
		--adapter "aozora-epub3=env AB_AOZORAEPUB3_JAR=$epub3_jar_q {{repo_root}}/adapters/aozora-epub3/aozora-epub3-adapter --mode aat" \
		--adapter "aozora=env AB_AOZORA_BIN=$aozora_pkg_q/bin/aozora {{repo_root}}/adapters/aozora/target/release/aozora-adapter --mode aat" \
		--aozora2html-label aozora2html \
		--aozora2html-bin "$aozora2html_pkg/bin/aozora2html" \
		--aozora2html-gem-home "$aozora2html_pkg/lib/ruby/gems" \
		--mapper-bin "{{repo_root}}/adapters/aozora2html/target/release/aozora2html-adapter" \
		--stage-split-sample "$stage_split_sample"; \
	echo "parser performance measurement: $out_dir/results.json"

source-inventory-smoke:
	@bash "{{repo_root}}/tests/source-inventory-smoke.sh"

source-representability-gate-smoke:
	@bash "{{repo_root}}/tests/source-representability-gate-smoke.sh"

source-inventory-flake-smoke:
	@system="$(nix eval --impure --raw --expr builtins.currentSystem)"; \
	nix build "{{repo_root}}#checks.$system.source-inventory-smoke" --print-build-logs

source-inventory-full JOBS="24" INDEX="" CORPUS="":
	@test -n "{{INDEX}}" || { echo "INDEX=/path/to/index.json is required" >&2; exit 2; }
	@test -n "{{CORPUS}}" || { echo "CORPUS=/path/to/aozorabunko is required" >&2; exit 2; }
	@cargo build -p ab-coverage --bin ab-source-inventory --release --jobs "{{JOBS}}"
	@mkdir -p "{{ab_db_root}}/source-inventory"
	@"{{repo_root}}/target/release/ab-source-inventory" \
		--matrix "{{repo_root}}/data/aozora-syntax-coverage.toml" \
		--index "{{INDEX}}" \
		--corpus "{{CORPUS}}" \
		--allowlist "{{repo_root}}/data/aozora-source-inventory-allowlist.toml" \
		--jobs "{{JOBS}}" \
		--output-json "{{repo_root}}/docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" \
		--report-md "{{repo_root}}/docs/superpowers/reports/2026-07-04-source-authority-representability.md" \
		--unknown-workset "{{ab_db_root}}/source-inventory/unknown-workset.json" \
		--strict-representability

aat-to-parser-ir-full-audit JOBS="24" REPORT_MD="docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-04-aat-parser-ir-full-corpus-conversion.summary.json" COMPAT_EDN="docs/superpowers/reports/2026-07-04-aat-parser-ir-compatibility-candidates.edn":
	@cargo build -p ab-aat-to-parser-ir --release --jobs "{{JOBS}}"
	@"{{repo_root}}/target/release/ab-aat-to-parser-ir" audit-corpus \
		--aat-dir "{{aozora_rs_full_aat_dir}}" \
		--aat-dir "{{aozora2_full_aat_dir}}" \
		--aat-dir "{{aozora2html_full_aat_dir}}" \
		--aat-dir "{{aozora_epub3_full_aat_dir}}" \
		--aat-dir "{{aozora_full_aat_dir}}" \
		--mapping "{{repo_root}}/data/aat-to-parser-ir-mapping-v1.json" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}" \
		--compat-edn-out "{{repo_root}}/{{COMPAT_EDN}}" \
		--jobs "{{JOBS}}" \
		--abc-root "{{repo_root}}/data/abc-schemas"

parser-ir-level3-publication-smoke:
	@bash "{{repo_root}}/tests/parser-ir-level3-publication-smoke.sh"

parser-ir-level3-melos-publication-smoke AAT="":
	@aat="{{AAT}}"; if [ -z "$aat" ]; then aat="{{aozora2html_full_aat_dir}}/000035_1567-32ff5a089d67.json"; fi; \
	AB_LEVEL3_AAT="$aat" AB_LEVEL3_EXPECT_SOURCE_NOTE=1 bash "{{repo_root}}/tests/parser-ir-level3-publication-smoke.sh"

parser-ir-level3-melos-eaj-compare-smoke AAT="" TEI_EAJ_FILE="data/complete/tei_lib_lv4/1567_tei.xml":
	@aat="{{AAT}}"; if [ -z "$aat" ]; then aat="{{aozora2html_full_aat_dir}}/000035_1567-32ff5a089d67.json"; fi; \
	AB_LEVEL3_AAT="$aat" AB_LEVEL3_TEI_EAJ_FILE="{{TEI_EAJ_FILE}}" bash "{{repo_root}}/tests/parser-ir-level3-tei-eaj-compare-smoke.sh"

parser-ir-level3-generated-workset-audit-smoke:
	@bash "{{repo_root}}/tests/parser-ir-level3-generated-workset-audit-smoke.sh"

parser-ir-level3-tei-eaj-generated-audit MAX_ROWS="0" OUT_DIR="" STRUCTURAL_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json":
	@cargo build -p ab-aat-to-parser-ir --release
	@out_dir="{{OUT_DIR}}"; if [ -z "$out_dir" ]; then out_dir="{{ab_db_root}}/parser-ir/tei-eaj-generated-comparison"; fi; \
	python3 "{{repo_root}}/reports/parser-ir/tei-eaj-generated-compare.py" \
		--workset "{{tei_eaj_workset}}" \
		--structural-summary "{{repo_root}}/{{STRUCTURAL_SUMMARY}}" \
		--mapping "{{repo_root}}/data/aat-to-parser-ir-mapping-v1.json" \
		--converter-bin "{{repo_root}}/target/release/ab-aat-to-parser-ir" \
		--abc-root "{{repo_root}}/../abc" \
		--abc-schema-root "{{repo_root}}/data/abc-schemas" \
		--out-dir "$out_dir" \
		--max-rows "{{MAX_ROWS}}"

parser-ir-level3-tei-eaj-generated-matrix-audit MAX_ROWS="0" OUT_DIR="" STRUCTURAL_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json":
	@cargo build -p ab-aat-to-parser-ir --release
	@out_dir="{{OUT_DIR}}"; if [ -z "$out_dir" ]; then out_dir="{{ab_db_root}}/parser-ir/tei-eaj-generated-matrix-comparison"; fi; \
	python3 "{{repo_root}}/reports/parser-ir/tei-eaj-generated-compare.py" \
		--workset "{{tei_eaj_workset}}" \
		--structural-summary "{{repo_root}}/{{STRUCTURAL_SUMMARY}}" \
		--mapping "{{repo_root}}/data/aat-to-parser-ir-mapping-v1.json" \
		--converter-bin "{{repo_root}}/target/release/ab-aat-to-parser-ir" \
		--abc-root "{{repo_root}}/../abc" \
		--abc-schema-root "{{repo_root}}/data/abc-schemas" \
		--out-dir "$out_dir" \
		--candidate-mode all \
		--max-rows "{{MAX_ROWS}}"

parser-ir-level3-admission-smoke:
	@bash "{{repo_root}}/tests/parser-ir-level3-admission-smoke.sh"

parser-ir-level3-admission-report MATRIX_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json" SOURCE_SUMMARY="docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" REPORT_MD="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json":
	@python3 "{{repo_root}}/reports/parser-ir/level3-admission.py" \
		--matrix-summary "{{repo_root}}/{{MATRIX_SUMMARY}}" \
		--source-summary "{{repo_root}}/{{SOURCE_SUMMARY}}" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}"

parser-ir-plain-prose-source-delta-smoke:
	@bash "{{repo_root}}/tests/parser-ir-plain-prose-source-delta-smoke.sh"

parser-ir-plain-prose-source-delta-report ADMISSION_SUMMARY="docs/superpowers/reports/2026-07-05-profile-aware-level3-tei-admission.summary.json" MATRIX_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-generated-matrix-comparison.summary.json" STRUCTURAL_SUMMARY="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json" SOURCE_SUMMARY="docs/superpowers/reports/2026-07-04-source-authority-representability.summary.json" MAPPING="data/aat-to-parser-ir-mapping-v1.json" REPORT_MD="docs/superpowers/reports/2026-07-05-plain-prose-source-delta.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-05-plain-prose-source-delta.summary.json":
	@python3 "{{repo_root}}/reports/parser-ir/plain-prose-source-delta.py" \
		--admission-summary "{{repo_root}}/{{ADMISSION_SUMMARY}}" \
		--matrix-summary "{{repo_root}}/{{MATRIX_SUMMARY}}" \
		--structural-summary "{{repo_root}}/{{STRUCTURAL_SUMMARY}}" \
		--source-summary "{{repo_root}}/{{SOURCE_SUMMARY}}" \
		--mapping "{{repo_root}}/{{MAPPING}}" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}" \
		--allow-missing-parser-evidence

tei-eaj-structural-expansion JOBS="24" REPORT_MD="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.md" SUMMARY_JSON="docs/superpowers/reports/2026-07-04-tei-eaj-structural-expansion.summary.json":
	@test -f "{{tei_eaj_workset}}" || { echo "missing TEI-EAJ workset export: {{tei_eaj_workset}}" >&2; exit 2; }
	@cargo build -p ab-aat-to-parser-ir --release --jobs "{{JOBS}}"
	@"{{repo_root}}/target/release/ab-aat-to-parser-ir" tei-eaj-structural-expansion \
		--workset "{{tei_eaj_workset}}" \
		--aat-dir "aozora-rs={{aozora_rs_full_aat_dir}}" \
		--aat-dir "aozora2={{aozora2_full_aat_dir}}" \
		--aat-dir "aozora2html={{aozora2html_full_aat_dir}}" \
		--aat-dir "aozora-epub3={{aozora_epub3_full_aat_dir}}" \
		--aat-dir "aozora={{aozora_full_aat_dir}}" \
		--mapping "{{repo_root}}/data/aat-to-parser-ir-mapping-v1.json" \
		--summary-json "{{repo_root}}/{{SUMMARY_JSON}}" \
		--report-md "{{repo_root}}/{{REPORT_MD}}" \
		--abc-root "{{repo_root}}/data/abc-schemas"

upstream-xhtml-full-smoke:
	@bash tests/aat-fidelity-upstream-xhtml-full-run-smoke.sh

fidelity-smoke-suite:
	@just aat-batch-triage-smoke
	@just fidelity-cross-summary-smoke
	@just upstream-xhtml-full-smoke

fidelity-cross-adapter-full:
	@rm -rf "{{ab_db_root}}/aat-fidelity/cross-adapter-full"
	@AB_AAT_FIDELITY_OUT_DIR="{{ab_db_root}}/aat-fidelity/cross-adapter-full" \
	 AB_AAT_FIDELITY_SUMMARY="{{ab_db_root}}/aat-fidelity/cross-adapter-full/summary.md" \
	 AB_AAT_FIDELITY_DB="{{ab_db_root}}/aat-fidelity/cross-adapter-full/fidelity.duckdb" \
	 "{{repo_root}}/reports/aat-fidelity/run-cross-adapter-report.sh"

fidelity-cross-adapter-full-for-case CASE_ID:
	@rm -rf "{{ab_db_root}}/aat-fidelity/cross-adapter-full"
	@AB_AAT_FIDELITY_CASE_ID="{{CASE_ID}}" \
	 AB_AAT_FIDELITY_OUT_DIR="{{ab_db_root}}/aat-fidelity/cross-adapter-full" \
	 AB_AAT_FIDELITY_SUMMARY="{{ab_db_root}}/aat-fidelity/cross-adapter-full/summary.md" \
	 AB_AAT_FIDELITY_DB="{{ab_db_root}}/aat-fidelity/cross-adapter-full/fidelity.duckdb" \
	 "{{repo_root}}/reports/aat-fidelity/run-cross-adapter-report.sh"

upstream-xhtml-full JOBS="0" MAX_CARDS="0" TRIAGE_LIMIT="100":
	@jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	"{{repo_root}}/reports/aat-fidelity/run-upstream-xhtml-full.sh" --aozora-root "{{repo_root}}/references/aozorabunko" --out-dir "{{ab_db_root}}/aat-fidelity/upstream-xhtml-full" --db "{{ab_db_root}}/aat-fidelity/cross-adapter/fidelity.duckdb" --report-id upstream-xhtml-full --jobs "$jobs" --max-cards "{{MAX_CARDS}}" --triage-limit "{{TRIAGE_LIMIT}}"

aozora2html-aat-full DIR="" JOBS="0" TIMEOUT="180s" REPORT_ID="" WORK_IDS="" FEATURES="":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-corpus/aozora2html-full-$(date -u +%Y%m%dT%H%M%SZ)"; fi; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	report_id="{{REPORT_ID}}"; if [ -z "$report_id" ]; then report_id="aozora2html-full-$(date -u +%F)"; fi; \
	args=(--out-dir "$run_dir" --jobs "$jobs" --timeout "{{TIMEOUT}}" --report-id "$report_id" --force); \
	if [ -n "{{WORK_IDS}}" ]; then args+=(--work-ids "{{WORK_IDS}}"); fi; \
	if [ -n "{{FEATURES}}" ]; then args+=(--features "{{FEATURES}}"); fi; \
	"{{repo_root}}/reports/aat-fidelity/run-aozora2html-aat-full.sh" "${args[@]}"

aozora-aat-full DIR="" JOBS="0" TIMEOUT="300s" REPORT_ID="" WORK_IDS="" FEATURES="":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-corpus/aozora-full-$(date -u +%Y%m%dT%H%M%SZ)"; fi; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	report_id="{{REPORT_ID}}"; if [ -z "$report_id" ]; then report_id="aozora-full-$(date -u +%F)"; fi; \
	args=(--out-dir "$run_dir" --jobs "$jobs" --timeout "{{TIMEOUT}}" --report-id "$report_id" --force); \
	if [ -n "{{WORK_IDS}}" ]; then args+=(--work-ids "{{WORK_IDS}}"); fi; \
	if [ -n "{{FEATURES}}" ]; then args+=(--features "{{FEATURES}}"); fi; \
	"{{repo_root}}/reports/aat-fidelity/run-aozora-aat-full.sh" "${args[@]}"

aozora-epub3-aat-full DIR="" JOBS="0" TIMEOUT="300s" REPORT_ID="" WORK_IDS="" FEATURES="":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-corpus/aozora-epub3-full-$(date -u +%Y%m%dT%H%M%SZ)"; fi; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	report_id="{{REPORT_ID}}"; if [ -z "$report_id" ]; then report_id="aozora-epub3-full-$(date -u +%F)"; fi; \
	args=(--out-dir "$run_dir" --jobs "$jobs" --timeout "{{TIMEOUT}}" --report-id "$report_id" --force); \
	if [ -n "{{WORK_IDS}}" ]; then args+=(--work-ids "{{WORK_IDS}}"); fi; \
	if [ -n "{{FEATURES}}" ]; then args+=(--features "{{FEATURES}}"); fi; \
	"{{repo_root}}/reports/aat-fidelity/run-aozora-epub3-aat-full.sh" "${args[@]}"

aozora-epub3-aat-full-smoke:
	@bash "{{repo_root}}/tests/aozora-epub3-aat-full-smoke.sh"

fidelity-full-run DIR="" JOBS="0" MAX_CARDS="0" TRIAGE_LIMIT="100":
	@run_dir="{{DIR}}"; if [ -z "$run_dir" ]; then run_dir="{{ab_db_root}}/aat-fidelity/run-$(date -u +%F_%H%M%S)"; fi; \
	mkdir -p "$run_dir/cross-adapter" "$run_dir/upstream-xhtml-full" "$run_dir/aat-fidelity"; \
	jobs="{{JOBS}}"; if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	AB_AAT_FIDELITY_OUT_DIR="$run_dir/cross-adapter" \
	AB_AAT_FIDELITY_SUMMARY="$run_dir/cross-adapter/summary.md" \
	AB_AAT_FIDELITY_DB="$run_dir/cross-adapter/fidelity.duckdb" \
	"{{repo_root}}/reports/aat-fidelity/run-cross-adapter-report.sh"; \
	"{{repo_root}}/reports/aat-fidelity/run-upstream-xhtml-full.sh" \
		--aozora-root "{{repo_root}}/references/aozorabunko" \
		--out-dir "$run_dir/upstream-xhtml-full" \
		--db "$run_dir/cross-adapter/fidelity.duckdb" \
		--report-id upstream-xhtml-full \
		--jobs "$jobs" \
		--max-cards "{{MAX_CARDS}}" \
		--triage-limit "{{TRIAGE_LIMIT}}"; \
	AB_DB_ROOT="$run_dir" bash tests/aat-batch-triage-smoke.sh; \
	AB_DB_ROOT="$run_dir" bash tests/aat-fidelity-cross-summary-xhtml-smoke.sh; \
	AB_DB_ROOT="$run_dir" bash tests/aat-fidelity-upstream-xhtml-full-run-smoke.sh; \
	echo "run complete: $run_dir"

full-fidelity-suite:
	@just clean-db
	@just fidelity-cross-adapter-full
	@just upstream-xhtml-full
	@just fidelity-smoke-suite

fresh-full-fidelity-suite JOBS="0" MAX_CARDS="0" TRIAGE_LIMIT="100":
	@just clean-db-full
	@just fidelity-cross-adapter-full
	@just upstream-xhtml-full jobs={{JOBS}} max_cards={{MAX_CARDS}} triage_limit={{TRIAGE_LIMIT}}
	@just fidelity-smoke-suite

fresh-fidelity-full-run DIR="" JOBS="0" MAX_CARDS="0" TRIAGE_LIMIT="100":
	@if [ "{{DIR}}" = "" ]; then \
	  just clean-db-full && just fidelity-full-run "" "{{JOBS}}" "{{MAX_CARDS}}" "{{TRIAGE_LIMIT}}"; \
	else \
	  just clean-db-full && just fidelity-full-run "{{DIR}}" "{{JOBS}}" "{{MAX_CARDS}}" "{{TRIAGE_LIMIT}}"; \
	fi

fidelity-run-list:
	@mkdir -p "{{ab_db_root}}/aat-fidelity"
	@find "{{ab_db_root}}/aat-fidelity" -maxdepth 1 -type d -name 'run-*' -print | sort

fidelity-run-clean KEEP_DAYS="30":
	@if [ "{{KEEP_DAYS}}" = "0" ]; then \
		echo "KEEP_DAYS must be > 0"; \
		exit 1; \
	fi
	@mkdir -p "{{ab_db_root}}/aat-fidelity"
	@find "{{ab_db_root}}/aat-fidelity" -maxdepth 1 -type d -name 'run-*' -mtime +{{KEEP_DAYS}} -print -exec rm -rf {} +

morph-warehouse-clean:
	@echo "cleaning canonical warehouse state under {{morph_warehouse_dir}}"
	@rm -rf \
		"{{morph_warehouse_dir}}/.staging" \
		"{{morph_warehouse_dir}}/.duckdb_tmp" \
		"{{morph_warehouse_dir}}/runs" \
		"{{morph_warehouse_dir}}/triage" \
		"{{morph_warehouse_dir}}/reports"
	@mkdir -p "{{morph_warehouse_dir}}/runs"

morph-warehouse-clean-dry-run:
	@echo "candidate cleanup paths under {{morph_warehouse_dir}}:"
	@for d in \
	  "{{morph_warehouse_dir}}/.staging" \
	  "{{morph_warehouse_dir}}/.duckdb_tmp" \
	  "{{morph_warehouse_dir}}/runs" \
	  "{{morph_warehouse_dir}}/triage" \
	  "{{morph_warehouse_dir}}/reports"; do \
	  if [ -e "$d" ]; then echo "$d"; fi; \
	done

# --- scratch/ hygiene -------------------------------------------------------
# scratch/ is gitignored materialized output (perf runs, morph smokes, coverage
# artifacts). Per the project principle that materialized outputs are caches,
# these are all regenerable via the morph-warehouse- / ab-validator- recipes.
# Two retention modes:
#   * scratch-clean KEEP_DAYS    - age-based prune (safe reusable default)
#   * scratch-keep-recent KEEP_N - keep only the N newest top-level entries
# Each has a *-dry-run variant. Run dry-run first; deletes are irreversible.

scratch-list:
	@echo "# scratch/ top-level entries (newest first)"
	@total=0; \
	for d in scratch/*; do \
	  [ -e "$d" ] || continue; \
	  b=$(du -sb "$d" 2>/dev/null | cut -f1); \
	  t=$(date -d "@$(stat -c '%Y' "$d")" +%F_%H:%M); \
	  printf '%s  %14s B  %s\n' "$t" "$b" "$d"; \
	  total=$((total + b)); \
	done; \
	printf '\nTOTAL  %14s B  (scratch/)\n' "$total"

scratch-clean-dry-run KEEP_DAYS='14':
	@cutoff=$(date -d "-{{KEEP_DAYS}} days" +%s); \
	total=0; \
	for d in scratch/*; do \
	  [ -e "$d" ] || continue; \
	  [ "$(stat -c '%Y' "$d")" -lt "$cutoff" ] || continue; \
	  b=$(du -sb "$d" 2>/dev/null | cut -f1); \
	  total=$((total + b)); \
	  printf '%-8s %14s B  %s\n' "delete" "$b" "$d"; \
	done; \
	printf '\n%-8s %14s B  (would be freed; scratch/ older than %s days)\n' "TOTAL" "$total" "{{KEEP_DAYS}}"

scratch-clean KEEP_DAYS='14':
	@echo "# pruning scratch/ entries older than {{KEEP_DAYS}} days (irreversible; dry-run first)"
	@cutoff=$(date -d "-{{KEEP_DAYS}} days" +%s); \
	freed=0; \
	for d in scratch/*; do \
	  [ -e "$d" ] || continue; \
	  [ "$(stat -c '%Y' "$d")" -lt "$cutoff" ] || continue; \
	  b=$(du -sb "$d" 2>/dev/null | cut -f1); \
	  freed=$((freed + b)); \
	  echo "rm $d"; \
	  rm -rf "$d"; \
	done; \
	printf '%14s B freed\n' "$freed"; \
	just scratch-list >/dev/null || true

scratch-keep-recent-dry-run KEEP_N='1':
	@mapfile -t entries < <(for d in scratch/*; do [ -e "$d" ] || continue; printf '%s\t%s\n' "$(stat -c '%Y' "$d")" "$d"; done | sort -rn); \
	keep=0; total=0; \
	for line in "${entries[@]}"; do \
	  path=${line#*$'\t'}; keep=$((keep + 1)); \
	  b=$(du -sb "$path" 2>/dev/null | cut -f1); \
	  if [ "$keep" -le "{{KEEP_N}}" ]; then \
	    printf '%-8s %14s B  %s\n' "keep" "$b" "$path"; \
	  else total=$((total + b)); printf '%-8s %14s B  %s\n' "delete" "$b" "$path"; fi; \
	done; \
	printf '\n%-8s %14s B  (would be freed; keeping newest %s)\n' "TOTAL" "$total" "{{KEEP_N}}"

scratch-keep-recent KEEP_N='1':
	@echo "# keeping newest {{KEEP_N}} scratch/ entries; deleting the rest (irreversible; dry-run first)"
	@just scratch-keep-recent-dry-run {{KEEP_N}}
	@mapfile -t entries < <(for d in scratch/*; do [ -e "$d" ] || continue; printf '%s\t%s\n' "$(stat -c '%Y' "$d")" "$d"; done | sort -rn); \
	keep=0; \
	for line in "${entries[@]}"; do \
	  path=${line#*$'\t'}; keep=$((keep + 1)); \
	  [ "$keep" -le "{{KEEP_N}}" ] && continue; \
	  echo "rm $path"; rm -rf "$path"; \
	done

morph-warehouse-run profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0":
	@just morph-warehouse-run-with-analyzers "{{profile}}" "{{aat_dir}}" "vibrato sudachi-a sudachi-c" "{{run_id}}" "{{jobs}}"

morph-warehouse-run-with-analyzers profile="full" aat_dir="{{morph_warehouse_aat_dir}}" analyzers="vibrato sudachi-a sudachi-c" run_id="" jobs="0":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id="{{run_id}}"; \
	if [ -z "$run_id" ]; then run_id="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}"; fi; \
	args=() ; \
	for analyzer in {{analyzers}}; do \
	  args+=(--analyzer "$analyzer"); \
	done; \
	AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
	TMPDIR="{{ab_db_root}}/tmp" \
	TMP="{{ab_db_root}}/tmp" \
	TEMP="{{ab_db_root}}/tmp" \
	cargo run --release -p ab-morph-run -- analyze-aat \
		--aat-dir "{{aat_dir}}" \
		"${args[@]}" \
		--warehouse-dir "{{morph_warehouse_dir}}" \
		--run-id "$run_id" \
		--warehouse-profile "{{profile}}" \
		--jobs "$jobs"

morph-warehouse-run-full aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0":
	@just morph-warehouse-run full "{{aat_dir}}" "{{run_id}}" "{{jobs}}"

morph-warehouse-run-triage aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0":
	@just morph-warehouse-run triage "{{aat_dir}}" "{{run_id}}" "{{jobs}}"

morph-warehouse-recreate profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0":
	@just morph-warehouse-clean
	@just morph-warehouse-run "{{profile}}" "{{aat_dir}}" "{{run_id}}" "{{jobs}}"

morph-available-vibrato-dictionaries:
	@for dir in "{{repo_root}}/dictionary/compiled" "{{repo_root}}/dictionary/optimized"; do \
	  for path in \
	    "$dir"/*.dic "$dir"/*.dic.zst \
	    "$dir"/*.model "$dir"/*.model.zst \
	    "$dir"/*.bin "$dir"/*.bin.zst; do \
		[ -e "$path" ] || continue; \
		name="$(basename "$path")"; \
		name="${name%.dic.zst}"; \
		name="${name%.dic}"; \
		name="${name%.model.zst}"; \
		name="${name%.model}"; \
		name="${name%.bin.zst}"; \
		name="${name%.bin}"; \
		echo "$name"; \
	  done; \
	done | sort -u

morph-available-vaporetto-dictionaries:
	@for dir in "{{repo_root}}/dictionary/compiled" "{{repo_root}}/dictionary/optimized"; do \
	  for path in \
	    "$dir"/*.dic "$dir"/*.dic.zst \
	    "$dir"/*.model "$dir"/*.model.zst \
	    "$dir"/*.bin "$dir"/*.bin.zst; do \
		[ -e "$path" ] || continue; \
		name="$(basename "$path")"; \
		name="${name%.dic.zst}"; \
		name="${name%.dic}"; \
		name="${name%.model.zst}"; \
		name="${name%.model}"; \
		name="${name%.bin.zst}"; \
		name="${name%.bin}"; \
		echo "$name"; \
	  done; \
	done | sort -u

morph-available-vaporetto-model-dictionaries:
	@for dir in "{{repo_root}}/dictionary/compiled" "{{repo_root}}/dictionary/optimized"; do \
	  for path in \
	    "$dir"/*.model "$dir"/*.model.zst \
	    "$dir"/*.bin "$dir"/*.bin.zst; do \
		[ -e "$path" ] || continue; \
		name="$(basename "$path")"; \
		name="${name%.model.zst}"; \
		name="${name%.model}"; \
		name="${name%.bin.zst}"; \
		name="${name%.bin}"; \
		echo "$name"; \
	  done; \
	done | sort -u

morph-vaporetto-dictionary-status:
	@printf "Available dictionary source snapshots:\n"
	@if [ -d "{{repo_root}}/dictionary/unidic-sources" ]; then \
		find "{{repo_root}}/dictionary/unidic-sources" -maxdepth 1 -mindepth 1 -type d -printf '  sources/%f\n' | sort; \
	else \
		echo "  (missing) {{repo_root}}/dictionary/unidic-sources"; \
	fi
	@printf "\nVaporetto dictionary artifacts:\n"
	@for dir in compiled optimized; do \
		if [ -d "{{repo_root}}/dictionary/$dir" ]; then \
			echo "  $dir/"; \
			for path in \
				"{{repo_root}}/dictionary/$dir"/*.model "{{repo_root}}/dictionary/$dir"/*.model.zst \
				"{{repo_root}}/dictionary/$dir"/*.bin "{{repo_root}}/dictionary/$dir"/*.bin.zst; do \
				[ -e "$path" ] || continue; \
				echo "    $(basename "$path")"; \
			done | sort; \
		else \
			echo "  (missing) {{repo_root}}/dictionary/$dir"; \
		fi; \
	done
	@printf "\nBuild command references:\n"
	@echo "  Compile Vaporetto-ready source artifacts: just morph-vaporetto-dictionary-build-source-ready"
	@echo "  Refresh legacy conversion (same artifact set): just morph-vaporetto-dictionary-transmute-legacy"
	@echo "  Build all (Vaporetto first then legacy conversions): just morph-vaporetto-dictionaries-rebuild-all"

morph-warehouse-run-all-vibrato-dictionaries profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id_prefix="" jobs="0":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id_prefix="{{run_id_prefix}}"; \
	if [ -z "$run_id_prefix" ]; then run_id_prefix="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}-dict"; fi; \
	for dict in $(just morph-available-vibrato-dictionaries); do \
	  just morph-warehouse-run-with-analyzers "{{profile}}" "{{aat_dir}}" "vibrato:${dict} sudachi-a sudachi-c" "${run_id_prefix}-${dict}" "{{jobs}}"; \
	done

morph-warehouse-run-all-vaporetto-dictionaries profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id_prefix="" jobs="0":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id_prefix="{{run_id_prefix}}"; \
	if [ -z "$run_id_prefix" ]; then run_id_prefix="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}-dict"; fi; \
	for dict in $(just morph-available-vaporetto-model-dictionaries); do \
	  just morph-warehouse-run-with-analyzers "{{profile}}" "{{aat_dir}}" "vaporetto:${dict} sudachi-a sudachi-c" "${run_id_prefix}-${dict}" "{{jobs}}"; \
	done

morph-warehouse-recreate-full jobs="0":
	@just morph-warehouse-recreate full "{{morph_warehouse_aat_dir}}" "" "{{jobs}}"

ab-validator-clean-warehouse:
	@echo "cleaning generated db + canonical warehouse artifacts under {{ab_db_root}} and {{morph_warehouse_dir}}"
	@just clean-db-full
	@just morph-warehouse-clean

ab-validator-clean-warehouse-dry-run:
	@just clean-db-full-dry-run
	@just morph-warehouse-clean-dry-run

ab-validator-recreate-warehouse-run profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0" build_report="0" report_limit="50":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id="{{run_id}}"; \
	if [ -z "$run_id" ]; then run_id="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}"; fi; \
	just clean-db-full; \
	just morph-warehouse-clean; \
	just morph-warehouse-run-with-analyzers "{{profile}}" "{{aat_dir}}" "vibrato sudachi-a sudachi-c" "$run_id" "{{jobs}}"; \
	if [ "{{build_report}}" = "1" ]; then \
		just morph-warehouse-build-latest-report "{{profile}}" "{{report_limit}}"; \
	fi

ab-validator-recreate-warehouse-full jobs="0" run_id="" build_report="0" report_limit="50":
	@just ab-validator-recreate-warehouse-run full "{{morph_warehouse_aat_dir}}" "{{run_id}}" "{{jobs}}" "{{build_report}}" "{{report_limit}}"

ab-validator-refresh-warehouse-full jobs="0" report_limit="50":
	@just ab-validator-recreate-warehouse-run full "{{morph_warehouse_aat_dir}}" "" "{{jobs}}" "1" "{{report_limit}}"

morph-warehouse-list-runs:
	@find "{{morph_warehouse_dir}}/runs" -maxdepth 1 -mindepth 1 -type d -printf '%f\n' | sort

morph-warehouse-build-report RUN_DIR PROFILE="triage" LIMIT="50" OUTPUT_DIR="":
	@if [ -z "{{OUTPUT_DIR}}" ]; then \
		run_dir="{{RUN_DIR}}"; if [ -z "$run_dir" ]; then echo "RUN_DIR required or use: morph-warehouse-build-latest-report" ; exit 1; fi; \
		out_dir="{{morph_warehouse_dir}}/reports/$(basename "$run_dir")"; \
	else \
		out_dir="{{OUTPUT_DIR}}"; \
		run_dir="{{RUN_DIR}}"; \
	fi; \
	"{{repo_root}}/reports/morph-warehouse/build-report.sh" "$run_dir" "$out_dir" "{{LIMIT}}"

morph-warehouse-build-latest-report PROFILE="full" LIMIT="50":
	@run_dir="$(find "{{morph_warehouse_dir}}/runs" -maxdepth 1 -mindepth 1 -type d -name '{{PROFILE}}-*' | sort | tail -n 1)"; \
	if [ -z "$run_dir" ]; then \
		echo "no warehouse runs matching profile '{{PROFILE}}' in {{morph_warehouse_dir}}/runs"; \
		exit 1; \
	fi; \
	out_dir="{{morph_warehouse_dir}}/reports/{{PROFILE}}-$(date -u +%F_%H%M%S)"; \
	"{{repo_root}}/reports/morph-warehouse/build-report.sh" "$run_dir" "$out_dir" "{{LIMIT}}"; \
	echo "$out_dir"

# ── Vibrato dictionary builds (self-contained via Nix + mecab-dic-converter) ──
# Each recipe fetches the NINJAL Unidic zip and converts MeCab → vibrato .dic.zst
# via mecab-dic-converter. The output is symlinked into dictionary/compiled/ so
# the analyzer auto-discovers it.

dictionary-build-cwj:
	@nix build .#vibrato-dict-cwj --no-link --print-out-paths | while read -r out; do \
		for dict in "$$out"/share/vibrato/*.dic.zst; do \
			[ -f "$$dict" ] || continue; \
			ln -sf "$$dict" "{{vibrato_compiled_dir}}/$$(basename "$$dict")"; \
			echo "  linked $$(basename "$$dict")"; \
		done; \
	done

dictionary-build-csj:
	@nix build .#vibrato-dict-csj --no-link --print-out-paths | while read -r out; do \
		for dict in "$$out"/share/vibrato/*.dic.zst; do \
			[ -f "$$dict" ] || continue; \
			ln -sf "$$dict" "{{vibrato_compiled_dir}}/$$(basename "$$dict")"; \
			echo "  linked $$(basename "$$dict")"; \
		done; \
	done

dictionary-build-novel:
	@nix build .#vibrato-dict-novel --no-link --print-out-paths | while read -r out; do \
		for dict in "$$out"/share/vibrato/*.dic.zst; do \
			[ -f "$$dict" ] || continue; \
			ln -sf "$$dict" "{{vibrato_compiled_dir}}/$$(basename "$$dict")"; \
			echo "  linked $$(basename "$$dict")"; \
		done; \
	done

dictionary-build-qkana:
	@nix build .#vibrato-dict-qkana --no-link --print-out-paths | while read -r out; do \
		for dict in "$$out"/share/vibrato/*.dic.zst; do \
			[ -f "$$dict" ] || continue; \
			ln -sf "$$dict" "{{vibrato_compiled_dir}}/$$(basename "$$dict")"; \
			echo "  linked $$(basename "$$dict")"; \
		done; \
	done

dictionary-build-kindai-bungo:
	@nix build .#vibrato-dict-kindai-bungo --no-link --print-out-paths | while read -r out; do \
		for dict in "$$out"/share/vibrato/*.dic.zst; do \
			[ -f "$$dict" ] || continue; \
			ln -sf "$$dict" "{{vibrato_compiled_dir}}/$$(basename "$$dict")"; \
			echo "  linked $$(basename "$$dict")"; \
		done; \
	done

# Build all available vibrato dictionaries and link them.
dictionary-build-all: dictionary-build-cwj dictionary-build-csj dictionary-build-novel dictionary-build-qkana dictionary-build-kindai-bungo
	@echo "All vibrato dictionaries built and linked."

morph-vibrato-dictionary-status:
	@printf "Available dictionary source snapshots:\n"
	@if [ -d "{{repo_root}}/dictionary/unidic-sources" ]; then \
		find "{{repo_root}}/dictionary/unidic-sources" -maxdepth 1 -mindepth 1 -type d -printf '  sources/%f\n' | sort; \
	else \
		echo "  (missing) {{repo_root}}/dictionary/unidic-sources"; \
	fi
	@echo "\nDictionary tooling (in project):"
	@echo "  VERSION            = 202512"
	@echo "  RKYV compiler path = {{vibrato_rkyv_dir}}"
	@echo "  Source root        = {{vibrato_unidic_sources}}"
	@echo "  Compiled output    = {{vibrato_compiled_dir}}"
	@echo "  Optimized output   = {{vibrato_optimized_dir}}"
	@echo "  Legacy loader      = vibrato-rkyv legacy format support enabled"
	@printf "\nCompilation artifacts:\n"
	@for dir in compiled optimized; do \
		if [ -d "{{repo_root}}/dictionary/$dir" ]; then \
			echo "  $dir/"; \
			for path in \
				"{{repo_root}}/dictionary/$dir"/*.dic "{{repo_root}}/dictionary/$dir"/*.dic.zst \
				"{{repo_root}}/dictionary/$dir"/*.model "{{repo_root}}/dictionary/$dir"/*.model.zst \
				"{{repo_root}}/dictionary/$dir"/*.bin "{{repo_root}}/dictionary/$dir"/*.bin.zst; do \
				[ -e "$path" ] || continue; \
				echo "    $(basename "$path")"; \
			done | sort; \
	else \
		echo "  (missing) {{repo_root}}/dictionary/$dir"; \
		fi; \
	done
	@printf "\nBuild command references:\n"
	@echo "  Compile source-ready dictionaries: just morph-vibrato-dictionary-build-source-ready"
	@echo "  Transmute legacy sys.dic binaries: just morph-vibrato-dictionary-transmute-legacy"
	@echo "  Build all (source + legacy):      just morph-vibrato-dictionaries-rebuild-all"

morph-vibrato-dictionary-build-source-ready VERSION="202512":
	@set -euo pipefail
	@if [ ! -d "{{vibrato_unidic_sources}}" ]; then \
		echo "missing source directory: {{vibrato_unidic_sources}}"; \
		exit 1; \
	fi
	@if [ ! -d "{{vibrato_rkyv_dir}}" ]; then \
		echo "missing vibrato-rkyv workspace: {{vibrato_rkyv_dir}}"; \
		echo "set VIBRATO_RKYV_DIR or clone and build third-party/vibrato-rkyv"; \
		exit 1; \
	fi
	@mkdir -p "{{vibrato_compiled_dir}}" "{{vibrato_optimized_dir}}"
	@for src in "{{vibrato_unidic_sources}}"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename "$src")"; \
		lex="$(find "$src" -name lex.csv -type f | head -n 1)"; \
		matrix="$(find "$src" -name matrix.def -type f | head -n 1)"; \
		char="$(find "$src" -name char.def -type f | head -n 1)"; \
		unk="$(find "$src" -name unk.def -type f | head -n 1)"; \
		compiled="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.dic.zst"; \
		optimized="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.dic.zst"; \
		if [ -z "$lex" ] || [ -z "$matrix" ] || [ -z "$char" ] || [ -z "$unk" ]; then \
			echo "SKIP ${name} (source files missing)"; \
			continue; \
		fi; \
		if [ -f "$compiled" ]; then \
			echo "OK ${name} already compiled"; \
		else \
			echo "BUILD ${name} -> ${compiled}"; \
			( \
				cd "{{vibrato_rkyv_dir}}" && \
				cargo run --release -p compiler -- build \
					--lexicon-in "$lex" \
					--matrix-in "$matrix" \
					--char-in "$char" \
					--unk-in "$unk" \
					--sysdic-out "$compiled" \
			); \
		fi; \
		if [ ! -f "$optimized" ]; then \
			ln -sf "$(realpath --relative-to={{vibrato_optimized_dir}} "$compiled")" "$optimized"; \
			echo "WIRE ${name} -> ${optimized}"; \
		fi; \
	done

morph-vibrato-dictionary-transmute-legacy VERSION="202512":
	@set -euo pipefail
	@if [ ! -d "{{vibrato_unidic_sources}}" ]; then \
		echo "missing source directory: {{vibrato_unidic_sources}}"; \
		exit 1; \
	fi
	@if [ ! -d "{{vibrato_rkyv_dir}}" ]; then \
		echo "missing vibrato-rkyv workspace: {{vibrato_rkyv_dir}}"; \
		echo "set VIBRATO_RKYV_DIR or clone and build third-party/vibrato-rkyv"; \
		exit 1; \
	fi
	@mkdir -p "{{vibrato_compiled_dir}}" "{{vibrato_optimized_dir}}"
	@for src in "{{vibrato_unidic_sources}}"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename "$src")"; \
		sysdic_zst="$(find "$src" -maxdepth 1 -name 'sys.dic.zst' -type f | head -n 1)"; \
		sysdic="$(find "$src" -maxdepth 1 -name sys.dic -type f | head -n 1)"; \
		compiled_transmuted="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.dic.zst"; \
		compiled_direct="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.dic"; \
		optimized_transmuted="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.dic.zst"; \
		optimized_direct="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.dic"; \
		staging_dir="$(mktemp -d /tmp/vibrato-transmute-XXXXXX)"; \
		if [ -f "$sysdic_zst" ]; then \
			fallback_source="$sysdic_zst"; \
			fallback_compiled="${compiled_transmuted}"; \
			fallback_optimized="$optimized_transmuted"; \
			echo "TRANS ${name} :: ${sysdic_zst} -> rkyv (legacy zstd sysdic)"; \
		elif [ -f "$sysdic" ]; then \
			fallback_source="$sysdic"; \
			fallback_compiled="$compiled_direct"; \
			fallback_optimized="$optimized_direct"; \
			echo "TRANS ${name} :: ${sysdic} -> rkyv"; \
		else \
			echo "SKIP ${name} (no sys.dic)"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		if [ -f "$compiled_transmuted" ] || [ -f "$compiled_direct" ]; then \
			echo "OK ${name} already has converted dictionary"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		if [ "$sysdic_zst" = "$fallback_source" ]; then \
			zstd -d --stdout "$fallback_source" > "$staging_dir/system.dic"; \
		else \
			cp "$fallback_source" "$staging_dir/system.dic"; \
		fi; \
		if ( \
			cd "{{vibrato_rkyv_dir}}" && \
			cargo run --release -p compiler -- transmute \
				-o "$staging_dir" \
				"$(realpath "$staging_dir/system.dic")" \
		); then \
			echo "OK ${name} transmuted to rkyv"; \
			if [ -f "$staging_dir/system.dic.zst" ]; then \
				mv "$staging_dir/system.dic.zst" "$compiled_transmuted"; \
				compiled_path="$compiled_transmuted"; \
				optimized_path="$optimized_transmuted"; \
			elif [ -f "$staging_dir/system.dic" ]; then \
				zstd -f "$staging_dir/system.dic" -o "$compiled_transmuted"; \
				compiled_path="$compiled_transmuted"; \
				optimized_path="$optimized_transmuted"; \
			else \
				echo "WARN ${name} transmute completed without dictionary output; using direct legacy wire"; \
				compiled_path="$fallback_compiled"; \
				optimized_path="$fallback_optimized"; \
			fi; \
		else \
			echo "WARN ${name} (transmute compatibility error)"; \
			compiled_path="$fallback_compiled"; \
			optimized_path="$fallback_optimized"; \
			ln -sf "$(realpath "$fallback_source")" "$compiled_path"; \
		fi; \
		if [ ! -e "$compiled_path" ]; then \
			echo "FAIL ${name} did not produce compiled artifact"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		if [ ! -f "$optimized_path" ]; then \
			ln -sf "$(realpath --relative-to={{vibrato_optimized_dir}} "$compiled_path")" "$optimized_path"; \
			echo "WIRE ${name} -> ${optimized_path}"; \
		fi; \
		rm -rf "$staging_dir"; \
		if [ -f "$compiled_transmuted" ]; then \
			echo "DONE ${name} (${compiled_transmuted})"; \
		else \
			echo "DONE ${name} (direct legacy: ${compiled_path})"; \
		fi; \
	done

morph-vibrato-dictionaries-rebuild-all VERSION="202512":
	@just morph-vibrato-dictionary-build-source-ready "{{VERSION}}"
	@just morph-vibrato-dictionary-transmute-legacy "{{VERSION}}"
	@echo "VIBRATO DICTIONARY REBUILD COMPLETE"

morph-vaporetto-dictionary-build-source-ready VERSION="202512":
	@set -euo pipefail
	@if [ ! -d "{{vibrato_unidic_sources}}" ]; then \
		echo "missing source directory: {{vibrato_unidic_sources}}"; \
		exit 1; \
	fi
	@mkdir -p "{{vibrato_compiled_dir}}" "{{vibrato_optimized_dir}}"
	@for src in "{{vibrato_unidic_sources}}"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename "$src")"; \
		model_zst="$(find "$src" -maxdepth 1 \( -name 'model.bin.zst' -o -name '*.model.zst' \) -type f | head -n 1)"; \
		model_plain="$(find "$src" -maxdepth 1 \( -name 'model.bin' -o -name '*.model' \) -type f | head -n 1)"; \
		if [ -z "$model_zst" ] && [ -z "$model_plain" ]; then \
			echo "SKIP ${name} (no Vaporetto model source)"; \
			continue; \
		fi; \
		compiled_model="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.model"; \
		compiled_model_zst="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.model.zst"; \
		optimized_model="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.model"; \
		optimized_model_zst="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.model.zst"; \
		legacy_compiled="{{vibrato_compiled_dir}}/${name}.model"; \
		legacy_optimized="{{vibrato_optimized_dir}}/${name}.model"; \
		if [ -n "$model_zst" ]; then \
			if [ ! -f "$compiled_model_zst" ]; then \
				echo "BUILD ${name} -> ${compiled_model_zst}"; \
				ln -sf "$(realpath "$model_zst")" "$compiled_model_zst"; \
			else \
				echo "OK ${name} model artifact already exists"; \
			fi; \
			if [ ! -L "$compiled_model" ] && [ ! -f "$compiled_model" ]; then \
				ln -sf "$(realpath --relative-to={{vibrato_compiled_dir}} "$compiled_model_zst")" "$compiled_model"; \
			fi; \
			if [ ! -f "$optimized_model_zst" ]; then \
				ln -sf "$(realpath --relative-to={{vibrato_optimized_dir}} "$compiled_model_zst")" "$optimized_model_zst"; \
				echo "WIRE ${name} -> ${optimized_model_zst}"; \
			fi; \
		else \
			if [ ! -f "$compiled_model" ]; then \
				echo "BUILD ${name} -> ${compiled_model}"; \
				ln -sf "$(realpath "$model_plain")" "$compiled_model"; \
			else \
				echo "OK ${name} model artifact already exists"; \
			fi; \
		fi; \
		if [ -L "$legacy_compiled" ] || [ -e "$legacy_compiled" ]; then \
			rm -f "$legacy_compiled"; \
		fi; \
		ln -sf "$(realpath --relative-to={{vibrato_compiled_dir}} "$compiled_model")" "$legacy_compiled"; \
		if [ -L "$legacy_optimized" ] || [ -e "$legacy_optimized" ]; then \
			rm -f "$legacy_optimized"; \
		fi; \
		ln -sf "$(realpath --relative-to={{vibrato_optimized_dir}} "$compiled_model")" "$legacy_optimized"; \
	done

morph-vaporetto-dictionary-transmute-legacy VERSION="202512":
	@just morph-vaporetto-dictionary-build-source-ready "{{VERSION}}"

morph-vaporetto-dictionaries-rebuild-all VERSION="202512":
	@just morph-vaporetto-dictionary-build-source-ready "{{VERSION}}"
	@just morph-vaporetto-dictionary-transmute-legacy "{{VERSION}}"
	@echo "VAPORETTO DICTIONARY REBUILD COMPLETE"

morph-vaporetto-dictionary-audit:
	@if [ ! -d "{{repo_root}}/dictionary/unidic-sources" ]; then \
		echo "missing: {{repo_root}}/dictionary/unidic-sources"; \
		exit 1; \
	fi
	@printf "dictionary,status,source_artifact,versioned_compiled,optimized,notes\n"
	@for src in "{{repo_root}}/dictionary/unidic-sources"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename "$src")"; \
		model_zst="$(find "$src" -maxdepth 1 \( -name 'model.bin.zst' -o -name '*.model.zst' \) -type f | head -n 1)"; \
		model_plain="$(find "$src" -maxdepth 1 \( -name 'model.bin' -o -name '*.model' \) -type f | head -n 1)"; \
		if [ -n "$model_zst" ]; then \
			source_artifact="model-zst"; \
			source_path="$model_zst"; \
		elif [ -n "$model_plain" ]; then \
			source_artifact="model-bin"; \
			source_path="$model_plain"; \
		else \
			source_artifact="missing"; \
			source_path=""; \
		fi; \
		compiled_versioned_zst="{{repo_root}}/dictionary/compiled/${name}-202512.model.zst"; \
		compiled_versioned="{{repo_root}}/dictionary/compiled/${name}-202512.model"; \
		optimized_versioned_zst="{{repo_root}}/dictionary/optimized/${name}-202512.model.zst"; \
		optimized_versioned="{{repo_root}}/dictionary/optimized/${name}-202512.model"; \
		compiled_alias="{{repo_root}}/dictionary/compiled/${name}.model"; \
		optimized_alias="{{repo_root}}/dictionary/optimized/${name}.model"; \
		if [ -n "$source_artifact" ] && [ "$source_artifact" != "missing" ]; then \
			notes="vaporetto model source available"; \
		else \
			notes="no source model file (no model.bin/ .model found)"; \
		fi; \
		if [ -f "$compiled_versioned_zst" ]; then \
			versioned_compiled="present-zst"; \
		elif [ -f "$compiled_versioned" ]; then \
			versioned_compiled="present"; \
		elif [ -f "$compiled_alias" ]; then \
			versioned_compiled="present-alias"; \
		else \
			versioned_compiled="missing"; \
		fi; \
		if [ -f "$optimized_versioned_zst" ]; then \
			optimized="present-zst"; \
		elif [ -f "$optimized_versioned" ]; then \
			optimized="present"; \
		elif [ -f "$optimized_alias" ]; then \
			optimized="present-alias"; \
		else \
			optimized="missing"; \
		fi; \
		printf '%s,%s,%s,%s,%s,%s\n' "$name" "$source_artifact" "$source_path" "$versioned_compiled" "$optimized" "$notes"; \
	done | sort

morph-vibrato-dictionary-audit:
	@if [ ! -d "{{repo_root}}/dictionary/unidic-sources" ]; then \
		echo "missing: {{repo_root}}/dictionary/unidic-sources"; \
		exit 1; \
	fi
	@printf "dictionary,status,versioned_compiled,optimized,notes\n"
	@for src in "{{repo_root}}/dictionary/unidic-sources"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename "$src")"; \
		lex="$(find "$src" -name lex.csv -type f | head -n 1)"; \
		matrix="$(find "$src" -name matrix.def -type f | head -n 1)"; \
		char="$(find "$src" -name char.def -type f | head -n 1)"; \
		unk="$(find "$src" -name unk.def -type f | head -n 1)"; \
		sysdic="$(find "$src" -maxdepth 1 -name 'sys.dic' -o -name 'sys.dic.zst' -type f | head -n 1)"; \
		if [ -n "$lex" ] && [ -n "$matrix" ] && [ -n "$char" ] && [ -n "$unk" ]; then \
			status="compile-ready"; \
			notes="source has lex/matrix/char/unk"; \
		elif [ -n "$sysdic" ]; then \
			status="legacy-direct"; \
			notes="source has sys.dic/sys.dic.zst"; \
		else \
			status="missing-source"; \
			notes="missing lex|matrix|char|unk and sys.dic"; \
		fi; \
		compiled_versioned="{{repo_root}}/dictionary/compiled/${name}-202512.dic.zst"; \
		compiled_compact="{{repo_root}}/dictionary/compiled/${name}.dic.zst"; \
		optimized_versioned="{{repo_root}}/dictionary/optimized/${name}-202512.dic.zst"; \
		optimized_alias="{{repo_root}}/dictionary/optimized/${name}.dic.zst"; \
		compiled_legacy="{{repo_root}}/dictionary/compiled/${name}-202512.dic"; \
		optimized_legacy="{{repo_root}}/dictionary/optimized/${name}-202512.dic"; \
		if [ -f "$compiled_versioned" ]; then \
			versioned_compiled="present"; \
		elif [ -f "$compiled_legacy" ]; then \
			versioned_compiled="present-legacy"; \
		elif [ -f "$compiled_compact" ]; then \
			versioned_compiled="compact-legacy"; \
		else \
			versioned_compiled="missing"; \
		fi; \
		if [ -f "$optimized_versioned" ]; then \
			optimized="present-202512"; \
		elif [ -f "$optimized_legacy" ]; then \
			optimized="present-legacy-202512"; \
		elif [ -f "$optimized_alias" ]; then \
			optimized="present-alias"; \
		else \
			optimized="missing"; \
		fi; \
		printf '%s,%s,%s,%s,%s\n' "$name" "$status" "$versioned_compiled" "$optimized" "$notes"; \
	done | sort

# ---- aozora-epub3 adapter ---------------------------------------------------

aozora-epub3-jar:
	@cd "{{repo_root}}/references/parsers/AozoraEpub3-JDK21" && ./gradlew jar

aozora-epub3-build PROFILE="release":
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora-epub3/Cargo.toml" --{{PROFILE}}

aozora-epub3-test:
	@cargo test --manifest-path "{{repo_root}}/adapters/aozora-epub3/Cargo.toml"

aozora-epub3-smoke: aozora-epub3-build
	@printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
	  | "{{repo_root}}/adapters/aozora-epub3/aozora-epub3-adapter" --mode aat \
	  | jq -e '.meta.adapter == "aozora-epub3" and .meta.parse_complete == true and (.blocks | length >= 1)' >/dev/null
	@echo "aozora-epub3 smoke ok"
