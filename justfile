set shell := ["bash", "-euo", "pipefail", "-c"]

repo_root := `pwd`
ab_db_root := env_var_or_default("AB_DB_ROOT", "/db/ab-validator")
morph_warehouse_dir := env_var_or_default("AB_MORPH_WAREHOUSE_DIR", "/db/ab-validator/morph-warehouse")
morph_warehouse_aat_dir := env_var_or_default("AB_MORPH_WAREHOUSE_AAT_DIR", "/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter")

default:
	@just --list

aozora2html-rust-build PROFILE="release":
	@cargo build --manifest-path "{{repo_root}}/adapters/aozora2html/Cargo.toml" --{{PROFILE}}

aozora2html-rust-clean:
	@rm -rf "{{repo_root}}/adapters/aozora2html/target"

aozora2html-rust-test:
	@cargo test --manifest-path "{{repo_root}}/adapters/aozora2html/Cargo.toml"

aozora2html-rust-parity:
	@AOZORA2HTML_PARITY=1 AOZORA2HTML_BACKEND=rust pytest "{{repo_root}}/adapters/aozora2html/tests/test_mapper.py" -vv

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

morph-warehouse-run profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id="" jobs="0":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id="{{run_id}}"; \
	if [ -z "$run_id" ]; then run_id="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}"; fi; \
	AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
	TMPDIR="{{ab_db_root}}/tmp" \
	TMP="{{ab_db_root}}/tmp" \
	TEMP="{{ab_db_root}}/tmp" \
	cargo run --release -p ab-morph-run -- analyze-aat \
		--aat-dir "{{aat_dir}}" \
		--analyzer vibrato \
		--analyzer sudachi-a \
		--analyzer sudachi-c \
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
	AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
	TMPDIR="{{ab_db_root}}/tmp" \
	TMP="{{ab_db_root}}/tmp" \
	TEMP="{{ab_db_root}}/tmp" \
	cargo run --release -p ab-morph-run -- analyze-aat \
		--aat-dir "{{aat_dir}}" \
		--analyzer vibrato \
		--analyzer sudachi-a \
		--analyzer sudachi-c \
		--warehouse-dir "{{morph_warehouse_dir}}" \
		--run-id "$run_id" \
		--warehouse-profile "{{profile}}" \
		--jobs "$jobs"; \
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
