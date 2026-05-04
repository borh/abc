set shell := ["bash", "-euo", "pipefail", "-c"]

repo_root := `pwd`
ab_db_root := env_var_or_default("AB_DB_ROOT", "/db/ab-validator")

default:
	@just --list

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
