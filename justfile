set shell := ["bash", "-euo", "pipefail", "-c"]

repo_root := `pwd`
ab_db_root := env_var_or_default("AB_DB_ROOT", "/db/ab-validator")
morph_warehouse_dir := env_var_or_default("AB_MORPH_WAREHOUSE_DIR", "/db/ab-validator/morph-warehouse")
morph_warehouse_aat_dir := env_var_or_default("AB_MORPH_WAREHOUSE_AAT_DIR", "/db/ab-validator/aat-corpus/aozora2html-aat/aozora2html-adapter")
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
	  for path in "$dir"/*.dic "$dir"/*.dic.zst; do \
	    [ -e "$path" ] || continue; \
	    name="$(basename "$path")"; \
	    name="${name%.dic.zst}"; \
	    name="${name%.dic}"; \
	    echo "$name"; \
	  done; \
	done | sort -u

morph-warehouse-run-all-vibrato-dictionaries profile="full" aat_dir="{{morph_warehouse_aat_dir}}" run_id_prefix="" jobs="0":
	@jobs="{{jobs}}"; \
	if [ "$jobs" = "0" ]; then jobs="$(nproc)"; fi; \
	run_id_prefix="{{run_id_prefix}}"; \
	if [ -z "$run_id_prefix" ]; then run_id_prefix="{{profile}}-$(date -u +%F_%H%M%S)-jobs${jobs}-dict"; fi; \
	for dict in $(just morph-available-vibrato-dictionaries); do \
	  just morph-warehouse-run-with-analyzers "{{profile}}" "{{aat_dir}}" "vibrato:${dict} sudachi-a sudachi-c" "${run_id_prefix}-${dict}" "{{jobs}}"; \
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
	@printf "\nCompilation artifacts:\n"
	@for dir in compiled optimized; do \
		if [ -d "{{repo_root}}/dictionary/$dir" ]; then \
			echo "  $dir/"; \
			for path in "{{repo_root}}/dictionary/$dir"/*.dic "{{repo_root}}/dictionary/$dir"/*.dic.zst; do \
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
		sysdic="$(find "$src" -maxdepth 1 -name sys.dic -type f | head -n 1)"; \
		sysdic_zst="$(find "$src" -maxdepth 1 -name 'sys.dic.zst' -type f | head -n 1)"; \
		compiled="{{vibrato_compiled_dir}}/${name}-{{VERSION}}.dic.zst"; \
		optimized="{{vibrato_optimized_dir}}/${name}-{{VERSION}}.dic.zst"; \
		staging_dir="$(mktemp -d /tmp/vibrato-transmute-XXXXXX)"; \
		if [ -f "$sysdic_zst" ]; then \
			echo "TRANS ${name} :: ${sysdic_zst} -> ${compiled} (legacy zstd sysdic)"; \
			zstd -d --stdout "$sysdic_zst" > "$staging_dir/system.dic"; \
		elif [ -f "$sysdic" ]; then \
			echo "TRANS ${name} :: ${sysdic} -> ${compiled}"; \
			cp "$sysdic" "$staging_dir/system.dic"; \
		else \
			echo "SKIP ${name} (no sys.dic)"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		if [ -f "${compiled}" ]; then \
			echo "OK ${name} already has converted dictionary"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		echo "REPACKAGE ${name} via transmute format conversion"; \
		if ( \
			cd "{{vibrato_rkyv_dir}}" && \
			cargo run --release -p compiler -- transmute \
				-o "$staging_dir" \
				"$(realpath "$staging_dir/system.dic")" \
		); then \
			if [ -f "$staging_dir/system.dic.zst" ]; then \
				mv "$staging_dir/system.dic.zst" "$compiled"; \
			elif [ -f "$staging_dir/system.dic" ]; then \
				zstd -f "$staging_dir/system.dic" -o "$compiled"; \
			else \
				echo "SKIP ${name} (transmute output missing)"; \
				rm -rf "$staging_dir"; \
				continue; \
			fi; \
		else \
			echo "SKIP ${name} (transmute compatibility error)"; \
			rm -rf "$staging_dir"; \
			continue; \
		fi; \
		rm -rf "$staging_dir"; \
		if [ ! -f "$optimized" ]; then \
			ln -sf "$(realpath --relative-to={{vibrato_optimized_dir}} "$compiled")" "$optimized"; \
			echo "WIRE ${name} -> ${optimized}"; \
		fi; \
	done

morph-vibrato-dictionaries-rebuild-all VERSION="202512":
	@just morph-vibrato-dictionary-build-source-ready "{{VERSION}}"
	@just morph-vibrato-dictionary-transmute-legacy "{{VERSION}}"
	@echo "VIBRATO DICTIONARY REBUILD COMPLETE"

morph-vibrato-dictionary-audit:
	@if [ ! -d "{{repo_root}}/dictionary/unidic-sources" ]; then \
		echo "missing: {{repo_root}}/dictionary/unidic-sources"; \
		exit 1; \
	fi
	@printf "dictionary,status,versioned_compiled,optimized,notes\n"
	@for src in "{{repo_root}}/dictionary/unidic-sources"/*; do \
		[ -d "$src" ] || continue; \
		name="$(basename \"$src\")"; \
		lex="$(find "$src" -name lex.csv -type f | head -n 1)"; \
		matrix="$(find "$src" -name matrix.def -type f | head -n 1)"; \
		char="$(find "$src" -name char.def -type f | head -n 1)"; \
		unk="$(find "$src" -name unk.def -type f | head -n 1)"; \
		if [ -n "$lex" ] && [ -n "$matrix" ] && [ -n "$char" ] && [ -n "$unk" ]; then \
			status="compile-ready"; \
			notes="source has lex/matrix/char/unk"; \
		else \
			status="precompiled-only"; \
			notes="missing one of lex|matrix|char|unk"; \
		fi; \
		compiled_versioned="{{repo_root}}/dictionary/compiled/${name}-202512.dic.zst"; \
		compiled_compact="{{repo_root}}/dictionary/compiled/${name}.dic.zst"; \
		optimized_versioned="{{repo_root}}/dictionary/optimized/${name}-202512.dic.zst"; \
		optimized_alias="{{repo_root}}/dictionary/optimized/${name}.dic.zst"; \
		if [ -f "$compiled_versioned" ]; then \
			versioned_compiled="present"; \
		elif [ -f "$compiled_compact" ]; then \
			versioned_compiled="compact-legacy"; \
		else \
			versioned_compiled="missing"; \
		fi; \
		if [ -f "$optimized_versioned" ]; then \
			optimized="present-202512"; \
		elif [ -f "$optimized_alias" ]; then \
			optimized="present-alias"; \
		else \
			optimized="missing"; \
		fi; \
		printf '%s,%s,%s,%s,%s\n' "$name" "$status" "$versioned_compiled" "$optimized" "$notes"; \
	done | sort
