{
  description = "Aozora Bunko Converter development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    local-pkgs = {
      url = "path:./nix/ci-empty-local-pkgs";
      flake = false;
    };
    tei-eaj-aozora-tei = {
      url = "github:TEI-EAJ/aozora_tei/77a675fc2771936f9544505d922d4cd45075338c";
      flake = false;
    };
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane/v0.23.4";
    };
  };

  outputs =
    {
      nixpkgs,
      local-pkgs,
      tei-eaj-aozora-tei,
      clj-nix,
      crane,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
      localPkgsOverlay =
        final: _prev:
        import local-pkgs {
          pkgs = final;
          craneLib = crane.mkLib final;
        };
    in
    {
      formatter = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
        in
        pkgs.nixfmt
      );

      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };
          tei = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./schemas/tei-profile.odd;
          };
          teiEajAozoraReports =
            subcommand:
            pkgs.writeShellScript "abc-tei-eaj-aozora-${subcommand}-launcher" ''
              exec ${pkgs.python3}/bin/python ${./tools/tei_eaj_aozora_reports.py} \
                --compare-script ${./prototypes/tei-eaj-comparison/tei_eaj_compare.py} \
                --tei-eaj-root ${tei-eaj-aozora-tei} \
                --source-rev 77a675fc2771936f9544505d922d4cd45075338c \
                ${subcommand} "$@"
            '';
          mkTeiEajAozoraReportApp =
            {
              subcommand,
              description,
            }:
            {
              type = "app";
              program = toString (teiEajAozoraReports subcommand);
              meta.description = description;
            };
        in
        {
          validate-design-bundle = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-design-bundle" ''
                export PATH="${
                  pkgs.lib.makeBinPath [
                    pkgs.git-cliff
                    pkgs.libxml2
                  ]
                }:''${PATH:-}"
                export TEI_SCHEMA_PATH="${tei.teiAllSchema}"
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-design-bundle "$@"
              ''
            );
            meta.description = "Validate ABC v0 design-bundle schemas and fixtures";
          };

          materialize-import = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-materialize-import" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/materialize-import "$@"
              ''
            );
            meta.description = "Materialize imported ab-validator output as ABC manifests";
          };

          materialize-publication = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-materialize-publication" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/materialize-publication "$@"
              ''
            );
            meta.description = "Materialize parser-IR publication plaintext and TEI artifacts";
          };

          materialize-publications-batch = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-materialize-publications-batch" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/materialize-publication "$@"
              ''
            );
            meta.description = "Materialize parser-IR publication plaintext and TEI artifacts from a batch JSON";
          };

          materialize-source-snapshot = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-materialize-source-snapshot" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/materialize-source-snapshot "$@"
              ''
            );
            meta.description = "Materialize a source corpus snapshot and source manifests";
          };

          manifest-to-rdf = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-manifest-to-rdf" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/manifest-to-rdf "$@"
              ''
            );
            meta.description = "Generate deterministic RDF/Turtle view from an ABC manifest";
          };

          aozora-ingest = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-aozora-ingest" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/aozora-ingest "$@"
              ''
            );
            meta.description = "Build a metadata-record JSON from an Aozora list_person_all_extended ZIP slice";
          };

          validate-corpus = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-corpus" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-corpus "$@"
              ''
            );
            meta.description = "Validate an ingested corpus directory through SHACL";
          };

          person-drift-history = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-person-drift-history" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/person-drift-history "$@"
              ''
            );
            meta.description = "Audit generated corpus snapshots for conservative person split/merge candidates";
          };

          aozora-history-audit = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-aozora-history-audit" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/aozora-history-audit "$@"
              ''
            );
            meta.description = "Extract two Aozora git refs, ingest them, validate current corpus, and report person drift candidates";
          };

          aozora-upstream-audit = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-aozora-upstream-audit" ''
                set -euo pipefail
                if [ ! -d examples/v0/example-persons ]; then
                  echo "aozora-upstream-audit: run from the ABC repository root; examples/v0/example-persons not found" >&2
                  exit 2
                fi
                exec ${pkgs.clojure}/bin/clojure -M:abc/aozora-history-audit \
                  --drift-persons-dir examples/v0/example-persons \
                  --fail-on-candidates \
                  --fail-on-drift-participant-updates \
                  "$@"
              ''
            );
            meta.description = "Default after-upstream Aozora audit with drift sidecars and failure gates enabled";
          };

          regenerate-tei-profile = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-regenerate-tei-profile" ''
                set -euo pipefail
                target="''${1:-$PWD}"
                if [ ! -d "$target/schemas" ]; then
                  echo "regenerate-tei-profile: $target/schemas not found" >&2
                  echo "usage: nix run .#regenerate-tei-profile [-- <repo-root>]" >&2
                  exit 2
                fi
                install -m 0644 "${tei.artifacts}/tei-profile.rng" "$target/schemas/tei-profile.rng"
                install -m 0644 "${tei.artifacts}/tei-profile.sch" "$target/schemas/tei-profile.sch"
                echo "Updated $target/schemas/tei-profile.{rng,sch} from schemas/tei-profile.odd."
              ''
            );
            meta.description = "Regenerate schemas/tei-profile.{rng,sch} from schemas/tei-profile.odd via TEI Stylesheets";
          };

          tei-eaj-aozora-tei-source = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-tei-eaj-aozora-tei-source" ''
                printf '%s\n' ${tei-eaj-aozora-tei}
              ''
            );
            meta.description = "Print the pinned TEI-EAJ/aozora_tei comparison source path";
          };

          tei-eaj-aozora-melos-report = mkTeiEajAozoraReportApp {
            subcommand = "melos";
            description = "Regenerate the ABC vs TEI-EAJ Melos comparison report";
          };

          tei-eaj-aozora-all-work-report = mkTeiEajAozoraReportApp {
            subcommand = "all-work";
            description = "Regenerate the all-work ABC vs TEI-EAJ comparison report";
          };

          tei-eaj-aozora-workset-json = mkTeiEajAozoraReportApp {
            subcommand = "workset-json";
            description = "Regenerate the machine-readable TEI-EAJ workset export";
          };

          tei-eaj-aozora-reports = mkTeiEajAozoraReportApp {
            subcommand = "all";
            description = "Regenerate TEI-EAJ comparison Markdown and JSON reports";
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };
          cljDepsCache = pkgs.mk-deps-cache {
            lockfile = ./deps-lock.json;
          };
          tei = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./schemas/tei-profile.odd;
          };
        in
        {
          clj-nix-focused-tests =
            pkgs.runCommand "abc-clj-nix-focused-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git-cliff
                  pkgs.libxml2
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source

                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                # The Nix sandbox has no network access; tests needing the upstream
                # TEI RelaxNG schema (fetched at app build time, not test time) skip
                # via this flag. End-to-end TEI validation runs via the
                # `nix run .#validate-design-bundle` app, not here.
                export ABC_TEI_SCHEMA_SKIP=1

                clojure -M:test:kaocha -m kaocha.runner

                mkdir -p "$out"
                echo "ABC Clojure tests passed with clj-nix dependency cache (kaocha auto-discovery)." > "$out/result.txt"
              '';

          aat-parser-ir-probe-tests =
            pkgs.runCommand "abc-aat-parser-ir-probe-tests"
              {
                nativeBuildInputs = [
                  pkgs.python3
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source

                python -m unittest prototypes/aat-to-parser-ir-probe/test_probe_mapping.py

                mkdir -p "$out"
                echo "AAT parser-IR probe tests passed." > "$out/result.txt"
              '';

          contract-surface = pkgs.runCommand "abc-contract-surface-check" { } ''
            test -f ${./resources/abc/ndc9.edn.xz}
            test -f ${./deps-lock.json}
            test -f ${./src/abc/annotation/schema.clj}
            test -f ${./src/abc/text.clj}
            test -f ${./src/abc/ndc.clj}
            test -f ${./src/abc/tools/hash.clj}
            test -f ${./src/abc/tools/jcs.clj}
            test -f ${./src/abc/tools/json.clj}
            test -f ${./src/abc/tools/schema.clj}
            test -f ${./src/abc/tools/aozora_csv.clj}
            test -f ${./src/abc/tools/aozora_history_audit.clj}
            test -f ${./src/abc/tools/aozora_ingest.clj}
            test -f ${./src/abc/git.clj}
            test -f ${./prototypes/tei-eaj-comparison/tei_eaj_compare.py}
            test -f ${./prototypes/tei-eaj-comparison/test_tei_eaj_compare.py}
            test -f ${./tools/tei_eaj_aozora_reports.py}
            test -f ${./src/abc/tools/manifest_index.clj}
            test -f ${./src/abc/tools/logging.clj}
            test -f ${./src/abc/tools/manifest_to_rdf.clj}
            test -f ${./src/abc/tools/materialize_publication.clj}
            test -f ${./src/abc/tools/metadata_record.clj}
            test -f ${./src/abc/tools/parser_ir_plaintext.clj}
            test -f ${./src/abc/tools/parser_ir_publication_policy.clj}
            test -f ${./src/abc/tools/parser_ir_tei.clj}
            test -f ${./src/abc/tools/parser_ir_vocabulary.clj}
            test -f ${./src/abc/tools/person_drift_history.clj}
            test -f ${./src/abc/tools/person_record.clj}
            test -f ${./src/abc/tools/rdf_prefixes.clj}
            test -f ${./src/abc/tools/schematron.clj}
            test -f ${./src/abc/tools/shacl.clj}
            test -f ${./src/abc/tools/tei.clj}
            test -f ${./src/abc/tools/tei_header.clj}
            test -f ${./src/abc/tools/validate_design_bundle.clj}
            test -f ${./tools/schema_contracts.py}
            test -f ${./schemas/README.md}
            test -f ${./schemas/schema-contracts.json}
            test -f ${./schemas/metadata-record.schema.json}
            test -f ${./schemas/person-record.schema.json}
            test -f ${./schemas/tei-profile.rng}
            test -f ${./schemas/tei-profile.sch}
            test -f ${./schemas/tei-validation-result.schema.json}
            test -f ${./test/abc/tools/hash_test.clj}
            test -f ${./test/abc/tools/jcs_test.clj}
            test -f ${./test/abc/tools/schema_test.clj}
            test -f ${./test/abc/tools/manifest_index_test.clj}
            test -f ${./test/abc/tools/manifest_to_rdf_test.clj}
            test -f ${./test/abc/tools/materialize_import_test.clj}
            test -f ${./test/abc/tools/materialize_publication_test.clj}
            test -f ${./test/abc/tools/parser_ir_plaintext_test.clj}
            test -f ${./test/abc/tools/parser_ir_publication_policy_test.clj}
            test -f ${./test/abc/tools/parser_ir_tei_test.clj}
            test -f ${./test/abc/tools/parser_ir_vocabulary_test.clj}
            test -f ${./test/abc/tools/aozora_csv_test.clj}
            test -f ${./test/abc/tools/aozora_history_audit_test.clj}
            test -f ${./test/abc/git_blob_test.clj}
            test -f ${./test/abc/tools/metadata_record_test.clj}
            test -f ${./test/abc/tools/person_drift_history_test.clj}
            test -f ${./test/abc/tools/person_record_test.clj}
            test -f ${./test/abc/tools/aozora_ingest_test.clj}
            test -f ${./test/abc/tools/schematron_test.clj}
            test -f ${./test/abc/tools/shacl_test.clj}
            test -f ${./test/abc/tools/tei_test.clj}
            test -f ${./test/abc/tools/tei_header_test.clj}
            test -f ${./test/abc/tools/tei_header_unit_test.clj}
            test -f ${./examples/v0/example-work/metadata-record.json}
            test -f ${./examples/v0/example-work/metadata-record.ttl}
            test -f ${./examples/v0/example-persons/000879.json}
            test -f ${./data/parser-ir-publication-policy-v0.json}
            test -f ${./test/abc/tools/validate_design_bundle_test.clj}
            test -f ${./test/abc/annotation_schema_test.clj}
            test -f ${./test/abc/text_test.clj}
            test -f ${./test/abc/ndc_test.clj}
            mkdir -p "$out"
            echo "ABC v0 contract source surface is present." > "$out/result.txt"
          '';

          tei-profile-drift = pkgs.runCommand "abc-tei-profile-drift" { } ''
            # ADR 0012: schemas/tei-profile.odd is canonical; the .rng and
            # .sch are reproducibly generated artifacts. This check
            # regenerates them from the ODD via TEI Stylesheets, applies
            # the same build-artifact canonicalization (timestamp strip +
            # ABC pattern-id rewrite), and byte-diffs against the
            # checked-in files. Any drift fails the build so the ODD
            # remains the single source of truth.
            diff -u ${./schemas/tei-profile.rng} ${tei.artifacts}/tei-profile.rng
            diff -u ${./schemas/tei-profile.sch} ${tei.artifacts}/tei-profile.sch
            mkdir -p "$out"
            echo "schemas/tei-profile.{rng,sch} match the ODD-derived artifacts." > "$out/result.txt"
          '';

          schema-contract-drift =
            pkgs.runCommand "abc-schema-contract-drift" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                python tools/schema_contracts.py
                mkdir -p "$out"
                echo "schemas/schema-contracts.json matches the checked-in schemas." > "$out/result.txt"
              '';

          tei-eaj-aozora-comparison-source = pkgs.runCommand "abc-tei-eaj-aozora-comparison-source" { } ''
            test -f ${tei-eaj-aozora-tei}/README.md
            test -f ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_tei.xml
            test -f ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_header_updated.xml
            grep -q "走れメロス" ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_tei.xml
            grep -q "Best Practice for TEI in Libraries" ${tei-eaj-aozora-tei}/README.md
            mkdir -p "$out"
            echo "Pinned TEI-EAJ/aozora_tei comparison source includes the Melos Level 4 fixtures." > "$out/result.txt"
          '';

          tei-eaj-comparison-probe-tests =
            pkgs.runCommand "abc-tei-eaj-comparison-probe-tests" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                cp -R ${./prototypes/tei-eaj-comparison} probe
                chmod -R u+w probe
                python -m unittest discover -s probe -p 'test_*.py'
                mkdir -p "$out"
                echo "TEI-EAJ comparison probe tests passed." > "$out/result.txt"
              '';

          tei-eaj-report-python-quality =
            pkgs.runCommand "abc-tei-eaj-report-python-quality"
              {
                nativeBuildInputs = [
                  pkgs.mypy
                  pkgs.python3
                  pkgs.ruff
                ];
              }
              ''
                set -euo pipefail
                cp ${./tools/tei_eaj_aozora_reports.py} tei_eaj_aozora_reports.py
                ruff format --check tei_eaj_aozora_reports.py
                ruff check tei_eaj_aozora_reports.py
                mypy --cache-dir "$TMPDIR/mypy-cache" tei_eaj_aozora_reports.py
                mkdir -p "$out"
                echo "TEI-EAJ report launcher passes ruff format/check and mypy." > "$out/result.txt"
              '';

          tei-eaj-aozora-report-generation =
            pkgs.runCommand "abc-tei-eaj-aozora-report-generation" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                set -euo pipefail
                mkdir -p abc
                cat > abc/melos.xml <<'XML'
                <?xml version="1.0" encoding="UTF-8"?>
                <TEI xmlns="http://www.tei-c.org/ns/1.0">
                  <teiHeader>
                    <fileDesc>
                      <titleStmt><title>走れメロス</title></titleStmt>
                      <publicationStmt><idno type="aozora-work-id">001567</idno></publicationStmt>
                      <sourceDesc><p>source</p></sourceDesc>
                    </fileDesc>
                  </teiHeader>
                  <text><body><p>メロス</p></body></text>
                </TEI>
                XML
                python ${./tools/tei_eaj_aozora_reports.py} \
                  --compare-script ${./prototypes/tei-eaj-comparison/tei_eaj_compare.py} \
                  --tei-eaj-root ${tei-eaj-aozora-tei} \
                  --source-rev 77a675fc2771936f9544505d922d4cd45075338c \
                  --abc-melos abc/melos.xml \
                  --abc-tei 1567=abc/melos.xml \
                  all melos.md all-work.md workset.json
                grep -q "TEI-EAJ Melos files found: 2" melos.md
                grep -q "TEI-EAJ XML files scanned: 62" all-work.md
                grep -q "Compared TEI-EAJ files: 2" all-work.md
                grep -q "Missing ABC counterparts: 55" all-work.md
                grep -q "TEI-EAJ files without candidate work IDs: 5" all-work.md
                python - <<'PY'
                import json

                with open("workset.json", encoding="utf-8") as fh:
                    export = json.load(fh)

                assert export["schema_version"] == "tei-eaj-aozora-workset-export-v1"
                assert export["summary"]["tei_eaj_file_count"] == 62
                assert export["summary"]["tei_eaj_work_id_count"] == 50
                assert export["summary"]["compared_file_count"] == 2
                assert export["summary"]["missing_counterpart_count"] == 55
                assert export["summary"]["no_work_id_count"] == 5
                assert export["missing_abc_counterpart_work_ids"]
                assert "1567" in export["candidate_work_ids"]
                assert any(row["tei_eaj_file"] == "data/complete/tei_lib_lv4/1567_tei.xml"
                           and row["comparison_status"] == "compared"
                           for row in export["files"])
                PY
                mkdir -p "$out"
                cp melos.md all-work.md workset.json "$out"/
                echo "TEI-EAJ comparison reports regenerate against the pinned source." > "$out/result.txt"
              '';

          adr-acceptance-criteria = pkgs.runCommand "abc-adr-acceptance-criteria" { } ''
            cp -R ${./.} source
            chmod -R u+w source
            cd source
            bash nix/check-acceptance-criteria.sh
            mkdir -p "$out"
            echo "ADR acceptance-criteria lint passed (ratcheted)." > "$out/result.txt"
          '';
          swi-prolog-smoke =
            pkgs.runCommand "abc-swi-prolog-smoke" { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                # SWI-Prolog is the pinned CI dialect for Layer C (design spec §7.5).
                # chiasmus_verify prolog is exploratory/manual only; this pin makes
                # the dialect (negation, tabling, module syntax, CLI, repro) a build input.
                cat > "$TMPDIR/smoke.pl" <<'PL'
                :- initialization(main).
                main :- write('swipl ok'), nl, halt.
                PL
                swipl --quiet -t main -f "$TMPDIR/smoke.pl" > "$out"
              '';
          prolog-cross-artifact =
            pkgs.runCommand "abc-prolog-cross-artifact" { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                set -euo pipefail
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                # Reads COMMITTED fact files (regenerated & byte-checked by the
                # clj-nix-focused-tests derivation via the emitter). swipl only —
                # the Clojure byte-compare lives in clj-nix-focused-tests, which
                # has the offline cljDepsCache + deps.edn replacement setup. A plain
                # `clojure -e` here would fail the Nix sandbox's offline classpath.
                # Invocation: -g "(Goal -> halt(0) ; halt(1))" -t halt -s FILE...
                # halt/1 takes an integer, not a goal (the "halt(\+ Goal)" form
                # is a type error); -s (script-load) the fact + query files so the
                # predicates are visible to the goal.
                swipl -q -g "(\\+ violating_dup_identity(_) -> halt(0) ; halt(1))" -t halt \
                      -s fixtures/v0/facts/prolog/manifest_identity.pl \
                      -s docs/adr/manifest-identity-emitter-sanity.pl
                swipl -q -g "(\\+ dangling_person(_) -> halt(0) ; halt(1))" -t halt \
                      -s fixtures/v0/facts/prolog/person_records.pl \
                      -s fixtures/v0/facts/prolog/drift.pl \
                      -s docs/adr/person-id-referential-integrity.pl
                mkdir -p "$out"
                echo "Prolog cross-artifact gates hold: emitter sanity + person-id referential integrity (SWI-Prolog)." > "$out/result.txt"
              '';
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };

        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              clojure
              git
              git-cliff
              jdk21
              jq
              libxml2
            ];
          };

          validation = pkgs.mkShell {
            packages = [
              pkgs.git
              pkgs.git-cliff
              pkgs.jq
              pkgs.libxml2
            ];
          };
        }
      );
    };
}
