{
  description = "Soranoha monorepo integration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    abc = {
      url = "path:./abc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.clj-nix.follows = "clj-nix";
      inputs.aozorabunko-src.follows = "aozorabunko-src";
    };

    ab-validator = {
      url = "path:./ab-validator";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.clj-nix.follows = "clj-nix";
      inputs.abc.follows = "abc";
      inputs.aozorabunko-src.follows = "aozorabunko-src";
    };

    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tei-p5 = {
      url = "github:TEIC/TEI/P5_Release_4.11.0";
      flake = false;
    };

    aozorabunko-src = {
      url = "github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      abc,
      ab-validator,
      clj-nix,
      tei-p5,
      aozorabunko-src,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      # Root and abc deliberately target Linux only (x86_64 + aarch64) via a
      # hand-rolled genAttrs, while ab-validator uses flake-utils.eachDefaultSystem
      # for its Rust builds (which include darwin). The root wraps only ab-validator's
      # Linux outputs. This split is intentional; do not unify without widening the
      # supported-system contract.
      forAllSystems = nixpkgs.lib.genAttrs systems;

      lib = nixpkgs.lib;

      pkgsFor = system: import nixpkgs { inherit system; };

      optionalOutputAttrs =
        flake: outputName: system:
        lib.attrByPath [ outputName system ] { } flake;

      monorepoScripts =
        pkgs:
        let
          runtimePath = nixpkgs.lib.makeBinPath [
            pkgs.bash
            pkgs.coreutils
            pkgs.git
            pkgs.nix
            pkgs.python3
          ];

          mkWrappedScript =
            name: body:
            pkgs.writeShellScript name ''
              set -euo pipefail
              export PATH="${runtimePath}:$PATH"
              ${body}
            '';
        in
        {
          schema-drift = mkWrappedScript "soranoha-schema-drift" ''exec bash scripts/monorepo-schema-drift.sh "$@"'';
          tei-version-coherence = mkWrappedScript "soranoha-tei-version-coherence" ''exec bash scripts/monorepo-tei-version-coherence.sh "$@"'';
          flake-input-policy = mkWrappedScript "soranoha-flake-input-policy" ''exec python scripts/monorepo-flake-input-policy.py "$@"'';
          # Kept explicit (not via mkWrappedScript): its multi-line body, when
          # spliced through the helper's ''-string, re-dedents to a different
          # script text and changes the derivation hash. Explicit form preserves it.
          validate-migration = pkgs.writeShellScript "soranoha-validate-migration" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            workspace_root="$PWD"
            bash tests/monorepo-active-path-hygiene-smoke.sh
            bash tests/root-flake-output-contract-smoke.sh
            bash scripts/monorepo-schema-drift.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            nix flake check --no-build "$@"
            (cd "$workspace_root/abc" && nix flake check --no-build "$@")
            (
              cd "$workspace_root/ab-validator"
              AB_WORKSPACE_ROOT="$workspace_root" nix flake check --no-build "$@"
            )
          '';
        };
    in
    {
      formatter = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
        in
        pkgs.nixfmt
      );

      apps = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          scripts = monorepoScripts pkgs;
          abcApps = optionalOutputAttrs abc "apps" system;
          abValidatorPackages = optionalOutputAttrs ab-validator "packages" system;
          mkScriptApp = program: description: {
            type = "app";
            program = "${program}";
            meta.description = description;
          };
          # build-publication materializes real TEI by shelling out to validator-
          # owned adapters. Inject those private dependencies (and the shell tools
          # the aozora2html wrapper needs) without re-exporting them from the root,
          # so `nix run .#soranoha` is hermetic and never falls back to a stub.
          mkAdapterAwareSoranohaApp =
            soranohaApp:
            pkgs.writeShellScript "soranoha-with-adapters" ''
              export PATH="${
                pkgs.lib.makeBinPath [
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.gnugrep
                  pkgs.perl
                  pkgs.glibc.bin
                  pkgs._7zz
                ]
              }:''${PATH:-}"
              # 7zz recovers work ZIPs java.util.zip can't parse (damaged central
              # directories); build-publication falls back to it per work.
              export AB_SEVENZIP_BIN="${pkgs._7zz}/bin/7zz"
              export AB_AOZORA2HTML_ADAPTER="${ab-validator}/adapters/aozora2html/aozora2html-adapter"
              export AB_AOZORA2HTML_BIN="${abValidatorPackages."upstream-parser-aozora2html"}/bin/aozora2html"
              export AB_AOZORA2HTML_MAPPER_BIN="${
                abValidatorPackages."aozora2html-adapter"
              }/bin/aozora2html-adapter"
              export AB_AAT_TO_PARSER_IR_BIN="${
                abValidatorPackages."ab-aat-to-parser-ir"
              }/bin/ab-aat-to-parser-ir"
              export AB_AAT_TO_PARSER_IR_MAPPING="${ab-validator}/data/aat-to-parser-ir-mapping-v1.json"
              exec ${soranohaApp.program} "$@"
            '';
        in
        (
          if builtins.hasAttr "soranoha" abcApps then
            {
              soranoha = mkScriptApp (mkAdapterAwareSoranohaApp abcApps.soranoha) (
                abcApps.soranoha.meta.description or "Soranoha snapshot publication command dispatcher"
              );
            }
          else
            { }
        )
        // {
          schema-drift = mkScriptApp scripts.schema-drift "Check monorepo ABC schema contract drift";
          tei-version-coherence = mkScriptApp scripts.tei-version-coherence "Check TEI P5 source/profile version coherence";
          flake-input-policy = mkScriptApp scripts.flake-input-policy "Check release-critical flake inputs are explicitly pinned";
          validate-migration = mkScriptApp scripts.validate-migration "Run Soranoha monorepo migration validation gates";
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          cljPkgs = import nixpkgs {
            inherit system;
            overlays = [ clj-nix.overlays.default ];
          };
          cljDepsCache = cljPkgs.mk-deps-cache {
            lockfile = ./abc/deps-lock.json;
          };
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
          abcApps = optionalOutputAttrs abc "apps" system;
          abValidatorPackages = optionalOutputAttrs ab-validator "packages" system;
          mkMonorepoCheck =
            name: nativeBuildInputs: script:
            pkgs.runCommand name
              {
                inherit nativeBuildInputs;
                src = self;
              }
              ''
                cd "$src"
                ${script}
                touch "$out"
              '';
        in
        {
          monorepo-adr-governance =
            cljPkgs.runCommand "soranoha-monorepo-adr-governance"
              {
                nativeBuildInputs = [
                  cljPkgs.clojure
                  cljPkgs.git
                  cljPkgs.jq
                ];
                src = self;
              }
              ''
                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export GITLIBS="$HOME/.gitlibs"
                cp -R "$src" "$TMPDIR/workspace"
                chmod -R u+w "$TMPDIR/workspace"
                git -C "$TMPDIR/workspace" init --quiet
                git -C "$TMPDIR/workspace" add --all
                git -C "$TMPDIR/workspace" \
                  -c user.name=adr-governance \
                  -c user.email=adr-governance.invalid \
                  commit --quiet --message="materialize root self snapshot"
                mkdir -p "$out"
                cd "$TMPDIR/workspace/abc"
                clojure -M:abc/adr-governance \
                  --repo-root "$TMPDIR/workspace/abc" \
                  --workspace-root "$TMPDIR/workspace" \
                  --mode enforce \
                  --report "$out/report.json"
                # Migration audit debt is pinned by stable finding identity,
                # including claim/artifact/input coordinates. Diagnostic wording
                # and changing hash values are not migration identities.
                ${cljPkgs.jq}/bin/jq -S \
                  -f "$src/abc/nix/adr-problem-identities.jq" \
                  "$src/abc/docs/reports/adr-evidence-migration.json" \
                  > expected-problem-identities.json
                ${cljPkgs.jq}/bin/jq -S \
                  -f "$src/abc/nix/adr-problem-identities.jq" \
                  "$out/report.json" \
                  > actual-problem-identities.json
                if ! cmp expected-problem-identities.json actual-problem-identities.json; then
                  echo "monorepo ADR governance audit debt differs from the pinned migration phase" >&2
                  exit 1
                fi
              '';
          monorepo-tei-p5-reference = tei.reference;
          monorepo-tei-version-coherence =
            mkMonorepoCheck "soranoha-monorepo-tei-version-coherence"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.gnugrep
              ]
              ''
                AB_TEI_P5_ROOT="${tei.reference}" bash scripts/monorepo-tei-version-coherence.sh "$src"
              '';
          monorepo-flake-input-policy =
            mkMonorepoCheck "soranoha-monorepo-flake-input-policy"
              [
                pkgs.python3
              ]
              ''
                python scripts/monorepo-flake-input-policy.py "$src"
              '';
          monorepo-schema-drift =
            mkMonorepoCheck "soranoha-monorepo-schema-drift"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash scripts/monorepo-schema-drift.sh
              '';
          monorepo-runtime-config =
            mkMonorepoCheck "soranoha-monorepo-runtime-config"
              [
                pkgs.bash
                pkgs.coreutils
              ]
              ''
                bash tests/runtime-config-smoke.sh
              '';
          monorepo-active-path-hygiene =
            mkMonorepoCheck "soranoha-monorepo-active-path-hygiene"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.findutils
                pkgs.ripgrep
              ]
              ''
                bash tests/monorepo-active-path-hygiene-smoke.sh
              '';
          monorepo-workflow-run-lib =
            mkMonorepoCheck "soranoha-monorepo-workflow-run-lib"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash tests/workflow-run-lib-smoke.sh
              '';
          monorepo-aat-run-set =
            mkMonorepoCheck "soranoha-monorepo-aat-run-set"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash tests/aat-run-set-smoke.sh
              '';
          monorepo-fidelity-lock-idempotency =
            mkMonorepoCheck "soranoha-monorepo-fidelity-lock-idempotency"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash tests/fidelity-lock-idempotency-smoke.sh
              '';
          monorepo-batch-run-staleness =
            mkMonorepoCheck "soranoha-monorepo-batch-run-staleness"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash ab-validator/tests/batch-run-staleness-smoke.sh
              '';
          monorepo-aat-materialization-workflow =
            mkMonorepoCheck "soranoha-monorepo-aat-materialization-workflow"
              [
                pkgs.bash
                pkgs.coreutils
                pkgs.python3
              ]
              ''
                bash tests/aat-materialization-workflow-smoke.sh
                bash tests/aat-diagnostic-run-set-smoke.sh
              '';
          monorepo-python-quality =
            mkMonorepoCheck "soranoha-monorepo-python-quality"
              [
                pkgs.findutils
                pkgs.git
                pkgs.gnused
                pkgs.mypy
                pkgs.python3
                pkgs.ruff
              ]
              ''
                export RUFF_CACHE_DIR="$TMPDIR/ruff-cache"
                export MYPY_CACHE_DIR="$TMPDIR/mypy-cache"
                bash scripts/python-quality.sh
              '';
          monorepo-nix-format =
            mkMonorepoCheck "soranoha-monorepo-nix-format"
              [
                pkgs.findutils
                pkgs.nixfmt
              ]
              ''
                find . \
                  -path './.git' -prune -o \
                  -path './.direnv' -prune -o \
                  -path './result*' -prune -o \
                  -name '*.nix' -print0 \
                  | xargs -0 nixfmt --check
              '';
          tei-eaj-aozora-alignment-probe-generation =
            mkMonorepoCheck "soranoha-tei-eaj-aozora-alignment-probe-generation"
              [
                pkgs.coreutils
                pkgs.gnugrep
                pkgs.python3
              ]
              ''
                work="$TMPDIR/tei-eaj-alignment-probe"
                mkdir -p "$work/abc"
                cd "$work"

                tei_root="$(${abcApps."tei-eaj-aozora-tei-source".program})"
                cp "$tei_root/data/complete/tei_lib_lv4/1567_tei.xml" abc/melos.xml
                chmod u+w abc/melos.xml
                python - <<'PY'
                from pathlib import Path

                path = Path("abc/melos.xml")
                text = path.read_text(encoding="utf-8")
                text = text.replace(
                    "</body>",
                    "<p>（古伝説と、シルレルの詩から。）</p></body>",
                )
                path.write_text(text, encoding="utf-8")
                PY

                export ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN="${
                  abValidatorPackages."ab-aat-to-parser-ir"
                }/bin/ab-aat-to-parser-ir"
                python "$src/abc/tools/tei_eaj_aozora_reports.py" \
                  --compare-script "$src/abc/tools/tei_eaj_compare.py" \
                  --tei-eaj-root "$tei_root" \
                  --source-rev 77a675fc2771936f9544505d922d4cd45075338c \
                  --abc-melos abc/melos.xml \
                  --abc-tei 1567=abc/melos.xml \
                  all-with-probes \
                  --max-probe-rows 4 \
                  melos.md all-work.md workset.json alignment-probe.json alignment-probe.md

                grep -q "tail_addition" alignment-probe.md
                python - <<'PY'
                import json

                with open("alignment-probe.json", encoding="utf-8") as fh:
                    probe_report = json.load(fh)
                with open("workset.json", encoding="utf-8") as fh:
                    workset = json.load(fh)

                assert probe_report["schema_version"] == "tei-eaj-alignment-probe-report-v1"
                assert probe_report["rows"]
                attached = [
                    row
                    for row in workset["files"]
                    if row.get("alignment_probe")
                ]
                assert attached
                assert any(
                    row["alignment_probe"]["diagnosis_counts"].get("tail_addition") == 1
                    for row in attached
                )
                PY
              '';
        }
      );

      packages = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
        in
        {
          tei-p5-reference = tei.reference;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          abcShells = optionalOutputAttrs abc "devShells" system;
          abValidatorShells = optionalOutputAttrs ab-validator "devShells" system;
        in
        {
          default = pkgs.mkShell {
            AB_BOOTSTRAP_VIBRATO_DICT = "0";
            TEI_SCHEMA_PATH = abcShells.default.TEI_SCHEMA_PATH;
            inputsFrom =
              lib.optionals (builtins.hasAttr "default" abcShells) [ abcShells.default ]
              ++ lib.optionals (builtins.hasAttr "default" abValidatorShells) [
                abValidatorShells.default
              ];
            packages = [
              pkgs.cljfmt
              pkgs.clj-kondo
              pkgs.git
              pkgs.just
              pkgs.jq
              pkgs.mypy
              pkgs.nixfmt
              pkgs.ruff
            ];
            shellHook = ''
              # sccache creates a Unix startup-notification socket beneath TMPDIR.
              # NIMAS session TMPDIR paths can exceed the socket-path limit.
              export TMPDIR=/tmp
              export TMP="$TMPDIR"
              export TEMPDIR="$TMPDIR"
              if [ -f scripts/soranoha-runtime-env.sh ]; then
                source scripts/soranoha-runtime-env.sh
              fi
            '';
          };
        }
      );
    };
}
