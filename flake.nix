{
  description = "Soranoha monorepo integration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    ab-validator = {
      url = "path:./ab-validator";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.clj-nix.follows = "clj-nix";
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

      # Soranoha targets Linux only (x86_64 and aarch64) via genAttrs,
      # while ab-validator uses flake-utils.eachDefaultSystem for Rust
      # builds (including Darwin). The root wraps only ab-validator's Linux outputs.
      forAllSystems = nixpkgs.lib.genAttrs systems;

      lib = nixpkgs.lib;

      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [ (_final: prev: { jdk = prev.jdk25_headless; }) ];
        };

      optionalOutputAttrs =
        flake: outputName: system:
        lib.attrByPath [ outputName system ] { } flake;

      # Resolve the runtime against the offline dependency cache with user
      # configuration disabled. The same closure and deps.edn form the stage
      # toolchain identity, preventing stale traces after dependency changes.
      # Tests share tool derivations but do not use the wrapper's full environment.
      soranohaCljContext =
        system:
        let
          pkgs = pkgsFor system;
          cljPkgs = import nixpkgs {
            inherit system;
            overlays = [ clj-nix.overlays.default ];
          };
          depsCache = cljPkgs.mk-deps-cache { lockfile = ./soranoha/deps-lock.json; };
        in
        {
          inherit depsCache;
          toolchainId =
            "clj-nix-"
            + builtins.hashString "sha256" "${pkgs.clojure}\n${depsCache}\n${builtins.hashFile "sha256" ./soranoha/deps.edn}";
        };

      # Maven model validation shares a mutable ID cache. Resolve dependencies
      # on one worker; publication stage concurrency is independent.
      mkSoranohaApp =
        system:
        { name, invocation }:
        let
          pkgs = pkgsFor system;
          abValidatorPackages = optionalOutputAttrs ab-validator "packages" system;
          kernelDepsCache = (soranohaCljContext system).depsCache;
          cljToolchainId = (soranohaCljContext system).toolchainId;
        in
        pkgs.writeShellScript name ''
          set -euo pipefail
          export PATH="${
            pkgs.lib.makeBinPath [
              pkgs.bash
              pkgs.coreutils
              pkgs.git
              pkgs.clojure
            ]
          }"
          export AB_AAT_TO_PARSER_IR_BIN="${
            abValidatorPackages."ab-aat-to-parser-ir"
          }/bin/ab-aat-to-parser-ir"
          export AB_AOZORA_BIN="${abValidatorPackages."ab-aozora"}/bin/ab-aozora"
          export AB_SOURCE_INVENTORY_BIN="${
            abValidatorPackages."ab-source-inventory"
          }/bin/ab-source-inventory"
          export AB_AOZORA_SYNTAX_MATRIX="${ab-validator}/data/aozora-syntax-coverage.toml"
          export AB_AAT_TO_PARSER_IR_MAPPING_V2="${
            builtins.path {
              path = "${ab-validator}/data/aat-to-parser-ir-mapping-v2.json";
              name = "aat-to-parser-ir-mapping-v2.json";
            }
          }"
          export HOME="${kernelDepsCache}"
          export JAVA_TOOL_OPTIONS="-Duser.home=${kernelDepsCache}"
          export CLJ_CONFIG="$HOME/.clojure"
          export GITLIBS="$HOME/.gitlibs"
          # inherited launcher variables would alter the JVM or classpath
          # without changing the reported identity
          unset JAVA_CMD CLJ_JVM_OPTS JAVA_OPTS JDK_JAVA_OPTIONS _JAVA_OPTIONS
          # classpath scratch must stay writable; cleaned via trap, so the
          # final clojure call must not exec-replace this shell
          scratch="$(mktemp -d)"
          trap 'rm -rf "$scratch"' EXIT
          export CLJ_CACHE="$scratch/cp-cache"
          export XDG_CONFIG_HOME="$scratch/xdg-config"
          cd "${./soranoha}"
          ${invocation} "$@" --clj-toolchain-id "${cljToolchainId}"
        '';

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
          tei-version-coherence = mkWrappedScript "soranoha-tei-version-coherence" ''exec bash scripts/monorepo-tei-version-coherence.sh "$@"'';
          flake-input-policy = mkWrappedScript "soranoha-flake-input-policy" ''exec python scripts/monorepo-flake-input-policy.py "$@"'';
          validate = pkgs.writeShellScript "soranoha-validate" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            workspace_root="$PWD"
            bash tests/monorepo-active-path-hygiene-smoke.sh
            bash tests/root-flake-output-contract-smoke.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            nix flake check --no-build "$@"
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
          profile = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./soranoha/schemas/tei-profile.odd;
          };
          mkScriptApp = program: description: {
            type = "app";
            program = "${program}";
            meta.description = description;
          };
        in
        {
          soranoha-kernel = mkScriptApp (mkSoranohaApp system {
            name = "soranoha-kernel";
            invocation = "clojure -Sthreads 1 -M:soranoha/build";
          }) "Soranoha kernel CLI (build/delta/verify) with content-derived Clojure toolchain identity";
          soranoha-replay = mkScriptApp (mkSoranohaApp system {
            name = "soranoha-replay";
            invocation = ''clojure -Sthreads 1 -J-Xmx4g -J--enable-native-access=ALL-UNNAMED -Sdeps '{:paths ["src" "resources" "test"]}' -M -m soranoha.bench.replay --assets-root ${./soranoha}'';
          }) "Replay source revisions through the production build and delta oracle";
          soranoha-publication-replay = mkScriptApp (mkSoranohaApp system {
            name = "soranoha-publication-replay";
            invocation = ''${pkgs.time}/bin/time --format 'replay_elapsed_seconds=%e replay_peak_rss_kib=%M' clojure -Sthreads 1 -J-Xmx4g -J--enable-native-access=ALL-UNNAMED -Sdeps '{:paths ["src" "resources" "test"]}' -M -m soranoha.bench.publication --time-bin ${pkgs.time}/bin/time --assets-root ${./soranoha}'';
          }) "Simulate complete publication with recorded observations and isolated fixture keys";
          soranoha-compare-serving = mkScriptApp (mkSoranohaApp system {
            name = "soranoha-compare-serving";
            invocation = ''clojure -Sthreads 1 -J-Xmx512m -J--enable-native-access=ALL-UNNAMED -Sdeps '{:paths ["src" "resources" "test"]}' -M -m soranoha.bench.serving --time-bin ${pkgs.time}/bin/time'';
          }) "Compare serving activation time and peak memory in balanced order";
          regenerate-tei-profile = mkScriptApp (pkgs.writeShellScript "regenerate-tei-profile" ''
            set -euo pipefail
            target="''${1:-$PWD/soranoha}"
            test -d "$target/schemas"
            ${pkgs.coreutils}/bin/install -m 0644 ${profile.artifacts}/tei-profile.{rng,sch} \
              ${profile.artifacts}/tei-profile-generation.json "$target/schemas/"
          '') "Regenerate the Soranoha TEI profile from its ODD";
          tei-version-coherence = mkScriptApp scripts.tei-version-coherence "Check TEI P5 source/profile version coherence";
          flake-input-policy = mkScriptApp scripts.flake-input-policy "Check release-critical flake inputs are explicitly pinned";
          validate = mkScriptApp scripts.validate "Run Soranoha validation gates";
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = pkgsFor system;
          soranohaClj = soranohaCljContext system;
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
          profile = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./soranoha/schemas/tei-profile.odd;
          };
          researchApps = optionalOutputAttrs ab-validator "apps" system;
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
          tei-profile-drift = pkgs.runCommand "soranoha-tei-profile-drift" { } ''
            diff -u ${./soranoha/schemas/tei-profile.rng} ${profile.artifacts}/tei-profile.rng
            diff -u ${./soranoha/schemas/tei-profile.sch} ${profile.artifacts}/tei-profile.sch
            diff -u \
              <(${pkgs.jq}/bin/jq -S '{odd_hash, rng_hash, schematron_hash}' ${./soranoha/schemas/tei-profile-generation.json}) \
              <(${pkgs.jq}/bin/jq -S '{odd_hash, rng_hash, schematron_hash}' ${profile.artifacts}/tei-profile-generation.json)
            touch "$out"
          '';
          soranoha-typecheck =
            let
              cljPkgs = import nixpkgs {
                inherit system;
                overlays = [ clj-nix.overlays.default ];
              };
              cache = cljPkgs.mk-deps-cache {
                lockfile = ./soranoha/dev/typecheck/deps-lock.json;
              };
            in
            pkgs.runCommand "soranoha-typecheck" { nativeBuildInputs = [ pkgs.clojure ]; } ''
              cp -R ${./soranoha/dev/typecheck} work
              chmod -R u+w work
              mkdir -p work/src/soranoha/ori
              cp ${./soranoha/src/soranoha/ori/publication_whitespace.clj} work/src/soranoha/ori/publication_whitespace.clj
              cd work
              export JAVA_TOOL_OPTIONS="-Duser.home=${cache}"
              export CLJ_CONFIG="${cache}/.clojure"
              export CLJ_CACHE="$TMPDIR/cp-cache"
              export GITLIBS="${cache}/.gitlibs"
              clojure -Sthreads 1 -M:check
              touch "$out"
            '';
          # The soranoha kernel + snh conformance suite plus its lint and
          # format gates, hermetic against the wrapper's Clojure, Git, and
          # dependency-cache derivations (the same store paths the wrapper
          # binds; not the wrapper's complete environment). git backs the
          # repository-view and publication-transaction test fixtures. The
          # maintained Clojure source is covered by the lint/format gate.
          soranoha-tests =
            pkgs.runCommand "soranoha-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git
                  pkgs.clj-kondo
                  pkgs.cljfmt
                  pkgs.cmark
                  (pkgs.python3.withPackages (python: [ python.rdflib ]))
                  # the static-serving acceptance runs the checked-in
                  # Caddyfile against an exported tree
                  pkgs.caddy
                ];
              }
              ''
                cp -R ${./soranoha} source
                chmod -R u+w source
                cd source

                find src test -name '*.clj' -print0 \
                  | xargs -0 clj-kondo --fail-level warning --lint
                find src test -name '*.clj' -print0 \
                  | xargs -0 cljfmt check

                export HOME="${soranohaClj.depsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${soranohaClj.depsCache}"
                export AB_AOZORA_BIN="${abValidatorPackages."ab-aozora"}/bin/ab-aozora"
                export AB_AAT_TO_PARSER_IR_BIN="${
                  abValidatorPackages."ab-aat-to-parser-ir"
                }/bin/ab-aat-to-parser-ir"
                export AB_AAT_TO_PARSER_IR_MAPPING_V2="${ab-validator}/data/aat-to-parser-ir-mapping-v2.json"
                export AB_SOURCE_INVENTORY_BIN="${
                  abValidatorPackages."ab-source-inventory"
                }/bin/ab-source-inventory"
                export AB_AOZORA_SYNTAX_MATRIX="${ab-validator}/data/aozora-syntax-coverage.toml"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                set -o pipefail
                clojure -Sthreads 1 -M:test 2>&1 | tee test-output.log
                if grep -q "^Reflection warning, soranoha/" test-output.log; then
                  exit 1
                fi

                mkdir -p "$out"
                echo "soranoha suite, lint, and format checks passed" > "$out/result.txt"
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
                pkgs.python3
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

                tei_root="$(${researchApps."tei-eaj-aozora-tei-source".program})"
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
                python "$src/ab-validator/research/tools/tei_eaj_aozora_reports.py" \
                  --compare-script "$src/ab-validator/research/tools/tei_eaj_compare.py" \
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
          abValidatorShells = optionalOutputAttrs ab-validator "devShells" system;
        in
        {
          default = pkgs.mkShell {
            AB_BOOTSTRAP_VIBRATO_DICT = "0";
            TEI_SCHEMA_PATH = "${./soranoha/schemas/tei-profile.rng}";
            inputsFrom = lib.optionals (builtins.hasAttr "default" abValidatorShells) [
              abValidatorShells.default
            ];
            packages = [
              pkgs.caddy
              pkgs.clojure
              pkgs.cljfmt
              pkgs.cmark
              pkgs.clj-kondo
              pkgs.git
              pkgs.just
              pkgs.jq
              pkgs.mypy
              pkgs.nixfmt
              pkgs.ruff
              (pkgs.python3.withPackages (python: [ python.rdflib ]))
            ];
            shellHook = ''
              if command -v sccache > /dev/null 2>&1; then
                # sccache creates a Unix startup-notification socket beneath TMPDIR.
                # NIMAS session TMPDIR paths can exceed the socket-path limit.
                export TMPDIR=/tmp
                export TMP="$TMPDIR"
                export TEMPDIR="$TMPDIR"
              fi
              if [ -f scripts/soranoha-runtime-env.sh ]; then
                source scripts/soranoha-runtime-env.sh
              fi
            '';
          };
        }
      );
    };
}
