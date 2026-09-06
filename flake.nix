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

      # The soranoha/ Clojure kernel CLI with the same private adapter
      # injection. The wrapper authenticates the Clojure runtime it names
      # instead of inheriting the caller's: classpath resolution runs
      # against the offline clj-nix dependency cache with HOME and the
      # Clojure/Maven/gitlibs configuration bound to store paths, so
      # user-level deps.edn merging or mutable caches cannot change what
      # executes. The toolchain identity the kernel refuses to run without
      # is derived from exactly that environment — the Clojure tool
      # closure, the dependency-cache closure, and deps.edn — so a runtime
      # or dependency change re-keys every pure-Clojure stage derivation
      # instead of silently reusing stale traces. The wrapper always
      # supplies that identity and runs the Nix-captured source; direct
      # clojure invocation remains the path for intentionally custom
      # identities.
      # One private Clojure context for the soranoha kernel — a single
      # dependency-cache derivation and derived toolchain identity. The
      # wrapper app consumes both; the soranoha-tests check consumes the
      # dependency cache and the same nixpkgs Clojure/Git pins, so both
      # build against identical tool derivations (the check does not run
      # inside the wrapper's full environment or consume the identity).
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

      mkKernelSoranohaApp =
        system:
        let
          pkgs = pkgsFor system;
          abValidatorPackages = optionalOutputAttrs ab-validator "packages" system;
          kernelDepsCache = (soranohaCljContext system).depsCache;
          cljToolchainId = (soranohaCljContext system).toolchainId;
        in
        pkgs.writeShellScript "soranoha-kernel" ''
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
          clojure -M:soranoha/build "$@" --clj-toolchain-id "${cljToolchainId}"
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
          mkScriptApp = program: description: {
            type = "app";
            program = "${program}";
            meta.description = description;
          };
        in
        (
          if builtins.hasAttr "soranoha" abcApps then
            {
              soranoha = abcApps.soranoha;
            }
          else
            { }
        )
        // {
          soranoha-kernel = mkScriptApp (mkKernelSoranohaApp system) "Soranoha kernel CLI (build/delta/verify) with content-derived Clojure toolchain identity";
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
          soranohaClj = soranohaCljContext system;
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
          abcApps = optionalOutputAttrs abc "apps" system;
          abValidatorPackages = optionalOutputAttrs ab-validator "packages" system;
          parserRqWiringPython = pkgs.python3.withPackages (pythonPackages: [
            pythonPackages.jsonschema
            pythonPackages.pytest
          ]);
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
          # The soranoha kernel + snh conformance suite plus its lint and
          # format gates, hermetic against the wrapper's Clojure, Git, and
          # dependency-cache derivations (the same store paths the wrapper
          # binds; not the wrapper's complete environment). git backs the
          # repository-view and publication-transaction test fixtures. The
          # ported tree retains its upstream formatting conventions and is
          # excluded from the kernel lint/format gate.
          soranoha-tests =
            pkgs.runCommand "soranoha-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git
                  pkgs.clj-kondo
                  pkgs.cljfmt
                  (pkgs.python3.withPackages (python: [ python.rdflib ]))
                  # the static-serving acceptance runs the checked-in
                  # Caddyfile against an exported tree
                  pkgs.caddy
                ];
              }
              ''
                cp -R ${./soranoha} source
                chmod -R u+w source
                # The canonicalization suite binds the kernel's canonicalizer
                # to abc's shared cross-language vectors at this relative path
                # (the two copies must never diverge byte-wise).
                mkdir -p abc/test/fixtures/canonicalization
                cp ${./abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json} \
                  abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json
                mkdir -p abc/schemas
                cp ${./abc/schemas/tei-profile.rng} abc/schemas/tei-profile.rng
                cp ${./abc/schemas/metadata-record.schema.json} abc/schemas/metadata-record.schema.json
                cp ${./abc/schemas/person-record.schema.json} abc/schemas/person-record.schema.json
                cd source

                find src test -name '*.clj' -not -path '*/ported/*' -print0 \
                  | xargs -0 clj-kondo --fail-level warning --lint
                find src test -name '*.clj' -not -path '*/ported/*' -print0 \
                  | xargs -0 cljfmt check

                export HOME="${soranohaClj.depsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${soranohaClj.depsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                clojure -M:test

                mkdir -p "$out"
                echo "soranoha suite, lint, and format checks passed" > "$out/result.txt"
              '';

          # One canonical strict-governance derivation. It lives here (not in
          # the abc component flake) because claim evidence paths are
          # monorepo-root-relative — abc/test/…, ab-validator/crates/…/tests/…
          # — so the validator must see both trees staged as siblings, which
          # abc's subtree-only sandbox cannot provide.
          monorepo-adr-governance =
            pkgs.runCommand "soranoha-monorepo-adr-governance"
              {
                nativeBuildInputs = [
                  cljPkgs.clojure
                  pkgs.coreutils
                ];
              }
              ''
                cp -R ${self}/abc abc
                cp -R ${self}/ab-validator ab-validator
                cp -R ${self}/soranoha soranoha
                chmod -R u+w abc ab-validator
                cd abc
                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"
                clojure -M:abc/adr-governance
                mkdir -p "$out"
                echo "ADR corpus is strictly valid." > "$out/result.txt"
              '';

          # The freshly built release binaries must match the APPROVED identity.
          # Approved hashes are resolved through parser-release-authority/authenticate
          # — the full integrity + decision-binding authority path runtime uses, not
          # a shape-only loader — and compared byte-for-byte against the actual
          # ab-validator build outputs. This lives in the monorepo flake because it
          # is the only place with both the abc authenticate boundary and the
          # standalone ab-validator package set (abc's own flake has neither an
          # ab-validator input nor a non-empty local-pkgs overlay).
          release-parser-build-matches-approved-identity =
            pkgs.runCommand "soranoha-release-parser-build-matches-approved-identity"
              {
                nativeBuildInputs = [
                  cljPkgs.clojure
                  pkgs.coreutils
                ];
              }
              ''
                cp -R ${self}/abc abc
                cp -R ${self}/soranoha soranoha
                chmod -R u+w abc
                cd abc
                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"
                read want_a want_c < <(clojure -M -e '(require (quote [abc.tools.parser-release-authority :as a]))
                  (let [r (a/authenticate {:release_parser_identity_path "data/release-parser-identity-v1.edn"
                                           :decisions_path "docs/adr/decisions.edn"})
                        h (fn [n] (:sha256 (first (filter #(= n (:name %))
                                                          (get-in r [:executable-provenance :executables])))))]
                    (println (subs (h "ab-aozora") 7) (subs (h "ab-aat-to-parser-ir") 7)))')
                a=$(sha256sum ${abValidatorPackages."ab-aozora"}/bin/ab-aozora | cut -d' ' -f1)
                c=$(sha256sum ${abValidatorPackages."ab-aat-to-parser-ir"}/bin/ab-aat-to-parser-ir | cut -d' ' -f1)
                [ "$a" = "$want_a" ] || { echo "ab-aozora $a != approved $want_a" >&2; exit 1; }
                [ "$c" = "$want_c" ] || { echo "converter $c != approved $want_c" >&2; exit 1; }
                mkdir -p "$out"
                echo "release parser build matches approved (authenticated) identity" > "$out/result.txt"
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
          parser-rq-production-wiring =
            mkMonorepoCheck "soranoha-parser-rq-production-wiring"
              [
                abValidatorPackages."parser-rq-candidate"
                parserRqWiringPython
                cljPkgs.clojure
                pkgs.bash
                pkgs.coreutils
                pkgs.systemd
              ]
              ''
                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export GITLIBS="$HOME/.gitlibs"
                export PARSER_RQ_CANDIDATE_ROOT="${abValidatorPackages."parser-rq-candidate"}"
                export PARSER_RQ_REPOSITORY_ROOT="$src"
                pytest -q abc/tools/test_parser_rq_campaign_orchestrator.py \
                  -k 'real_candidate or bounded_production_chain'
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
              pkgs.caddy
              pkgs.cljfmt
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
