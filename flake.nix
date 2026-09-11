{
  description = "Soranoha monorepo integration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    ab-validator = {
      url = "path:./ab-validator";
      inputs = {
        nixpkgs.follows = "nixpkgs";
        clj-nix.follows = "clj-nix";
        aozorabunko-src.follows = "aozorabunko-src";
      };
    };

    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tei-p5 = {
      url = "github:TEIC/TEI/P5_Release_4.11.0";
      flake = false;
    };

    # The upstream repository left GitHub in 2026. This is the project's own
    # mirror of its history, on the Forgejo host the runner can reach; the
    # revision is upstream's own commit and the tree hashes the same.
    aozorabunko-src = {
      url = "git+ssh://forgejo@code.hyakutake-barbel.ts.net:63333/bor/aozorabunko.git?rev=9bac324dfa6af3a5a035542440934266f76fb45d&shallow=1";
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

      inherit (nixpkgs) lib;

      pkgsFor =
        system:
        import nixpkgs {
          inherit system;
          overlays = [ (_final: prev: { jdk = prev.jdk25_headless; }) ];
        };

      # The dependency cache doubles as HOME so nothing under the real one
      # reaches the classpath; the scratch directory holds the classpath
      # cache and XDG config so both stay writable.
      cljCacheExports = cache: scratch: ''
        export HOME="${cache}"
        export JAVA_TOOL_OPTIONS="-Duser.home=${cache}"
        export CLJ_CONFIG="${cache}/.clojure"
        export GITLIBS="${cache}/.gitlibs"
        export CLJ_CACHE="${scratch}/cp-cache"
        export XDG_CONFIG_HOME="${scratch}/xdg-config"
      '';

      exportsOf =
        env: lib.concatLines (lib.mapAttrsToList (name: value: ''export ${name}="${value}"'') env);

      # Everything an output needs for one system, computed once per system
      # rather than once per output.
      perSystem =
        system:
        let
          pkgs = pkgsFor system;
          # clj-nix's dependency-cache builder, without the jdk override: the
          # cache is a fetch, and the toolchain identity below hashes it.
          cljNix = import nixpkgs {
            inherit system;
            overlays = [ clj-nix.overlays.default ];
          };
          abValidatorPackages = ab-validator.packages.${system};

          # Resolve the runtime against the offline dependency cache with user
          # configuration disabled. The same closure and deps.edn form the stage
          # toolchain identity, preventing stale traces after dependency changes.
          # Tests share tool derivations but do not use the wrapper's full environment.
          depsCache = cljNix.mk-deps-cache { lockfile = ./soranoha/deps-lock.json; };
          toolchainId =
            "clj-nix-"
            + builtins.hashString "sha256" "${pkgs.clojure}\n${depsCache}\n${builtins.hashFile "sha256" ./soranoha/deps.edn}";

          # The documents the browse layer serves, laid out exactly as they sit in
          # the repository, so one repository-relative path names a file for both
          # the served site and the link check. A developer running from the
          # `soranoha` directory reaches the same layout through the default root
          # of `..`; the wrapper points SORANOHA_SITE_DOCS at this instead,
          # because a Nix store path has no repository around it.
          siteDocs = pkgs.runCommand "soranoha-site-docs" { } ''
            mkdir -p "$out/soranoha"
            cp -R ${./docs} "$out/docs"
            cp -R ${./soranoha/docs} "$out/soranoha/docs"
            cp -R ${./soranoha/schemas} "$out/soranoha/schemas"
            mkdir -p "$out/soranoha/resources/assessment"
            cp ${./soranoha/resources/assessment/source-1.schema.json} \
              "$out/soranoha/resources/assessment/source-1.schema.json"
            cp ${./LICENSE} "$out/LICENSE"
            cp ${./LICENSE-CC0} "$out/LICENSE-CC0"
            # the research layer's identifiers: every schema under these two
            # directories, and the mapping and policy documents that carry
            # their own IRIs
            mkdir -p "$out/ab-validator/schemas" "$out/ab-validator/research/schemas" \
              "$out/ab-validator/research/data" "$out/ab-validator/data"
            cp ${ab-validator}/schemas/*.schema.json "$out/ab-validator/schemas/"
            cp ${ab-validator}/research/schemas/*.schema.json "$out/ab-validator/research/schemas/"
            cp ${ab-validator}/data/aat-to-parser-ir-mapping-v1.json \
              ${ab-validator}/data/aat-to-parser-ir-mapping-v2.json "$out/ab-validator/data/"
            cp ${ab-validator}/research/data/source-region-publication-policy-v0.json \
              "$out/ab-validator/research/data/"
          '';
        in
        {
          inherit
            system
            pkgs
            cljNix
            abValidatorPackages
            depsCache
            toolchainId
            ;
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
          profile = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./soranoha/schemas/tei-profile.odd;
          };
          # The adapter binaries and data the kernel's stages call. The mapping
          # is copied out of the ab-validator tree so its store path follows
          # its own content, not every edit to that tree.
          abToolEnv = {
            AB_AAT_TO_PARSER_IR_BIN = "${abValidatorPackages.ab-aat-to-parser-ir}/bin/ab-aat-to-parser-ir";
            AB_AOZORA_BIN = "${abValidatorPackages.ab-aozora}/bin/ab-aozora";
            AB_SOURCE_INVENTORY_BIN = "${abValidatorPackages.ab-source-inventory}/bin/ab-source-inventory";
            AB_AOZORA_SYNTAX_MATRIX = "${ab-validator}/data/aozora-syntax-coverage.toml";
            AB_AAT_TO_PARSER_IR_MAPPING_V2 = builtins.path {
              path = "${ab-validator}/data/aat-to-parser-ir-mapping-v2.json";
              name = "aat-to-parser-ir-mapping-v2.json";
            };
            SORANOHA_SITE_DOCS = "${siteDocs}";
          };
        };

      forEachSystem = f: forAllSystems (system: f (perSystem system));

      # Maven model validation shares a mutable ID cache. Resolve dependencies
      # on one worker; publication stage concurrency is independent.
      mkSoranohaApp =
        {
          pkgs,
          depsCache,
          toolchainId,
          abToolEnv,
          ...
        }:
        { name, invocation }:
        pkgs.writeShellScript name ''
          set -euo pipefail
          export PATH="${
            lib.makeBinPath [
              pkgs.bash
              pkgs.coreutils
              pkgs.git
              pkgs.clojure
            ]
          }"
          ${exportsOf abToolEnv}
          # classpath scratch must stay writable; cleaned via trap, so the
          # final clojure call must not exec-replace this shell
          scratch="$(mktemp -d)"
          trap 'rm -rf "$scratch"' EXIT
          ${cljCacheExports depsCache "$scratch"}
          # inherited launcher variables would alter the JVM or classpath
          # without changing the reported identity
          unset JAVA_CMD CLJ_JVM_OPTS JAVA_OPTS JDK_JAVA_OPTIONS _JAVA_OPTIONS
          cd "${./soranoha}"
          ${invocation} "$@" --clj-toolchain-id "${toolchainId}"
        '';

      # The bench entry points run from the source tree with the test path
      # on the classpath.
      benchInvocation =
        heap: main: args:
        "clojure -Sthreads 1 -J-Xmx${heap} -J--enable-native-access=ALL-UNNAMED -Sdeps '{:paths [\"src\" \"resources\" \"test\"]}' -M -m ${main} ${args}";

      monorepoScripts =
        pkgs:
        let
          runtimePath = lib.makeBinPath [
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
          validate = mkWrappedScript "soranoha-validate" ''
            workspace_root="$PWD"
            bash tests/monorepo-active-path-hygiene-smoke.sh
            bash tests/root-flake-output-contract-smoke.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            python scripts/monorepo-schema-hash-coherence.py
            python scripts/catalog-figures-check.py --quotes-only
            nix flake check --no-build "$@"
            (
              cd "$workspace_root/ab-validator"
              AB_WORKSPACE_ROOT="$workspace_root" nix flake check --no-build "$@"
            )
          '';
        };
    in
    {
      formatter = forEachSystem ({ pkgs, ... }: pkgs.nixfmt);

      apps = forEachSystem (
        { pkgs, profile, ... }@ctx:
        let
          scripts = monorepoScripts pkgs;
          mkScriptApp = program: description: {
            type = "app";
            program = "${program}";
            meta.description = description;
          };
          time = "${pkgs.time}/bin/time";
        in
        {
          soranoha-kernel = mkScriptApp (mkSoranohaApp ctx {
            name = "soranoha-kernel";
            invocation = "clojure -Sthreads 1 -M:soranoha/build";
          }) "Soranoha kernel CLI (build/delta/verify) with content-derived Clojure toolchain identity";
          soranoha-replay = mkScriptApp (mkSoranohaApp ctx {
            name = "soranoha-replay";
            invocation = benchInvocation "4g" "soranoha.bench.replay" "--assets-root ${./soranoha}";
          }) "Replay source revisions through the production build and delta oracle";
          soranoha-publication-replay = mkScriptApp (mkSoranohaApp ctx {
            name = "soranoha-publication-replay";
            invocation =
              "${time} --format 'replay_elapsed_seconds=%e replay_peak_rss_kib=%M' "
              +
                benchInvocation "4g" "soranoha.bench.publication"
                  "--time-bin ${time} --assets-root ${./soranoha}";
          }) "Simulate complete publication with recorded observations and isolated fixture keys";
          soranoha-compare-serving = mkScriptApp (mkSoranohaApp ctx {
            name = "soranoha-compare-serving";
            invocation = benchInvocation "512m" "soranoha.bench.serving" "--time-bin ${time}";
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

      checks = forEachSystem (
        {
          pkgs,
          cljNix,
          abValidatorPackages,
          depsCache,
          tei,
          profile,
          abToolEnv,
          ...
        }:
        let
          # A check that runs a script from the repository root. bash,
          # coreutils and python are on every check's path; `extraTools`
          # adds what one script needs beyond them.
          mkMonorepoCheck =
            name: extraTools: script:
            pkgs.runCommand name
              {
                nativeBuildInputs = [
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.python3
                ]
                ++ extraTools;
                src = self;
              }
              ''
                cd "$src"
                ${script}
                touch "$out"
              '';
          teiEaj = ab-validator.inputs.tei-eaj-aozora-tei;
        in
        {
          # Evaluates the ceremony image without building it. nix flake check
          # evaluates a NixOS configuration's toplevel, which never reaches the
          # image derivation, so an option conflict there would otherwise show
          # up only when someone builds the image. The context is discarded so
          # this depends on the evaluation alone, not on the 1 GiB build.
          monorepo-ceremony-image =
            pkgs.runCommand "monorepo-ceremony-image"
              {
                image = builtins.unsafeDiscardStringContext self.nixosConfigurations.snh-ceremony.config.system.build.isoImage.drvPath;
              }
              ''
                echo "$image" > "$out"
              '';
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
              cache = cljNix.mk-deps-cache {
                lockfile = ./soranoha/dev/typecheck/deps-lock.json;
              };
            in
            pkgs.runCommand "soranoha-typecheck" { nativeBuildInputs = [ pkgs.clojure ]; } ''
              cp -R ${./soranoha/dev/typecheck} work
              chmod -R u+w work
              mkdir -p work/src/soranoha/ori
              cp ${./soranoha/src/soranoha/ori/publication_whitespace.clj} work/src/soranoha/ori/publication_whitespace.clj
              cd work
              ${cljCacheExports cache "$TMPDIR"}
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

                ${exportsOf abToolEnv}
                ${cljCacheExports depsCache "$TMPDIR"}

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
            mkMonorepoCheck "soranoha-monorepo-tei-version-coherence" [ pkgs.gnugrep ]
              ''
                AB_TEI_P5_ROOT="${tei.reference}" bash scripts/monorepo-tei-version-coherence.sh "$src"
              '';
          monorepo-flake-input-policy = mkMonorepoCheck "soranoha-monorepo-flake-input-policy" [ ] ''
            python scripts/monorepo-flake-input-policy.py "$src"
          '';
          monorepo-schema-hash-coherence = mkMonorepoCheck "soranoha-monorepo-schema-hash-coherence" [ ] ''
            python scripts/monorepo-schema-hash-coherence.py "$src"
          '';
          monorepo-figure-quotes = mkMonorepoCheck "soranoha-monorepo-figure-quotes" [ ] ''
            python scripts/catalog-figures-check.py --quotes-only
          '';
          monorepo-runtime-config = mkMonorepoCheck "soranoha-monorepo-runtime-config" [ ] ''
            bash tests/runtime-config-smoke.sh
          '';
          monorepo-active-path-hygiene =
            mkMonorepoCheck "soranoha-monorepo-active-path-hygiene"
              [
                pkgs.findutils
                pkgs.ripgrep
              ]
              ''
                bash tests/monorepo-active-path-hygiene-smoke.sh
              '';
          monorepo-workflow-run-lib = mkMonorepoCheck "soranoha-monorepo-workflow-run-lib" [ ] ''
            bash tests/workflow-run-lib-smoke.sh
          '';
          monorepo-aat-run-set = mkMonorepoCheck "soranoha-monorepo-aat-run-set" [ ] ''
            bash tests/aat-run-set-smoke.sh
          '';
          monorepo-fidelity-lock-idempotency =
            mkMonorepoCheck "soranoha-monorepo-fidelity-lock-idempotency" [ ]
              ''
                bash tests/fidelity-lock-idempotency-smoke.sh
              '';
          monorepo-batch-run-staleness = mkMonorepoCheck "soranoha-monorepo-batch-run-staleness" [ ] ''
            bash ab-validator/tests/batch-run-staleness-smoke.sh
          '';
          monorepo-aat-materialization-workflow =
            mkMonorepoCheck "soranoha-monorepo-aat-materialization-workflow" [ ]
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
            mkMonorepoCheck "soranoha-tei-eaj-aozora-alignment-probe-generation" [ pkgs.gnugrep ]
              ''
                work="$TMPDIR/tei-eaj-alignment-probe"
                mkdir -p "$work/abc"
                cd "$work"

                cp "${teiEaj}/data/complete/tei_lib_lv4/1567_tei.xml" abc/melos.xml
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

                export ABC_TEI_EAJ_ALIGNMENT_PROBE_BIN="${abValidatorPackages.ab-aat-to-parser-ir}/bin/ab-aat-to-parser-ir"
                python "$src/ab-validator/research/tools/tei_eaj_aozora_reports.py" \
                  --compare-script "$src/ab-validator/research/tools/tei_eaj_compare.py" \
                  --tei-eaj-root "${teiEaj}" \
                  --source-rev ${teiEaj.rev} \
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

      packages = forEachSystem (
        { tei, profile, ... }:
        {
          tei-p5-reference = tei.reference;
          # The compiled profile, exposed so the checked-in artifacts can be
          # regenerated. `apps` and `checks` import the same derivation, but
          # only to consume it; without a package output there was no way to
          # build the files that ori/validate.clj holds the ODD's hashes
          # against, and editing the ODD failed 22 tests with no stated remedy.
          # `just regenerate-tei-profile` copies this output into place.
          tei-profile-artifacts = profile.artifacts;
        }
      );

      devShells = forEachSystem (
        { system, pkgs, ... }:
        {
          default = pkgs.mkShell {
            inputsFrom = [ ab-validator.devShells.${system}.default ];
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
              pkgs.python3
            ];
            shellHook = ''
              if [ -f scripts/soranoha-runtime-env.sh ]; then
                source scripts/soranoha-runtime-env.sh
              fi
            '';
          };
        }
      );

      # The offline key ceremony's boot image, described in
      # docs/key-ceremony.md. The module is what an estate configuration
      # imports; the configuration builds the image from this repository
      # alone, so the image the inventory records can be rebuilt and compared.
      nixosModules.snh-ceremony-iso = ./nix/snh-ceremony-iso.nix;

      nixosConfigurations.snh-ceremony = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ self.nixosModules.snh-ceremony-iso ];
      };
    };
}
