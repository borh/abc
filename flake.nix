{
  description = "Soranoha monorepo integration flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    abc = {
      url = "path:./abc";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    ab-validator = {
      url = "path:./ab-validator";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    tei-p5 = {
      url = "github:TEIC/TEI/P5_Release_4.11.0";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      abc,
      ab-validator,
      tei-p5,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;

      prefixAttrs =
        prefix: attrs:
        builtins.listToAttrs (
          map (name: {
            name = "${prefix}${name}";
            value = attrs.${name};
          }) (builtins.attrNames attrs)
        );

      optionalOutputAttrs =
        flake: outputName: system:
        if builtins.hasAttr outputName flake && builtins.hasAttr system flake.${outputName} then
          flake.${outputName}.${system}
        else
          { };

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
        in
        {
          schema-drift = pkgs.writeShellScript "soranoha-schema-drift" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec bash scripts/monorepo-schema-drift.sh "$@"
          '';

          split-import-parity-audit = pkgs.writeShellScript "soranoha-split-import-parity-audit" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec python scripts/monorepo-parity-audit.py "$@"
          '';

          tei-version-coherence = pkgs.writeShellScript "soranoha-tei-version-coherence" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec bash scripts/monorepo-tei-version-coherence.sh "$@"
          '';

          flake-input-policy = pkgs.writeShellScript "soranoha-flake-input-policy" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec python scripts/monorepo-flake-input-policy.py "$@"
          '';

          python-quality = pkgs.writeShellScript "soranoha-python-quality" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec bash scripts/python-quality.sh "$@"
          '';

          validate-migration = pkgs.writeShellScript "soranoha-validate-migration" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            bash scripts/monorepo-schema-drift.sh
            bash scripts/monorepo-tei-version-coherence.sh
            python scripts/monorepo-flake-input-policy.py
            nix flake check --no-build "$@"
          '';
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
          pkgs = import nixpkgs { inherit system; };
          scripts = monorepoScripts pkgs;
        in
        prefixAttrs "abc-" (optionalOutputAttrs abc "apps" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "apps" system)
        // {
          schema-drift = {
            type = "app";
            program = "${scripts.schema-drift}";
            meta.description = "Check monorepo ABC schema contract drift";
          };
          split-import-parity-audit = {
            type = "app";
            program = "${scripts.split-import-parity-audit}";
            meta.description = "Audit monorepo tracked-file parity against split repositories";
          };
          tei-version-coherence = {
            type = "app";
            program = "${scripts.tei-version-coherence}";
            meta.description = "Check TEI P5 source/profile version coherence";
          };
          flake-input-policy = {
            type = "app";
            program = "${scripts.flake-input-policy}";
            meta.description = "Check release-critical flake inputs are explicitly pinned";
          };
          python-quality = {
            type = "app";
            program = "${scripts.python-quality}";
            meta.description = "Run monorepo Python ruff and mypy checks";
          };
          validate-migration = {
            type = "app";
            program = "${scripts.validate-migration}";
            meta.description = "Run Soranoha monorepo migration validation gates";
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
        in
        prefixAttrs "abc-" (optionalOutputAttrs abc "checks" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "checks" system)
        // {
          monorepo-tei-p5-reference = tei.reference;
          monorepo-tei-version-coherence =
            pkgs.runCommand "soranoha-monorepo-tei-version-coherence"
              {
                nativeBuildInputs = [
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.gnugrep
                ];
                src = self;
                teiRoot = tei.reference;
              }
              ''
                cd "$src"
                AB_TEI_P5_ROOT="$teiRoot" bash scripts/monorepo-tei-version-coherence.sh "$src"
                touch "$out"
              '';
          monorepo-flake-input-policy =
            pkgs.runCommand "soranoha-monorepo-flake-input-policy"
              {
                nativeBuildInputs = [
                  pkgs.python3
                ];
                src = self;
              }
              ''
                cd "$src"
                python scripts/monorepo-flake-input-policy.py "$src"
                touch "$out"
              '';
          monorepo-schema-drift =
            pkgs.runCommand "soranoha-monorepo-schema-drift"
              {
                nativeBuildInputs = [
                  pkgs.bash
                  pkgs.coreutils
                  pkgs.python3
                ];
                src = self;
              }
              ''
                cd "$src"
                bash scripts/monorepo-schema-drift.sh
                touch "$out"
              '';
          monorepo-runtime-config =
            pkgs.runCommand "soranoha-monorepo-runtime-config"
              {
                nativeBuildInputs = [
                  pkgs.bash
                  pkgs.coreutils
                ];
                src = self;
              }
              ''
                cd "$src"
                bash tests/runtime-config-smoke.sh
                touch "$out"
              '';
          monorepo-python-quality =
            pkgs.runCommand "soranoha-monorepo-python-quality"
              {
                nativeBuildInputs = [
                  pkgs.findutils
                  pkgs.git
                  pkgs.gnused
                  pkgs.mypy
                  pkgs.python3
                  pkgs.ruff
                ];
                src = self;
              }
              ''
                cd "$src"
                export RUFF_CACHE_DIR="$TMPDIR/ruff-cache"
                export MYPY_CACHE_DIR="$TMPDIR/mypy-cache"
                bash scripts/python-quality.sh
                touch "$out"
              '';
          monorepo-nix-format =
            pkgs.runCommand "soranoha-monorepo-nix-format"
              {
                nativeBuildInputs = [
                  pkgs.findutils
                  pkgs.nixfmt
                ];
                src = self;
              }
              ''
                cd "$src"
                find . \
                  -path './.git' -prune -o \
                  -path './.direnv' -prune -o \
                  -path './result*' -prune -o \
                  -name '*.nix' -print0 \
                  | xargs -0 nixfmt --check
                touch "$out"
              '';
        }
      );

      packages = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
        in
        prefixAttrs "abc-" (optionalOutputAttrs abc "packages" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "packages" system)
        // {
          tei-p5-reference = tei.reference;
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          abcShells = optionalOutputAttrs abc "devShells" system;
          abValidatorShells = optionalOutputAttrs ab-validator "devShells" system;
          inherit (nixpkgs) lib;
        in
        prefixAttrs "abc-" abcShells
        // prefixAttrs "ab-validator-" abValidatorShells
        // {
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
              if [ -f scripts/soranoha-runtime-env.sh ]; then
                source scripts/soranoha-runtime-env.sh
              fi
            '';
          };
        }
      );
    };
}
