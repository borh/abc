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
      tei-p5,
      aozorabunko-src,
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
            bash tests/monorepo-active-path-hygiene-smoke.sh
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
          abcApps = optionalOutputAttrs abc "apps" system;
          mkScriptApp = program: description: {
            type = "app";
            program = "${program}";
            meta.description = description;
          };
        in
        prefixAttrs "abc-" abcApps
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "apps" system)
        // (if builtins.hasAttr "soranoha" abcApps then { soranoha = abcApps.soranoha; } else { })
        // {
          schema-drift = mkScriptApp scripts.schema-drift "Check monorepo ABC schema contract drift";
          tei-version-coherence = mkScriptApp scripts.tei-version-coherence "Check TEI P5 source/profile version coherence";
          flake-input-policy = mkScriptApp scripts.flake-input-policy "Check release-critical flake inputs are explicitly pinned";
          python-quality = mkScriptApp scripts.python-quality "Run monorepo Python ruff and mypy checks";
          validate-migration = mkScriptApp scripts.validate-migration "Run Soranoha monorepo migration validation gates";
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs { inherit system; };
          tei = import ./nix/tei.nix { inherit pkgs tei-p5; };
          abValidatorChecks = optionalOutputAttrs ab-validator "checks" system;
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
        prefixAttrs "abc-" (optionalOutputAttrs abc "checks" system)
        // prefixAttrs "ab-validator-" abValidatorChecks
        // {
          parser-ir-ortho-publication-smoke = abValidatorChecks.parser-ir-ortho-publication-smoke;
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
