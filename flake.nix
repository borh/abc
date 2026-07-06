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
  };

  outputs =
    {
      self,
      nixpkgs,
      abc,
      ab-validator,
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

          parity-audit = pkgs.writeShellScript "soranoha-parity-audit" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            exec python3 scripts/monorepo-parity-audit.py "$@"
          '';

          validate-migration = pkgs.writeShellScript "soranoha-validate-migration" ''
            set -euo pipefail
            export PATH="${runtimePath}:$PATH"
            python3 scripts/monorepo-parity-audit.py
            bash scripts/monorepo-schema-drift.sh
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
          parity-audit = {
            type = "app";
            program = "${scripts.parity-audit}";
            meta.description = "Audit monorepo tracked-file parity against split repositories";
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
        in
        prefixAttrs "abc-" (optionalOutputAttrs abc "checks" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "checks" system)
        // {
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
        }
      );

      packages = forAllSystems (
        system:
        prefixAttrs "abc-" (optionalOutputAttrs abc "packages" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "packages" system)
      );

      devShells = forAllSystems (
        system:
        prefixAttrs "abc-" (optionalOutputAttrs abc "devShells" system)
        // prefixAttrs "ab-validator-" (optionalOutputAttrs ab-validator "devShells" system)
      );
    };
}
