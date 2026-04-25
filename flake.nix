{
  description = "Development and build environment for the ab-validator Rust workspace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    reference-aozora2-src = {
      url = "github:takahashim/aozora2";
      flake = false;
    };

    reference-aozora-rs-src = {
      url = "github:kinoko0518/aozora-rs";
      flake = false;
    };

    reference-aozora-parser-js-src = {
      url = "github:cognitom/aozora-parser.js";
      flake = false;
    };

    reference-aozorabunko-extractor-src = {
      url = "github:globis-org/aozorabunko-extractor";
      flake = false;
    };

    reference-aozora-epub3-src = {
      url = "github:AozoraEpub3-JDK21/AozoraEpub3-JDK21";
      flake = false;
    };
  };

  outputs =
    {
      self,
      nixpkgs,
      reference-aozora-epub3-src,
      reference-aozora-parser-js-src,
      reference-aozora-rs-src,
      reference-aozora2-src,
      reference-aozorabunko-extractor-src,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [ (import rust-overlay) ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        lib = pkgs.lib;

        rustToolchain = pkgs.rust-bin.stable.latest.default.override {
          extensions = [
            "rust-src"
            "rustfmt"
            "clippy"
            "rust-analyzer"
          ];
        };

        rustPlatform = pkgs.makeRustPlatform {
          cargo = rustToolchain;
          rustc = rustToolchain;
        };

        hasCargoManifest = builtins.pathExists ./Cargo.toml;
        hasCargoLock = builtins.pathExists ./Cargo.lock;

        cleanProjectSource =
          src:
          lib.cleanSourceWith {
            inherit src;
            filter =
              path: type:
              let
                baseName = baseNameOf path;
              in
              !(lib.hasInfix "/target/" path)
              && !(lib.hasInfix "/.direnv/" path)
              && !(lib.hasInfix "/node_modules/" path)
              && !(lib.hasInfix "/build/" path)
              && baseName != "result"
              && baseName != ".git";
          };

        source = cleanProjectSource ./.;

        buildRustReference =
          {
            name,
            src,
            lockFile,
            cargoBuildFlags ? [ "--workspace" ],
            cargoTestFlags ? [ "--workspace" ],
            doCheck ? true,
          }:
          rustPlatform.buildRustPackage {
            pname = name;
            version = "0.1.0";

            src = cleanProjectSource src;
            cargoLock.lockFile = lockFile;

            inherit
              cargoBuildFlags
              cargoTestFlags
              doCheck
              ;
          };

        referenceAozora2 = buildRustReference {
          name = "reference-aozora2";
          src = reference-aozora2-src;
          lockFile = reference-aozora2-src + "/Cargo.lock";
        };

        referenceAozoraRs = buildRustReference {
          name = "reference-aozora-rs";
          src = reference-aozora-rs-src;
          lockFile = reference-aozora-rs-src + "/Cargo.lock";
          cargoBuildFlags = [
            "--package"
            "aozora-rs-core"
            "--package"
            "aozora-rs-xhtml"
            "--package"
            "aozora-rs-zip"
          ];
          cargoTestFlags = [
            "--package"
            "aozora-rs-core"
            "--package"
            "aozora-rs-xhtml"
            "--package"
            "aozora-rs-zip"
          ];
          doCheck = false;
        };

        referenceAozoraParserJs = pkgs.stdenvNoCC.mkDerivation {
          pname = "reference-aozora-parser-js";
          version = "0.0.0";

          src = cleanProjectSource reference-aozora-parser-js-src;

          installPhase = ''
            runHook preInstall
            mkdir -p "$out/lib/aozora-parser.js"
            cp package.json README.md aozora-parser.pegjs gulpfile.js "$out/lib/aozora-parser.js/"
            for path in dist test; do
              if [ -e "$path" ]; then
                cp -R "$path" "$out/lib/aozora-parser.js/"
              fi
            done
            runHook postInstall
          '';

          passthru.buildNote = "This package installs the vendored JS parser source. Add package-lock.json to enable a reproducible npm build/test derivation.";
        };

        rubyWithExtractorGems = pkgs.ruby.withPackages (gems: [
          gems.rubyzip
          gems."ruby-progressbar"
        ]);

        referenceAozorabunkoExtractor =
          pkgs.runCommand "reference-aozorabunko-extractor"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
              src = cleanProjectSource reference-aozorabunko-extractor-src;
              rubyPath = lib.makeBinPath [ rubyWithExtractorGems ];
            }
            ''
              mkdir -p "$out/bin" "$out/lib/aozorabunko-extractor"
              cp -R "$src"/. "$out/lib/aozorabunko-extractor/"

              for script in clean_text_in_jsonl deduplicate_books extract_chats save_as_jsonl; do
                makeWrapper "$out/lib/aozorabunko-extractor/$script.rb" "$out/bin/$script" \
                  --prefix PATH : "$rubyPath"
              done
            '';

        referenceAozoraEpub3 = pkgs.writeShellApplication {
          name = "reference-aozora-epub3";
          runtimeInputs = [
            pkgs.jdk21
            pkgs.gradle
          ];
          text = ''
            cat >&2 <<'EOF'
            AozoraEpub3-JDK21 does not include Gradle dependency locks
            or vendored Maven artifacts, so a pure Nix package cannot build it yet.

            From a development shell, build it with:

              cd references/parsers/AozoraEpub3-JDK21
              gradle build

            To make this output fully reproducible, add Gradle dependency locking or
            generated Nix dependency metadata for the Maven/plugin graph.
            EOF
            exit 1
          '';
        };

        referenceParsers = pkgs.symlinkJoin {
          name = "reference-parsers";
          paths = [
            referenceAozora2
            referenceAozoraRs
            referenceAozoraParserJs
            referenceAozorabunkoExtractor
          ];
        };

        nonRustReferenceMetadata = pkgs.runCommand "reference-parser-metadata-check" { } ''
          test -f ${reference-aozora-parser-js-src}/package.json
          test -f ${reference-aozora-epub3-src}/build.gradle
          test -f ${reference-aozorabunko-extractor-src}/Gemfile.lock
          touch "$out"
        '';

        referenceParserShell = pkgs.mkShell {
          packages = devTools ++ [
            rubyWithExtractorGems
            pkgs.bundler
          ];
        };

        abValidator =
          if hasCargoManifest && hasCargoLock then
            rustPlatform.buildRustPackage {
              pname = "ab-validator";
              version = "0.1.0";

              src = source;
              cargoLock.lockFile = ./Cargo.lock;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.python3
              ];

              buildInputs =
                [ ]
                ++ lib.optionals pkgs.stdenv.isDarwin [
                  pkgs.libiconv
                  pkgs.darwin.apple_sdk.frameworks.Security
                  pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
                ];

              doCheck = true;
            }
          else
            pkgs.writeShellApplication {
              name = "ab-validator";
              text = ''
                cat >&2 <<'EOF'
                The ab-validator Rust workspace has not been scaffolded yet.
                Create Cargo.toml and Cargo.lock, then run:

                  nix build .#ab-validator
                  nix develop
                EOF
                exit 1
              '';
            };

        workspaceCheck =
          if hasCargoManifest && hasCargoLock then
            rustPlatform.buildRustPackage {
              pname = "ab-validator-check";
              version = "0.1.0";

              src = source;
              cargoLock.lockFile = ./Cargo.lock;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.python3
              ];

              buildInputs =
                [ ]
                ++ lib.optionals pkgs.stdenv.isDarwin [
                  pkgs.libiconv
                  pkgs.darwin.apple_sdk.frameworks.Security
                  pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
                ];

              cargoBuildFlags = [ "--workspace" ];
              cargoTestFlags = [ "--workspace" ];
              doCheck = true;
            }
          else
            pkgs.runCommand "ab-validator-workspace-not-yet-scaffolded" { } ''
              touch "$out"
            '';

        devTools = [
          rustToolchain
          pkgs.cargo-nextest
          pkgs.cargo-watch
          pkgs.criterion
          pkgs.just
          pkgs.pkg-config
          pkgs.openssl
          pkgs.ripgrep
          pkgs.fd
          pkgs.jq
          pkgs.hyperfine
          pkgs.python3
          pkgs.nodejs_22
          pkgs.jdk21
          pkgs.gradle
        ];
      in
      {
        packages = {
          default = abValidator;
          ab-validator = abValidator;
          reference-aozora2 = referenceAozora2;
          reference-aozora-rs = referenceAozoraRs;
          reference-aozora-parser-js = referenceAozoraParserJs;
          reference-aozorabunko-extractor = referenceAozorabunkoExtractor;
          reference-aozora-epub3 = referenceAozoraEpub3;
          reference-parsers = referenceParsers;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = abValidator;
        };

        checks = {
          default = workspaceCheck;
          ab-validator = workspaceCheck;
          reference-aozora2 = referenceAozora2;
          reference-aozora-rs = referenceAozoraRs;
          reference-parser-metadata = nonRustReferenceMetadata;
        };

        devShells = {
          default = pkgs.mkShell {
            packages = devTools;

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";

            shellHook = ''
              export CARGO_HOME="''${CARGO_HOME:-$PWD/.cargo}"
              export RUST_BACKTRACE="1"
            '';
          };

          reference-parsers = referenceParserShell;
        };

        formatter = pkgs.nixfmt;
      }
    );
}
