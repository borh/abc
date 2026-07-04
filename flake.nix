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

    aozorabunko-src = {
      url = "github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165";
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
      aozorabunko-src,
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

        sudachiDictionaryFullZip = pkgs.fetchurl {
          url = "http://sudachi.s3-website-ap-northeast-1.amazonaws.com/sudachidict/sudachi-dictionary-20260116-full.zip";
          hash = "sha256-Kh7aWgJApC9F2vgAPZffVWXF0lK7LVjnGAe7vQgvfuo=";
        };

        sudachiDictionaryFull =
          pkgs.runCommand "sudachi-dictionary-20260116-full"
            {
              nativeBuildInputs = [ pkgs.unzip ];
            }
            ''
                runHook preInstall
                mkdir -p "$out/share/sudachi"
              unzip -j ${sudachiDictionaryFullZip} '*.dic' -d "$out/share/sudachi"
              dic="$(find "$out/share/sudachi" -maxdepth 1 -type f -name '*.dic' | head -n 1)"
              test -n "$dic"
              if [ "$dic" != "$out/share/sudachi/system_full.dic" ]; then
                mv "$dic" "$out/share/sudachi/system_full.dic"
              fi
              ln -s system_full.dic "$out/share/sudachi/system.dic"
              runHook postInstall
            '';

        aozoraRsGaijiMenkuten = pkgs.fetchurl {
          url = "https://x0213.org/codetable/jisx0213-2004-std.txt";
          hash = "sha256-OIrngiy/Cuz/CbhGbdYw6jQ2c9AaPH351hlHrRrVst0=";
        };

        aozoraRsGaijiChukiPdf = pkgs.fetchurl {
          url = "https://www.aozora.gr.jp/gaiji_chuki/gaiji_chuki.pdf";
          hash = "sha256-/eC1rOdQWy94f/PsxeTzENGD3f7ubWABN3LyLcniIec=";
        };

        cargoGitOutputHashes = {
          "sudachi-0.6.11-a1" = "sha256-nQiBcAY/NGbyw1/+3ACZ3HtGgc9Ow54+8auyT1Udo0w=";
          "vibrato-rkyv-0.7.7" = "sha256-ZPDiLrA8Losm28tgw/apjFdo07gRTVTZFPM8QLy3MPA=";
        };

        abCargoLock = {
          lockFile = ./Cargo.lock;
          outputHashes = cargoGitOutputHashes;
        };

        sudachiRustSource = pkgs.fetchgit {
          url = "https://github.com/WorksApplications/sudachi.rs.git";
          rev = "54e85e8f7e0a6c4b570cd7b103506b080dc60c92";
          hash = cargoGitOutputHashes."sudachi-0.6.11-a1";
        };

        abCargoDeps = pkgs.runCommand "cargo-vendor-dir" { } ''
          cp -Lr --reflink=auto ${rustPlatform.importCargoLock abCargoLock} "$out"
          chmod -R u+w "$out"

          # The locked Sudachi crate lives under sudachi/ in its git repo, but
          # the crate source includes repo-root resources via ../../resources.
          cp -R ${sudachiRustSource}/resources "$out/resources"
        '';

        aozora2htmlCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora2html/Cargo.lock;
        };

        aozora2htmlGem = pkgs.fetchurl {
          url = "https://rubygems.org/downloads/aozora2html-3.0.1.gem";
          hash = "sha256-TcEQby6RGtCW8GG8jDIUB55LUoDSvP3tX95BfW3OuEE=";
        };

        rubyWithAozora2htmlRuntime = pkgs.ruby.withPackages (gems: [
          gems.rubyzip
        ]);

        aozora2htmlParser = pkgs.stdenvNoCC.mkDerivation {
          pname = "aozora2html-parser";
          version = "3.0.1";

          nativeBuildInputs = [
            pkgs.makeWrapper
            rubyWithAozora2htmlRuntime
          ];

          dontUnpack = true;

          installPhase = ''
            runHook preInstall

            export HOME="$TMPDIR"
            gem install \
              --local \
              --ignore-dependencies \
              --install-dir "$out/lib/ruby/gems" \
              --bindir "$out/libexec/bin" \
              --no-document \
              ${aozora2htmlGem}

            substituteInPlace "$out/libexec/bin/aozora2html" \
              --replace-fail "#! ruby" "#! ${rubyWithAozora2htmlRuntime}/bin/ruby"

            wrapProgram "$out/libexec/bin/aozora2html" \
              --prefix PATH : "${lib.makeBinPath [ rubyWithAozora2htmlRuntime ]}" \
              --set GEM_HOME "$out/lib/ruby/gems" \
              --prefix GEM_PATH : "$out/lib/ruby/gems"

            mkdir -p "$out/bin"
            ln -s "$out/libexec/bin/aozora2html" "$out/bin/aozora2html"

            runHook postInstall
          '';
        };

        vibratoDictionaryPreCheck = ''
          if [ -z "''${AB_VIBRATO_DICT:-}" ]; then
            for dir in "${source}/dictionary/compiled" "${source}/dictionary/optimized"; do
              for candidate in \
                "$dir/unidic-cwj-202512.dic" \
                "$dir/unidic-cwj-202512.dic.zst" \
                "$dir/unidic-cwj.dic" \
                "$dir/unidic-cwj.dic.zst"
              do
                if [ -f "$candidate" ]; then
                  export AB_VIBRATO_DICT="$candidate"
                  break 2
                fi
              done
            done
          elif [ ! -f "$AB_VIBRATO_DICT" ]; then
            echo "AB_VIBRATO_DICT is set but does not point to a file: $AB_VIBRATO_DICT" >&2
            exit 1
          fi

          if [ -z "''${AB_VIBRATO_DICT:-}" ]; then
            echo "AB_VIBRATO_DICT is not set and no default dictionary was found in dictionary/{compiled,optimized}/unidic-cwj-202512.{dic,dic.zst}." >&2
            exit 1
          fi

          dict_input="$AB_VIBRATO_DICT"
          dict_name="$(basename "$dict_input")"
          if [ "''${dict_name##*.}" = "zst" ]; then
            mkdir -p "$TMPDIR/ab-validator-vibrato"
            dict_output="$TMPDIR/ab-validator-vibrato/''${dict_name%.zst}"
            if [ ! -f "$dict_output" ] || [ "$dict_input" -nt "$dict_output" ]; then
              zstd -dc "$dict_input" > "$dict_output"
            fi
            export AB_VIBRATO_DICT="$dict_output"
          fi

          export XDG_CACHE_HOME="$TMPDIR/xdg-cache"
          mkdir -p "$XDG_CACHE_HOME"
        '';

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
              cargoDeps = abCargoDeps;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.python3
                pkgs.zstd
              ];

              buildInputs = [
                pkgs.pdfium-binaries
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [
                pkgs.libiconv
                pkgs.darwin.apple_sdk.frameworks.Security
                pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
              ];

              AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
              AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
              AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
              AB_ABC_ROOT = "${source}/data/abc-schemas";

              preCheck = vibratoDictionaryPreCheck;
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
              cargoDeps = abCargoDeps;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.python3
                pkgs.zstd
              ];

              buildInputs = [
                pkgs.pdfium-binaries
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [
                pkgs.libiconv
                pkgs.darwin.apple_sdk.frameworks.Security
                pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
              ];

              AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
              AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
              AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
              AB_ABC_ROOT = "${source}/data/abc-schemas";

              cargoBuildFlags = [ "--workspace" ];
              cargoTestFlags = [
                "--workspace"
                "--features"
                "ab-morph-run/test-analyzer"
              ];
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
          pkgs.duckdb
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

        aozora2htmlTools = [
          pkgs.ruby
          pkgs.bundler
          pkgs.python3
          pkgs.python3.pkgs.lxml
          pkgs.python3.pkgs.jsonschema
          pkgs.python3.pkgs.pytest
        ];

        pythonWithAatSchemaDeps = pkgs.python3.withPackages (ps: [
          ps.jsonschema
          ps.tomli
          ps.pytest
        ]);

        aozora2htmlRustParityShell = pkgs.writeShellApplication {
          name = "aozora2html-rust-parity";
          runtimeInputs = [
            pkgs.perl
            rustToolchain
            pythonWithAatSchemaDeps
          ];
          text = ''
            repo_root="$PWD"
            if [ ! -d "$repo_root/adapters/aozora2html" ]; then
              repo_root="${source}"
            fi
            export AB_AOZORA2HTML_BIN="${aozora2htmlParser}/bin/aozora2html"
            cargo \
              --config "source.crates-io.replace-with='vendored-sources'" \
              --config "source.vendored-sources.directory='${aozora2htmlCargoDeps}'" \
              build --manifest-path "$repo_root/adapters/aozora2html/Cargo.toml" --release --offline
            python3 -m pytest "$repo_root/adapters/aozora2html/tests/test_mapper.py" -vv
          '';
        };

        aozora2htmlRustParityCheck =
          pkgs.runCommand "aozora2html-rust-parity-check"
            {
              nativeBuildInputs = [
                pkgs.perl
                rustToolchain
                pythonWithAatSchemaDeps
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              export AB_AOZORA2HTML_BIN="${aozora2htmlParser}/bin/aozora2html"
              cargo \
                --config "source.crates-io.replace-with='vendored-sources'" \
                --config "source.vendored-sources.directory='${aozora2htmlCargoDeps}'" \
                build --manifest-path "$work_dir/source/adapters/aozora2html/Cargo.toml" --release --offline
              python3 -m pytest "$work_dir/source/adapters/aozora2html/tests/test_mapper.py" -vv
              touch "$out"
            '';

        aatOracleDataSchemaSmokeShell = pkgs.writeShellApplication {
          name = "aat-oracle-data-schema-smoke";
          runtimeInputs = [
            pythonWithAatSchemaDeps
          ];
          text = ''
            export AB_VALIDATOR_DIRECT_PYTHON=1
            bash "${source}/tests/aat-oracle-data-schema-smoke.sh"
          '';
        };

        aatOracleDataSchemaSmokeCheck =
          pkgs.runCommand "aat-oracle-data-schema-smoke-check"
            {
              nativeBuildInputs = [
                pythonWithAatSchemaDeps
              ];
            }
            ''
              export AB_VALIDATOR_DIRECT_PYTHON=1
              export AB_DB_ROOT="$TMPDIR/ab-validator"
              bash "${source}/tests/aat-oracle-data-schema-smoke.sh"
              touch "$out"
            '';

        adapterFidelityNotesSchemaSmokeShell = pkgs.writeShellApplication {
          name = "adapter-fidelity-notes-schema-smoke";
          runtimeInputs = [
            pythonWithAatSchemaDeps
          ];
          text = ''
            export AB_VALIDATOR_DIRECT_PYTHON=1
            bash "${source}/tests/adapter-fidelity-notes-schema-smoke.sh"
          '';
        };

        adapterFidelityNotesSchemaSmokeCheck =
          pkgs.runCommand "adapter-fidelity-notes-schema-smoke-check"
            {
              nativeBuildInputs = [
                pythonWithAatSchemaDeps
              ];
            }
            ''
              export AB_VALIDATOR_DIRECT_PYTHON=1
              export AB_DB_ROOT="$TMPDIR/ab-validator"
              bash "${source}/tests/adapter-fidelity-notes-schema-smoke.sh"
              touch "$out"
            '';

        taxonomyGenerator = rustPlatform.buildRustPackage {
          pname = "ab-taxonomy-generator";
          version = "0.1.0";

          src = source;
          cargoDeps = abCargoDeps;

          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "generate_taxonomy"
          ];

          doCheck = false;
        };

        taxonomyDriftCheck =
          pkgs.runCommand "taxonomy-drift-check"
            {
              nativeBuildInputs = [
                taxonomyGenerator
                pkgs.diffutils
              ];
            }
            ''
              generated="$TMPDIR/generated-feature-taxonomy.md"

              generate_taxonomy \
                --annotation-dir "${aozorabunko-src}/annotation" \
                --corpus-dir "${aozorabunko-src}/cards" \
                --corpus-limit 0 \
                --reference "$TMPDIR/parser-report-not-present.md" \
                --write "$generated"

              if ! cmp -s "${source}/data/generated-feature-taxonomy.md" "$generated"; then
                echo "data/generated-feature-taxonomy.md is out of date. Regenerated diff:" >&2
                diff -u --binary "${source}/data/generated-feature-taxonomy.md" "$generated" >&2
                exit 1
              fi

              touch "$out"
            '';

        abAatToParserIr =
          if hasCargoManifest && hasCargoLock then
            rustPlatform.buildRustPackage {
              pname = "ab-aat-to-parser-ir";
              version = "0.1.0";

              src = source;
              cargoDeps = abCargoDeps;

              nativeBuildInputs = [
                pkgs.pkg-config
                pkgs.zstd
              ];

              buildInputs = [
                pkgs.pdfium-binaries
              ]
              ++ lib.optionals pkgs.stdenv.isDarwin [
                pkgs.libiconv
                pkgs.darwin.apple_sdk.frameworks.Security
                pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
              ];

              AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
              AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
              AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
              AB_ABC_ROOT = "${source}/data/abc-schemas";

              cargoBuildFlags = [
                "--package"
                "ab-aat-to-parser-ir"
              ];
              doCheck = false;
            }
          else
            pkgs.writeShellApplication {
              name = "ab-aat-to-parser-ir";
              text = ''
                echo 'Rust workspace not scaffolded' >&2
                exit 1
              '';
            };

        abAatToParserIrCheck =
          pkgs.runCommand "ab-aat-to-parser-ir-smoke-check"
            {
              nativeBuildInputs = [
                pkgs.babashka
                pkgs.clojure
                pkgs.jq
                pythonWithAatSchemaDeps
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"

              export TMPDIR="$work_dir/tmp"
              mkdir -p "$TMPDIR"
              export HOME="$work_dir/home"
              mkdir -p "$HOME"
              export AB_ABC_ROOT="${source}/data/abc-schemas"
              export AB_AAT_TO_PARSER_IR_BIN="${abAatToParserIr}/bin/ab-aat-to-parser-ir"

              bash tests/aat-to-parser-ir-cli-smoke.sh
              touch "$out"
            '';
      in
      {
        packages = {
          default = abValidator;
          ab-validator = abValidator;
          ab-aat-to-parser-ir = abAatToParserIr;
          reference-aozora2 = referenceAozora2;
          reference-aozora-rs = referenceAozoraRs;
          reference-aozora-parser-js = referenceAozoraParserJs;
          reference-aozorabunko-extractor = referenceAozorabunkoExtractor;
          reference-aozora-epub3 = referenceAozoraEpub3;
          reference-parsers = referenceParsers;
          sudachi-dictionary-full = sudachiDictionaryFull;
        };

        apps.default = flake-utils.lib.mkApp {
          drv = abValidator;
        };

        apps.aat-oracle-data-schema-smoke = flake-utils.lib.mkApp {
          drv = aatOracleDataSchemaSmokeShell;
        };

        apps.aozora2html-rust-parity = flake-utils.lib.mkApp {
          drv = aozora2htmlRustParityShell;
        };

        apps.ab-aat-to-parser-ir = flake-utils.lib.mkApp {
          drv = abAatToParserIr;
        };

        apps.adapter-fidelity-notes-schema-smoke = flake-utils.lib.mkApp {
          drv = adapterFidelityNotesSchemaSmokeShell;
        };

        checks = {
          default = workspaceCheck;
          ab-validator = workspaceCheck;
          reference-aozora2 = referenceAozora2;
          reference-aozora-rs = referenceAozoraRs;
          reference-parser-metadata = nonRustReferenceMetadata;
          aat-oracle-data-schema-smoke = aatOracleDataSchemaSmokeCheck;
          aozora2html-rust-parity = aozora2htmlRustParityCheck;
          adapter-fidelity-notes-schema-smoke = adapterFidelityNotesSchemaSmokeCheck;
          taxonomy-drift = taxonomyDriftCheck;
          aat-to-parser-ir-smoke = abAatToParserIrCheck;
        };

        devShells = {
          default = pkgs.mkShell {
            packages = devTools;

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
            AB_SUDACHI_DICT = "${sudachiDictionaryFull}/share/sudachi/system.dic";
            AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
            AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
            AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
            AB_DUCKDB_BIN = "${pkgs.duckdb}/bin/duckdb";

            shellHook = ''
              export CARGO_HOME="''${CARGO_HOME:-$PWD/.cargo}"
              export RUST_BACKTRACE="1"
            '';
          };

          aozora2html = pkgs.mkShell {
            packages = aozora2htmlTools;
          };

          reference-parsers = referenceParserShell;
        };

        formatter = pkgs.nixfmt;
      }
    );
}
