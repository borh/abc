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

    reference-aozora-src = {
      url = "github:P4suta/aozora";
      flake = false;
    };

    reference-aozora-notation-spec-src = {
      url = "github:P4suta/aozora-notation-spec";
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

    mecab-dic-converter-src = {
      url = "github:tomokane/mecab-dic-converter/d24dcf25ce47170ca9e661c003b5d3345e98dac9";
      flake = false;
    };

  };

  outputs =
    {
      self,
      nixpkgs,
      reference-aozora-epub3-src,
      reference-aozora-notation-spec-src,
      reference-aozora-parser-js-src,
      reference-aozora-rs-src,
      reference-aozora-src,
      reference-aozora2-src,
      reference-aozorabunko-extractor-src,
      aozorabunko-src,
      mecab-dic-converter-src,
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

        referenceAozora = buildRustReference {
          name = "reference-aozora";
          src = reference-aozora-src;
          lockFile = reference-aozora-src + "/Cargo.lock";
          cargoBuildFlags = [
            "--package"
            "aozora-cli"
          ];
          cargoTestFlags = [
            "--package"
            "aozora"
            "--package"
            "aozora-cli"
          ];
          doCheck = false;
        };

        referenceAozoraNotationSpec =
          pkgs.runCommand "reference-aozora-notation-spec"
            {
              src = cleanProjectSource reference-aozora-notation-spec-src;
            }
            ''
              mkdir -p "$out"
              cp -R "$src"/. "$out"/
              test -f "$out/conformance/schema/vector.schema.json"
              test -d "$out/conformance/vectors"
              test -f "$out/conformance/RUNNER.md"
              test -f "$out/src/grammar/aozora.abnf"
            '';

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

        # ── mecab-dic-converter: MeCab compiled dict → vibrato .dic.zst ──

        mecabDicConverterCargoLock = {
          lockFile = mecab-dic-converter-src + "/Cargo.lock";
          outputHashes = {
            "vibrato-rkyv-0.7.7" = "sha256-M6ALFpSjs9M+6tvCmn2ZTevUS7NBL6RmnE5GB/qVMEo=";
            "crawdad-rkyv-0.4.0-rkyv.2" = "sha256-FlSXUYHNFUIuEK4sLhbCKJsgRm/EKnHDu7VPpdpvu10=";
          };
        };

        # Two variants of mecab-dic-converter: one with pointer_width_64
        # stripped (compatible with ab-validator's vibrato-rkyv for small
        # dictionaries) and one with pointer_width_64 enabled (required for
        # large dictionaries like unidic-novel whose matrix dimensions overflow
        # 32-bit types).

        mecabDicConverter = rustPlatform.buildRustPackage {
          pname = "mecab-dic-converter";
          version = "0.1.0";

          src = mecab-dic-converter-src;
          cargoLock = mecabDicConverterCargoLock;

          buildFeatures = [ "vibrato-export" ];

          # Tests require MeCab dictionary files at specific paths.
          doCheck = false;

          meta.description = "Convert compiled MeCab dictionaries to Vibrato/Lindera formats";
        };

        # Build a vibrato .dic.zst from a NINJAL Unidic zip (MeCab format).
        buildUnidicVibratoDict =
          {
            name,
            url,
            hash,
          }:
          let
            unidicSrc = pkgs.fetchzip {
              inherit url hash;
              name = "${name}-src";
              stripRoot = false;
            };
          in
          pkgs.runCommand "vibrato-dict-${name}-202512"
            {
              nativeBuildInputs = [ mecabDicConverter ];
            }
            ''
              mkdir -p "$out/share/vibrato"
              mecab-dic-converter build-vibrato \
                --dictionary-root ${unidicSrc} \
                --output "$out/share/vibrato/${name}-202512.dic.zst"
            '';

        # ── Individual vibrato dictionary packages ──

        vibratoDictCwj = buildUnidicVibratoDict {
          name = "unidic-cwj";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-cwj-202512.zip";
          hash = "sha256-lNvcTlhqSWoLPF104eS7r08Dw5SQYRwzTQXy6DxdfjM=";
        };

        vibratoDictCsj = buildUnidicVibratoDict {
          name = "unidic-csj";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-csj-202512.zip";
          hash = "sha256-W0toSrgD8R+KLWos1XuaR3qJhiiYLXtwyd1sUvuYFAM=";
        };

        vibratoDictNovel = buildUnidicVibratoDict {
          name = "unidic-novel";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-novel-v202512.zip";
          hash = "sha256-19wqA64F2CJFYaHzZTMCxOcwYoOTd6C98GHa5b/RUH8=";
        };

        vibratoDictQkana = buildUnidicVibratoDict {
          name = "unidic-qkana";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-qkana-v202512.zip";
          hash = "sha256-bzlybu2NBEFCsfTjU05LIke3wxS6Lfm4HJoS8EOrv2U=";
        };

        vibratoDictKindaiBungo = buildUnidicVibratoDict {
          name = "unidic-kindai-bungo";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-kindai-bungo-v202512.zip";
          hash = "sha256-92+UbTLIuatnm65S+528yK+4A3T93ZFlOQ5+1C71D0c=";
        };

        # Combined package: all built vibrato dictionaries.
        vibratoDictionaries = pkgs.symlinkJoin {
          name = "vibrato-dictionaries";
          paths = [
            vibratoDictCwj
            vibratoDictCsj
            vibratoDictNovel
            vibratoDictQkana
            vibratoDictKindaiBungo
          ];
        };

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

        aozoraCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora/Cargo.lock;
        };

        # Vendored crate deps for the excluded aozora-epub3 adapter crate, so
        # the smoke check can build the mapper fully offline in the Nix store.
        aozoraEpub3CargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora-epub3/Cargo.lock;
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

        referenceAozoraMetadataCheck =
          pkgs.runCommand "reference-aozora-metadata-check"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.ripgrep
              ];
            }
            ''
              export AB_REFERENCE_AOZORA="${referenceAozora}"
              export AB_REFERENCE_AOZORA_NOTATION_SPEC="${referenceAozoraNotationSpec}"
              bash "${source}/tests/reference-aozora-metadata-smoke.sh"
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

        sourceInventoryBin = rustPlatform.buildRustPackage {
          pname = "ab-source-inventory";
          version = "0.1.0";

          src = source;
          cargoDeps = abCargoDeps;

          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "ab-source-inventory"
          ];

          doCheck = false;
        };

        sourceInventorySmokeCheck =
          pkgs.runCommand "source-inventory-smoke-check"
            {
              nativeBuildInputs = [
                sourceInventoryBin
                pkgs.jq
                pkgs.ripgrep
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
              export AB_SOURCE_INVENTORY_BIN="${sourceInventoryBin}/bin/ab-source-inventory"

              bash tests/source-inventory-smoke.sh
              touch "$out"
            '';

        sourceRepresentabilityGateCheck =
          pkgs.runCommand "source-representability-gate-check"
            {
              nativeBuildInputs = [
                sourceInventoryBin
                pkgs.jq
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
              export AB_SOURCE_INVENTORY_BIN="${sourceInventoryBin}/bin/ab-source-inventory"

              bash tests/source-representability-gate-smoke.sh
              touch "$out"
            '';

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

        aozoraAdapterSmokeCheck =
          pkgs.runCommand "aozora-adapter-smoke-check"
            {
              nativeBuildInputs = [
                rustToolchain
                pkgs.bash
                pkgs.jq
                pkgs.python3
                pkgs.ripgrep
                pkgs.python3Packages.jsonschema
              ];
            }
            ''
              work_dir="$TMPDIR/work"
              cp -R ${source} "$work_dir"
              chmod -R u+w "$work_dir"
              export AB_AOZORA_BIN="${referenceAozora}/bin/aozora"
              cargo --config "source.crates-io.replace-with='vendored-sources'" \
                --config "source.vendored-sources.directory='${aozoraCargoDeps}'" \
                build --manifest-path "$work_dir/adapters/aozora/Cargo.toml" --release --offline
              bash "$work_dir/tests/aozora-adapter-smoke.sh"
              touch "$out"
            '';

        aozoraNotationSpecComparatorSmokeCheck =
          pkgs.runCommand "aozora-notation-spec-comparator-smoke-check"
            {
              nativeBuildInputs = [
                pkgs.bash
                pkgs.jq
                pkgs.python3
                pkgs.ripgrep
              ];
            }
            ''
              work_dir="$TMPDIR/work"
              cp -R ${source} "$work_dir"
              chmod -R u+w "$work_dir"
              substituteInPlace "$work_dir/tests/aozora-notation-spec-comparator-smoke.sh" \
                --replace-fail '#!/usr/bin/env bash' '#!${pkgs.bash}/bin/bash'
              bash "$work_dir/tests/aozora-notation-spec-comparator-smoke.sh"
              touch "$out"
            '';

        # Reproducible adapter check: build the mapper fully offline from the
        # vendored cargo deps and validate fixture-driven AAT against
        # data/aat-schema.json. This checks the Rust mapper only -- the
        # AozoraEpub3 JAR cannot be built/pinned in Nix yet (see
        # referenceAozoraEpub3), so the full wrapper+JAR smoke runs via
        # `just aozora-epub3-smoke` and tests/aozora-epub3-adapter-smoke.sh
        # against a locally-built JAR.
        aozoraEpub3SmokeCheck =
          pkgs.runCommand "aozora-epub3-smoke-check"
            {
              nativeBuildInputs = [
                rustToolchain
                pythonWithAatSchemaDeps
                pkgs.jq
              ];
            }
            ''
                            work_dir="$(mktemp -d)"
                            cp -R "${source}" "$work_dir/source"
                            chmod -R +w "$work_dir/source"
                            cd "$work_dir/source"

                            cargo \
                              --config "source.crates-io.replace-with='vendored-sources'" \
                              --config "source.vendored-sources.directory='${aozoraEpub3CargoDeps}'" \
                              build --manifest-path "$work_dir/source/adapters/aozora-epub3/Cargo.toml" --release --offline

                            bin="$work_dir/source/adapters/aozora-epub3/target/release/aozora-epub3-adapter"
                            printf 'test' > "$work_dir/src.txt"
                            python3 - "$bin" "$work_dir/src.txt" "$work_dir/source/data/aat-schema.json" "$work_dir/source/adapters/aozora-epub3/tests/fixtures" <<'PY'
              import json, subprocess, sys, glob
              from pathlib import Path
              bin_p, src, schema_p, fx_dir = sys.argv[1], sys.argv[2], sys.argv[3], sys.argv[4]
              schema = json.loads(Path(schema_p).read_text())
              import jsonschema
              fixtures = sorted(glob.glob(str(fx_dir) + "/*.xhtml"))
              assert fixtures, "no fixtures found at " + fx_dir
              for fx in fixtures:
                  out = subprocess.run(
                      [bin_p, "--mode", "aat", "--source", src, "--xhtml", fx],
                      capture_output=True,
                  )
                  assert out.returncode in (0, 2), f"{fx}: rc={out.returncode} {out.stderr.decode()[:200]}"
                  aat = json.loads(out.stdout)
                  jsonschema.validate(aat, schema)
              print(f"aozora-epub3 smoke: {len(fixtures)} fixtures schema-valid")
              PY
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
          reference-aozora = referenceAozora;
          reference-aozora-notation-spec = referenceAozoraNotationSpec;
          reference-aozora-parser-js = referenceAozoraParserJs;
          reference-aozorabunko-extractor = referenceAozorabunkoExtractor;
          reference-aozora-epub3 = referenceAozoraEpub3;
          reference-parsers = referenceParsers;
          sudachi-dictionary-full = sudachiDictionaryFull;
          mecab-dic-converter = mecabDicConverter;
          vibrato-dict-cwj = vibratoDictCwj;
          vibrato-dict-csj = vibratoDictCsj;
          vibrato-dict-novel = vibratoDictNovel;
          vibrato-dict-qkana = vibratoDictQkana;
          vibrato-dict-kindai-bungo = vibratoDictKindaiBungo;
          vibrato-dictionaries = vibratoDictionaries;
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
          reference-aozora = referenceAozora;
          reference-aozora-notation-spec = referenceAozoraNotationSpec;
          reference-aozora-metadata = referenceAozoraMetadataCheck;
          reference-parser-metadata = nonRustReferenceMetadata;
          aat-oracle-data-schema-smoke = aatOracleDataSchemaSmokeCheck;
          aozora2html-rust-parity = aozora2htmlRustParityCheck;
          aozora-smoke = aozoraAdapterSmokeCheck;
          aozora-notation-spec-comparator-smoke = aozoraNotationSpecComparatorSmokeCheck;
          aozora-epub3-smoke = aozoraEpub3SmokeCheck;
          adapter-fidelity-notes-schema-smoke = adapterFidelityNotesSchemaSmokeCheck;
          taxonomy-drift = taxonomyDriftCheck;
          aat-to-parser-ir-smoke = abAatToParserIrCheck;
          source-inventory-smoke = sourceInventorySmokeCheck;
          source-representability-gate = sourceRepresentabilityGateCheck;
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

              # Create dictionary directories so the analyzer can discover
              # symlinked dictionaries at runtime.
              mkdir -p dictionary/compiled dictionary/optimized

              # Build a vibrato dictionary from NINJAL and symlink it into
              # dictionary/compiled/ so the analyzer auto-discovers it.
              # Usage: vibrato-dict-link cwj
              #        vibrato-dict-link novel
              vibrato-dict-link() {
                local name="''${1:-cwj}"
                local pkg="vibrato-dict-$name"
                echo "building .#$pkg ..." >&2
                nix build ".#$pkg" --no-link --print-out-paths | while read -r out; do
                  for dict in "$out"/share/vibrato/*.dic.zst; do
                    [ -f "$dict" ] || continue
                    ln -sf "$dict" "dictionary/compiled/$(basename "$dict")"
                    echo "  linked $(basename "$dict")" >&2
                  done
                done
              }
              export -f vibrato-dict-link

              # Bootstrap: if no vibrato dictionaries are linked, build the
              # default cwj dictionary automatically. This runs once per
              # checkout; subsequent shells see the existing symlink.
              if ! compgen -G "dictionary/compiled/*.dic.zst" > /dev/null && \
                 ! compgen -G "dictionary/compiled/*.dic" > /dev/null; then
                echo "" >&2
                echo "No vibrato dictionaries found. Building default (unidic-cwj) …" >&2
                vibrato-dict-link cwj
              fi
            '';
          };

          aozora2html = pkgs.mkShell {
            packages = aozora2htmlTools;
          };

          # Provides the toolchain to build the JAR locally and run the full
          # wrapper smoke. Set AB_AOZORAEPUB3_JAR to override the JAR path.
          aozora-epub3 = pkgs.mkShell {
            packages = [
              rustToolchain
              pkgs.jdk21
              pkgs.gradle
              pkgs.jq
              pkgs.unzip
              pkgs.python3
            ];
            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          };

          reference-parsers = referenceParserShell;
        };

        formatter = pkgs.nixfmt;
      }
    );
}
