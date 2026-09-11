{
  description = "Development and build environment for the ab-validator Rust workspace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    soranoha-assets = {
      url = "path:../soranoha";
      flake = false;
    };
    tei-eaj-aozora-tei = {
      url = "github:TEI-EAJ/aozora_tei/77a675fc2771936f9544505d922d4cd45075338c";
      flake = false;
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    aozorabunko-src = {
      url = "git+ssh://forgejo@code.hyakutake-barbel.ts.net:63333/bor/aozorabunko.git?rev=9bac324dfa6af3a5a035542440934266f76fb45d&shallow=1";
      flake = false;
    };

    mecab-dic-converter-src = {
      url = "github:tomokane/mecab-dic-converter/d24dcf25ce47170ca9e661c003b5d3345e98dac9";
      flake = false;
    };
  };

  outputs =
    {
      nixpkgs,
      soranoha-assets,
      tei-eaj-aozora-tei,
      aozorabunko-src,
      mecab-dic-converter-src,
      flake-utils,
      rust-overlay,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          (import rust-overlay)
          (_final: prev: { jdk = prev.jdk25_headless; })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
        };

        inherit (pkgs) lib;

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

        cleanProjectSource =
          src:
          lib.cleanSourceWith {
            inherit src;
            filter =
              path: _type:
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

        # Cargo resolves every workspace member even for a single-package build.
        # Keep the workspace Rust inputs and nothing else.
        publicationRustFiles = lib.fileset.unions [
          ./Cargo.toml
          ./Cargo.lock
          (lib.fileset.fileFilter (file: file.name == "Cargo.toml" || file.hasExt "rs") ./crates)
          ./crates/ab-aozora-encoding/data
          ./crates/ab-aozora-render/assets
          ./crates/ab-aozora-facade/README.md
        ];
        aozoraSource = lib.fileset.toSource {
          root = ./.;
          fileset = publicationRustFiles;
        };
        parserIrSource = lib.fileset.toSource {
          root = ./.;
          fileset = lib.fileset.unions [
            publicationRustFiles
            ./data/aat-schema-v1.json
            ./data/aat-schema.json
            ./data/aat-parser-ir-divergence-bundle-v1.schema.json
            ./schemas/aat-parser-ir-mapping.schema.json
            ./schemas/parser-ir.schema.json
            ./schemas/aat-parser-ir-divergence.schema.json
          ];
        };

        stageSharedVectors = ''
          mkdir -p ../soranoha/test/fixtures
          cp -R ${soranoha-assets}/test/fixtures/canonicalization ../soranoha/test/fixtures/
        '';
        teiEajPythonTests =
          pkgs.runCommand "tei-eaj-python-tests"
            {
              nativeBuildInputs = [
                pkgs.python3
                pkgs.python3Packages.pytest
              ];
            }
            ''
              cp -R ${source}/tools/tei-eaj tei-eaj
              chmod -R u+w tei-eaj
              cd tei-eaj
              python -m pytest -q .
              touch "$out"
            '';
        teiEajApp = subcommand: {
          type = "app";
          program = toString (
            pkgs.writeShellScript "tei-eaj-${subcommand}" ''
              exec ${pkgs.python3}/bin/python ${source}/tools/tei-eaj/tei_eaj_aozora_reports.py \
                --compare-script ${source}/tools/tei-eaj/tei_eaj_compare.py \
                --tei-eaj-root ${tei-eaj-aozora-tei} \
                --source-rev ${tei-eaj-aozora-tei.rev} ${subcommand} "$@"
            ''
          );
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
              mkdir -p "$out/share/sudachi"
              unzip -j ${sudachiDictionaryFullZip} '*.dic' -d "$out/share/sudachi"
              dic="$(find "$out/share/sudachi" -maxdepth 1 -type f -name '*.dic' | head -n 1)"
              test -n "$dic"
              if [ "$dic" != "$out/share/sudachi/system_full.dic" ]; then
                mv "$dic" "$out/share/sudachi/system_full.dic"
              fi
              ln -s system_full.dic "$out/share/sudachi/system.dic"
            '';

        # ── mecab-dic-converter: MeCab compiled dict → vibrato .dic.zst ──

        mecabDicConverterCargoLock = {
          lockFile = mecab-dic-converter-src + "/Cargo.lock";
          outputHashes = {
            # NOTE: this vibrato-rkyv-0.7.7 hash intentionally differs from the
            # like-named key in `cargoGitOutputHashes` below; mecab-dic-converter
            # pins a different rev/tree of the fork than the ab-validator
            # workspace does, so the vendored source hashes are not the same key
            # by coincidence. Do not "deduplicate" these two values.
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

        # The NINJAL UniDic release every dictionary below is built from; the
        # release also names the dictionary files the analyzer resolves.
        unidicRelease = "202512";

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
          pkgs.runCommand "vibrato-dict-${name}-${unidicRelease}"
            {
              nativeBuildInputs = [ mecabDicConverter ];
            }
            ''
              mkdir -p "$out/share/vibrato"
              mecab-dic-converter build-vibrato \
                --dictionary-root ${unidicSrc} \
                --output "$out/share/vibrato/${name}-${unidicRelease}.dic.zst"
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

        # 近世 (Edo-period) editions: for the ~2% of Aozora authored by pre-Meiji
        # writers (曲亭馬琴, 井原西鶴, …) whose raw 旧字旧仮名 texts the 近代/現代
        # dictionaries mis-segment. Not in the default run set; opt in by name.
        vibratoDictKinseiEdo = buildUnidicVibratoDict {
          name = "unidic-kinsei-edo";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-kinsei-edo-v202512.zip";
          hash = "sha256-5mrD9DjCadBDzC01zPpY9dkz1xOQkJsiUZCBmEaYvdQ=";
        };

        vibratoDictKinseiBungo = buildUnidicVibratoDict {
          name = "unidic-kinsei-bungo";
          url = "https://clrd.ninjal.ac.jp/unidic_archive/2512/unidic-kinsei-bungo-v202512.zip";
          hash = "sha256-0kw9488O5+61/hI5DIPmmv+tL960e1vrs+20XT8oFjU=";
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
            vibratoDictKinseiEdo
            vibratoDictKinseiBungo
          ];
        };

        cargoGitOutputHashes = {
          "sudachi-0.6.11-a1" = "sha256-nQiBcAY/NGbyw1/+3ACZ3HtGgc9Ow54+8auyT1Udo0w=";
          "vibrato-rkyv-0.7.7" = "sha256-ZPDiLrA8Losm28tgw/apjFdo07gRTVTZFPM8QLy3MPA=";
        };

        abCargoLock = {
          lockFile = ./Cargo.lock;
          outputHashes = cargoGitOutputHashes;
        };

        # The two git dependencies of the workspace, at the revisions
        # Cargo.lock pins; the same revisions name their sources in the
        # cargo configuration the quality checks write.
        sudachiRsUrl = "https://github.com/WorksApplications/sudachi.rs.git";
        sudachiRsRev = "54e85e8f7e0a6c4b570cd7b103506b080dc60c92";
        vibratoRkyvUrl = "https://github.com/o24s/vibrato-rkyv.git";
        vibratoRkyvRev = "6467251cdb945f8f0ca0c6bfd8c96036ab9d4e12";

        sudachiRustSource = pkgs.fetchgit {
          url = sudachiRsUrl;
          rev = sudachiRsRev;
          hash = cargoGitOutputHashes."sudachi-0.6.11-a1";
        };
        # The crate source includes repo-root resources via ../../resources;
        # the analyzers and the CLI read them from here at run time.
        sudachiResources = "${sudachiRustSource}/resources";

        abCargoDeps = pkgs.runCommand "cargo-vendor-dir" { } ''
          cp -Lr --reflink=auto ${rustPlatform.importCargoLock abCargoLock} "$out"
          chmod -R u+w "$out"

          # hegeltest-c ships its generated C header and skips cbindgen when
          # Cargo packages it. Nix's vendor tree is equivalent, but its path
          # lacks Cargo's target/package marker; preserve that package boundary
          # instead of letting cbindgen resolve a second dependency universe.
          substituteInPlace "$out/hegeltest-c-0.29.0/build.rs" \
            --replace-fail \
              'if crate_dir.components().any(|c| c.as_os_str() == "package")' \
              'if env::var_os("NIX_BUILD_TOP").is_some() || crate_dir.components().any(|c| c.as_os_str() == "package")'

          # The locked Sudachi crate lives under sudachi/ in its git repo, but
          # the crate source includes repo-root resources via ../../resources.
          cp -R ${sudachiResources} "$out/resources"
        '';

        # Darwin-only linkage the workspace CLIs need when built on macOS. The
        # Security and SystemConfiguration frameworks come with the default
        # Darwin SDK.
        workspaceExtraBuildInputs = lib.optionals pkgs.stdenv.isDarwin [ pkgs.libiconv ];

        # Common skeleton for a workspace Rust binary built from `source` against
        # the shared abCargoDeps vendor dir. Each call site passes only its real
        # differences (package flags, extra deps, doCheck). `env` holds the
        # variables baked into the build; a simple bin passes
        # `nativeBuildInputs = [ ]` to drop the pkg-config default.
        mkRustBin =
          {
            pname,
            cargoBuildFlags ? null,
            nativeBuildInputs ? [ pkgs.pkg-config ],
            buildInputs ? [ ],
            env ? { },
            doCheck ? false,
            extra ? { },
          }:
          rustPlatform.buildRustPackage (
            {
              inherit
                pname
                nativeBuildInputs
                buildInputs
                doCheck
                env
                ;
              version = "0.1.0";
              src = source;
              cargoDeps = abCargoDeps;
            }
            // lib.optionalAttrs (cargoBuildFlags != null) { inherit cargoBuildFlags; }
            // extra
          );

        abValidator = mkRustBin {
          pname = "ab-validator";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          doCheck = true;
          # the test suite reads the canonicalization vectors the kernel
          # shares with the workspace
          extra.preBuild = stageSharedVectors;
        };

        workspaceCheck = mkRustBin {
          pname = "ab-validator-check";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          cargoBuildFlags = [ "--workspace" ];
          doCheck = true;
          extra = {
            cargoTestFlags = [ "--workspace" ];
            preBuild = stageSharedVectors;
          };
        };

        cargoQualityEnv = {
          nativeBuildInputs = [
            rustToolchain
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];

          buildInputs = workspaceExtraBuildInputs;

          src = source;
        };

        cargoQualityPrelude = ''
          set -euo pipefail
          cp -R "$src" source
          chmod -R u+w source
          cd source
          ${stageSharedVectors}
          export HOME="$TMPDIR/home"
          export CARGO_HOME="$TMPDIR/cargo-home"
          mkdir -p "$HOME" "$CARGO_HOME"
          mkdir -p .cargo
          cat > .cargo/config.toml <<EOF
          [source.crates-io]
          replace-with = "vendored-sources"

          [source."git+${sudachiRsUrl}?rev=${sudachiRsRev}"]
          git = "${sudachiRsUrl}"
          rev = "${sudachiRsRev}"
          replace-with = "vendored-sources"

          [source."git+${vibratoRkyvUrl}?rev=${vibratoRkyvRev}"]
          git = "${vibratoRkyvUrl}"
          rev = "${vibratoRkyvRev}"
          replace-with = "vendored-sources"

          [source.vendored-sources]
          directory = "${abCargoDeps}"
          EOF
        '';

        cargoFmtCheck = pkgs.runCommand "ab-validator-cargo-fmt-check" cargoQualityEnv ''
          ${cargoQualityPrelude}
          cargo fmt --all -- --check
          touch "$out"
        '';

        cargoCheck = pkgs.runCommand "ab-validator-cargo-check" cargoQualityEnv ''
          ${cargoQualityPrelude}
          cargo check --workspace --all-targets --offline --locked
          touch "$out"
        '';

        cargoClippyCheck = pkgs.runCommand "ab-validator-cargo-clippy-check" cargoQualityEnv ''
          ${cargoQualityPrelude}
          cargo clippy \
            --workspace \
            --all-targets \
            --all-features \
            --offline \
            --locked \
            -- \
            -D warnings
          touch "$out"
        '';

        cargoDenyCheck =
          pkgs.runCommand "ab-validator-cargo-deny-check"
            (
              cargoQualityEnv
              // {
                nativeBuildInputs = cargoQualityEnv.nativeBuildInputs ++ [ pkgs.cargo-deny ];
              }
            )
            ''
              ${cargoQualityPrelude}
              cargo deny check licenses bans sources
              touch "$out"
            '';

        devTools = [
          rustToolchain
          pkgs.cargo-deny
          pkgs.cargo-nextest
          pkgs.cargo-watch
          pkgs.just
          pkgs.pkg-config
          pkgs.ripgrep
          pkgs.fd
          pkgs.jq
          pkgs.hyperfine
          pkgs.python3
          pkgs.jdk
        ];

        pythonWithAatSchemaDeps = pkgs.python3.withPackages (ps: [
          ps.jsonschema
          ps.tomli
          ps.pytest
        ]);

        mkSmokeCheck =
          {
            name,
            testScript,
            nativeBuildInputs ? [ ],
            extraEnv ? { },
            extraPreScript ? "",
          }:
          let
            envExports = lib.concatStringsSep "\n" (lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") extraEnv);
          in
          pkgs.runCommand name
            {
              nativeBuildInputs = nativeBuildInputs ++ [
                pkgs.bash
                pkgs.coreutils
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
              ${envExports}
              ${extraPreScript}

              bash "${testScript}"
              touch "$out"
            '';

        taxonomyGenerator = mkRustBin {
          pname = "ab-taxonomy-generator";
          nativeBuildInputs = [ ];
          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "generate_taxonomy"
          ];
        };

        sourceInventoryBin = mkRustBin {
          pname = "ab-source-inventory";
          nativeBuildInputs = [ ];
          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "ab-source-inventory"
          ];
          extra.src = aozoraSource;
        };

        sourceInventorySmokeCheck = mkSmokeCheck {
          name = "source-inventory-smoke-check";
          testScript = "tests/source-inventory-smoke.sh";
          nativeBuildInputs = [
            sourceInventoryBin
            pkgs.jq
            pkgs.ripgrep
          ];
          extraEnv = {
            AB_SOURCE_INVENTORY_BIN = "${sourceInventoryBin}/bin/ab-source-inventory";
          };
        };

        sourceRepresentabilityGateCheck = mkSmokeCheck {
          name = "source-representability-gate-check";
          testScript = "tests/source-representability-gate-smoke.sh";
          nativeBuildInputs = [
            sourceInventoryBin
            pkgs.jq
            pkgs.ripgrep
          ];
          extraEnv = {
            AB_SOURCE_INVENTORY_BIN = "${sourceInventoryBin}/bin/ab-source-inventory";
          };
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

        monorepoWorkspaceLayoutSmokeCheck = mkSmokeCheck {
          name = "monorepo-workspace-layout-smoke-check";
          testScript = "tests/monorepo-workspace-layout-smoke.sh";
          nativeBuildInputs = [
            pkgs.git
            pkgs.just
            pkgs.python3
          ];
          extraPreScript = "git init -q";
        };

        abAatToParserIr = mkRustBin {
          pname = "ab-aat-to-parser-ir";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          extra.src = parserIrSource;
          cargoBuildFlags = [
            "--package"
            "ab-aat-to-parser-ir"
          ];
        };

        # The one analyzer entry point that survives in the tree. The Sudachi
        # resource directory is baked in as the binary's default (see
        # ab-morph-analyzers::sudachi); the crate's own default is the
        # build-sandbox path and does not exist at run time.
        tokenizeBin = mkRustBin {
          pname = "tokenize";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          env = {
            AB_SUDACHI_RESOURCE_DIR = sudachiResources;
          };
          cargoBuildFlags = [
            "--package"
            "ab-morph-analyzers"
            "--bin"
            "tokenize"
          ];
        };

        # `nix run .#tokenize -- --analyzer vibrato:unidic-cwj-202512 <<< text`
        # runs one analyzer against the flake dictionaries, for checking a
        # phrase by hand. AB_VIBRATO_CACHE_DIR still redirects the
        # decompressed-dictionary cache (about 1 GB per dictionary).
        tokenizeApp = pkgs.writeShellApplication {
          name = "tokenize";
          text = ''
            export AB_VIBRATO_DICT_DIR="${vibratoDictionaries}/share/vibrato"
            export AB_SUDACHI_DICT="${sudachiDictionaryFull}/share/sudachi/system.dic"
            exec ${tokenizeBin}/bin/tokenize "$@"
          '';
        };

        # The AAT adapter is pinned by executable content for corpus measurements.
        abAozora = mkRustBin {
          pname = "ab-aozora";
          extra.src = aozoraSource;
          cargoBuildFlags = [
            "--package"
            "ab-aozora"
          ];
          # Release identity is authenticated by reproducible build hash, not a
          # mutable git rev. Baking self.rev changed the hash every commit; pin
          # "unknown" so the recorded parser_build_hash is stable/reproducible.
          env = {
            AB_AOZORA_GIT_REV = "unknown";
          };
        };

        abAatToParserIrCheck = mkSmokeCheck {
          name = "ab-aat-to-parser-ir-smoke-check";
          testScript = "tests/aat-to-parser-ir-cli-smoke.sh";
          nativeBuildInputs = [
            pkgs.babashka
            pkgs.clojure
            pkgs.jq
            pythonWithAatSchemaDeps
          ];
          extraEnv = {
            AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
          };
        };

      in
      {
        packages = {
          default = abValidator;
          ab-validator = abValidator;
          ab-aat-to-parser-ir = abAatToParserIr;
          ab-aozora = abAozora;
          ab-source-inventory = sourceInventoryBin;
          tokenize = tokenizeBin;
          sudachi-dictionary-full = sudachiDictionaryFull;
          mecab-dic-converter = mecabDicConverter;
          vibrato-dict-cwj = vibratoDictCwj;
          vibrato-dict-csj = vibratoDictCsj;
          vibrato-dict-novel = vibratoDictNovel;
          vibrato-dict-qkana = vibratoDictQkana;
          vibrato-dict-kindai-bungo = vibratoDictKindaiBungo;
          vibrato-dict-kinsei-edo = vibratoDictKinseiEdo;
          vibrato-dict-kinsei-bungo = vibratoDictKinseiBungo;
          vibrato-dictionaries = vibratoDictionaries;
        };

        apps =
          let
            mkApp =
              drv: description:
              flake-utils.lib.mkApp { inherit drv; }
              // {
                meta.description = description;
              };
          in
          {
            tei-eaj-aozora-alignment-probe = teiEajApp "alignment-probe";
            tei-eaj-aozora-reports-with-probes = teiEajApp "all-with-probes";
            tei-eaj-aozora-reports = teiEajApp "all";
            ab-aat-to-parser-ir = mkApp abAatToParserIr "Run the AAT to parser-IR conversion CLI";
            tokenize = mkApp tokenizeApp "Tokenize stdin with one workspace analyzer (--analyzer vibrato:<dictionary> or sudachi-a|b|c) and print a JSON object per morpheme";
          };

        checks = {
          tei-eaj-python-tests = teiEajPythonTests;
          default = workspaceCheck;
          cargo-fmt = cargoFmtCheck;
          cargo-check = cargoCheck;
          cargo-clippy = cargoClippyCheck;
          cargo-deny = cargoDenyCheck;
          taxonomy-drift = taxonomyDriftCheck;
          monorepo-workspace-layout-smoke = monorepoWorkspaceLayoutSmokeCheck;
          aat-to-parser-ir-smoke = abAatToParserIrCheck;
          source-inventory-smoke = sourceInventorySmokeCheck;
          source-representability-gate = sourceRepresentabilityGateCheck;
        };

        devShells = {
          default = pkgs.mkShell {
            packages = devTools;

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
            AB_SUDACHI_DICT = "${sudachiDictionaryFull}/share/sudachi/system.dic";
            AB_SUDACHI_RESOURCE_DIR = sudachiResources;

            shellHook = ''
              if command -v sccache > /dev/null 2>&1; then
                # sccache creates a Unix startup-notification socket beneath TMPDIR.
                # NIMAS session TMPDIR paths can exceed the socket-path limit.
                export TMPDIR=/tmp
                export TMP="$TMPDIR"
                export TEMPDIR="$TMPDIR"
              fi
              export CARGO_HOME="''${CARGO_HOME:-$PWD/.cargo}"
              export RUST_BACKTRACE="1"

              # Every nix-built vibrato UniDic dictionary, joined into one store
              # path, so the analyzer resolves each by name (unidic-cwj-202512,
              # unidic-kindai-bungo-202512, ...) with nothing linked into the
              # repository. The dev shell holds the store path, so the
              # dictionaries survive garbage collection.
              export AB_VIBRATO_DICT_DIR="${vibratoDictionaries}/share/vibrato"
            '';
          };

        };

        formatter = pkgs.nixfmt;
      }
    );
}
