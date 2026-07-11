{
  description = "Development and build environment for the ab-validator Rust workspace";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    flake-utils.url = "github:numtide/flake-utils";

    abc = {
      url = "path:../abc";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.clj-nix.follows = "clj-nix";
    };

    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    rust-overlay = {
      url = "github:oxalica/rust-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    upstream-aozora2-src = {
      url = "github:takahashim/aozora2/93420b53c7d52579a0ca3fde466cef8ce6d89879";
      flake = false;
    };

    upstream-aozora-rs-src = {
      url = "github:kinoko0518/aozora-rs/2b2b8f641aee9fd92ed282f03ab060c18542700c";
      flake = false;
    };

    upstream-aozora-notation-spec-src = {
      url = "github:P4suta/aozora-notation-spec/b60665fd50b596c967254f99b61f418495656fef";
      flake = false;
    };

    upstream-aozora-parser-js-src = {
      url = "github:cognitom/aozora-parser.js/abaf45422051f418905d9d269f1d2db28ebeed05";
      flake = false;
    };

    upstream-aozorabunko-extractor-src = {
      url = "github:globis-org/aozorabunko-extractor/ce439c2b43a4ec0312d12bb89d49e8b186ff0c27";
      flake = false;
    };

    aozorabunko-src = {
      url = "github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165";
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
      abc,
      clj-nix,
      upstream-aozora-notation-spec-src,
      upstream-aozora-parser-js-src,
      upstream-aozora-rs-src,
      upstream-aozora2-src,
      upstream-aozorabunko-extractor-src,
      aozorabunko-src,
      mecab-dic-converter-src,
      flake-utils,
      rust-overlay,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        overlays = [
          clj-nix.overlays.default
          (import rust-overlay)
        ];
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

        abcSource = cleanProjectSource abc.outPath;
        abcCljDepsCache = pkgs.mk-deps-cache {
          lockfile = "${abc.outPath}/deps-lock.json";
        };

        abcSchemaRootForNix = pkgs.runCommand "ab-validator-abc-schema-root" { } ''
          mkdir -p "$out/schemas"
          cp "${source}/data/abc-schemas/nix-schemas"/*.schema.json "$out/schemas/"
          cp "${source}/data/abc-schemas/schema-contracts.json" \
            "$out/schemas/schema-contracts.json"
        '';

        buildRustUpstreamParser =
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

        upstreamParserAozora2 = buildRustUpstreamParser {
          name = "upstream-parser-aozora2";
          src = upstream-aozora2-src;
          lockFile = upstream-aozora2-src + "/Cargo.lock";
        };

        upstreamParserAozoraRs = buildRustUpstreamParser {
          name = "upstream-parser-aozora-rs";
          src = upstream-aozora-rs-src;
          lockFile = upstream-aozora-rs-src + "/Cargo.lock";
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

        upstreamAozoraNotationSpec =
          pkgs.runCommand "upstream-aozora-notation-spec"
            {
              src = cleanProjectSource upstream-aozora-notation-spec-src;
            }
            ''
              mkdir -p "$out"
              cp -R "$src"/. "$out"/
              test -f "$out/conformance/schema/vector.schema.json"
              test -d "$out/conformance/vectors"
              test -f "$out/conformance/RUNNER.md"
              test -f "$out/src/grammar/aozora.abnf"
            '';

        upstreamParserAozoraParserJs = pkgs.stdenvNoCC.mkDerivation {
          pname = "upstream-parser-aozora-parser-js";
          version = "0.0.0";

          src = cleanProjectSource upstream-aozora-parser-js-src;

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

        upstreamToolAozorabunkoExtractor =
          pkgs.runCommand "upstream-tool-aozorabunko-extractor"
            {
              nativeBuildInputs = [ pkgs.makeWrapper ];
              src = cleanProjectSource upstream-aozorabunko-extractor-src;
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

        upstreamParserAozoraEpub3Version = "1.3.6-jdk21";

        upstreamParserAozoraEpub3Release = pkgs.fetchurl {
          url = "https://github.com/AozoraEpub3-JDK21/AozoraEpub3-JDK21/releases/download/v${upstreamParserAozoraEpub3Version}/AozoraEpub3-${upstreamParserAozoraEpub3Version}.tar.gz";
          hash = "sha256-iqrnh9ALiNrAmQSmGgHszkl2+tVP2YC0Ebq62NT6JVo=";
        };

        upstreamParserAozoraEpub3 =
          pkgs.runCommand "upstream-parser-aozora-epub3-${upstreamParserAozoraEpub3Version}"
            {
              nativeBuildInputs = [
                pkgs.gnutar
                pkgs.gzip
              ];
            }
            ''
              mkdir -p "$out/bin" "$out/lib/aozora-epub3" "$out/share/licenses/aozora-epub3"
              tar -xzf ${upstreamParserAozoraEpub3Release} -C "$out/lib/aozora-epub3"

              ln -s "$out/lib/aozora-epub3/AozoraEpub3.jar" "$out/lib/AozoraEpub3.jar"

              cat > "$out/bin/upstream-parser-aozora-epub3" <<'SH'
              #!/usr/bin/env bash
              set -euo pipefail
              cd "__AOZORA_EPUB3_HOME__"
              exec "__JAVA__" -jar "__AOZORA_EPUB3_HOME__/AozoraEpub3.jar" "$@"
              SH
              substituteInPlace "$out/bin/upstream-parser-aozora-epub3" \
                --replace-fail "__AOZORA_EPUB3_HOME__" "$out/lib/aozora-epub3" \
                --replace-fail "__JAVA__" "${pkgs.jdk21}/bin/java"
              chmod +x "$out/bin/upstream-parser-aozora-epub3"

              cp "$out/lib/aozora-epub3/gpl.txt" "$out/share/licenses/aozora-epub3/"
              cp "$out/lib/aozora-epub3/LICENSE.txt" "$out/share/licenses/aozora-epub3/"
              cp "$out/lib/aozora-epub3/THIRD-PARTY-NOTICES.txt" "$out/share/licenses/aozora-epub3/"
            '';

        upstreamParsers = pkgs.symlinkJoin {
          name = "upstream-parsers";
          paths = [
            upstreamParserAozora2
            upstreamParserAozoraRs
            upstreamParserAozoraEpub3
            upstreamParserAozoraParserJs
            upstreamToolAozorabunkoExtractor
          ];
        };

        aozorabunkoCorpus = pkgs.symlinkJoin {
          name = "aozorabunko-corpus";
          paths = [ aozorabunko-src ];
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
            # like-named key in `cargoGitOutputHashes` below — mecab-dic-converter
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

        # 近世 (Edo-period) editions — for the ~2% of Aozora authored by pre-Meiji
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

        # Shared gaiji provisioning for the CLIs/adapters whose build.rs (via
        # third_party/aozora-rs-gaiji) needs the pinned JIS X 0213 menkuten
        # table, the Aozora gaiji_chuki PDF, and a pdfium binary. See
        # aozoraRsAdapter for the full rationale.
        gaijiEnv = {
          AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
          AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
          AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";
        };

        gaijiBuildInputs = [
          pkgs.pdfium-binaries
        ]
        ++ lib.optionals pkgs.stdenv.isDarwin [
          pkgs.libiconv
          pkgs.darwin.apple_sdk.frameworks.Security
          pkgs.darwin.apple_sdk.frameworks.SystemConfiguration
        ];

        # Stub for a workspace CLI when the Rust workspace is not scaffolded
        # (no Cargo.toml/Cargo.lock). Defined once instead of re-inlined per CLI.
        mkUnscaffoldedStub =
          name:
          pkgs.writeShellApplication {
            inherit name;
            text = ''
              echo 'Rust workspace not scaffolded' >&2
              exit 1
            '';
          };

        # Common skeleton for a workspace Rust binary built from `source` against
        # the shared abCargoDeps vendor dir. Each call site passes only its real
        # differences (package flags, extra deps, gaiji opt-in, doCheck).
        mkRustBin =
          {
            pname,
            cargoBuildFlags ? null,
            # Defaults suit the gated workspace CLIs. An ungated simple bin must
            # pass `nativeBuildInputs = [ ]` and `gated = false` explicitly, else
            # it silently gains pkg-config and stub-on-unscaffolded behavior.
            nativeBuildInputs ? [ pkgs.pkg-config ],
            buildInputs ? [ ],
            env ? { },
            doCheck ? false,
            gated ? true,
            stub ? (mkUnscaffoldedStub pname),
            extra ? { },
          }:
          let
            drv = rustPlatform.buildRustPackage (
              {
                inherit
                  pname
                  nativeBuildInputs
                  buildInputs
                  doCheck
                  ;
                version = "0.1.0";
                src = source;
                cargoDeps = abCargoDeps;
              }
              // lib.optionalAttrs (cargoBuildFlags != null) { inherit cargoBuildFlags; }
              // env
              // extra
            );
          in
          if gated then (if hasCargoManifest && hasCargoLock then drv else stub) else drv;

        # ── Tokenizer CLI runners ──
        #
        # Standalone Vibrato and Sudachi CLIs built from the exact sources the
        # workspace already pins, wired to the flake dictionaries. These exist
        # so tokenizer-profile fixture evidence and ad hoc probes run pinned,
        # reproducible runners instead of ambient tools (ADR 0027 deferred
        # decision; docs/handoffs/ruby-annotation-probe-2026-07-09.md caveat).

        vibratoRkyvSource = pkgs.fetchgit {
          url = "https://github.com/o24s/vibrato-rkyv.git";
          rev = "6467251cdb945f8f0ca0c6bfd8c96036ab9d4e12";
          hash = cargoGitOutputHashes."vibrato-rkyv-0.7.7";
        };

        # The vibrato-rkyv repo ships no workspace Cargo.lock (the ab-validator
        # workspace pins it as a git dependency, which needs none). The lock
        # under third_party/vibrato-rkyv-cli/ was generated once from the
        # pinned rev so the CLI builds offline.
        vibratoCli = rustPlatform.buildRustPackage {
          pname = "vibrato-cli";
          version = "0.7.7-6467251";

          src = vibratoRkyvSource;
          cargoLock = {
            lockFile = ./third_party/vibrato-rkyv-cli/Cargo.lock;
            # The fork's evaluate/map crates depend on sudachi.rs at the same
            # rev the ab-validator workspace already pins.
            outputHashes."sudachi-0.6.11-a1" = cargoGitOutputHashes."sudachi-0.6.11-a1";
          };
          postPatch = ''
            cp ${./third_party/vibrato-rkyv-cli/Cargo.lock} Cargo.lock

            # Force pointer_width_64 on the transitive rkyv so dictionaries
            # produced by mecab-dic-converter load; same pattern as the
            # ab-validator workspace Cargo.toml. The committed Cargo.lock was
            # generated with this dependency edge present.
            cat >> tokenize/Cargo.toml <<'TOML'

            [dependencies.rkyv]
            version = "0.8"
            features = ["pointer_width_64"]
            TOML
          '';

          cargoBuildFlags = [
            "-p"
            "tokenize"
          ];
          doCheck = false;

          meta.description = "Vibrato (rkyv fork) tokenize CLI; loads the flake's .dic.zst dictionaries directly";
        };

        sudachiCli = rustPlatform.buildRustPackage {
          pname = "sudachi-cli";
          version = "0.6.11-a1";

          src = sudachiRustSource;
          cargoLock.lockFile = sudachiRustSource + "/Cargo.lock";

          cargoBuildFlags = [
            "-p"
            "sudachi-cli"
          ];
          doCheck = false;

          meta.description = "Sudachi.rs CLI built from the workspace-pinned source";
        };

        vibratoTokenizeApp = pkgs.writeShellApplication {
          name = "vibrato-tokenize";
          text = ''
            # AB_VIBRATO_DICT selects a flake dictionary by short name
            # (unidic-novel default; cwj, csj, qkana, kindai-bungo,
            # kinsei-edo, kinsei-bungo) or an explicit .dic.zst path.
            # AB_VIBRATO_CACHE_DIR redirects the decompressed-dictionary cache
            # (~1 GB per dictionary) away from a small $HOME.
            if [ -n "''${AB_VIBRATO_CACHE_DIR:-}" ]; then
              export XDG_CACHE_HOME="$AB_VIBRATO_CACHE_DIR"
            fi
            dict="''${AB_VIBRATO_DICT:-unidic-novel}"
            case "$dict" in
              */* | *.dic.zst) dic_path="$dict" ;;
              unidic-*) dic_path="${vibratoDictionaries}/share/vibrato/$dict-202512.dic.zst" ;;
              *) dic_path="${vibratoDictionaries}/share/vibrato/unidic-$dict-202512.dic.zst" ;;
            esac
            exec ${vibratoCli}/bin/tokenize -i "$dic_path" "$@"
          '';
        };

        sudachiApp = pkgs.writeShellApplication {
          name = "sudachi";
          text = ''
            # Default config, resources, and system dictionary come from the
            # flake pins; any explicit -r/-p/-l argument disables the
            # corresponding default.
            defaults=()
            case " $* " in
              *" -r "* | *" --config-file "*) ;;
              *) defaults+=(-r "${sudachiRustSource}/resources/sudachi.json") ;;
            esac
            case " $* " in
              *" -p "* | *" --resource_dir "*) ;;
              *) defaults+=(-p "${sudachiRustSource}/resources") ;;
            esac
            case " $* " in
              *" -l "* | *" --dict "*) ;;
              *) defaults+=(-l "${sudachiDictionaryFull}/share/sudachi/system_full.dic") ;;
            esac
            exec ${sudachiCli}/bin/sudachi "''${defaults[@]}" "$@"
          '';
        };

        aozora2htmlCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora2html/Cargo.lock;
        };

        # Vendored crate deps for the excluded aozora-epub3 adapter crate, so
        # the smoke check can build the mapper fully offline in the Nix store.
        aozoraEpub3CargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora-epub3/Cargo.lock;
        };

        # ── Repo's own Rust adapters, packaged as reproducible derivations ──
        #
        # Prior to this the legacy comparison-lane adapters under adapters/
        # were built only via `cargo build` inside justfile recipes — the
        # ADR's F5 finding: the adapter *build* itself was not
        # pinned/reproducible. Each Rust adapter is a crate excluded from the
        # root workspace (see the `exclude` list in ./Cargo.toml) with its own
        # Cargo.lock. We package them with `rustPlatform.buildRustPackage`,
        # reusing the existing importCargoLock cargo-deps where they already
        # exist (aozora2htmlCargoDeps, aozoraEpub3CargoDeps) and adding new
        # vendored deps for the two adapters that lacked them. All builds are
        # fully offline; deps resolve from the importCargoLock vendor dirs.
        #
        # doCheck is disabled: F5 is about pinning the *build*, and the adapter
        # test suites are already exercised by the dedicated smoke/parity checks
        # (aozora-epub3-smoke, aozora2html-rust-parity, …), several
        # of which need fixtures/oracles this plain build derivation does not
        # stage. This mirrors the doCheck = false convention used by the other
        # single-binary build derivations here (taxonomyGenerator,
        # sourceInventoryBin).

        # aozora2 has a `path` dependency that reaches outside the adapter dir
        # (crates/ab-source-syntax). Its vendored deps are keyed to the adapter's
        # own Cargo.lock, but the build src must be the whole ab-validator tree
        # so that path dep resolves; buildAndTestSubdir + cargoRoot point cargo
        # at the adapter's self-contained workspace.
        aozora2AdapterCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora2/Cargo.lock;
        };

        # aozora-rs, like aozora2, has `path` deps that reach outside the adapter
        # dir (crates/ab-ir, crates/ab-source-syntax and the third_party/aozora-rs-gaiji
        # patch), so its build src is the whole ab-validator tree with
        # buildAndTestSubdir + cargoRoot pointing cargo at the adapter's workspace.
        # Its only git dependency is aozora-rs-core (aozora-rs-gaiji is [patch]ed to
        # the vendored third_party path), so importCargoLock needs a single outputHash.
        # This was previously blocked: crates/ab-ir now transitively requires
        # ab-ortho-detect → bincode, and the committed Cargo.lock predated that
        # change, so an offline build failed with `no matching package named bincode`.
        # The lock has since been regenerated against the current crates/ tree.
        aozoraRsAdapterCargoDeps = rustPlatform.importCargoLock {
          lockFile = ./adapters/aozora-rs/Cargo.lock;
          outputHashes = {
            "aozora-rs-core-0.1.0" = "sha256-FstLN45x25KjRr9Q7ORJ8W7/lRVlvIld0xAEpKZ9EUE=";
          };
        };

        mkAdapterCargoQualityCheck =
          {
            name,
            manifestPath,
            cargoDeps,
            extraEnv ? { },
            checkSuffix ? "cargo-quality-check",
            cargoCommand ? ''
              cargo fmt --manifest-path "$manifest" -- --check
              cargo clippy --manifest-path "$manifest" \
                --all-targets --offline --locked -- -D warnings
            '',
          }:
          pkgs.runCommand "${name}-${checkSuffix}"
            (
              {
                nativeBuildInputs = [ rustToolchain ];
              }
              // extraEnv
            )
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              export CARGO_HOME="$work_dir/cargo-home"
              mkdir -p "$CARGO_HOME"
              cp "${cargoDeps}/.cargo/config.toml" "$CARGO_HOME/config.toml"
              substituteInPlace "$CARGO_HOME/config.toml" \
                --replace-fail 'directory = "cargo-vendor-dir"' 'directory = "${cargoDeps}"'
              manifest="$work_dir/source/${manifestPath}"
              ${cargoCommand}
              touch "$out"
            '';

        adapterCargoQualityChecks = [
          (mkAdapterCargoQualityCheck {
            name = "aozora2";
            manifestPath = "adapters/aozora2/Cargo.toml";
            cargoDeps = aozora2AdapterCargoDeps;
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora2html";
            manifestPath = "adapters/aozora2html/Cargo.toml";
            cargoDeps = aozora2htmlCargoDeps;
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora-rs";
            manifestPath = "adapters/aozora-rs/Cargo.toml";
            cargoDeps = aozoraRsAdapterCargoDeps;
            extraEnv = gaijiEnv;
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora-epub3";
            manifestPath = "adapters/aozora-epub3/Cargo.toml";
            cargoDeps = aozoraEpub3CargoDeps;
          })
        ];

        adapterCargoQualityCheck = pkgs.runCommand "adapter-cargo-quality-check" { } ''
          ${pkgs.lib.concatMapStringsSep "\n" (check: "test -e ${check}") adapterCargoQualityChecks}
          touch "$out"
        '';

        adapterDecodingContractChecks = [
          (mkAdapterCargoQualityCheck {
            name = "aozora2";
            manifestPath = "adapters/aozora2/Cargo.toml";
            cargoDeps = aozora2AdapterCargoDeps;
            checkSuffix = "decoding-contract-check";
            cargoCommand = ''
              cargo test --manifest-path "$manifest" \
                --offline --locked source_decoding_contract
            '';
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora2html";
            manifestPath = "adapters/aozora2html/Cargo.toml";
            cargoDeps = aozora2htmlCargoDeps;
            checkSuffix = "decoding-contract-check";
            cargoCommand = ''
              cargo test --manifest-path "$manifest" \
                --offline --locked source_decoding_contract
            '';
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora-rs";
            manifestPath = "adapters/aozora-rs/Cargo.toml";
            cargoDeps = aozoraRsAdapterCargoDeps;
            checkSuffix = "decoding-contract-check";
            extraEnv = gaijiEnv;
            cargoCommand = ''
              cargo test --manifest-path "$manifest" \
                --offline --locked source_decoding_contract
            '';
          })
          (mkAdapterCargoQualityCheck {
            name = "aozora-epub3";
            manifestPath = "adapters/aozora-epub3/Cargo.toml";
            cargoDeps = aozoraEpub3CargoDeps;
            checkSuffix = "decoding-contract-check";
            cargoCommand = ''
              cargo test --manifest-path "$manifest" \
                --offline --locked source_decoding_contract
            '';
          })
        ];

        adapterDecodingContractCheck = pkgs.runCommand "adapter-decoding-contract-check" { } ''
          ${pkgs.lib.concatMapStringsSep "\n" (check: "test -e ${check}") adapterDecodingContractChecks}
          touch "$out"
        '';

        aozora2Adapter = rustPlatform.buildRustPackage {
          pname = "aozora2-adapter";
          version = "0.1.0";

          src = source;
          cargoDeps = aozora2AdapterCargoDeps;

          buildAndTestSubdir = "adapters/aozora2";
          cargoRoot = "adapters/aozora2";

          doCheck = false;

          meta.description = "Repo adapter wrapping the aozora2 (aozora-core) parser into AAT";
        };

        aozoraRsAdapter = rustPlatform.buildRustPackage {
          pname = "aozora-rs-adapter";
          version = "0.1.0";

          src = source;
          cargoDeps = aozoraRsAdapterCargoDeps;

          buildAndTestSubdir = "adapters/aozora-rs";
          cargoRoot = "adapters/aozora-rs";

          buildInputs = [
            pkgs.pdfium-binaries
          ];

          # third_party/aozora-rs-gaiji's build.rs otherwise downloads the JIS X
          # 0213 menkuten table, a pdfium binary, and Aozora's gaiji_chuki.pdf
          # from the live network at build time — impossible in the Nix sandbox
          # and non-reproducible even outside it (the PDF/table drift upstream).
          # The vendored fork in third_party/ adds these env-var escape hatches;
          # we satisfy all three from the pinned inputs already used elsewhere in
          # this flake (aozoraRsGaijiMenkuten, aozoraRsGaijiChukiPdf,
          # pdfium-binaries) so the build is fully offline and reproducible.
          AB_AOZORA_RS_GAIJI_MENKUTEN_PATH = "${aozoraRsGaijiMenkuten}";
          AB_AOZORA_RS_GAIJI_CHUKI_PDF = "${aozoraRsGaijiChukiPdf}";
          AB_AOZORA_RS_GAIJI_PDFIUM_DIR = "${pkgs.pdfium-binaries}/lib";

          doCheck = false;

          meta.description = "Repo adapter wrapping the aozora-rs-core parser into AAT";
        };

        aozora2htmlAdapter = rustPlatform.buildRustPackage {
          pname = "aozora2html-adapter";
          version = "0.1.0";

          src = cleanProjectSource ./adapters/aozora2html;
          cargoDeps = aozora2htmlCargoDeps;

          doCheck = false;

          meta.description = "Repo adapter mapping aozora2html XHTML output into AAT";
        };

        aozoraEpub3Adapter = rustPlatform.buildRustPackage {
          pname = "aozora-epub3-adapter";
          version = "0.1.0";

          src = cleanProjectSource ./adapters/aozora-epub3;
          cargoDeps = aozoraEpub3CargoDeps;

          doCheck = false;

          meta.description = "Repo adapter mapping AozoraEpub3 XHTML output into AAT";
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

        upstreamNonRustMetadata = pkgs.runCommand "upstream-parser-metadata-check" { } ''
          test -f ${upstream-aozora-parser-js-src}/package.json
          test -f ${upstreamParserAozoraEpub3}/lib/AozoraEpub3.jar
          test -f ${upstreamParserAozoraEpub3}/share/licenses/aozora-epub3/gpl.txt
          test -f ${upstreamParserAozoraEpub3}/share/licenses/aozora-epub3/THIRD-PARTY-NOTICES.txt
          test -f ${upstream-aozorabunko-extractor-src}/Gemfile.lock
          touch "$out"
        '';

        upstreamParserShell = pkgs.mkShell {
          packages = devTools ++ [
            rubyWithExtractorGems
            pkgs.bundler
          ];
        };

        abValidator = mkRustBin {
          pname = "ab-validator";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
          doCheck = true;
          stub = pkgs.writeShellApplication {
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
          extra = {
            preCheck = vibratoDictionaryPreCheck;
          };
        };

        workspaceCheck = mkRustBin {
          pname = "ab-validator-check";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
          cargoBuildFlags = [ "--workspace" ];
          doCheck = true;
          stub = pkgs.runCommand "ab-validator-workspace-not-yet-scaffolded" { } ''
            touch "$out"
          '';
          extra = {
            cargoTestFlags = [
              "--workspace"
              "--features"
              "ab-morph-run/test-analyzer"
            ];
          };
        };

        cargoQualityEnv = {
          nativeBuildInputs = [
            rustToolchain
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];

          buildInputs = gaijiBuildInputs;

          src = source;
        };

        cargoQualityPrelude = ''
          set -euo pipefail
          cp -R "$src" source
          chmod -R u+w source
          cd source
          export HOME="$TMPDIR/home"
          export CARGO_HOME="$TMPDIR/cargo-home"
          mkdir -p "$HOME" "$CARGO_HOME"
          export AB_AOZORA_RS_GAIJI_MENKUTEN_PATH="${aozoraRsGaijiMenkuten}"
          export AB_AOZORA_RS_GAIJI_CHUKI_PDF="${aozoraRsGaijiChukiPdf}"
          export AB_AOZORA_RS_GAIJI_PDFIUM_DIR="${pkgs.pdfium-binaries}/lib"
          export AB_ABC_ROOT="${abcSchemaRootForNix}"
          mkdir -p .cargo
          cat > .cargo/config.toml <<EOF
          [source.crates-io]
          replace-with = "vendored-sources"

          [source."git+https://github.com/WorksApplications/sudachi.rs.git?rev=54e85e8f7e0a6c4b570cd7b103506b080dc60c92"]
          git = "https://github.com/WorksApplications/sudachi.rs.git"
          rev = "54e85e8f7e0a6c4b570cd7b103506b080dc60c92"
          replace-with = "vendored-sources"

          [source."git+https://github.com/o24s/vibrato-rkyv.git?rev=6467251cdb945f8f0ca0c6bfd8c96036ab9d4e12"]
          git = "https://github.com/o24s/vibrato-rkyv.git"
          rev = "6467251cdb945f8f0ca0c6bfd8c96036ab9d4e12"
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

        pythonWithAatDuckdb = pkgs.python3.withPackages (ps: [ ps.duckdb ]);

        stageAbcSchemas = ''
          abc_root="$work_dir/abc"
          mkdir -p "$abc_root/schemas"
          cp "$work_dir/source/data/abc-schemas/nix-schemas"/*.schema.json \
            "$abc_root/schemas/"
          cp "$work_dir/source/data/abc-schemas/schema-contracts.json" \
            "$abc_root/schemas/schema-contracts.json"
          export AB_ABC_ROOT="$abc_root"
        '';

        stageAbcPublicationRoot = ''
          abc_root="$work_dir/abc"
          cp -R "${abcSource}" "$abc_root"
          chmod -R +w "$abc_root"
          export HOME="${abcCljDepsCache}"
          export JAVA_TOOL_OPTIONS="-Duser.home=${abcCljDepsCache}"
          export CLJ_CONFIG="$HOME/.clojure"
          export CLJ_CACHE="$TMPDIR/cp-cache"
          export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
          export GITLIBS="$HOME/.gitlibs"
          mkdir -p "$CLJ_CACHE" "$XDG_CONFIG_HOME"
          export AB_ABC_ROOT="$abc_root"
        '';

        mkSmokeCheck =
          {
            name,
            testScript,
            nativeBuildInputs ? [ ],
            extraEnv ? { },
            extraPreScript ? "",
          }:
          let
            envExports = pkgs.lib.concatStringsSep "\n" (
              pkgs.lib.mapAttrsToList (k: v: "export ${k}=\"${v}\"") extraEnv
            );
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

        # Shared body for the aozora2html Rust-mapper parity smoke: build the
        # mapper offline from the vendored deps, then run the pytest oracle.
        # `root` is the shell expression naming the checked-out repo root.
        aozora2htmlParityText = root: ''
          export AB_AOZORA2HTML_BIN="${aozora2htmlParser}/bin/aozora2html"
          cargo \
            --config "source.crates-io.replace-with='vendored-sources'" \
            --config "source.vendored-sources.directory='${aozora2htmlCargoDeps}'" \
            build --manifest-path "${root}/adapters/aozora2html/Cargo.toml" --release --offline
          python -m pytest "${root}/adapters/aozora2html/tests/test_mapper.py" -vv
        '';

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
          ''
          + aozora2htmlParityText "$repo_root";
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
            (
              ''
                work_dir="$(mktemp -d)"
                cp -R "${source}" "$work_dir/source"
                chmod -R +w "$work_dir/source"
                cd "$work_dir/source"
              ''
              + aozora2htmlParityText "$work_dir/source"
              + ''
                touch "$out"
              ''
            );

        # `reports/**` pytest (aat-fidelity dump comparator + lib helpers)
        # wired into the sandbox: same copy-source-then-run idiom as the
        # aozora2html Rust-mapper parity check above.
        reportsPytestCheck =
          pkgs.runCommand "reports-pytest-check"
            {
              nativeBuildInputs = [ pythonWithAatSchemaDeps ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              python -m pytest \
                reports/aat-fidelity/tests \
                reports/lib/tests \
                reports/parser-conformance/tests \
                reports/source-regions/tests \
                -q
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

        aatOracleDataSchemaSmokeCheck = mkSmokeCheck {
          name = "aat-oracle-data-schema-smoke-check";
          testScript = "tests/aat-oracle-data-schema-smoke.sh";
          nativeBuildInputs = [ pythonWithAatSchemaDeps ];
          extraEnv = {
            AB_VALIDATOR_DIRECT_PYTHON = "1";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };

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

        adapterFidelityNotesSchemaSmokeCheck = mkSmokeCheck {
          name = "adapter-fidelity-notes-schema-smoke-check";
          testScript = "tests/adapter-fidelity-notes-schema-smoke.sh";
          nativeBuildInputs = [ pythonWithAatSchemaDeps ];
          extraEnv = {
            AB_VALIDATOR_DIRECT_PYTHON = "1";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };

        taxonomyGenerator = mkRustBin {
          pname = "ab-taxonomy-generator";
          nativeBuildInputs = [ ];
          cargoBuildFlags = [
            "--package"
            "ab-coverage"
            "--bin"
            "generate_taxonomy"
          ];
          gated = false;
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
          gated = false;
        };

        abOracleBin = mkRustBin {
          pname = "ab-oracle";
          nativeBuildInputs = [ ];
          cargoBuildFlags = [
            "--package"
            "ab-oracle"
          ];
          gated = false;
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

        aatFidelityDuckdbSmokeCheck = mkSmokeCheck {
          name = "aat-fidelity-duckdb-smoke-check";
          testScript = "tests/aat-fidelity-duckdb-smoke.sh";
          nativeBuildInputs = [
            pythonWithAatDuckdb
            pkgs.duckdb
            pkgs.glibc.bin
          ];
          extraEnv = {
            AB_AAT_TRIAGE_PYTHON = "${pythonWithAatDuckdb}/bin/python3";
            AB_DUCKDB_BIN = "${pkgs.duckdb}/bin/duckdb";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };

        aatOracleAuditSmokeCheck = mkSmokeCheck {
          name = "aat-oracle-audit-smoke-check";
          testScript = "tests/aat-oracle-audit-smoke.sh";
          nativeBuildInputs = [
            abOracleBin
            pkgs.ripgrep
          ];
          extraEnv = {
            AB_ORACLE_BIN = "${abOracleBin}/bin/ab-oracle";
          };
          extraPreScript = ''
            export AB_DB_ROOT="$TMPDIR/ab-validator"
          '';
        };

        level3AdmissionSmokeCheck = mkSmokeCheck {
          name = "parser-ir-level3-admission-smoke-check";
          testScript = "tests/parser-ir-level3-admission-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        plainProseSourceDeltaSmokeCheck = mkSmokeCheck {
          name = "parser-ir-plain-prose-source-delta-smoke-check";
          testScript = "tests/parser-ir-plain-prose-source-delta-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        publicationBundleSmokeCheck = mkSmokeCheck {
          name = "parser-ir-publication-bundle-smoke-check";
          testScript = "tests/parser-ir-publication-bundle-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        publicationBundleBatchSmokeCheck = mkSmokeCheck {
          name = "parser-ir-publication-bundle-batch-smoke-check";
          testScript = "tests/parser-ir-publication-bundle-batch-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        parserIrOrthoPublicationSmokeCheck = mkSmokeCheck {
          name = "parser-ir-ortho-publication-smoke-check";
          testScript = "tests/parser-ir-ortho-publication-smoke.sh";
          nativeBuildInputs = [
            pkgs.clojure
            pkgs.git
            pkgs.jq
            pkgs.ripgrep
            pkgs.zstd
          ];
          extraEnv = {
            AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
            AB_VIBRATO_DICT = "${vibratoDictCwj}/share/vibrato/unidic-cwj-202512.dic.zst";
          };
          extraPreScript = stageAbcPublicationRoot;
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

        abcSchemaContractDriftCheck =
          pkgs.runCommand "abc-schema-contract-drift-check"
            {
              nativeBuildInputs = [ pkgs.python3 ];
            }
            ''
              cp -R "${source}" source
              chmod -R +w source
              cd source
              abc_root="$TMPDIR/abc"
              mkdir -p "$abc_root/schemas"
              cp data/abc-schemas/schema-contracts.json "$abc_root/schemas/schema-contracts.json"
              python scripts/compare_abc_schema_contracts.py --abc "$abc_root"
              touch "$out"
            '';

        abcSchemaContractCompareSmokeCheck = mkSmokeCheck {
          name = "abc-schema-contract-compare-smoke-check";
          testScript = "tests/abc-schema-contract-compare-smoke.sh";
          nativeBuildInputs = [
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        monorepoPathHygieneSmokeCheck = mkSmokeCheck {
          name = "monorepo-path-hygiene-smoke-check";
          testScript = "tests/monorepo-path-hygiene-smoke.sh";
          nativeBuildInputs = [
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        monorepoWorkspaceLayoutSmokeCheck = mkSmokeCheck {
          name = "monorepo-workspace-layout-smoke-check";
          testScript = "tests/monorepo-workspace-layout-smoke.sh";
          nativeBuildInputs = [
            pkgs.git
            pkgs.just
            pkgs.python3
          ];
          extraPreScript = ''
            git init -q
            mkdir -p "$work_dir/abc/schemas"
            cp "${source}/data/abc-schemas/schema-contracts.json" "$work_dir/abc/schemas/schema-contracts.json"
            export AB_ABC_ROOT="$work_dir/abc"
          '';
        };

        abAatToParserIr = mkRustBin {
          pname = "ab-aat-to-parser-ir";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
          cargoBuildFlags = [
            "--package"
            "ab-aat-to-parser-ir"
          ];
        };

        # The morphological-analysis engine (`ab-morph-run analyze-aat`). Built
        # through nix so the morph-warehouse skip gate can pin it by content
        # (store path / binary hash) — running it via `cargo` from live source
        # would leave the engine version out of the run's input identity.
        abMorphRun = mkRustBin {
          pname = "ab-morph-run";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.zstd
          ];
          buildInputs = gaijiBuildInputs;
          env = gaijiEnv // {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
          cargoBuildFlags = [
            "--package"
            "ab-morph-run"
          ];
        };

        # ab-index: builds the corpus feature index (index.json) consumed by
        # ab-check. Packaged through nix so run-aat-full.sh can pin it by content
        # (store path) in the AAT dump's input identity — running it via cargo from
        # live source would leave the indexer's version out of the dump identity.
        abIndex = mkRustBin {
          pname = "ab-index";
          cargoBuildFlags = [
            "--package"
            "ab-index"
          ];
          env = {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
        };

        # ab-check: the fidelity engine that produces the aat/ tree (runs the
        # adapter per work, emits AAT JSON). Packaged through nix so its version is
        # pinnable by content in the dump identity — it is the primary output
        # producer, so leaving it unpinned is the worst engine-hole.
        abCheck = mkRustBin {
          pname = "ab-check";
          cargoBuildFlags = [
            "--package"
            "ab-check"
          ];
          env = {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
        };

        # Permanent stdin→AAT fork adapter binary (Phase 2). Packaged so
        # run-aat-full.sh can pin it by content as a first-class lane.
        # AB_AOZORA_GIT_REV: bake the flake source rev into --version so a
        # nix-built binary identifies its code (dirty tree -> "unknown",
        # which the gates reject — gates build via cargo with the rev
        # passed explicitly).
        abAozora = mkRustBin {
          pname = "ab-aozora";
          cargoBuildFlags = [
            "--package"
            "ab-aozora"
          ];
          env = {
            AB_AOZORA_GIT_REV = self.rev or "unknown";
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
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
            AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
          };
          extraPreScript = stageAbcSchemas;
        };

        aozoraNotationSpecComparatorSmokeCheck = mkSmokeCheck {
          name = "aozora-notation-spec-comparator-smoke-check";
          testScript = "tests/aozora-notation-spec-comparator-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.python3
            pkgs.ripgrep
          ];
        };

        # Reproducible adapter check: build the mapper fully offline from the
        # vendored cargo deps, validate fixture-driven AAT against
        # data/aat-schema.json, and smoke the full wrapper+JAR path against
        # the pinned AozoraEpub3 release JAR exposed by upstreamParserAozoraEpub3.
        aozoraEpub3SmokeCheck =
          pkgs.runCommand "aozora-epub3-smoke-check"
            {
              nativeBuildInputs = [
                rustToolchain
                pythonWithAatSchemaDeps
                pkgs.jdk21
                pkgs.jq
                pkgs.unzip
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
              python - "$bin" "$work_dir/src.txt" "$work_dir/source/data/aat-schema.json" "$work_dir/source/adapters/aozora-epub3/tests/fixtures" <<'PY'
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

              export AB_AOZORAEPUB3_JAR="${upstreamParserAozoraEpub3}/lib/AozoraEpub3.jar"
              printf 'テスト作品\nテスト著者\n\n-------------------------------------------------------\n凡例\n-------------------------------------------------------\n\n吾輩《わがはい》は猫である。\n\n底本：テスト出版\n' \
                | ${pkgs.bash}/bin/bash "$work_dir/source/adapters/aozora-epub3/aozora-epub3-adapter" --mode aat \
                | jq -e '.meta.adapter == "aozora-epub3" and .meta.parse_complete == true and (.blocks | length >= 1)' >/dev/null
              touch "$out"
            '';
      in
      {
        packages = {
          default = abValidator;
          ab-validator = abValidator;
          ab-aat-to-parser-ir = abAatToParserIr;
          ab-morph-run = abMorphRun;
          ab-index = abIndex;
          ab-check = abCheck;
          ab-aozora = abAozora;
          aat-triage-python = pythonWithAatDuckdb;
          ab-source-inventory = sourceInventoryBin;
          ab-oracle = abOracleBin;
          aozora2-adapter = aozora2Adapter;
          aozora2html-adapter = aozora2htmlAdapter;
          aozora-epub3-adapter = aozoraEpub3Adapter;
          aozora-rs-adapter = aozoraRsAdapter;
          aozorabunko-corpus = aozorabunkoCorpus;
          upstream-parser-aozora2 = upstreamParserAozora2;
          upstream-parser-aozora-rs = upstreamParserAozoraRs;
          upstream-parser-aozora2html = aozora2htmlParser;
          upstream-aozora-notation-spec = upstreamAozoraNotationSpec;
          upstream-parser-aozora-parser-js = upstreamParserAozoraParserJs;
          upstream-tool-aozorabunko-extractor = upstreamToolAozorabunkoExtractor;
          upstream-parser-aozora-epub3 = upstreamParserAozoraEpub3;
          upstream-parsers = upstreamParsers;
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
          vibrato-cli = vibratoCli;
          sudachi-cli = sudachiCli;
          vibrato-tokenize = vibratoTokenizeApp;
          sudachi = sudachiApp;
        };

        apps.default =
          flake-utils.lib.mkApp {
            drv = abValidator;
          }
          // {
            meta.description = "Run the ab-validator CLI";
          };

        apps.aat-oracle-data-schema-smoke =
          flake-utils.lib.mkApp {
            drv = aatOracleDataSchemaSmokeShell;
          }
          // {
            meta.description = "Run the AAT oracle data schema smoke test";
          };

        apps.aozora2html-rust-parity =
          flake-utils.lib.mkApp {
            drv = aozora2htmlRustParityShell;
          }
          // {
            meta.description = "Run the aozora2html Rust mapper parity smoke test";
          };

        apps.ab-aat-to-parser-ir =
          flake-utils.lib.mkApp {
            drv = abAatToParserIr;
          }
          // {
            meta.description = "Run the AAT to parser-IR conversion CLI";
          };

        apps.ab-oracle =
          flake-utils.lib.mkApp {
            drv = abOracleBin;
          }
          // {
            meta.description = "Run the ab-oracle cross-adapter fidelity oracle";
          };

        apps.adapter-fidelity-notes-schema-smoke =
          flake-utils.lib.mkApp {
            drv = adapterFidelityNotesSchemaSmokeShell;
          }
          // {
            meta.description = "Run the adapter fidelity notes schema smoke test";
          };

        apps.vibrato-tokenize =
          flake-utils.lib.mkApp {
            drv = vibratoTokenizeApp;
          }
          // {
            meta.description = "Tokenize stdin with the pinned Vibrato CLI; AB_VIBRATO_DICT selects a flake dictionary (default unidic-novel)";
          };

        apps.sudachi =
          flake-utils.lib.mkApp {
            drv = sudachiApp;
          }
          // {
            meta.description = "Run the pinned Sudachi CLI with the flake's full system dictionary as default";
          };

        checks = {
          default = workspaceCheck;
          ab-validator = workspaceCheck;
          cargo-fmt = cargoFmtCheck;
          cargo-check = cargoCheck;
          cargo-clippy = cargoClippyCheck;
          cargo-test = workspaceCheck;
          upstream-parser-aozora2 = upstreamParserAozora2;
          upstream-parser-aozora-rs = upstreamParserAozoraRs;
          aozora2-adapter = aozora2Adapter;
          aozora2html-adapter = aozora2htmlAdapter;
          aozora-epub3-adapter = aozoraEpub3Adapter;
          aozora-rs-adapter = aozoraRsAdapter;
          upstream-aozora-notation-spec = upstreamAozoraNotationSpec;
          upstream-parser-metadata = upstreamNonRustMetadata;
          aat-oracle-data-schema-smoke = aatOracleDataSchemaSmokeCheck;
          aozora2html-rust-parity = aozora2htmlRustParityCheck;
          adapters-cargo-quality = adapterCargoQualityCheck;
          adapter-decoding-contract = adapterDecodingContractCheck;
          aozora-notation-spec-comparator-smoke = aozoraNotationSpecComparatorSmokeCheck;
          aozora-epub3-smoke = aozoraEpub3SmokeCheck;
          adapter-fidelity-notes-schema-smoke = adapterFidelityNotesSchemaSmokeCheck;
          taxonomy-drift = taxonomyDriftCheck;
          abc-schema-contract-drift = abcSchemaContractDriftCheck;
          abc-schema-contract-compare-smoke = abcSchemaContractCompareSmokeCheck;
          monorepo-path-hygiene-smoke = monorepoPathHygieneSmokeCheck;
          monorepo-workspace-layout-smoke = monorepoWorkspaceLayoutSmokeCheck;
          parser-ir-level3-admission-smoke = level3AdmissionSmokeCheck;
          parser-ir-plain-prose-source-delta-smoke = plainProseSourceDeltaSmokeCheck;
          parser-ir-ortho-publication-smoke = parserIrOrthoPublicationSmokeCheck;
          parser-ir-publication-bundle-smoke = publicationBundleSmokeCheck;
          parser-ir-publication-bundle-batch-smoke = publicationBundleBatchSmokeCheck;
          aat-to-parser-ir-smoke = abAatToParserIrCheck;
          source-inventory-smoke = sourceInventorySmokeCheck;
          source-representability-gate = sourceRepresentabilityGateCheck;
          aat-fidelity-duckdb-smoke = aatFidelityDuckdbSmokeCheck;
          aat-oracle-audit-smoke = aatOracleAuditSmokeCheck;
          reports-pytest = reportsPytestCheck;
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

              # All five nix-built vibrato UniDic dictionaries, joined into one
              # store path. Exporting its share/vibrato dir means the analyzer
              # resolves every dictionary by name (unidic-cwj-202512,
              # unidic-kindai-bungo-202512, …) with NO symlinking into the repo
              # and no per-dictionary `just dictionary-build-*` step. GC-safe:
              # the dev shell holds the store path.
              export AB_VIBRATO_DICT_DIR="${vibratoDictionaries}/share/vibrato"

              # Legacy dictionary/compiled/ dirs are still created + honored as a
              # fallback for tooling that predates AB_VIBRATO_DICT_DIR.
              mkdir -p dictionary/compiled dictionary/optimized

              # Build a vibrato dictionary from NINJAL and symlink it into
              # dictionary/compiled/ so the analyzer auto-discovers it.
              # Usage: vibrato-dict-link cwj
              #        vibrato-dict-link novel
              vibrato-dict-link() {
                local flake_dir
                flake_dir="$(git rev-parse --show-toplevel 2>/dev/null)/ab-validator"
                if [ ! -e "$flake_dir/flake.nix" ]; then flake_dir="."; fi
                local name="''${1:-cwj}"
                local pkg="vibrato-dict-$name"
                local attr="$pkg"
                if ! nix eval "$flake_dir#packages.$(nix eval --impure --raw --expr builtins.currentSystem).$attr" >/dev/null 2>&1; then
                  attr="ab-validator-$pkg"
                fi
                echo "building .#$attr ..." >&2
                nix build "$flake_dir#$attr" --no-link --print-out-paths | while read -r out; do
                  for dict in "$out"/share/vibrato/*.dic.zst; do
                    [ -f "$dict" ] || continue
                    ln -sf "$dict" "dictionary/compiled/$(basename "$dict")"
                    echo "  linked $(basename "$dict")" >&2
                  done
                done
              }
              export -f vibrato-dict-link

              # Bootstrap: only needed when AB_VIBRATO_DICT_DIR is disabled AND
              # no dictionaries are linked — build the default cwj dictionary so
              # the fallback path still works. With AB_VIBRATO_DICT_DIR set (the
              # default above) every dictionary is already resolvable.
              if [ "''${AB_BOOTSTRAP_VIBRATO_DICT:-1}" != "0" ] && \
                 [ -z "''${AB_VIBRATO_DICT_DIR:-}" ] && \
                 ! compgen -G "dictionary/compiled/*.dic.zst" > /dev/null && \
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

          # Provides the toolchain to run the pinned AozoraEpub3 release JAR
          # and the full wrapper smoke. Set AB_AOZORAEPUB3_JAR to override the
          # JAR path for local experiments.
          aozora-epub3 = pkgs.mkShell {
            packages = [
              rustToolchain
              pkgs.jdk21
              pkgs.jq
              pkgs.unzip
              pkgs.python3
            ];
            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
          };

          upstream-parsers = upstreamParserShell;
        };

        formatter = pkgs.nixfmt;
      }
    );
}
