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

    upstream-aozora-notation-spec-src = {
      url = "github:P4suta/aozora-notation-spec/b60665fd50b596c967254f99b61f418495656fef";
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

        stageParserRqAbcAuthorities = ''
          mkdir -p \
            ../abc/data \
            ../abc/schemas \
            ../abc/test/fixtures/parser-rq/classified-source-capture \
            ../abc/test/fixtures/parser-rq/diagnostic-gap
          cp "${abcSource}/data/parser-rq-ab-aozora-classified-source-v1.json" \
            ../abc/data/
          cp "${abcSource}/data/parser-rq-ab-aozora-diagnostic-gap-v1.json" \
            ../abc/data/
          cp "${abcSource}/data/parser-rq-classified-source-authority-v1.json" \
            ../abc/data/
          cp "${abcSource}/schemas/parser-rq-ab-aozora-diagnostics-v3.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/schemas/parser-rq-classified-source-ledger.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/schemas/parser-rq-classified-source-policy.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/schemas/parser-rq-capture-generation.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/schemas/parser-rq-diagnostic-gap-policy.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/schemas/parser-rq-source-recognition-work.schema.json" \
            ../abc/schemas/
          cp "${abcSource}/test/fixtures/parser-rq/classified-source-capture/source.txt" \
            ../abc/test/fixtures/parser-rq/classified-source-capture/
          cp "${abcSource}/test/fixtures/parser-rq/diagnostic-gap/raw-diagnostics-valid.json" \
            ../abc/test/fixtures/parser-rq/diagnostic-gap/
        '';

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
          cp -R ${sudachiRustSource}/resources "$out/resources"
        '';

        # Darwin-only linkage the workspace CLIs need when built on macOS.
        workspaceExtraBuildInputs = lib.optionals pkgs.stdenv.isDarwin [
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
        # differences (package flags, extra deps, doCheck).
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
          cargoHash = "sha256-t6qNqA3fq2TNqBizPIU4Hf9nnO3Fxr/7ldaRD2hcl5A=";

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

        abValidator = mkRustBin {
          pname = "ab-validator";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          env = {
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

        parserRqCandidate = mkRustBin {
          pname = "parser-rq-candidate";
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = workspaceExtraBuildInputs;
          env = {
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
          };
          cargoBuildFlags = [
            "-p"
            "ab-check"
            "-p"
            "ab-aozora"
            "-p"
            "ab-aat-to-parser-ir"
            "-p"
            "ab-parser-rq-source-accountability"
            "-p"
            "ab-parser-rq-diagnostic-authorization"
          ];
          doCheck = false;
          extra = {
            preBuild = stageParserRqAbcAuthorities;
            postInstall = ''
              ln -s ${pkgs.time}/bin/time "$out/bin/time"
            '';
          };
        };

        workspaceCheck = mkRustBin {
          pname = "ab-validator-check";
          nativeBuildInputs = [
            pkgs.pkg-config
            pkgs.python3
            pkgs.zstd
          ];
          buildInputs = workspaceExtraBuildInputs;
          env = {
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
            # The workspace tests compile ab-aozora-capture's four
            # `include_bytes!` embeds of sibling-abc files and read committed
            # witness stores under abc/test/fixtures/, so the full abc tree
            # must sit beside the staged workspace; a minimal file list cannot
            # anticipate every fixture a test reads. Without this the check
            # could not compile and was only ever evaluated, never built.
            preBuild = ''
              cp -R ${abcSource} ../abc
            '';
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
          ${stageParserRqAbcAuthorities}
          export HOME="$TMPDIR/home"
          export CARGO_HOME="$TMPDIR/cargo-home"
          mkdir -p "$HOME" "$CARGO_HOME"
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

        # `reports/**` pytest (aat-fidelity dump comparator + lib helpers)
        # wired into the sandbox: copy-source-then-run idiom.
        reportsPytestCheck =
          pkgs.runCommand "reports-pytest-check"
            {
              nativeBuildInputs = [
                pkgs.git
                pythonWithAatSchemaDeps
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              rm data/abc-schemas/schemas
              cp -R "${abcSource}/schemas" data/abc-schemas/schemas
              python -m pytest \
                reports/aat-fidelity/tests \
                reports/lib/tests \
                reports/parser-conformance/tests \
                reports/source-regions/tests \
                -q
              touch "$out"
            '';

        parserRqPublicationPytestCheck =
          pkgs.runCommand "parser-rq-publication-pytest-check"
            {
              nativeBuildInputs = [ pythonWithAatSchemaDeps ];
            }
            ''
              # Stage the monorepo layout, not the ab-validator subtree alone.
              # test_predicate_hardening_identity.py resolves its repository root
              # as parents[4] of the test file, which is correct for a checkout
              # and requires both ab-validator/ and abc/ to be present: the
              # reviewed semantic closure spans both trees. Staging only this
              # subtree made every test in that file error at import.
              work_dir="$(mktemp -d)"
              mkdir -p "$work_dir/repo"
              cp -R "${source}" "$work_dir/repo/ab-validator"
              cp -R "${abcSource}" "$work_dir/repo/abc"
              chmod -R +w "$work_dir/repo"
              cd "$work_dir/repo/ab-validator"
              python -m pytest reports/parser-ir/tests -q
              touch "$out"
            '';

        parserRqCoreAttemptPythonTests =
          pkgs.runCommand "parser-rq-core-attempt-python-tests"
            {
              nativeBuildInputs = [ pythonWithAatSchemaDeps ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              python -m pytest \
                reports/parser-ir/test_parser_rq_core_attempt_capture.py -q
              touch "$out"
            '';

        parserRqCampaignProvenancePythonTests =
          pkgs.runCommand "parser-rq-campaign-provenance-python-tests"
            {
              nativeBuildInputs = [ pythonWithAatSchemaDeps ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              python -m pytest \
                reports/parser-ir/test_parser_rq_campaign_provenance.py -q
              touch "$out"
            '';

        parserRqPredicateHardeningCapturePythonTests =
          pkgs.runCommand "parser-rq-predicate-hardening-capture-python-tests"
            {
              nativeBuildInputs = [ pythonWithAatSchemaDeps ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              python -m pytest \
                reports/parser-ir/test_parser_rq_predicate_hardening_capture.py -q
              touch "$out"
            '';

        parserRqResourceCgroupSmokeApp = pkgs.writeShellApplication {
          name = "parser-rq-resource-cgroup-smoke";
          runtimeInputs = [
            pkgs.systemd
            pkgs.python3
            pkgs.coreutils
            pkgs.jq
          ];
          text = ''
            exec bash ${source}/tests/parser-rq-resource-cgroup-live-smoke.sh \
              ${source}/reports/parser-ir/parser-rq-resource-wrapper.py
          '';
        };

        parserRqResourceCaptureSmokeCheck =
          pkgs.runCommand "parser-rq-resource-capture-smoke"
            {
              nativeBuildInputs = [
                pkgs.python3
                pkgs.python3Packages.pytest
                pkgs.coreutils
              ];
            }
            ''
              python -m pytest -q \
                ${source}/reports/parser-ir/tests/test_parser_rq_resource_capture.py \
                ${source}/reports/parser-ir/tests/test_parser_rq_resource_wrapper.py
              bash ${source}/tests/parser-rq-resource-capture-smoke.sh ${source}
              touch "$out"
            '';

        parserRqPredicateHardeningCaptureSmokeCheck =
          pkgs.runCommand "parser-rq-predicate-hardening-capture-smoke"
            {
              nativeBuildInputs = [
                abAozora
                abAatToParserIr
                pkgs.clojure
                pkgs.python3
                pkgs.coreutils
                pkgs.diffutils
              ];
              AB_AOZORA_BIN = "${abAozora}/bin/ab-aozora";
              AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
            }
            ''
              bash ${source}/tests/parser-rq-predicate-hardening-capture-smoke.sh \
                ${source} ${abcSource}
              export HOME="${abcCljDepsCache}"
              export JAVA_TOOL_OPTIONS="-Duser.home=${abcCljDepsCache}"
              export CLJ_CONFIG="$HOME/.clojure"
              export CLJ_CACHE="$TMPDIR/cp-cache"
              export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
              export GITLIBS="$HOME/.gitlibs"
              mkdir -p "$CLJ_CACHE" "$XDG_CONFIG_HOME"
              cd ${abcSource}
              clojure -M:test:kaocha -m kaocha.runner \
                --focus abc.tools.parser-rq-predicate-hardening-capture-test
              touch "$out"
            '';

        phase5CheckpointCheck =
          pkgs.runCommand "phase5-checkpoint-check"
            {
              nativeBuildInputs = [
                pkgs.git
                pythonWithAatSchemaDeps
              ];
            }
            ''
              work_dir="$(mktemp -d)"
              cp -R "${source}" "$work_dir/source"
              chmod -R +w "$work_dir/source"
              cd "$work_dir/source"
              python -m pytest \
                reports/aat-fidelity/tests/test_verify_phase5_checkpoint.py \
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
          buildInputs = workspaceExtraBuildInputs;
          env = {
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
          buildInputs = workspaceExtraBuildInputs;
          env = {
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
        abAozora = mkRustBin {
          pname = "ab-aozora";
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
            AB_ABC_ROOT = "${abcSchemaRootForNix}";
            AB_AAT_TO_PARSER_IR_BIN = "${abAatToParserIr}/bin/ab-aat-to-parser-ir";
          };
          extraPreScript = stageAbcSchemas;
        };

        aatParserIrSchemaHashSmokeCheck = mkSmokeCheck {
          name = "aat-parser-ir-schema-hash-smoke-check";
          testScript = "tests/aat-parser-ir-schema-hash-smoke.sh";
          nativeBuildInputs = [
            pkgs.ripgrep
            pythonWithAatSchemaDeps
          ];
          extraPreScript = stageAbcSchemas;
        };

        aatParserIrMappingPolicySmokeCheck = mkSmokeCheck {
          name = "aat-parser-ir-mapping-policy-smoke-check";
          testScript = "tests/aat-parser-ir-mapping-policy-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pythonWithAatSchemaDeps
          ];
          extraEnv = {
            AB_MAPPING_USE_SYSTEM_PYTHON = "1";
          };
          extraPreScript = stageAbcSchemas;
        };

        aatParserIrMappingSmokeCheck = mkSmokeCheck {
          name = "aat-parser-ir-mapping-smoke-check";
          testScript = "tests/aat-parser-ir-mapping-smoke.sh";
          nativeBuildInputs = [
            pkgs.jq
            pkgs.ripgrep
            pythonWithAatSchemaDeps
          ];
          extraEnv = {
            AB_MAPPING_SMOKE_HERMETIC = "1";
            AB_MAPPING_USE_SYSTEM_PYTHON = "1";
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

      in
      {
        packages = {
          default = abValidator;
          ab-validator = abValidator;
          parser-rq-candidate = parserRqCandidate;
          ab-aat-to-parser-ir = abAatToParserIr;
          ab-morph-run = abMorphRun;
          ab-index = abIndex;
          ab-check = abCheck;
          ab-aozora = abAozora;
          aat-triage-python = pythonWithAatDuckdb;
          ab-source-inventory = sourceInventoryBin;
          ab-oracle = abOracleBin;
          aozorabunko-corpus = aozorabunkoCorpus;
          upstream-aozora-notation-spec = upstreamAozoraNotationSpec;
          upstream-tool-aozorabunko-extractor = upstreamToolAozorabunkoExtractor;
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

        apps.parser-rq-resource-cgroup-smoke =
          flake-utils.lib.mkApp {
            drv = parserRqResourceCgroupSmokeApp;
          }
          // {
            meta.description = "Run the host-controlled cgroup-v2 resource smoke";
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
          cargo-deny = cargoDenyCheck;
          cargo-test = workspaceCheck;
          upstream-aozora-notation-spec = upstreamAozoraNotationSpec;
          aat-oracle-data-schema-smoke = aatOracleDataSchemaSmokeCheck;
          aozora-notation-spec-comparator-smoke = aozoraNotationSpecComparatorSmokeCheck;
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
          aat-parser-ir-schema-hash-smoke = aatParserIrSchemaHashSmokeCheck;
          aat-parser-ir-mapping-policy-smoke = aatParserIrMappingPolicySmokeCheck;
          aat-parser-ir-mapping-smoke = aatParserIrMappingSmokeCheck;
          source-inventory-smoke = sourceInventorySmokeCheck;
          source-representability-gate = sourceRepresentabilityGateCheck;
          aat-fidelity-duckdb-smoke = aatFidelityDuckdbSmokeCheck;
          aat-oracle-audit-smoke = aatOracleAuditSmokeCheck;
          reports-pytest = reportsPytestCheck;
          parser-rq-publication-pytest = parserRqPublicationPytestCheck;
          parser-rq-core-attempt-python-tests = parserRqCoreAttemptPythonTests;
          parser-rq-campaign-provenance-python-tests = parserRqCampaignProvenancePythonTests;
          parser-rq-predicate-hardening-capture-python-tests = parserRqPredicateHardeningCapturePythonTests;
          parser-rq-resource-capture-smoke = parserRqResourceCaptureSmokeCheck;
          parser-rq-predicate-hardening-capture-smoke = parserRqPredicateHardeningCaptureSmokeCheck;
          phase5-checkpoint = phase5CheckpointCheck;
        };

        devShells = {
          default = pkgs.mkShell {
            packages = devTools;

            RUST_SRC_PATH = "${rustToolchain}/lib/rustlib/src/rust/library";
            AB_SUDACHI_DICT = "${sudachiDictionaryFull}/share/sudachi/system.dic";
            AB_DUCKDB_BIN = "${pkgs.duckdb}/bin/duckdb";

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

        };

        formatter = pkgs.nixfmt;
      }
    );
}
