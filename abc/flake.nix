{
  description = "Aozora Bunko Converter development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    local-pkgs = {
      url = "path:./nix/ci-empty-local-pkgs";
      flake = false;
    };
    tei-eaj-aozora-tei = {
      url = "github:TEI-EAJ/aozora_tei/77a675fc2771936f9544505d922d4cd45075338c";
      flake = false;
    };
    # Canonical Aozora Bunko source tree, pinned to the same revision as
    # ab-validator's `aozorabunko-src`. Supplies the authoritative work/person
    # catalog ZIP that `aozora-ingest` reads, so the ingester no longer depends
    # on a manually-located (and often stale) local CSV.
    aozorabunko-src = {
      url = "github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165";
      flake = false;
    };
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    crane = {
      url = "github:ipetkov/crane/v0.23.4";
    };
  };

  outputs =
    {
      nixpkgs,
      local-pkgs,
      tei-eaj-aozora-tei,
      aozorabunko-src,
      clj-nix,
      crane,
      ...
    }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
      localPkgsOverlay =
        final: _prev:
        import local-pkgs {
          pkgs = final;
          craneLib = crane.mkLib final;
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
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };
          # Offline clj-nix dependency cache (same lockfile the `checks` use), so
          # apps resolve their classpath hermetically instead of downloading from
          # Maven Central / cloning git deps at runtime.
          cljDepsCache = pkgs.mk-deps-cache {
            lockfile = ./deps-lock.json;
          };
          presentationFontTools = pkgs.python3.withPackages (ps: [
            ps.fonttools
            ps.brotli
          ]);
          presentationFontConfig = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.noto-fonts-cjk-sans ];
            impureFontDirectories = [ ];
            includes = [ ];
          };
          presentationLauncher = pkgs.writeShellScript "abc-presentation-diagrams" ''
            set -euo pipefail
            export HOME="${cljDepsCache}"
            export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
            export CLJ_CONFIG="${cljDepsCache}/.clojure"
            export GITLIBS="${cljDepsCache}/.gitlibs"
            presentationCache="$(mktemp -d)"
            trap 'rm -rf "$presentationCache"' EXIT
            export CLJ_CACHE="$presentationCache/clojure"
            export FONTCONFIG_FILE="${presentationFontConfig}"
            unset FONTCONFIG_PATH XDG_CONFIG_HOME XDG_CONFIG_DIRS XDG_DATA_DIRS
            export XDG_CACHE_HOME="$presentationCache/xdg-cache"
            export XDG_DATA_HOME="$presentationCache/xdg-data"
            mkdir -p "$CLJ_CACHE" "$XDG_CACHE_HOME" "$XDG_DATA_HOME"
            if [ -f abc/deps.edn ] && [ -f abc/docs/architecture-stages.edn ]; then
              cd abc
            elif [ -f deps.edn ] && [ -f docs/architecture-stages.edn ]; then
              :
            else
              echo "presentation-diagrams: run from the monorepo root or abc/" >&2
              exit 2
            fi
            export ABC_GRAPHVIZ_DOT="${pkgs.graphviz}/bin/dot"
            export ABC_FONTTOOLS_SUBSET="${presentationFontTools}/bin/pyftsubset"
            export ABC_PRESENTATION_FONT="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc"
            export ABC_PRESENTATION_FONT_DIR="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk"
            export DOTFONTPATH="$ABC_PRESENTATION_FONT_DIR"
            ${pkgs.clojure}/bin/clojure -M:abc/presentation-diagrams "$@"
          '';
          # Reliable, CWD-independent launcher for a `-M:<alias>` Clojure tool.
          # Resolves deps.edn + the abc `src` classpath from the pinned flake
          # source (not the caller's working directory), points Clojure at the
          # offline dependency cache, and gives it a writable cpcache dir. `env`
          # injects app-specific environment (catalog paths, schema paths, …).
          mkCljLauncher =
            {
              name,
              alias,
              env ? "",
            }:
            pkgs.writeShellScript name ''
              export HOME="${cljDepsCache}"
              export CLJ_CONFIG="${cljDepsCache}/.clojure"
              export GITLIBS="${cljDepsCache}/.gitlibs"
              export CLJ_CACHE="$(mktemp -d)"
              ${env}
              # Preserve the caller's working directory before cd'ing to the
              # pinned source root, so commands can resolve relative path args
              # (e.g. build-publication --output-root) against where the user ran.
              export ABC_INVOCATION_PWD="$PWD"
              cd ${./.}
              exec ${pkgs.clojure}/bin/clojure -J-Duser.home=${cljDepsCache} -M:${alias} "$@"
            '';
          mkCljApp =
            {
              name,
              alias,
              description,
              env ? "",
            }:
            {
              type = "app";
              program = toString (mkCljLauncher {
                inherit name alias env;
              });
              meta.description = description;
            };
          tei = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./schemas/tei-profile.odd;
          };
          teiEajAozoraReports =
            subcommand:
            pkgs.writeShellScript "abc-tei-eaj-aozora-${subcommand}-launcher" ''
              exec ${pkgs.python3}/bin/python ${./tools/tei_eaj_aozora_reports.py} \
                --compare-script ${./tools/tei_eaj_compare.py} \
                --tei-eaj-root ${tei-eaj-aozora-tei} \
                --source-rev 77a675fc2771936f9544505d922d4cd45075338c \
                ${subcommand} "$@"
            '';
          mkTeiEajAozoraReportApp =
            {
              subcommand,
              description,
            }:
            {
              type = "app";
              program = toString (teiEajAozoraReports subcommand);
              meta.description = description;
            };
        in
        {
          validate-design-bundle = mkCljApp {
            name = "abc-validate-design-bundle";
            alias = "abc/validate-design-bundle";
            description = "Validate ABC v0 design-bundle schemas and fixtures";
            env = ''
              export PATH="${
                pkgs.lib.makeBinPath [
                  pkgs.git-cliff
                  pkgs.libxml2
                ]
              }:''${PATH:-}"
              export TEI_SCHEMA_PATH="${tei.teiAllSchema}"
            '';
          };

          presentation-diagrams = {
            type = "app";
            program = toString presentationLauncher;
            meta.description = "Generate or drift-check academic presentation SVG diagrams";
          };

          soranoha = mkCljApp {
            name = "soranoha";
            alias = "abc/soranoha";
            description = "Soranoha snapshot publication command dispatcher";
          };

          adr-governance = mkCljApp {
            name = "abc-adr-governance";
            alias = "abc/adr-governance";
            description = "Validate ADR lifecycle, claims, and evidence governance";
          };

          materialize-import = mkCljApp {
            name = "abc-materialize-import";
            alias = "abc/materialize-import";
            description = "Materialize imported ab-validator output as ABC manifests";
          };

          materialize-publication = mkCljApp {
            name = "abc-materialize-publication";
            alias = "abc/materialize-publication";
            description = "Materialize parser-IR publication plaintext and TEI artifacts";
          };

          materialize-publications-batch = mkCljApp {
            name = "abc-materialize-publications-batch";
            alias = "abc/materialize-publication";
            description = "Materialize parser-IR publication plaintext and TEI artifacts from a batch JSON";
          };

          materialize-source-snapshot = mkCljApp {
            name = "abc-materialize-source-snapshot";
            alias = "abc/materialize-source-snapshot";
            description = "Materialize a source corpus snapshot and source manifests";
          };

          manifest-to-rdf = mkCljApp {
            name = "abc-manifest-to-rdf";
            alias = "abc/manifest-to-rdf";
            description = "Generate deterministic RDF/Turtle view from an ABC manifest";
          };

          aozora-ingest = mkCljApp {
            name = "abc-aozora-ingest";
            alias = "abc/aozora-ingest";
            description = "Build a metadata-record JSON from the canonical (pinned) Aozora catalog, or a --zip slice";
            env = ''
              export ABC_AOZORA_CATALOG_ZIP="${aozorabunko-src}/index_pages/list_person_all_extended_utf8.zip"
              export ABC_AOZORA_CATALOG_URL="github:aozorabunko/aozorabunko/0e9ea3e586eb0aa34039fabfc85a407d2f98b165"
            '';
          };

          validate-corpus = mkCljApp {
            name = "abc-validate-corpus";
            alias = "abc/validate-corpus";
            description = "Validate an ingested corpus directory through SHACL";
          };

          person-drift-history = mkCljApp {
            name = "abc-person-drift-history";
            alias = "abc/person-drift-history";
            description = "Audit generated corpus snapshots for conservative person split/merge candidates";
          };

          aozora-history-audit = mkCljApp {
            name = "abc-aozora-history-audit";
            alias = "abc/aozora-history-audit";
            description = "Extract two Aozora git refs, ingest them, validate current corpus, and report person drift candidates";
          };

          aozora-upstream-audit = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-aozora-upstream-audit" ''
                set -euo pipefail
                if [ ! -d examples/v0/example-persons ]; then
                  echo "aozora-upstream-audit: run from the ABC repository root; examples/v0/example-persons not found" >&2
                  exit 2
                fi
                exec ${pkgs.clojure}/bin/clojure -M:abc/aozora-history-audit \
                  --drift-persons-dir examples/v0/example-persons \
                  --fail-on-candidates \
                  --fail-on-drift-participant-updates \
                  "$@"
              ''
            );
            meta.description = "Default after-upstream Aozora audit with drift sidecars and failure gates enabled";
          };

          regenerate-tei-profile = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-regenerate-tei-profile" ''
                set -euo pipefail
                target="''${1:-$PWD}"
                if [ ! -d "$target/schemas" ]; then
                  echo "regenerate-tei-profile: $target/schemas not found" >&2
                  echo "usage: nix run .#regenerate-tei-profile [-- <repo-root>]" >&2
                  exit 2
                fi
                install -m 0644 "${tei.artifacts}/tei-profile.rng" "$target/schemas/tei-profile.rng"
                install -m 0644 "${tei.artifacts}/tei-profile.sch" "$target/schemas/tei-profile.sch"
                echo "Updated $target/schemas/tei-profile.{rng,sch} from schemas/tei-profile.odd."
              ''
            );
            meta.description = "Regenerate schemas/tei-profile.{rng,sch} from schemas/tei-profile.odd via TEI Stylesheets";
          };

          tei-eaj-aozora-tei-source = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-tei-eaj-aozora-tei-source" ''
                printf '%s\n' ${tei-eaj-aozora-tei}
              ''
            );
            meta.description = "Print the pinned TEI-EAJ/aozora_tei comparison source path";
          };

          tei-eaj-aozora-melos-report = mkTeiEajAozoraReportApp {
            subcommand = "melos";
            description = "Regenerate the ABC vs TEI-EAJ Melos comparison report";
          };

          tei-eaj-aozora-all-work-report = mkTeiEajAozoraReportApp {
            subcommand = "all-work";
            description = "Regenerate the all-work ABC vs TEI-EAJ comparison report";
          };

          tei-eaj-aozora-workset-json = mkTeiEajAozoraReportApp {
            subcommand = "workset-json";
            description = "Regenerate the machine-readable TEI-EAJ workset export";
          };

          tei-eaj-aozora-reports = mkTeiEajAozoraReportApp {
            subcommand = "all";
            description = "Regenerate TEI-EAJ comparison Markdown and JSON reports";
          };

          tei-eaj-aozora-alignment-probe = mkTeiEajAozoraReportApp {
            subcommand = "alignment-probe";
            description = "Regenerate TEI-EAJ alignment probes for a workset export";
          };

          tei-eaj-aozora-reports-with-probes = mkTeiEajAozoraReportApp {
            subcommand = "all-with-probes";
            description = "Regenerate TEI-EAJ comparison reports and attach alignment probes to the workset export";
          };
        }
      );

      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };
          cljDepsCache = pkgs.mk-deps-cache {
            lockfile = ./deps-lock.json;
          };
          presentationFontTools = pkgs.python3.withPackages (ps: [
            ps.fonttools
            ps.brotli
          ]);
          presentationFontConfig = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.noto-fonts-cjk-sans ];
            impureFontDirectories = [ ];
            includes = [ ];
          };
          tei = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./schemas/tei-profile.odd;
          };
          manifestLines =
            path:
            builtins.filter (line: line != "" && !(pkgs.lib.hasPrefix "#" line)) (
              pkgs.lib.splitString "\n" (builtins.readFile path)
            );
          contractSurfacePaths = manifestLines ./nix/contract-surface.txt;

          copyWritableSource = ''
            cp -R ${./.} source
            chmod -R u+w source
            cd source
          '';

          cljSandboxEnv = ''
            export HOME="${cljDepsCache}"
            export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
            export CLJ_CONFIG="$HOME/.clojure"
            export CLJ_CACHE="$TMPDIR/cp-cache"
            export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
            export GITLIBS="$HOME/.gitlibs"
          '';
        in
        {
          clj-nix-focused-tests =
            pkgs.runCommand "abc-clj-nix-focused-tests"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs._7zz
                  pkgs.git
                  pkgs.git-cliff
                  pkgs.libxml2
                ];
              }
              ''
                # Replay tests exercise the real CLI and pin coupling. Keep the
                # monorepo lock outside the writable abc source fixture, matching
                # the repository layout while preserving its store-read-only mode.
                cp ${../flake.lock} flake.lock
                ${copyWritableSource}
                ${cljSandboxEnv}
                # Keep TEI schema-backed tests active in the sandbox. The schema is
                # a pinned fixed-output artifact, so tests do not need network access.
                export TEI_SCHEMA_PATH="${tei.teiAllSchema}"

                clojure -M:test:kaocha -m kaocha.runner

                mkdir -p "$out"
                echo "ABC Clojure tests passed with clj-nix dependency cache (kaocha auto-discovery)." > "$out/result.txt"
              '';

          diagram-drift =
            pkgs.runCommand "abc-diagram-drift"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git-cliff
                  pkgs.libxml2
                ];
              }
              ''
                ${copyWritableSource}
                ${cljSandboxEnv}
                # Regenerate the two committed diagrams in memory and byte-compare
                # to the checked-in files; also runs the ADR header-hygiene and
                # architecture-stage lints. Any drift or lint problem exits non-zero.
                clojure -M:abc/diagrams --check

                mkdir -p "$out"
                echo "ADR + architecture diagrams current; header/sidecar/stage lints clean." > "$out/result.txt"
              '';

          presentation-diagram-drift =
            pkgs.runCommand "abc-presentation-diagram-drift"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.graphviz
                  pkgs.imagemagick
                  pkgs.librsvg
                  pkgs.noto-fonts-cjk-sans
                  presentationFontTools
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export GITLIBS="$HOME/.gitlibs"
                export FONTCONFIG_FILE="${presentationFontConfig}"
                unset FONTCONFIG_PATH XDG_CONFIG_HOME XDG_CONFIG_DIRS XDG_DATA_DIRS
                export XDG_CACHE_HOME="$TMPDIR/font-cache"
                export XDG_DATA_HOME="$TMPDIR/xdg-data"
                mkdir -p "$CLJ_CACHE" "$XDG_CACHE_HOME" "$XDG_DATA_HOME"
                grep -Fq \
                  '<dir>${pkgs.noto-fonts-cjk-sans}</dir>' \
                  "$FONTCONFIG_FILE"
                activeFontPaths="$TMPDIR/fontconfig-active-paths"
                sed -n \
                  -e '/^[[:space:]]*<dir[ >]/p' \
                  -e '/^[[:space:]]*<include[ >]/p' \
                  "$FONTCONFIG_FILE" > "$activeFontPaths"
                for forbidden in \
                  /etc/fonts \
                  /usr/share/fonts \
                  /usr/local/share/fonts \
                  '~/.nix-profile' \
                  /nix/var/nix/profiles
                do
                  if grep -Fq "$forbidden" "$activeFontPaths"; then
                    echo "presentation Fontconfig contains forbidden path: $forbidden" >&2
                    exit 1
                  fi
                done
                while IFS= read -r fontPath; do
                  fontPath="$(printf '%s\n' "$fontPath" | sed 's/^[[:space:]]*//')"
                  case "$fontPath" in
                    '<dir>/nix/store/'*'</dir>' | '<dir prefix="xdg">fonts</dir>')
                      ;;
                    *)
                      echo "presentation Fontconfig contains non-store path: $fontPath" >&2
                      exit 1
                      ;;
                  esac
                done < "$activeFontPaths"
                test -z "$(find "$XDG_DATA_HOME" -mindepth 1 -print -quit)"
                export ABC_GRAPHVIZ_DOT="${pkgs.graphviz}/bin/dot"
                export ABC_FONTTOOLS_SUBSET="${presentationFontTools}/bin/pyftsubset"
                export ABC_PRESENTATION_FONT="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc"
                export ABC_PRESENTATION_FONT_DIR="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk"
                export DOTFONTPATH="$ABC_PRESENTATION_FONT_DIR"
                clojure -M:abc/presentation-diagrams --check
                for svg in docs/figures/*.svg; do
                  png="$TMPDIR/$(basename "$svg" .svg).png"
                  rsvg-convert --width 1920 --height 1080 "$svg" --output "$png"
                  test "$(magick identify -format '%wx%h' "$png")" = "1920x1080"
                  test "$(magick identify -format '%k' "$png")" -gt 1
                done
                mkdir -p "$out"
                echo "Academic presentation DOT and SVG artifacts are current." > "$out/result.txt"
              '';

          clj-kondo =
            pkgs.runCommand "abc-clj-kondo"
              {
                nativeBuildInputs = [
                  pkgs.cljfmt
                  pkgs.clj-kondo
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                clj-kondo --fail-level error --lint src test
                cljfmt check src test
                mkdir -p "$out"
                echo "ABC focused Clojure lint and format checks passed." > "$out/result.txt"
              '';

          source-bundle-corpus =
            pkgs.runCommand "abc-source-bundle-corpus"
              {
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs._7zz
                ];
              }
              ''
                ${copyWritableSource}
                ${cljSandboxEnv}
                actual="$TMPDIR/aozorabunko-source-bundle-summary.json"
                clojure -M -m abc.tools.source-bundle-report \
                  ${aozorabunko-src} "$actual"
                cmp data/source-bundle/aozorabunko-0e9ea3e-summary.json "$actual"

                mkdir -p "$out"
                cp "$actual" "$out/summary.json"
              '';

          aat-parser-ir-probe-tests =
            pkgs.runCommand "abc-aat-parser-ir-probe-tests"
              {
                nativeBuildInputs = [
                  pkgs.python3
                ];
              }
              ''
                ${copyWritableSource}
                python -m unittest prototypes/aat-to-parser-ir-probe/test_probe_mapping.py

                mkdir -p "$out"
                echo "AAT parser-IR probe tests passed." > "$out/result.txt"
              '';

          contract-surface = pkgs.runCommand "abc-contract-surface-check" { } ''
            cd ${./.}
            for path in ${pkgs.lib.escapeShellArgs contractSurfacePaths}; do
              test -f "$path"
            done
            mkdir -p "$out"
            echo "ABC v0 contract source surface is present." > "$out/result.txt"
          '';

          tei-profile-drift = pkgs.runCommand "abc-tei-profile-drift" { } ''
            # ADR 0012: schemas/tei-profile.odd is canonical; the .rng and
            # .sch are reproducibly generated artifacts. This check
            # regenerates them from the ODD via TEI Stylesheets, applies
            # the same build-artifact canonicalization (timestamp strip +
            # ABC pattern-id rewrite), and byte-diffs against the
            # checked-in files. Any drift fails the build so the ODD
            # remains the single source of truth.
            diff -u ${./schemas/tei-profile.rng} ${tei.artifacts}/tei-profile.rng
            diff -u ${./schemas/tei-profile.sch} ${tei.artifacts}/tei-profile.sch
            mkdir -p "$out"
            echo "schemas/tei-profile.{rng,sch} match the ODD-derived artifacts." > "$out/result.txt"
          '';

          schema-contract-drift =
            pkgs.runCommand "abc-schema-contract-drift" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                python tools/check_schema_contracts_manifest.py
                mkdir -p "$out"
                echo "schemas/schema-contracts.json matches the checked-in schemas." > "$out/result.txt"
              '';

          tei-eaj-aozora-comparison-source = pkgs.runCommand "abc-tei-eaj-aozora-comparison-source" { } ''
            test -f ${tei-eaj-aozora-tei}/README.md
            test -f ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_tei.xml
            test -f ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_header_updated.xml
            grep -q "走れメロス" ${tei-eaj-aozora-tei}/data/complete/tei_lib_lv4/1567_tei.xml
            grep -q "Best Practice for TEI in Libraries" ${tei-eaj-aozora-tei}/README.md
            mkdir -p "$out"
            echo "Pinned TEI-EAJ/aozora_tei comparison source includes the Melos Level 4 fixtures." > "$out/result.txt"
          '';

          tei-eaj-comparison-tests =
            pkgs.runCommand "abc-tei-eaj-comparison-tests" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                cp ${./tools/tei_eaj_compare.py} tei_eaj_compare.py
                cp ${./tools/tei_eaj_aozora_reports.py} tei_eaj_aozora_reports.py
                cp ${./tools/test_tei_eaj_compare.py} test_tei_eaj_compare.py
                chmod u+w tei_eaj_compare.py tei_eaj_aozora_reports.py test_tei_eaj_compare.py
                python -m unittest discover -s . -p 'test_*.py'
                mkdir -p "$out"
                echo "TEI-EAJ comparison tests passed." > "$out/result.txt"
              '';

          tei-eaj-report-python-quality =
            pkgs.runCommand "abc-tei-eaj-report-python-quality"
              {
                nativeBuildInputs = [
                  pkgs.mypy
                  pkgs.python3
                  pkgs.ruff
                ];
              }
              ''
                set -euo pipefail
                cp ${./tools/tei_eaj_aozora_reports.py} tei_eaj_aozora_reports.py
                cp ${./tools/tei_eaj_compare.py} tei_eaj_compare.py
                ruff format --check --line-length 100 tei_eaj_aozora_reports.py
                ruff format --check --line-length 100 tei_eaj_compare.py
                ruff check --line-length 100 --ignore E501 tei_eaj_aozora_reports.py tei_eaj_compare.py
                mypy --cache-dir "$TMPDIR/mypy-cache" tei_eaj_aozora_reports.py
                mypy --cache-dir "$TMPDIR/mypy-cache" tei_eaj_compare.py
                mkdir -p "$out"
                echo "TEI-EAJ report tools pass ruff format/check and mypy." > "$out/result.txt"
              '';

          tei-eaj-aozora-report-generation =
            pkgs.runCommand "abc-tei-eaj-aozora-report-generation" { nativeBuildInputs = [ pkgs.python3 ]; }
              ''
                set -euo pipefail
                mkdir -p abc
                cat > abc/melos.xml <<'XML'
                <?xml version="1.0" encoding="UTF-8"?>
                <TEI xmlns="http://www.tei-c.org/ns/1.0">
                  <teiHeader>
                    <fileDesc>
                      <titleStmt><title>走れメロス</title></titleStmt>
                      <publicationStmt><idno type="aozora-work-id">001567</idno></publicationStmt>
                      <sourceDesc><p>source</p></sourceDesc>
                    </fileDesc>
                  </teiHeader>
                  <text><body><p>メロス</p></body></text>
                </TEI>
                XML
                python ${./tools/tei_eaj_aozora_reports.py} \
                  --compare-script ${./tools/tei_eaj_compare.py} \
                  --tei-eaj-root ${tei-eaj-aozora-tei} \
                  --source-rev 77a675fc2771936f9544505d922d4cd45075338c \
                  --abc-melos abc/melos.xml \
                  --abc-tei 1567=abc/melos.xml \
                  all melos.md all-work.md workset.json
                grep -q "TEI-EAJ Melos files found: 2" melos.md
                grep -q "TEI-EAJ XML files scanned: 62" all-work.md
                grep -q "Compared TEI-EAJ files: 2" all-work.md
                grep -q "Missing ABC counterparts: 55" all-work.md
                grep -q "TEI-EAJ files without candidate work IDs: 5" all-work.md
                python - <<'PY'
                import json

                with open("workset.json", encoding="utf-8") as fh:
                    export = json.load(fh)

                assert export["schema_version"] == "tei-eaj-aozora-workset-export-v1"
                assert export["summary"]["tei_eaj_file_count"] == 62
                assert export["summary"]["tei_eaj_work_id_count"] == 51
                assert export["summary"]["compared_file_count"] == 2
                assert export["summary"]["missing_counterpart_count"] == 55
                assert export["summary"]["no_work_id_count"] == 5
                assert export["missing_abc_counterpart_work_ids"]
                assert "1567" in export["candidate_work_ids"]
                assert any(row["tei_eaj_file"] == "data/complete/tei_lib_lv4/1567_tei.xml"
                           and row["comparison_status"] == "compared"
                           for row in export["files"])
                PY
                mkdir -p "$out"
                cp melos.md all-work.md workset.json "$out"/
                echo "TEI-EAJ comparison reports regenerate against the pinned source." > "$out/result.txt"
              '';

          adr-governance =
            pkgs.runCommand "abc-adr-governance"
              {
                nativeBuildInputs = [ pkgs.clojure ];
              }
              ''
                ${copyWritableSource}
                ${cljSandboxEnv}
                clojure -M:abc/adr-governance --mode audit

                mkdir -p "$out"
                echo "ADR lifecycle, dependency, claim, artifact, freshness, and evidence audit completed." > "$out/result.txt"
              '';
          swi-prolog-smoke =
            pkgs.runCommand "abc-swi-prolog-smoke" { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                # SWI-Prolog is the pinned CI dialect for Layer C (design spec §7.5).
                # chiasmus_verify prolog is exploratory/manual only; this pin makes
                # the dialect (negation, tabling, module syntax, CLI, repro) a build input.
                cat > "$TMPDIR/smoke.pl" <<'PL'
                :- initialization(main).
                main :- write('swipl ok'), nl, halt.
                PL
                swipl --quiet -t main -f "$TMPDIR/smoke.pl" > "$out"
              '';
          prolog-cross-artifact =
            pkgs.runCommand "abc-prolog-cross-artifact" { nativeBuildInputs = [ pkgs.swi-prolog ]; }
              ''
                set -euo pipefail
                cp -R ${./.} source
                chmod -R u+w source
                cd source
                # Reads COMMITTED fact files (regenerated & byte-checked by the
                # clj-nix-focused-tests derivation via the emitter). swipl only —
                # the Clojure byte-compare lives in clj-nix-focused-tests, which
                # has the offline cljDepsCache + deps.edn replacement setup. A plain
                # `clojure -e` here would fail the Nix sandbox's offline classpath.
                # Invocation: -g "(Goal -> halt(0) ; halt(1))" -t halt -s FILE...
                # halt/1 takes an integer, not a goal (the "halt(\+ Goal)" form
                # is a type error); -s (script-load) the fact + query files so the
                # predicates are visible to the goal.
                swipl -q -g "(\\+ violating_dup_identity(_) -> halt(0) ; halt(1))" -t halt \
                      -s fixtures/v0/facts/prolog/manifest_identity.pl \
                      -s docs/adr/manifest-identity-emitter-sanity.pl
                swipl -q -g "(\\+ dangling_person(_) -> halt(0) ; halt(1))" -t halt \
                      -s fixtures/v0/facts/prolog/person_records.pl \
                      -s fixtures/v0/facts/prolog/drift.pl \
                      -s docs/adr/person-id-referential-integrity.pl
                mkdir -p "$out"
                echo "Prolog cross-artifact gates hold: emitter sanity + person-id referential integrity (SWI-Prolog)." > "$out/result.txt"
              '';
        }
      );

      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              localPkgsOverlay
            ];
          };
          tei = import ./nix/tei-profile-artifacts.nix {
            inherit pkgs;
            odd = ./schemas/tei-profile.odd;
          };
          presentationFontTools = pkgs.python3.withPackages (ps: [
            ps.fonttools
            ps.brotli
          ]);
          presentationFontConfig = pkgs.makeFontsConf {
            fontDirectories = [ pkgs.noto-fonts-cjk-sans ];
            impureFontDirectories = [ ];
            includes = [ ];
          };

        in
        {
          default = pkgs.mkShell {
            TEI_SCHEMA_PATH = "${tei.teiAllSchema}";
            ABC_GRAPHVIZ_DOT = "${pkgs.graphviz}/bin/dot";
            ABC_FONTTOOLS_SUBSET = "${presentationFontTools}/bin/pyftsubset";
            ABC_PRESENTATION_FONT = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc";
            ABC_PRESENTATION_FONT_DIR = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk";
            DOTFONTPATH = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk";
            FONTCONFIG_FILE = "${presentationFontConfig}";
            shellHook = ''
              unset FONTCONFIG_PATH XDG_CONFIG_HOME XDG_CONFIG_DIRS XDG_DATA_DIRS
              presentationFontCache="$(mktemp -d)"
              export XDG_CACHE_HOME="$presentationFontCache/xdg-cache"
              export XDG_DATA_HOME="$presentationFontCache/xdg-data"
              mkdir -p "$XDG_CACHE_HOME" "$XDG_DATA_HOME"
              trap 'rm -rf "$presentationFontCache"' EXIT
            '';
            packages = with pkgs; [
              cljfmt
              clojure
              git
              git-cliff
              graphviz
              jdk21
              jq
              libxml2
              noto-fonts-cjk-sans
              presentationFontTools
            ];
          };

          validation = pkgs.mkShell {
            TEI_SCHEMA_PATH = "${tei.teiAllSchema}";
            packages = [
              pkgs.git
              pkgs.git-cliff
              pkgs.jq
              pkgs.libxml2
            ];
          };
        }
      );
    };
}
