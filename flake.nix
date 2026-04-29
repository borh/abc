{
  description = "Aozora Bunko Converter development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    local-pkgs = {
      url = "path:/home/bor/Projects/nix/pkgs";
      flake = false;
    };
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, local-pkgs, clj-nix, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      apps = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              clj-nix.overlays.default
              (final: _prev: import local-pkgs { pkgs = final; })
            ];
          };
          tei-schema = pkgs.fetchurl {
            url = "https://www.tei-c.org/release/xml/tei/custom/schema/relaxng/tei_all.rng";
            hash = "sha256-7MSfAMN/SQtd9xa2cuPbpjXrHknM2I+5aYWUmN9CwIQ=";
          };
        in
        {
          validate-design-bundle = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-design-bundle" ''
                export PATH="${
                  pkgs.lib.makeBinPath [
                    pkgs.git-cliff
                    pkgs.libxml2
                  ]
                }:''${PATH:-}"
                export TEI_SCHEMA_PATH="${tei-schema}"
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-design-bundle "$@"
              ''
            );
            meta.description = "Validate ABC v0 design-bundle schemas and fixtures";
          };

          materialize-import = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-materialize-import" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/materialize-import "$@"
              ''
            );
            meta.description = "Materialize imported ab-validator output as ABC manifests";
          };

          manifest-to-rdf = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-manifest-to-rdf" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/manifest-to-rdf "$@"
              ''
            );
            meta.description = "Generate deterministic RDF/Turtle view from an ABC manifest";
          };

          aozora-ingest = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-aozora-ingest" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/aozora-ingest "$@"
              ''
            );
            meta.description = "Build a metadata-record JSON from an Aozora list_person_all_extended ZIP slice";
          };

          validate-corpus = {
            type = "app";
            program = toString (
              pkgs.writeShellScript "abc-validate-corpus" ''
                exec ${pkgs.clojure}/bin/clojure -M:abc/validate-corpus "$@"
              ''
            );
            meta.description = "Validate an ingested corpus directory through SHACL";
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
              (final: _prev: import local-pkgs { pkgs = final; })
            ];
          };
          cljDepsCache = pkgs.mk-deps-cache {
            lockfile = ./deps-lock.json;
          };
        in
        {
          clj-nix-focused-tests = pkgs.runCommand "abc-clj-nix-focused-tests"
            {
              nativeBuildInputs = [
                pkgs.clojure
                pkgs.git-cliff
                pkgs.libxml2
              ];
            }
            ''
              cp -R ${./.} source
              chmod -R u+w source
              cd source
              cp ${./nix/clj-nix-deps.edn} deps.edn

              export HOME="${cljDepsCache}"
              export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
              export CLJ_CONFIG="$HOME/.clojure"
              export CLJ_CACHE="$TMPDIR/cp-cache"
              export GITLIBS="$HOME/.gitlibs"

              # The Nix sandbox has no network access; tests that need
              # the upstream TEI RelaxNG schema (fetched at app build
              # time, not at test time) skip cleanly when this flag is
              # set. End-to-end TEI validation runs via the
              # `nix run .#validate-design-bundle` app, not here.
              export ABC_TEI_SCHEMA_SKIP=1

              clojure -M:abc/focused-test

              mkdir -p "$out"
              echo "ABC focused Clojure tests passed with clj-nix dependency cache." > "$out/result.txt"
            '';

          contract-surface = pkgs.runCommand "abc-contract-surface-check" { } ''
            test -f ${./resources/abc/ndc9.edn.xz}
            test -f ${./nix/clj-nix-deps.edn}
            test -f ${./deps-lock.json}
            test -f ${./src/abc/annotation/schema.clj}
            test -f ${./src/abc/text.clj}
            test -f ${./src/abc/ndc.clj}
            test -f ${./src/abc/tools/hash.clj}
            test -f ${./src/abc/tools/jcs.clj}
            test -f ${./src/abc/tools/json.clj}
            test -f ${./src/abc/tools/schema.clj}
            test -f ${./src/abc/tools/aozora_csv.clj}
            test -f ${./src/abc/tools/aozora_ingest.clj}
            test -f ${./src/abc/tools/manifest_index.clj}
            test -f ${./src/abc/tools/logging.clj}
            test -f ${./src/abc/tools/manifest_to_rdf.clj}
            test -f ${./src/abc/tools/metadata_record.clj}
            test -f ${./src/abc/tools/person_record.clj}
            test -f ${./src/abc/tools/rdf_prefixes.clj}
            test -f ${./src/abc/tools/schematron.clj}
            test -f ${./src/abc/tools/shacl.clj}
            test -f ${./src/abc/tools/tei.clj}
            test -f ${./src/abc/tools/tei_header.clj}
            test -f ${./src/abc/tools/validate_design_bundle.clj}
            test -f ${./schemas/metadata-record.schema.json}
            test -f ${./schemas/person-record.schema.json}
            test -f ${./schemas/tei-profile.rng}
            test -f ${./schemas/tei-profile.sch}
            test -f ${./schemas/tei-validation-result.schema.json}
            test -f ${./test/abc/tools/hash_test.clj}
            test -f ${./test/abc/tools/jcs_test.clj}
            test -f ${./test/abc/tools/schema_test.clj}
            test -f ${./test/abc/tools/manifest_index_test.clj}
            test -f ${./test/abc/tools/manifest_to_rdf_test.clj}
            test -f ${./test/abc/tools/materialize_import_test.clj}
            test -f ${./test/abc/tools/aozora_csv_test.clj}
            test -f ${./test/abc/tools/metadata_record_test.clj}
            test -f ${./test/abc/tools/person_record_test.clj}
            test -f ${./test/abc/tools/aozora_ingest_test.clj}
            test -f ${./test/abc/tools/schematron_test.clj}
            test -f ${./test/abc/tools/shacl_test.clj}
            test -f ${./test/abc/tools/tei_test.clj}
            test -f ${./test/abc/tools/tei_header_test.clj}
            test -f ${./examples/v0/example-work/metadata-record.json}
            test -f ${./examples/v0/example-work/metadata-record.ttl}
            test -f ${./examples/v0/example-persons/000879.json}
            test -f ${./test/abc/tools/validate_design_bundle_test.clj}
            test -f ${./test/abc/annotation_schema_test.clj}
            test -f ${./test/abc/text_test.clj}
            test -f ${./test/abc/ndc_test.clj}
            mkdir -p "$out"
            echo "ABC v0 contract source surface is present." > "$out/result.txt"
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
              (final: _prev: import local-pkgs { pkgs = final; })
            ];
          };

          mecab = pkgs.mecab.overrideAttrs (oldAttrs: {
            postInstall = (oldAttrs.postInstall or "") + ''
              rm -f $out/lib/mecab/dic/unidi-cwj $out/lib/mecab/dic/unidic-cwj
              ln -s ${pkgs.unidic-cwj}/share/mecab/dic/unidic-cwj $out/lib/mecab/dic/unidic-cwj
            '';
          });
          unidic = pkgs.unidic-cwj;
          mecabDicDir = "${unidic}/share/mecab/dic/unidic-cwj";
        in
        {
          default = pkgs.mkShell {
            packages = with pkgs; [
              clojure
              git
              git-cliff
              jdk21
              jq
              mecab
              unidic
              libxml2
            ];

            LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath [
              mecab
              pkgs.systemd
            ];

            MECABRC = "${mecab}/etc/mecabrc";
            MECAB_DICDIR = mecabDicDir;

            shellHook = ''
              export LD_LIBRARY_PATH="${
                pkgs.lib.makeLibraryPath [
                  mecab
                  pkgs.systemd
                ]
              }:''${LD_LIBRARY_PATH:-}"
              export MECABRC="${mecab}/etc/mecabrc"
              export MECAB_DICDIR="${mecabDicDir}"
            '';
          };

          validation = pkgs.mkShell {
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
