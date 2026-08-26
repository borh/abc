{
  description = "Soranoha publication kernel and snh protocol implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    clj-nix = {
      url = "github:jlesquembre/clj-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs =
    { nixpkgs, clj-nix, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];
      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      checks = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [ clj-nix.overlays.default ];
          };
          # Offline clj-nix dependency cache so the check resolves its
          # classpath hermetically instead of downloading from Maven Central.
          cljDepsCache = pkgs.mk-deps-cache { lockfile = ./deps-lock.json; };
        in
        {
          clj-nix-tests =
            pkgs.runCommand "soranoha-clj-nix-tests"
              {
                # git backs the repository-view and publication-transaction
                # test fixtures (local origin + clones)
                nativeBuildInputs = [
                  pkgs.clojure
                  pkgs.git
                ];
              }
              ''
                cp -R ${./.} source
                chmod -R u+w source
                # The canonicalization suite binds the kernel's canonicalizer
                # to abc's shared cross-language vectors at this relative path
                # (the two copies must never diverge byte-wise).
                mkdir -p abc/test/fixtures/canonicalization
                cp ${../abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json} \
                  abc/test/fixtures/canonicalization/rfc8785-safe-integer-domain-abc-v1-vectors.json
                cd source

                export HOME="${cljDepsCache}"
                export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
                export CLJ_CONFIG="$HOME/.clojure"
                export CLJ_CACHE="$TMPDIR/cp-cache"
                export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
                export GITLIBS="$HOME/.gitlibs"

                clojure -M:test

                mkdir -p "$out"
                echo "Soranoha kernel + snh conformance suite passed (kaocha auto-discovery)." > "$out/result.txt"
              '';
        }
      );
    };
}
