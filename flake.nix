{
  description = "Aozora Bunko Converter development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    local-pkgs = {
      url = "path:/home/bor/Projects/nix/pkgs";
      flake = false;
    };
  };

  outputs =
    { nixpkgs, local-pkgs, ... }:
    let
      systems = [
        "x86_64-linux"
        "aarch64-linux"
      ];

      forAllSystems = nixpkgs.lib.genAttrs systems;
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
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
          python = pkgs.python3.withPackages (pythonPackages: [
            pythonPackages.jsonschema
          ]);
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
              python
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
              python
            ];
          };
        }
      );
    };
}
