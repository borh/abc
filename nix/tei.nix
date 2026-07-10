{ pkgs, tei-p5 }:

let
  # TEI P5 version. Deliberately duplicated in abc/nix/tei-profile-artifacts.nix
  # (separate flake — no shared constant across the path: boundary). Drift between
  # the two is caught by the `monorepo-tei-version-coherence` check, which is the
  # single source of truth for coherence. Bump both together.
  teiP5Version = "4.11.0";
  teiP5ReleaseTag = "P5_Release_${teiP5Version}";
in
{
  version = teiP5Version;
  releaseTag = teiP5ReleaseTag;

  reference =
    pkgs.runCommand "tei-p5-reference"
      {
        src = tei-p5;
        expectedVersion = teiP5Version;
      }
      ''
        mkdir -p "$out"
        for path in "$src"/P5/*; do
          ln -s "$path" "$out/$(basename "$path")"
        done

        actual_version="$(cat "$out/VERSION")"
        if [ "$actual_version" != "$expectedVersion" ]; then
          echo "Expected TEI P5 $expectedVersion, got $actual_version" >&2
          exit 1
        fi

        test -f "$out/Source/Specs/ruby.xml"
        test -f "$out/Source/Specs/hi.xml"
        test -f "$out/Source/Guidelines/en/HD-Header.xml"
      '';
}
