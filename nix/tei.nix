{ pkgs, tei-p5 }:

let
  # The reference and profile generator must use the same TEI P5 version;
  # monorepo-tei-version-coherence checks their declarations against the pin.
  teiP5Version = "4.11.0";
in
{
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
