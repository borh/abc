# Aozora Bunko corpus Converter

The Aozora Bunko corpus Converter (ABC) is a library and system for converting texts and metadata from the public [Aozora Bunko](https://en.wikipedia.org/wiki/Aozora_Bunko) [GitHub repository](https://github.com/aozorabunko/aozorabunko) into [TEI P5](http://www.tei-c.org/guidelines/p5/) XML and [Linked Open Data](https://en.wikipedia.org/wiki/Linked_data#Linked_open_data).

## Usage

Uses the [Clojure CLI tools](https://clojure.org/guides/getting_started). Tool entry points are defined as `:abc/*` aliases in `deps.edn`; the runnable tools live under `src/abc/tools/`. A Nix flake provides a reproducible dev shell and CI checks.

### CLI

```bash
# dev shell (clojure, git, git-cliff, jdk21, jq, libxml2)
nix develop

# run a tool, e.g. validate the design bundle
clojure -M:abc/validate-design-bundle
# or: nix run .#validate-design-bundle
```

### Interactive Access

```bash
nix develop -c clojure
# or: clojure   (if the CLI is installed outside Nix)
```

Then connect with your editor.

## Testing

Run all tests:

```bash
clojure -M:test:kaocha -m kaocha.runner
# or: ./bin/kaocha
```

In the Nix sandbox / CI, tests run via the `clj-nix-focused-tests` check with
clj-nix's offline classpath:

```bash
nix build .#checks.x86_64-linux.clj-nix-focused-tests
```

## License

Copyright © 2018 Bor Hodošček

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
