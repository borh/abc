# Aozora Bunko corpus Converter

The Aozora Bunko corpus Converter (ABC) is a library and system for converting texts and metadata from the public [Aozora Bunko](https://en.wikipedia.org/wiki/Aozora_Bunko) [GitHub repository](https://github.com/aozorabunko/aozorabunko) into [TEI P5](http://www.tei-c.org/guidelines/p5/) XML and [Linked Open Data](https://en.wikipedia.org/wiki/Linked_data#Linked_open_data).

## Usage

Use either the build tool [Boot](http://boot-clj.com/) or the official [Clojure CLI tools](https://clojure.org/guides/getting_started).

### CLI

Extracting LOD from a local clone of the Aozora Bunko repository:

```bash
clojure -m abc.core -i ../../Dependencies/aozorabunko -o dist
```

This will save a Turtle-formatted file `aozora-bunko.ttl` under `dist/`.

### Interactive Access

For CLI tools:

```bash
clj
```

For boot:

```bash
boot dev
```

And then connect with your favorite editor.

## Testing

Run all tests:

```bash
clojure -Atest:runner
```

Continuously running test process for use during development:

```bash
boot watch deps-test bat-test
```

## License

Copyright © 2018 Bor Hodošček

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
