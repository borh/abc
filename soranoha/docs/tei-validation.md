# TEI validation

Soranoha owns the [TEI profile ODD](../schemas/tei-profile.odd). It defines two
validation layers: Relax NG checks XML structure, and Schematron checks constraints
such as nonempty ruby readings, resolved local references, header metadata, and
preservation-record shape. The generated [RNG](../schemas/tei-profile.rng) and
[Schematron](../schemas/tei-profile.sch) are derived from that ODD.

The `tei-validation.json` output records both layers, rule IDs, severities, and
whether warnings are allowed. Its profile identity is the ODD hash. Toolchain
metadata binds the exact ODD, RNG, and Schematron hashes, with generator identity
from [tei-profile-generation.json](../schemas/tei-profile-generation.json).
Generation metadata whose hashes disagree with the supplied profile is rejected.
Custom profiles may omit generator metadata; omitted fields are not emitted as
null placeholders.

Validation results accompany the published TEI under the existing include-and-flag
policy. A schema pass does not establish source fidelity or publication rights.
The independent [source-accountability report](source-accountability.md) records
lexical source occurrences and supports explicit interpretation claim accounting.
It does not certify exported semantics; assessment evaluation controls admission.

The pinned generation recipe uses TEI P5 4.11.0 and TEI Stylesheets 7.60.0 with
Saxon-HE. Run from the repository root after editing the ODD:

```sh
nix run .#regenerate-tei-profile
nix build .#checks.x86_64-linux.tei-profile-drift
just soranoha-tests
```

The drift check compares generated artifacts and their provenance with the
checked-in profile. Runtime validation uses Jing for Relax NG and ph-schematron's
XSLT backend for Schematron. Rule IDs are `snh-*` and are published in each
work's `tei-validation.json`; the [extension vocabulary](tei-vocabulary.md)
documents them alongside the `snh:` attributes they constrain.
