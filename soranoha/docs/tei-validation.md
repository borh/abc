# TEI validation

Soranoha owns the [TEI profile ODD](../schemas/tei-profile.odd). It defines two
validation layers: Relax NG validates XML structure, while Schematron enforces
semantic constraints such as nonempty ruby readings, resolved local references,
header metadata, and preservation-record shape. The generated
[RNG](../schemas/tei-profile.rng) and [Schematron](../schemas/tei-profile.sch)
are derived from that ODD.

The `tei-validation.json` output records three layers, not two. `relax_ng` and
`schematron` are the profile's; `well_formed_xml` is the serializer's own
report that it produced parseable XML, always passed, and it does not
contribute to the overall status. Each finding carries `rule_id`, `severity`,
the `layer` it came from, and `allowed`, which is false exactly for the
severities that fail the work. Its profile identity is the ODD hash. Toolchain
metadata binds the exact ODD, RNG, and Schematron hashes, with generator identity
from [tei-profile-generation.json](../schemas/tei-profile-generation.json).
Generation metadata whose hashes disagree with the supplied profile is rejected.
Custom profiles may omit generator metadata; omitted fields are not emitted as
null placeholders.

Validation results accompany the published TEI under the existing include-and-flag
policy. A schema pass establishes conformance to the profile. Source fidelity is
recorded separately by the independent
[source-accountability report](source-accountability.md), which records lexical
source occurrences and supports explicit interpretation claim accounting, and
admission is controlled by assessment evaluation.

The pinned generation recipe uses TEI P5 4.11.0 and TEI Stylesheets 7.60.0 with
Saxon-HE. Run from the repository root after editing the ODD:

```sh
nix run .#regenerate-tei-profile
just evidence-gate
```

`regenerate-tei-profile` rewrites the RNG, the Schematron and
`tei-profile-generation.json` in place; `evidence-gate` runs the drift check
and the test suite for the current system, so neither step names a platform.
An edit to the ODD's prose alone moves `odd_hash` and leaves the other two
hashes as they were.

The drift check compares generated artifacts and their provenance with the
checked-in profile. Runtime validation uses Jing for Relax NG and ph-schematron's
XSLT backend for Schematron. Schematron rule IDs are `snh-*` and are published
in each work's `tei-validation.json`. A Relax NG failure is also a finding, but its
`rule_id` is the constant `relax-ng` rather than an `snh-*` name, because the
grammar reports a structural violation without a named rule; a consumer that
filters on the `snh-` prefix drops those. The
[extension vocabulary](tei-vocabulary.md) documents the `snh-*` identifiers
alongside the `snh:` attributes they constrain.
