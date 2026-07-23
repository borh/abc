# Canonicalization Fixtures

Status: Draft

The fixture `manifest-identity-object.json` is intentionally ordered for human
reading. Implementations must compute ArtifactID from RFC 8785 JCS bytes, not
from the file's original whitespace or key order.

Expected canonical bytes are recorded in
`manifest-identity-object.canonical.json`. Because the object contains only
strings and nulls, `jq -cS` produces the same bytes expected from RFC 8785 JCS
for this fixture.

Do not use `jq -cS` as a general JCS implementation. This command is used here
only because the fixture contains no numbers, booleans, Unicode escaping edge
cases, or arrays.

Expected SHA-256 digest:

```text
3d1a9d081bcee4ca7c2ab821219ae6a21f785f9c2b89482e6db7a71412a17b94
```

Command used:

```bash
jq -cS . fixtures/canonicalization/manifest-identity-object.json | sha256sum
```

`array-ordering-negative-a.json` and `array-ordering-negative-b.json` contain
the same hashes in different array order. They must produce different JCS
bytes unless an ABC schema explicitly sorts that field before canonicalization;
JCS itself does not reorder arrays.
