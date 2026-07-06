# Aozora CSV Slice Provenance

## list_person_all_extended_utf8_127.csv

- **Local snapshot:** `references/aozorabunko/index_pages/list_person_all_extended_utf8.zip`
- **Original upstream URL:** `https://www.aozora.gr.jp/index_pages/list_person_all_extended_utf8.zip`
- **Slice generated at:** 2026-04-28T00:00:00Z
- **Local ZIP SHA-256:** `5ea13273dd457f89af31de39f559ea9c6f5435d9bda1ae3d46d6a683b8bc3c92`
- **License:** CC0 (Aozora Bunko data is public domain)
- **Slice:** Header + the single row whose 作品ID column is `"000127"` (羅生門 by 芥川竜之介, single-author).

To reproduce from the local snapshot:

```bash
ORIG_ZIP="references/aozorabunko/index_pages/list_person_all_extended_utf8.zip"
unzip -p "$ORIG_ZIP" | head -1 > examples/v0/example-work/aozora-csv/list_person_all_extended_utf8_127.csv
unzip -p "$ORIG_ZIP" | awk -F'"' '$2 == "000127"' >> examples/v0/example-work/aozora-csv/list_person_all_extended_utf8_127.csv
```

To reproduce from upstream: download the URL above (the local snapshot's hash is recorded so divergence is detectable).
