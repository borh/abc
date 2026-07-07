#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
out_dir="${AB_DB_ROOT:-/db/ab-validator}/aat-fidelity/upstream-xhtml-builder-smoke"
manifest="$out_dir/manifest.tsv"
metadata="$out_dir/metadata.csv"

rm -rf "$out_dir"
mkdir -p "$out_dir/cards/000001/files" "$out_dir/cards/000002/files"

python - <<PY
from pathlib import Path
from zipfile import ZipFile

root = Path("$out_dir")
(root / "cards/000001/files/1.txt").write_text("吾輩《わがはい》は猫である。\\n", encoding="utf-8")
(root / "cards/000002/files/2.txt").write_text("本文に［＃割り注］注釈［＃割り注終わり］が入る。\\n", encoding="utf-8")
for case in ("000001/files/1", "000002/files/2"):
    txt = root / "cards" / f"{case}.txt"
    with ZipFile(root / "cards" / f"{case}_ruby.zip", "w") as zf:
        zf.write(txt, txt.name)
for html in (
    root / "cards/000001/files/1_100.html",
    root / "cards/000002/files/2_200.html",
):
    html.write_text('<html><body><div class="main_text">fixture</div></body></html>', encoding="utf-8")
PY

cat > "$out_dir/cards/000001/card1.html" <<'HTML'
<html><body>
<h1>図書カード：No.1</h1>
<table class="download">
<tr><td>テキストファイル(ルビあり)</td><td>zip</td><td><a href="./files/1_ruby.zip">1_ruby.zip</a></td></tr>
<tr><td>XHTMLファイル</td><td>なし</td><td><a href="./files/1_100.html">1_100.html</a></td></tr>
</table>
</body></html>
HTML

cat > "$out_dir/cards/000002/card2.html" <<'HTML'
<html><body>
<h1>図書カード：No.2</h1>
<table class="download">
<tr><td>テキストファイル(ルビあり)</td><td>zip</td><td><a href="./files/2_ruby.zip">2_ruby.zip</a></td></tr>
<tr><td>XHTMLファイル</td><td>なし</td><td><a href="./files/2_200.html">2_200.html</a></td></tr>
</table>
</body></html>
HTML

python "$repo_root/reports/aat-fidelity/build-upstream-xhtml-manifest.py" \
  --card-url "$out_dir/cards/000001/card1.html" \
  --card-url "https://www.aozora.gr.jp/cards/000002/card2.html" \
  --aozora-root "$out_dir" \
  --sample-size 2 \
  --classify-source \
  --out-manifest "$manifest" \
  --out-metadata "$metadata"

test -s "$manifest"
test -s "$metadata"
rg -n '^case_id\tsource\tupstream_xhtml$' "$manifest"
rg -n '^000001_1\t.*/1_ruby.zip\t.*/1_100.html$' "$manifest"
rg -n '^000002_2\t.*/2_ruby.zip\t.*/2_200.html$' "$manifest"
rg -n '000001_1,.*ruby' "$metadata"
rg -n '000002_2,.*warichu' "$metadata"

echo "aat fidelity upstream xhtml manifest builder smoke ok: $out_dir"
