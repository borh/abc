import json
import subprocess
import sys
from pathlib import Path

SCRIPT = Path(__file__).resolve().parents[1] / "audit-aat-delta.py"
LEGACY_WARNING = ("aozora upstream spans are sanitized-source byte offsets; "
                  "line_start and line_end are synthesized as 1")


def meta(version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git aaa)",
         warnings=None):
    return {"adapter": "ab-aozora", "adapter_version": version,
            "parse_complete": True, "source_encoding": "utf-8",
            "source_hash": "sha256:x", "warnings": warnings or []}


def doc(blocks, **meta_kw):
    return {"version": 1, "work_id": "stdin", "blocks": blocks, "meta": meta(**meta_kw)}


def text(value, start=0, end=None):
    end = (start + len(value.encode())) if end is None else end
    return {"kind": "text", "value": value,
            "span": {"byte_start": start, "byte_end": end, "line_start": 1, "line_end": 1}}


def raw_marker(source, marker_kind, start=0):
    return {"kind": "raw", "source": source, "x-source-marker-kind": marker_kind,
            "x-provenance": "parser-derived",
            "span": {"byte_start": start, "byte_end": start + len(source.encode()),
                     "line_start": 1, "line_end": 1}}


def para(*content):
    return {"kind": "paragraph", "content": list(content)}


def write_dump(tmp_path, name, docs):
    d = tmp_path / name
    d.mkdir()
    for work, document in docs.items():
        (d / f"{work}.json").write_bytes(
            json.dumps(document, ensure_ascii=False, sort_keys=True).encode() + b"\n")
    return d


def run(mode, base, cand, tmp_path):
    out = tmp_path / "summary.json"
    proc = subprocess.run([sys.executable, str(SCRIPT), mode, str(base), str(cand),
                           "--summary-json", str(out)], capture_output=True, text=True)
    summary = json.loads(out.read_text()) if out.exists() else None
    return proc.returncode, summary, proc.stderr


OPEN = "［＃ここから罫囲み］"
CLOSE = "［＃ここで罫囲み終わり］"


def test_identical_dumps_pass(tmp_path):
    docs = {"w1": doc([para(text("あ\n"))])}
    base = write_dump(tmp_path, "a", docs)
    cand = write_dump(tmp_path, "b", docs)
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0 and summary["verdict"] == "PASS"


def test_identity_pointer_change_is_class3(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("あ\n"))],
        version="ab-aozora 0.2.0 aat-schema 1 facade 0.2.0 wire-schema 3 (git bbb)")})
    code, summary, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 0


def test_same_paragraph_pair_rewrites_to_container(tmp_path):
    # Baseline inner node "\n中身\n" (span 10..18); the grammar strips the
    # boundary newlines (value → "中身", span untouched — mirroring the Rust
    # strip helpers, which mutate values only) and strips the leading "\n"
    # of the post-close text.
    inner = text("\n中身\n", 10)          # span 10..18
    base = write_dump(tmp_path, "a", {"w1": doc([
        para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), inner,
             raw_marker(CLOSE, "containerClose", 40), text("\n後\n", 70))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([
        para(text("前\n")),
        {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]},
        para(text("後\n", 70, 75))])})
    code, summary, err = run("container-rewrite", base, cand, tmp_path)
    assert code == 0, (summary, err)
    assert summary["classes"]["rewritten"] == 1


def test_unrelated_change_in_marker_work_fails(tmp_path):
    inner = text("\n中身\n", 10)
    base = write_dump(tmp_path, "a", {"w1": doc([
        para(text("前\n"), raw_marker(OPEN, "containerOpen", 4), inner,
             raw_marker(CLOSE, "containerClose", 40))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([
        para(text("変わった\n")),   # unrelated text change smuggled in
        {"kind": "keigakomi_block", "children": [para(text("中身", 10, 18))]}])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_diff_without_markers_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(text("い\n"))])})
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


def test_missing_file_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(text("あ\n"))])})
    cand = write_dump(tmp_path, "b", {})
    (tmp_path / "b").mkdir(exist_ok=True)
    code, _, _ = run("container-rewrite", base, cand, tmp_path)
    assert code == 2


# --- span-confinement mode ---

def spanned(value, bs, be, ls, le):
    return {"kind": "text", "value": value,
            "span": {"byte_start": bs, "byte_end": be, "line_start": ls, "line_end": le}}


def test_span_only_change_passes(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))],
        warnings=[{"message": LEGACY_WARNING, "line": 1}])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 100, 104, 3, 3))])})
    code, summary, err = run("span-confinement", base, cand, tmp_path)
    assert code == 0, (summary, err)


def test_value_change_fails_in_span_mode(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("い\n", 0, 4, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_invalid_span_fails(tmp_path):
    base = write_dump(tmp_path, "a", {"w1": doc([para(spanned("あ\n", 0, 4, 1, 1))])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(spanned("あ\n", 4, 0, 1, 1))])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2


def test_wholesale_line1_synthesis_trips(tmp_path):
    nodes = [spanned(f"x{i}\n", i * 4, i * 4 + 3, 1, 1) for i in range(12)]
    base = write_dump(tmp_path, "a", {"w1": doc([para(*nodes)])})
    cand = write_dump(tmp_path, "b", {"w1": doc([para(*nodes)])})
    code, _, _ = run("span-confinement", base, cand, tmp_path)
    assert code == 2
