from __future__ import annotations

import importlib.util
import json
import stat
import sys
from pathlib import Path


SCRIPT = Path(__file__).with_name("parser-rq-predicate-hardening-capture.py")
SPEC = importlib.util.spec_from_file_location("capture", SCRIPT)
assert SPEC and SPEC.loader
module = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(module)


def executable(path: Path, body: str) -> Path:
    path.write_text(f"#!{sys.executable}\n" + body)
    path.chmod(path.stat().st_mode | stat.S_IXUSR)
    return path


def test_capture_uses_explicit_closed_membership_and_one_invocation(tmp_path: Path) -> None:
    log = tmp_path / "calls"
    aozora = executable(
        tmp_path / "aozora",
        f"import json,sys\nopen({str(log)!r},'a').write(sys.argv[-1]+'\\n')\n"
        "print(json.dumps({'schemaVersion':3,'data':[]}))\n",
    )
    converter = executable(
        tmp_path / "converter",
        f"import json,sys\nopen({str(log)!r},'a').write('qualify\\n')\n"
        "args=sys.argv\nout=args[args.index('--record-out')+1]\n"
        "parser_out=args[args.index('--parser-ir-out')+1]\n"
        "open(out,'w').write(json.dumps({'status':'valid'}))\n"
        "open(parser_out,'w').write(json.dumps({'nodes':[]}))\n",
    )
    source_root = tmp_path / "source"
    source_root.mkdir()
    source = source_root / "source.txt"
    source.write_text("body")
    corpus = {"entries": [{"work_id": "work-a", "source_path": "source.txt"}]}
    output = tmp_path / "out"
    diagnostic, parser = module.capture(
        corpus,
        source_root=source_root,
        aozora=aozora,
        converter=converter,
        mapping=tmp_path / "mapping",
        abc_root=tmp_path,
        identity_ref="sha256:" + "a" * 64,
        parser_policy=tmp_path / "policy",
        store=tmp_path / "store",
        output=output,
    )
    assert diagnostic["expected_work_ids"] == ["work-a"]
    assert parser["expected_work_ids"] == ["work-a"]
    assert log.read_text().splitlines() == ["diagnostics", "aat", "qualify"]
    assert json.loads((output / "raw-diagnostics-index.json").read_text()) == diagnostic
    assert json.loads((output / "parser-ir" / "work-a.json").read_text()) == {"nodes": []}
    parser_ref = parser["records"][0]["parser_ir"]
    assert parser_ref["sha256"].startswith("sha256:")
    assert json.loads((tmp_path / "store" / parser_ref["locator"]).read_text()) == {"nodes": []}


def test_capture_rejects_duplicate_or_implicit_membership(tmp_path: Path) -> None:
    kwargs = dict(
        source_root=tmp_path,
        aozora=tmp_path / "aozora",
        converter=tmp_path / "converter",
        mapping=tmp_path / "mapping",
        abc_root=tmp_path,
        identity_ref="sha256:" + "a" * 64,
        parser_policy=tmp_path / "policy",
        store=tmp_path / "store",
        output=tmp_path / "out",
    )
    for corpus in ({}, {"entries": []}, {"entries": [{"work_id": "x"}, {"work_id": "x"}]}):
        try:
            module.capture(corpus, **kwargs)
        except ValueError:
            pass
        else:
            raise AssertionError("invalid membership was accepted")
