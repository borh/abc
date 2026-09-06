#!/usr/bin/env python
from __future__ import annotations

import argparse
import hashlib
import json
import re
import shlex
import subprocess
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import Any

from diagnostics_scoring import project_schema3_envelope


@dataclass
class Adapter:
    label: str
    mode: str
    command: list[str]
    diagnostics_command: list[str] | None = None


@dataclass
class Row:
    vector: str
    feature: str
    level: str
    adapter: str
    status: str
    failures: list[str]
    warnings: list[str]
    skips: list[str]


def parse_adapter(spec: str) -> Adapter:
    label, sep, rest = spec.partition("=")
    if not sep or not label or not rest:
        raise SystemExit(f"--adapter must be label=mode:command, got {spec!r}")
    mode, mode_sep, command = rest.partition(":")
    if not mode_sep or mode not in {"inspect", "aat"} or not command:
        raise SystemExit(f"--adapter mode must be inspect or aat, got {spec!r}")
    return Adapter(label=label, mode=mode, command=shlex.split(command))


def parse_adapter_diagnostics(spec: str) -> tuple[str, list[str]]:
    label, sep, command = spec.partition("=")
    if not sep or not label or not command:
        raise SystemExit(f"--adapter-diagnostics must be label=command, got {spec!r}")
    return label, shlex.split(command)


def load_vectors(vectors_dir: Path) -> list[dict[str, Any]]:
    vectors = []
    for path in sorted(vectors_dir.glob("*/vector.json")):
        data = json.loads(path.read_text(encoding="utf-8"))
        if data["name"] != path.parent.name:
            raise SystemExit(f"{path}: name must match directory")
        vectors.append(data)
    if not vectors:
        raise SystemExit(f"no vector.json files found under {vectors_dir}")
    return vectors


# --- inspect-mode comparison (reference `aozora inspect` adapter) ------------


def inspect(adapter: Adapter, kind: str, source: str) -> tuple[dict[str, Any] | None, str | None]:
    proc = subprocess.run(
        adapter.command + [kind, "-"],
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        value = json.loads(proc.stdout)
    except json.JSONDecodeError as error:
        return None, f"invalid JSON: {error}"
    if value.get("schemaVersion") != 2 or not isinstance(value.get("data"), list):
        return None, "unsupported inspect envelope"
    return value, None


# --- AAT-mode comparison: project AAT blocks -> ordered spec `kind` sequence -
#
# Adapters differ in span availability, so this comparison uses only the
# ordered sequence of projected node kinds.
#
# Two representations of the same construct must be handled: typed nodes (aozora-rs style) and flat
# `raw` marker nodes (aozora2 style, e.g. `raw source="BlockStart(Chitsuki)"`).

# AAT `style` style_type -> spec kind. Default (unlisted) decoration -> emphasis,
# because spec `emphasis` is a GENERIC decoration wrapper (bold/italic/box/accent/
# font-size all project to it); only 傍点 (boten) has its own spec kind `bouten`.
STYLE_TYPE_TO_KIND = {
    "boten": "bouten",  # aozora-rs / aozora2 spelling
    "bouten": "bouten",  # ab-aozora spelling
    "kaeriten": "kaeriten",
    "burasage": "indent",
    "jisage_line": "indent",
    "jizume": "indent",
}
STYLE_DEFAULT_KIND = "emphasis"

# AAT inline node `kind` (non-style, non-raw, non-text) -> spec kind.
# Unlisted inline kinds (e.g. warigaki, yokogumi) are recorded as unmapped.
INLINE_KIND_TO_KIND = {
    "ruby": "ruby",
    "gaiji": "gaiji",
    "tcy": "combineUpright",
    "figure": "illustration",
    "accent": "emphasis",
    "font_size": "emphasis",
    "keigakomi": "emphasis",
}

# AAT block-container kinds emit a paired containerOpen/containerClose in the spec
# node vocabulary (mirrors spec ［＃ここから…］ / ［＃ここで…終わり］).
CONTAINER_BLOCK_KINDS = {
    "jisage_block",
    "quote_block",
    "keigakomi_block",
    "yokogumi_block",
    "caption_block",
}


def run_aat(adapter: Adapter, source: str) -> tuple[dict[str, Any] | None, str | None]:
    proc = subprocess.run(
        adapter.command,
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        return json.loads(proc.stdout), None
    except json.JSONDecodeError as error:
        return None, f"invalid JSON: {error}"


def run_diagnostics(adapter: Adapter, source: str) -> tuple[list | None, str | None]:
    proc = subprocess.run(
        adapter.diagnostics_command,
        input=source,
        text=True,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        check=False,
    )
    if proc.returncode != 0:
        return None, proc.stderr.strip() or f"exit {proc.returncode}"
    try:
        projected = project_schema3_envelope(proc.stdout.encode("utf-8"))
    except ValueError as error:
        return None, str(error)
    return [
        {
            "code": entry.code,
            "severity": entry.severity,
            "span": {"start": entry.span_start, "end": entry.span_end},
        }
        for entry in projected
    ], None


def project_aat(blocks: list[dict[str, Any]]) -> tuple[list[str], set[str]]:
    """Flatten AAT blocks (depth-first, source order) to a spec `kind` sequence."""
    out: list[str] = []
    unmapped: set[str] = set()

    def emit_inline(node: dict[str, Any]) -> None:
        # `x-source-marker-kind` (an AAT extension some adapters emit, e.g.
        # ab-aozora) already carries the spec `kind` the adapter classified this
        # marker as -- trust it directly rather than re-deriving from raw text.
        marker = node.get("x-source-marker-kind")
        if marker:
            out.append(marker)
            for child in node.get("content", []) or []:
                emit_inline(child)
            return
        kind = node.get("kind")
        if kind == "text":
            return
        if kind == "style":
            out.append(STYLE_TYPE_TO_KIND.get(node.get("style_type"), STYLE_DEFAULT_KIND))
        elif kind == "raw":
            source = node.get("source", "")
            if source.startswith("BlockStart("):
                out.append("containerOpen")
            elif source.startswith("BlockEnd("):
                out.append("containerClose")
            else:
                out.append("directive")
            return
        elif kind in INLINE_KIND_TO_KIND:
            out.append(INLINE_KIND_TO_KIND[kind])
        else:
            unmapped.add(str(kind))
        for child in node.get("content", []) or []:
            emit_inline(child)

    def emit_block(block: dict[str, Any]) -> None:
        marker = block.get("x-source-marker-kind")
        if marker:
            out.append(marker)
            for child in block.get("content", []) or []:
                emit_inline(child)
            return
        kind = block.get("kind")
        if kind in CONTAINER_BLOCK_KINDS:
            out.append("containerOpen")
            for child in block.get("content", []) or []:
                emit_inline(child)
            out.append("containerClose")
            return
        if kind == "heading":
            out.append("heading")
        elif kind != "paragraph":
            unmapped.add(f"block:{kind}")
        for child in block.get("content", []) or []:
            emit_inline(child)

    for block in blocks:
        emit_block(block)
    return out, unmapped


def expected_kind_seq(vector: dict[str, Any]) -> list[str] | None:
    nodes = vector["expected"].get("nodes")
    if nodes is None:
        return None
    return [node["kind"] for node in nodes]


# --- span-deviation manifest --------------------------------------------------
#
# Pre-committed, hand-reviewed authorization for diagnostic-span deviations vs
# third-party vectors (rotation B decoded-source offsets). Fail-closed on both
# sides: unlisted divergence still fails, and unknown/unused manifest entries
# are a hard error (stale authorization must not silently linger).

MANIFEST_FIELDS = {
    "vector": str,
    "reason": str,
    "original_expected": list,
    "expected": list,
    "source_sha256": str,
}


def load_span_deviation_manifest(path) -> dict:
    entries = json.loads(Path(path).read_text(encoding="utf-8"))
    if not isinstance(entries, list):
        raise SystemExit("span-deviation-manifest must be a JSON list")
    manifest = {}
    for e in entries:
        if not isinstance(e, dict) or set(e) != set(MANIFEST_FIELDS):
            raise SystemExit(
                f"span-deviation-manifest entry fields must be exactly "
                f"{sorted(MANIFEST_FIELDS)}: {e!r}"
            )
        for key, typ in MANIFEST_FIELDS.items():
            if not isinstance(e[key], typ):
                raise SystemExit(f"span-deviation-manifest {key!r} must be {typ.__name__}: {e!r}")
        if not re.fullmatch(r"[0-9a-f]{64}", e["source_sha256"]):
            raise SystemExit(f"span-deviation-manifest source_sha256 must be 64-hex: {e!r}")
        if e["vector"] in manifest:
            raise SystemExit(f"span-deviation-manifest duplicate vector {e['vector']!r}")
        manifest[e["vector"]] = e
    return manifest


def check_manifest_consumed(manifest: dict, vector_names: set, used: set) -> None:
    unknown = sorted(set(manifest) - vector_names)
    if unknown:
        raise SystemExit(f"span-deviation-manifest entries match no loaded vector: {unknown}")
    unused = sorted(set(manifest) - used)
    if unused:
        raise SystemExit(
            f"span-deviation-manifest entries never exercised (stale authorization): {unused}"
        )


# --- scoring -----------------------------------------------------------------
#
# Projections an adapter cannot faithfully answer are recorded as explicit
# `skips` -- they do NOT mask a pass/fail on the scored projection(s):
#   inspect: scores nodes/pairs/diagnostics; serialize/html are skips.
#   aat:     scores nodes (kind sequence); pairs/diagnostics/serialize/html skip.


def evaluate(adapter: Adapter, vector: dict[str, Any], manifest=None, manifest_used=None) -> Row:
    failures: list[str] = []
    warnings: list[str] = []
    skips: list[str] = []
    scored = 0
    level = vector["meta"]["level"]
    expected = vector["expected"]

    if adapter.mode == "aat":
        exp_seq = expected_kind_seq(vector)
        if exp_seq is None:
            skips.append("nodes: vector has no expected.nodes")
        else:
            aat, error = run_aat(adapter, vector["source"])
            scored += 1
            if error:
                failures.append(f"nodes: adapter error: {error}")
            else:
                projected, unmapped = project_aat(aat.get("blocks", []) or [])
                if projected != exp_seq:
                    failures.append(
                        "nodes: kind sequence differs\n"
                        f"      expected: {exp_seq}\n"
                        f"      got:      {projected}"
                    )
                if unmapped:
                    warnings.append(f"unmapped AAT node kinds: {sorted(unmapped)}")
        for projection in ("pairs", "serialize", "html"):
            if expected.get(projection) is not None:
                skips.append(f"{projection}: not comparable for AAT adapter (kind-sequence only)")
        want_diag = expected.get("diagnostics")
        if want_diag is not None:
            if adapter.diagnostics_command is None:
                skips.append("diagnostics: not comparable for AAT adapter (kind-sequence only)")
            else:
                scored += 1
                entry = (manifest or {}).get(vector["name"])
                bad_manifest = False
                if entry is not None:
                    digest = hashlib.sha256(vector["source"].encode("utf-8")).hexdigest()
                    if digest != entry["source_sha256"]:
                        failures.append(
                            "diagnostics: manifest source hash mismatch "
                            "(vector changed since authorization)"
                        )
                        bad_manifest = True
                    elif entry["original_expected"] != want_diag:
                        failures.append(
                            "diagnostics: manifest original_expected is stale "
                            "(vector expectation changed since authorization)"
                        )
                        bad_manifest = True
                    else:
                        want_diag = entry["expected"]
                        if manifest_used is not None:
                            manifest_used.add(vector["name"])
                if not bad_manifest:
                    actual_diag, error = run_diagnostics(adapter, vector["source"])
                    if error:
                        failures.append(f"diagnostics: {error}")
                    elif actual_diag != want_diag:
                        failures.append(f"diagnostics: expected {want_diag!r}, got {actual_diag!r}")
    else:
        for projection in ("nodes", "pairs", "diagnostics"):
            want = expected.get(projection)
            if want is None:
                continue
            envelope, error = inspect(adapter, projection, vector["source"])
            scored += 1
            if error:
                failures.append(f"{projection}: {error}")
                continue
            actual = envelope["data"]
            if actual != want:
                failures.append(f"{projection}: expected {want!r}, got {actual!r}")
        for projection in ("serialize", "html"):
            if expected.get(projection) is not None:
                skips.append(
                    f"{projection}: not compared (inspect exposes node/pairs/diagnostics only)"
                )

    if failures and level == "must":
        status = "fail"
    elif failures:
        warnings.extend(failures)
        failures = []
        status = "warning"
    elif scored == 0:
        status = "skip"
    else:
        status = "pass"

    return Row(
        vector=vector["name"],
        feature=vector["meta"]["feature"],
        level=level,
        adapter=adapter.label,
        status=status,
        failures=failures,
        warnings=warnings,
        skips=skips,
    )


def render_markdown(rows: list[Row], vectors_dir: Path) -> str:
    out = [
        "# Aozora Notation-Spec Comparison",
        "",
        f"- vectors_dir: `{vectors_dir}`",
        f"- rows: {len(rows)}",
        "",
        "| vector | feature | level | adapter | status | failures | warnings | skips |",
        "| --- | --- | --- | --- | --- | --- | --- | --- |",
    ]
    for row in rows:
        out.append(
            "| {} | {} | {} | {} | {} | {} | {} | {} |".format(
                row.vector,
                row.feature,
                row.level,
                row.adapter,
                row.status,
                "<br>".join(row.failures),
                "<br>".join(row.warnings),
                "<br>".join(row.skips),
            )
        )
    out.append("")
    return "\n".join(out)


def main() -> None:
    parser = argparse.ArgumentParser()
    parser.add_argument("--vectors-dir", type=Path, required=True)
    parser.add_argument("--adapter", action="append", default=[])
    parser.add_argument("--adapter-diagnostics", action="append", default=[])
    parser.add_argument("--summary-json", type=Path, required=True)
    parser.add_argument("--report-md", type=Path, required=True)
    parser.add_argument("--span-deviation-manifest", type=Path, default=None)
    args = parser.parse_args()

    adapters = [parse_adapter(spec) for spec in args.adapter]
    if not adapters:
        raise SystemExit("at least one --adapter is required")

    for spec in args.adapter_diagnostics:
        label, command = parse_adapter_diagnostics(spec)
        matches = [a for a in adapters if a.label == label]
        if not matches or matches[0].mode != "aat":
            raise SystemExit(f"--adapter-diagnostics {label!r}: no aat adapter with that label")
        matches[0].diagnostics_command = command

    vectors = load_vectors(args.vectors_dir)
    manifest = None
    manifest_used: set = set()
    if args.span_deviation_manifest is not None:
        manifest = load_span_deviation_manifest(args.span_deviation_manifest)
    rows = [
        evaluate(adapter, vector, manifest=manifest, manifest_used=manifest_used)
        for vector in vectors
        for adapter in adapters
    ]
    if manifest is not None:
        check_manifest_consumed(manifest, {v["name"] for v in vectors}, manifest_used)
    summary = {
        "schema_version": 2,
        "vectors_dir": str(args.vectors_dir),
        "totals": {
            "vectors": len(vectors),
            "adapters": len(adapters),
            "rows": len(rows),
            "pass": sum(1 for row in rows if row.status == "pass"),
            "warning": sum(1 for row in rows if row.status == "warning"),
            "fail": sum(1 for row in rows if row.status == "fail"),
            "skip": sum(1 for row in rows if row.status == "skip"),
        },
        "rows": [asdict(row) for row in rows],
    }
    args.summary_json.parent.mkdir(parents=True, exist_ok=True)
    args.report_md.parent.mkdir(parents=True, exist_ok=True)
    args.summary_json.write_text(
        json.dumps(summary, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    args.report_md.write_text(render_markdown(rows, args.vectors_dir), encoding="utf-8")


if __name__ == "__main__":
    main()
