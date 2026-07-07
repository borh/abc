#!/usr/bin/env python
"""Compare upstream Aozora XHTML with locally generated aozora2html XHTML."""

from __future__ import annotations

import argparse
import copy
import hashlib
import json
import re
from datetime import UTC, datetime
from pathlib import Path

import duckdb
from lxml import etree

XHTML_NS = "http://www.w3.org/1999/xhtml"
NS = {"x": XHTML_NS}
HTML_CHARSET_RE = re.compile(rb"charset\s*=\s*['\"]?([A-Za-z0-9._-]+)", re.IGNORECASE)
XML_ENCODING_RE = re.compile(rb"^\s*<\?xml[^>]*encoding\s*=", re.IGNORECASE)
NON_MAIN_TEXT_CLASSES = {
    "bibliographical_information",
    "notation_notes",
}


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def parser_input(data: bytes) -> bytes:
    if XML_ENCODING_RE.search(data[:256]):
        return data
    match = HTML_CHARSET_RE.search(data[:4096])
    if not match:
        return data

    charset = match.group(1).decode("ascii", errors="ignore").lower()
    if charset in {"shift_jis", "shift-jis", "sjis", "windows-31j", "cp932"}:
        charset = "cp932"
    try:
        return data.decode(charset).encode("utf-8")
    except (LookupError, UnicodeDecodeError):
        return data


def parse_xhtml(data: bytes) -> etree._Element | None:
    parser = etree.XMLParser(recover=True, ns_clean=False, resolve_entities=False)
    try:
        return etree.fromstring(parser_input(data), parser=parser)
    except etree.XMLSyntaxError:
        return None


def find_main_text(root: etree._Element) -> etree._Element | None:
    candidates = root.xpath(
        ".//x:div[contains(concat(' ', normalize-space(@class), ' '), ' main_text ')]",
        namespaces=NS,
    )
    return candidates[0] if candidates else None


def normalize_text(value: str) -> str:
    return re.sub(r"\s+", "", value)


def class_names(node: etree._Element) -> set[str]:
    return set((node.get("class") or "").split())


def prune_non_body_descendants(main: etree._Element) -> etree._Element:
    pruned = copy.deepcopy(main)
    for descendant in list(pruned.xpath(".//*[local-name()='div']")):
        if class_names(descendant) & NON_MAIN_TEXT_CLASSES:
            parent = descendant.getparent()
            if parent is not None:
                parent.remove(descendant)
    return pruned


def main_text_value(data: bytes) -> str:
    root = parse_xhtml(data)
    if root is None:
        return ""
    main = find_main_text(root)
    if main is None:
        return ""
    main = prune_non_body_descendants(main)
    return normalize_text("".join(main.itertext()))


def is_adapter_error_payload(data: bytes) -> bool:
    try:
        payload = json.loads(data.decode("utf-8"))
    except (UnicodeDecodeError, json.JSONDecodeError):
        return False
    if not isinstance(payload, dict):
        return False
    meta = payload.get("meta")
    return isinstance(meta, dict) and meta.get("parse_complete") is False


def create_tables(conn: duckdb.DuckDBPyConnection) -> None:
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS fidelity_xhtml_observations (
          report_id TEXT NOT NULL,
          case_id TEXT NOT NULL,
          loaded_at_utc TIMESTAMP NOT NULL,
          upstream_xhtml_path TEXT NOT NULL,
          local_xhtml_path TEXT NOT NULL,
          upstream_sha256 TEXT NOT NULL,
          local_sha256 TEXT NOT NULL,
          raw_equal BOOLEAN NOT NULL,
          comparison_status TEXT NOT NULL,
          upstream_main_text_hash TEXT NOT NULL,
          local_main_text_hash TEXT NOT NULL,
          main_text_equal BOOLEAN NOT NULL,
          rendered_body_proxy_eligible BOOLEAN NOT NULL,
          proxy_basis TEXT NOT NULL,
          upstream_main_text TEXT NOT NULL,
          local_main_text TEXT NOT NULL,
          card_url TEXT NOT NULL,
          source_url TEXT NOT NULL,
          upstream_url TEXT NOT NULL,
          feature_tags TEXT NOT NULL,
          manifest_status TEXT NOT NULL,
          first_diff_index INTEGER NOT NULL,
          upstream_diff_context TEXT NOT NULL,
          local_diff_context TEXT NOT NULL,
          PRIMARY KEY (report_id, case_id)
        )
        """
    )
    existing_columns = {
        row[1]
        for row in conn.execute("PRAGMA table_info('fidelity_xhtml_observations')").fetchall()
    }
    if "comparison_status" not in existing_columns:
        conn.execute(
            "ALTER TABLE fidelity_xhtml_observations "
            "ADD COLUMN comparison_status TEXT DEFAULT 'unknown'"
        )
    for column in ("card_url", "source_url", "upstream_url", "feature_tags", "manifest_status"):
        if column not in existing_columns:
            conn.execute(
                f"ALTER TABLE fidelity_xhtml_observations ADD COLUMN {column} TEXT DEFAULT ''"
            )
    if "first_diff_index" not in existing_columns:
        conn.execute(
            "ALTER TABLE fidelity_xhtml_observations ADD COLUMN first_diff_index INTEGER DEFAULT -1"
        )
    for column in ("upstream_diff_context", "local_diff_context"):
        if column not in existing_columns:
            conn.execute(
                f"ALTER TABLE fidelity_xhtml_observations ADD COLUMN {column} TEXT DEFAULT ''"
            )
    if "rendered_body_proxy_eligible" not in existing_columns:
        conn.execute(
            "ALTER TABLE fidelity_xhtml_observations "
            "ADD COLUMN rendered_body_proxy_eligible BOOLEAN DEFAULT false"
        )
    if "proxy_basis" not in existing_columns:
        conn.execute(
            "ALTER TABLE fidelity_xhtml_observations "
            "ADD COLUMN proxy_basis TEXT DEFAULT 'not_eligible'"
        )


def first_diff(upstream: str, local: str, *, context_chars: int = 24) -> tuple[int, str, str]:
    if upstream == local:
        return -1, "", ""
    limit = min(len(upstream), len(local))
    index = 0
    while index < limit and upstream[index] == local[index]:
        index += 1
    start = max(index - context_chars, 0)
    end = index + context_chars
    return index, upstream[start:end], local[start:end]


def comparison_status(
    *,
    raw_equal: bool,
    upstream_main_text: str,
    local_main_text: str,
    upstream_adapter_error: bool = False,
    local_adapter_error: bool = False,
    upstream_parse_ok: bool = True,
    local_parse_ok: bool = True,
) -> str:
    if upstream_adapter_error:
        return "upstream_adapter_error"
    if local_adapter_error:
        return "local_adapter_error"
    if not upstream_parse_ok:
        return "upstream_parse_error"
    if not local_parse_ok:
        return "local_parse_error"
    if raw_equal:
        return "raw_equal"
    if not upstream_main_text and not local_main_text:
        return "both_missing_main_text"
    if not upstream_main_text:
        return "upstream_missing_main_text"
    if not local_main_text:
        return "local_missing_main_text"
    if upstream_main_text == local_main_text:
        return "main_text_equal"
    return "main_text_mismatch"


def proxy_basis(status: str) -> str:
    if status == "raw_equal":
        return "raw_xhtml_equal"
    if status == "main_text_equal":
        return "normalized_main_text_equal"
    return "not_eligible"


def load_observation(
    *,
    db_path: Path,
    report_id: str,
    case_id: str,
    upstream_xhtml: Path,
    local_xhtml: Path,
    card_url: str = "",
    source_url: str = "",
    upstream_url: str = "",
    feature_tags: str = "",
    manifest_status: str = "",
) -> None:
    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = duckdb.connect(str(db_path))
    create_tables(conn)
    load_observation_with_conn(
        conn=conn,
        report_id=report_id,
        case_id=case_id,
        upstream_xhtml=upstream_xhtml,
        local_xhtml=local_xhtml,
        card_url=card_url,
        source_url=source_url,
        upstream_url=upstream_url,
        feature_tags=feature_tags,
        manifest_status=manifest_status,
    )
    conn.close()


def load_observation_with_conn(
    *,
    conn: duckdb.DuckDBPyConnection,
    report_id: str,
    case_id: str,
    upstream_xhtml: Path,
    local_xhtml: Path,
    card_url: str = "",
    source_url: str = "",
    upstream_url: str = "",
    feature_tags: str = "",
    manifest_status: str = "",
) -> None:
    upstream_bytes = upstream_xhtml.read_bytes()
    local_bytes = local_xhtml.read_bytes()
    upstream_adapter_error = is_adapter_error_payload(upstream_bytes)
    local_adapter_error = is_adapter_error_payload(local_bytes)
    upstream_parse_ok = parse_xhtml(upstream_bytes) is not None
    local_parse_ok = parse_xhtml(local_bytes) is not None
    upstream_main_text = main_text_value(upstream_bytes)
    local_main_text = main_text_value(local_bytes)
    raw_equal = upstream_bytes == local_bytes
    main_text_equal = upstream_main_text == local_main_text
    status = comparison_status(
        raw_equal=raw_equal,
        upstream_main_text=upstream_main_text,
        local_main_text=local_main_text,
        upstream_adapter_error=upstream_adapter_error,
        local_adapter_error=local_adapter_error,
        upstream_parse_ok=upstream_parse_ok,
        local_parse_ok=local_parse_ok,
    )
    basis = proxy_basis(status)
    diff_index, upstream_diff_context, local_diff_context = first_diff(
        upstream_main_text, local_main_text
    )

    conn.execute(
        "DELETE FROM fidelity_xhtml_observations WHERE report_id = ? AND case_id = ?",
        [report_id, case_id],
    )
    conn.execute(
        """
        INSERT INTO fidelity_xhtml_observations
          (report_id, case_id, loaded_at_utc, upstream_xhtml_path, local_xhtml_path,
           upstream_sha256, local_sha256, raw_equal, comparison_status,
           upstream_main_text_hash, local_main_text_hash, main_text_equal,
           rendered_body_proxy_eligible, proxy_basis, upstream_main_text,
           local_main_text, card_url, source_url, upstream_url,
           feature_tags, manifest_status, first_diff_index, upstream_diff_context,
           local_diff_context)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        [
            report_id,
            case_id,
            datetime.now(UTC).replace(tzinfo=None),
            str(upstream_xhtml),
            str(local_xhtml),
            sha256_hex(upstream_bytes),
            sha256_hex(local_bytes),
            raw_equal,
            status,
            sha256_hex(upstream_main_text.encode("utf-8")),
            sha256_hex(local_main_text.encode("utf-8")),
            main_text_equal,
            basis != "not_eligible",
            basis,
            upstream_main_text,
            local_main_text,
            card_url,
            source_url,
            upstream_url,
            feature_tags,
            manifest_status,
            diff_index,
            upstream_diff_context,
            local_diff_context,
        ],
    )


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=Path, required=True)
    parser.add_argument("--report-id", default="cross-adapter")
    parser.add_argument("--case-id", required=True)
    parser.add_argument("--upstream-xhtml", type=Path, required=True)
    parser.add_argument("--local-xhtml", type=Path, required=True)
    parser.add_argument("--card-url", default="")
    parser.add_argument("--source-url", default="")
    parser.add_argument("--upstream-url", default="")
    parser.add_argument("--feature-tags", default="")
    parser.add_argument("--manifest-status", default="")
    args = parser.parse_args()

    load_observation(
        db_path=args.db,
        report_id=args.report_id,
        case_id=args.case_id,
        upstream_xhtml=args.upstream_xhtml,
        local_xhtml=args.local_xhtml,
        card_url=args.card_url,
        source_url=args.source_url,
        upstream_url=args.upstream_url,
        feature_tags=args.feature_tags,
        manifest_status=args.manifest_status,
    )
    print(f"loaded XHTML observation {args.case_id} into {args.db} as {args.report_id}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
