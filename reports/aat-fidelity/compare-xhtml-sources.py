#!/usr/bin/env python3
"""Compare upstream Aozora XHTML with locally generated aozora2html XHTML."""

from __future__ import annotations

import argparse
import hashlib
import re
from datetime import UTC, datetime
from pathlib import Path

import duckdb
from lxml import etree

XHTML_NS = "http://www.w3.org/1999/xhtml"
NS = {"x": XHTML_NS}


def sha256_hex(data: bytes) -> str:
    return hashlib.sha256(data).hexdigest()


def parse_xhtml(data: bytes) -> etree._Element:
    parser = etree.XMLParser(recover=True, ns_clean=False, resolve_entities=False)
    return etree.fromstring(data, parser=parser)


def find_main_text(root: etree._Element) -> etree._Element | None:
    candidates = root.xpath(
        ".//x:div[contains(concat(' ', normalize-space(@class), ' '), ' main_text ')]",
        namespaces=NS,
    )
    return candidates[0] if candidates else None


def normalize_text(value: str) -> str:
    return re.sub(r"\s+", "", value)


def main_text_value(data: bytes) -> str:
    root = parse_xhtml(data)
    main = find_main_text(root)
    if main is None:
        return ""
    return normalize_text("".join(main.itertext()))


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
          upstream_main_text_hash TEXT NOT NULL,
          local_main_text_hash TEXT NOT NULL,
          main_text_equal BOOLEAN NOT NULL,
          upstream_main_text TEXT NOT NULL,
          local_main_text TEXT NOT NULL,
          PRIMARY KEY (report_id, case_id)
        )
        """
    )


def load_observation(
    *,
    db_path: Path,
    report_id: str,
    case_id: str,
    upstream_xhtml: Path,
    local_xhtml: Path,
) -> None:
    upstream_bytes = upstream_xhtml.read_bytes()
    local_bytes = local_xhtml.read_bytes()
    upstream_main_text = main_text_value(upstream_bytes)
    local_main_text = main_text_value(local_bytes)

    db_path.parent.mkdir(parents=True, exist_ok=True)
    conn = duckdb.connect(str(db_path))
    create_tables(conn)
    conn.execute(
        "DELETE FROM fidelity_xhtml_observations WHERE report_id = ? AND case_id = ?",
        [report_id, case_id],
    )
    conn.execute(
        """
        INSERT INTO fidelity_xhtml_observations
          (report_id, case_id, loaded_at_utc, upstream_xhtml_path, local_xhtml_path,
           upstream_sha256, local_sha256, raw_equal,
           upstream_main_text_hash, local_main_text_hash, main_text_equal,
           upstream_main_text, local_main_text)
        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
        """,
        [
            report_id,
            case_id,
            datetime.now(UTC).replace(tzinfo=None),
            str(upstream_xhtml),
            str(local_xhtml),
            sha256_hex(upstream_bytes),
            sha256_hex(local_bytes),
            upstream_bytes == local_bytes,
            sha256_hex(upstream_main_text.encode("utf-8")),
            sha256_hex(local_main_text.encode("utf-8")),
            upstream_main_text == local_main_text,
            upstream_main_text,
            local_main_text,
        ],
    )
    conn.close()


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--db", type=Path, required=True)
    parser.add_argument("--report-id", default="cross-adapter")
    parser.add_argument("--case-id", required=True)
    parser.add_argument("--upstream-xhtml", type=Path, required=True)
    parser.add_argument("--local-xhtml", type=Path, required=True)
    args = parser.parse_args()

    load_observation(
        db_path=args.db,
        report_id=args.report_id,
        case_id=args.case_id,
        upstream_xhtml=args.upstream_xhtml,
        local_xhtml=args.local_xhtml,
    )
    print(
        f"loaded XHTML observation {args.case_id} into {args.db} as {args.report_id}"
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
