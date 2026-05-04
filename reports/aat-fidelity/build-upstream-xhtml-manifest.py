#!/usr/bin/env python3
"""Build a manifest of Aozora source ZIP and upstream XHTML pairs.

The output manifest is intentionally compatible with
run-upstream-xhtml-observations.sh: case_id, source, upstream_xhtml.
Metadata needed for auditing and stratification is written separately.
"""

from __future__ import annotations

import argparse
import csv
import hashlib
import io
import random
import re
import sys
import urllib.parse
import urllib.request
from dataclasses import dataclass
from html.parser import HTMLParser
from pathlib import Path
from zipfile import ZipFile


AOZORA_ROOT = "https://www.aozora.gr.jp/"
FEATURE_PATTERNS: list[tuple[str, re.Pattern[str]]] = [
    ("ruby", re.compile(r"《[^》]+》|｜[^《\n]+《[^》]+》")),
    ("gaiji", re.compile(r"※［＃")),
    ("warichu", re.compile(r"割り注")),
    ("inline_annotation", re.compile(r"傍点|傍線|注記|左に|右に|割り注")),
    ("heading", re.compile(r"見出し")),
    ("layout", re.compile(r"字下げ|字上げ|地付き|地から|字詰め|ぶら下げ|横組み|縦中横")),
    ("media", re.compile(r"キャプション|[（(][^）)\n]+\.(?:png|jpe?g|gif)[^）)\n]*[）)]入る")),
]
STRATA = ["inline_annotation", "layout", "gaiji", "media", "ruby", "heading"]


@dataclass(frozen=True)
class Page:
    url: str
    body: str


@dataclass(frozen=True)
class Pair:
    case_id: str
    source: str
    upstream_xhtml: str
    card_url: str
    feature_tags: tuple[str, ...]
    status: str


class LinkParser(HTMLParser):
    def __init__(self) -> None:
        super().__init__()
        self.links: list[str] = []

    def handle_starttag(self, tag: str, attrs: list[tuple[str, str | None]]) -> None:
        if tag.lower() != "a":
            return
        for key, value in attrs:
            if key.lower() == "href" and value:
                self.links.append(value)


def read_text_resource(url_or_path: str) -> Page:
    if re.match(r"^https?://", url_or_path):
        req = urllib.request.Request(url_or_path, headers={"User-Agent": "ab-validator/aat-fidelity"})
        with urllib.request.urlopen(req, timeout=30) as resp:
            raw = resp.read()
            final_url = resp.geturl()
    elif url_or_path.startswith("file://"):
        parsed = urllib.parse.urlparse(url_or_path)
        path = Path(urllib.request.url2pathname(parsed.path))
        raw = path.read_bytes()
        final_url = str(path)
    else:
        path = Path(url_or_path)
        raw = path.read_bytes()
        final_url = str(path)

    for enc in ("utf-8", "euc_jp", "cp932"):
        try:
            return Page(final_url, raw.decode(enc))
        except UnicodeDecodeError:
            continue
    return Page(final_url, raw.decode("utf-8", errors="replace"))


def extract_links(html: str) -> list[str]:
    parser = LinkParser()
    parser.feed(html)
    return parser.links


def resolve_href(base_url: str, href: str) -> str:
    if re.match(r"^https?://", base_url):
        return urllib.parse.urljoin(base_url, href)
    if base_url.startswith("file://"):
        parsed = urllib.parse.urlparse(base_url)
        base_path = Path(urllib.request.url2pathname(parsed.path)).parent
    else:
        base_path = Path(base_url).parent
    return str((base_path / href).resolve())


def case_id_from_card_url(card_url: str) -> str:
    path = urllib.parse.urlparse(card_url).path if re.match(r"^https?://", card_url) else card_url
    m = re.search(r"/cards/(\d+)/card(\d+)\.html$", path)
    if m:
        return f"{m.group(1)}_{m.group(2)}"
    m = re.search(r"card(\d+)\.html$", path)
    if m:
        parent = Path(path).parent.name
        return f"{parent}_{m.group(1)}" if parent.isdigit() else m.group(1)
    return hashlib.sha1(card_url.encode("utf-8")).hexdigest()[:12]


def discover_card_urls(page_urls: list[str]) -> list[str]:
    card_urls: list[str] = []
    seen: set[str] = set()
    for page_url in page_urls:
        page = read_text_resource(page_url)
        for href in extract_links(page.body):
            if re.search(r"(?:^|/)card\d+\.html(?:#.*)?$", href):
                resolved = resolve_href(page.url, href.split("#", 1)[0])
                if resolved not in seen:
                    card_urls.append(resolved)
                    seen.add(resolved)
    return card_urls


def pair_from_card(card_url: str, classify_source: bool) -> Pair | None:
    page = read_text_resource(card_url)
    links = [resolve_href(page.url, href) for href in extract_links(page.body)]
    source_candidates = [
        href
        for href in links
        if re.search(r"/?files/.*_ruby_?.*\.zip$", href)
        or re.search(r"/?files/.*_ruby_?.*\.txt$", href)
    ]
    if not source_candidates:
        source_candidates = [href for href in links if re.search(r"/?files/.*\.zip$", href)]
    xhtml_candidates = [
        href
        for href in links
        if re.search(r"/?files/.*\.html$", href) and "card" not in Path(urllib.parse.urlparse(href).path).name
    ]
    if not source_candidates or not xhtml_candidates:
        return None

    source = sorted(source_candidates, key=lambda value: (0 if "ruby" in value else 1, value))[0]
    upstream_xhtml = sorted(xhtml_candidates)[0]
    feature_tags: tuple[str, ...] = ()
    if classify_source:
        try:
            feature_tags = classify_source_resource(source)
        except Exception as exc:  # keep discovery running; record the unresolved case.
            print(f"warning: source classification failed for {source}: {exc}", file=sys.stderr)

    return Pair(
        case_id=case_id_from_card_url(page.url),
        source=source,
        upstream_xhtml=upstream_xhtml,
        card_url=page.url,
        feature_tags=feature_tags,
        status="paired",
    )


def fetch_bytes(url_or_path: str) -> bytes:
    if re.match(r"^https?://", url_or_path):
        req = urllib.request.Request(url_or_path, headers={"User-Agent": "ab-validator/aat-fidelity"})
        with urllib.request.urlopen(req, timeout=60) as resp:
            return resp.read()
    if url_or_path.startswith("file://"):
        parsed = urllib.parse.urlparse(url_or_path)
        return Path(urllib.request.url2pathname(parsed.path)).read_bytes()
    return Path(url_or_path).read_bytes()


def decode_source_bytes(raw: bytes) -> str:
    for enc in ("utf-8-sig", "cp932", "shift_jis", "euc_jp"):
        try:
            return raw.decode(enc)
        except UnicodeDecodeError:
            continue
    return raw.decode("utf-8", errors="replace")


def classify_source_resource(source: str) -> tuple[str, ...]:
    raw = fetch_bytes(source)
    if source.lower().endswith(".zip") or raw.startswith(b"PK\x03\x04"):
        with ZipFile(io.BytesIO(raw)) as zf:
            txt_names = sorted(name for name in zf.namelist() if name.lower().endswith(".txt"))
            if not txt_names:
                return ()
            raw = zf.read(txt_names[0])
    text = decode_source_bytes(raw)
    return tuple(name for name, pattern in FEATURE_PATTERNS if pattern.search(text))


def select_stratified(pairs: list[Pair], sample_size: int, seed: int) -> list[Pair]:
    if sample_size <= 0 or len(pairs) <= sample_size:
        return pairs
    rng = random.Random(seed)
    remaining = pairs[:]
    rng.shuffle(remaining)
    selected: list[Pair] = []
    selected_ids: set[str] = set()

    while len(selected) < sample_size:
        progressed = False
        for stratum in STRATA:
            for pair in list(remaining):
                if pair.case_id in selected_ids:
                    continue
                if stratum in pair.feature_tags:
                    selected.append(pair)
                    selected_ids.add(pair.case_id)
                    progressed = True
                    break
                if len(selected) >= sample_size:
                    break
            if len(selected) >= sample_size:
                break
        if not progressed:
            for pair in remaining:
                if pair.case_id not in selected_ids:
                    selected.append(pair)
                    selected_ids.add(pair.case_id)
                    break
        if len(selected_ids) >= len(pairs):
            break
    return selected[:sample_size]


def read_card_url_file(path: Path) -> list[str]:
    urls: list[str] = []
    for line in path.read_text().splitlines():
        line = line.strip()
        if not line or line.startswith("#"):
            continue
        urls.append(line)
    return urls


def write_manifest(path: Path, pairs: list[Pair]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, delimiter="\t", lineterminator="\n")
        writer.writerow(["case_id", "source", "upstream_xhtml"])
        for pair in pairs:
            writer.writerow([pair.case_id, pair.source, pair.upstream_xhtml])


def write_metadata(path: Path, pairs: list[Pair]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with path.open("w", encoding="utf-8", newline="") as f:
        writer = csv.writer(f, lineterminator="\n")
        writer.writerow(["case_id", "feature_tags", "card_url", "source", "upstream_xhtml", "status"])
        for pair in pairs:
            writer.writerow([
                pair.case_id,
                ";".join(pair.feature_tags),
                pair.card_url,
                pair.source,
                pair.upstream_xhtml,
                pair.status,
            ])


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--card-url", action="append", default=[])
    parser.add_argument("--card-url-file", type=Path, action="append", default=[])
    parser.add_argument("--person-url", action="append", default=[])
    parser.add_argument("--index-url", action="append", default=[])
    parser.add_argument("--max-cards", type=int, default=0)
    parser.add_argument("--sample-size", type=int, default=50)
    parser.add_argument("--seed", type=int, default=20260504)
    parser.add_argument("--classify-source", action="store_true")
    parser.add_argument("--out-manifest", type=Path, required=True)
    parser.add_argument("--out-metadata", type=Path, required=True)
    args = parser.parse_args()

    card_urls = list(args.card_url)
    for path in args.card_url_file:
        card_urls.extend(read_card_url_file(path))
    card_urls.extend(discover_card_urls(args.person_url + args.index_url))

    seen: set[str] = set()
    unique_card_urls = []
    for url in card_urls:
        if not re.match(r"^https?://", url) and not url.startswith("file://"):
            url = str(Path(url).resolve())
        if url not in seen:
            unique_card_urls.append(url)
            seen.add(url)
    if args.max_cards > 0:
        unique_card_urls = unique_card_urls[: args.max_cards]

    pairs: list[Pair] = []
    for card_url in unique_card_urls:
        try:
            pair = pair_from_card(card_url, args.classify_source)
        except Exception as exc:
            print(f"warning: card discovery failed for {card_url}: {exc}", file=sys.stderr)
            continue
        if pair is not None:
            pairs.append(pair)

    selected = select_stratified(pairs, args.sample_size, args.seed)
    write_manifest(args.out_manifest, selected)
    write_metadata(args.out_metadata, selected)
    print(f"cards_seen={len(unique_card_urls)}")
    print(f"pairs={len(pairs)}")
    print(f"selected={len(selected)}")
    print(f"manifest={args.out_manifest}")
    print(f"metadata={args.out_metadata}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
