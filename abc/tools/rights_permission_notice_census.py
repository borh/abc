#!/usr/bin/env python3
"""Census of permission notices on the Aozora catalog's `あり` works.

Answers the measurable half of Q6 in
`docs/evidence/external/aozora-rights-source-contract.md`. That document's F5b
established that 337 works carry `作品著作権フラグ = あり` together with a text
URL, and `guide/kijyunn.html` (source S2) states where a permission may appear:

    図書カード・作品ファイル中、もしくは図書カードからリンクした作者にかかわる
    ウェッブページに、著作権者による特別の許諾（クリエイティブ・コモンズ・
    ライセンス等）が明記されていれば…

So the catalog CSV cannot answer it -- none of those three places is a catalog
column -- and this census reads the cards and work files out of the same pinned
checkout instead. It reports which of the three places carries a notice, and
which license string that notice names.

Every measurement here is lexical: which notice text appears, in which file,
naming which license. Nothing here decides whether a given license permits what
Soranoha's publication pipeline does with a work; that is a legal judgement, it
is not ABC's to make, and it is not derivable from the strings this tool reads.
Reports and always exits zero: a census, not a gate.

Usage: python3 tools/rights_permission_notice_census.py <aozorabunko-root>
"""

from __future__ import annotations

import collections
import csv
import hashlib
import html
import io
import os
import re
import sys
import zipfile
from urllib.parse import urlparse

ZIP_RELATIVE = "index_pages/list_person_all_extended_utf8.zip"

# Aozora's own server. A work whose text sits elsewhere cannot be inspected from
# the pinned checkout at all, so it is counted and excluded rather than silently
# treated as carrying no notice -- the distinction between "no permission found"
# and "not looked at" is the whole point of this census.
AOZORA_HOST = "www.aozora.gr.jp"

# Cards are served as UTF-8 across the whole `あり` set in the pinned snapshot;
# the work text inside the zips is Shift_JIS-family, which is what `cp932`
# covers including the NEC/IBM extensions Aozora files use.
CARD_ENCODING = "utf-8"
WORK_ENCODING = "cp932"

# Aozora writes the same notice two ways: with the license name in 「」 followed
# by でライセンスされています, and with ・-separated segments followed by
# ・ライセンスで提供されています. Both appear among the 337, so matching only the
# quoted form would report a work carrying BY-NC-ND 4.0 as carrying nothing.
PERMISSION = re.compile(
    r"クリエイティブ・コモンズ[・\s]*「?\s*(.{4,40}?)\s*」?[・\s]*"
    r"(?:ライセンス)?で(?:ライセンス|提供)されています"
)

# Notice text differs in spacing, in full-width vs ASCII hyphens, in the
# trailing ・ライセンス segment, and in a parenthetical gloss such as
# `(CC BY 2.1 JP)`. None of those distinguish one license from another, so they
# are normalized away before the identities are counted -- otherwise the same
# license reports as several.
GLOSS = re.compile(r"[（(][^）)]*[）)]")
SEPARATORS = re.compile(r"[\s　・]+")


def license_identity(raw: str) -> str:
    """The license a notice names, with presentation differences normalized."""
    value = GLOSS.sub("", raw)
    value = value.replace("‐", "-").replace("－", "-").replace("−", "-")
    value = SEPARATORS.sub("", value)
    return value.strip("-・")


def card_text(path: str) -> str:
    """A card's visible text, tags stripped and entities resolved."""
    raw = open(path, encoding=CARD_ENCODING, errors="replace").read()
    return html.unescape(re.sub(r"\s+", " ", re.sub(r"<[^>]+>", " ", raw)))


def work_notices(path: str) -> tuple[list[str], str | None]:
    """License strings found in a work archive's text members.

    Returns the notices and, when the archive could not be fully read, the
    reason. Aozora ships a handful of archives that stock `zipfile` rejects
    outright and one whose member fails its CRC; ABC's own admission path
    recovers those, but this census does not reimplement that recovery. It
    reports them as unread so they are not miscounted as carrying no notice.
    """
    found: list[str] = []
    try:
        with zipfile.ZipFile(path) as archive:
            for name in archive.namelist():
                if not name.lower().endswith(".txt"):
                    continue
                body = archive.read(name).decode(WORK_ENCODING, errors="replace")
                match = PERMISSION.search(body)
                if match:
                    found.append(license_identity(match.group(1)))
    except (zipfile.BadZipFile, OSError) as error:
        return found, f"{type(error).__name__}: {error}"
    return found, None


def load(root: str) -> tuple[list[dict[str, str]], str]:
    with open(f"{root}/{ZIP_RELATIVE}", "rb") as handle:
        raw = handle.read()
    with zipfile.ZipFile(io.BytesIO(raw)) as archive:
        csv_bytes = archive.read(archive.namelist()[0])
    rows = list(csv.DictReader(io.StringIO(csv_bytes.decode("utf-8-sig"))))
    return rows, f"sha256:{hashlib.sha256(csv_bytes).hexdigest()}"


def main(root: str) -> int:
    rows, csv_hash = load(root)
    by_work: dict[str, list[dict[str, str]]] = collections.defaultdict(list)
    for row in rows:
        by_work[row["作品ID"]].append(row)

    subjects = [
        work
        for work in by_work.values()
        if work[0]["作品著作権フラグ"] == "あり" and work[0]["テキストファイルURL"].strip()
    ]

    print("== snapshot binding ==")
    print(f"  csv sha256   {csv_hash}")
    print(f"  あり works with a テキストファイルURL : {len(subjects)}")

    card_license: dict[str, str] = {}
    file_license: dict[str, str] = {}
    off_host: list[str] = []
    unread: list[tuple[str, str]] = []
    banner = 0

    for work in subjects:
        work_id = work[0]["作品ID"]
        card_path = os.path.join(root, urlparse(work[0]["図書カードURL"].strip()).path.lstrip("/"))
        text = card_text(card_path)
        if "＊著作権存続＊" in text:
            banner += 1
        match = PERMISSION.search(text)
        if match:
            card_license[work_id] = license_identity(match.group(1))

        url = urlparse(work[0]["テキストファイルURL"].strip())
        if url.netloc != AOZORA_HOST:
            off_host.append(work_id)
            continue
        notices, reason = work_notices(os.path.join(root, url.path.lstrip("/")))
        if notices:
            file_license[work_id] = notices[0]
        if reason is not None:
            unread.append((work_id, reason))

    print("\n== scope actually inspected ==")
    print(f"  cards read (all あり works)                   : {len(subjects)}")
    print(f"    of which show the ＊著作権存続＊ banner       : {banner}")
    print(f"  work files on {AOZORA_HOST}, read      : {len(subjects) - len(off_host)}")
    print(f"  work files hosted elsewhere, NOT inspected    : {len(off_host)}")
    print(f"  work archives stock zipfile could not read    : {len(unread)}")
    for work_id, reason in unread:
        print(f"      {work_id}  {reason}")

    union = set(card_license) | set(file_license)
    print("\n== where the notice lives ==")
    # The card is not sufficient on its own. S2 names three places and this is
    # the measurement of that: a card-only reading finds a small fraction of the
    # notices that exist, so "the card says nothing" is not "there is no
    # permission". The third place S2 names -- a linked author page -- is not
    # measured here; see the residual note below.
    print(f"  notice on the 図書カード                       : {len(card_license)}")
    print(f"  notice in the 作品ファイル                     : {len(file_license)}")
    print(f"  union (either place)                          : {len(union)}")
    print(
        f"    card only                                   : {len(set(card_license) - set(file_license))}"
    )
    print(
        f"    work file only                              : {len(set(file_license) - set(card_license))}"
    )
    both = set(card_license) & set(file_license)
    disagreeing = [w for w in both if card_license[w] != file_license[w]]
    print(f"    both                                        : {len(both)}")
    print(f"      naming the same license                   : {len(both) - len(disagreeing)}")
    print(f"      naming different licenses                 : {len(disagreeing)}")
    for work_id in sorted(disagreeing):
        print(f"        {work_id}: card={card_license[work_id]} file={file_license[work_id]}")

    print("\n== license identities over the union ==")
    # Reported as the strings Aozora wrote, normalized only for presentation.
    # Which of these permits which downstream use is not decided here.
    identities = collections.Counter(
        file_license.get(work_id) or card_license[work_id] for work_id in union
    )
    for identity, count in identities.most_common():
        print(f"  {count:4d}  {identity}")

    print("\n== residual ==")
    print(f"  あり works with a notice in neither place inspected : {len(subjects) - len(union)}")
    print("    Not a finding of 'no permission exists'. It excludes the")
    print(
        f"    {len(off_host)} off-host texts and {len(unread)} unreadable archives above, and this"
    )
    print("    census does not follow 図書カード links to author pages, which is")
    print("    the third place S2 says a permission may be recorded.")
    return 0


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print(__doc__, file=sys.stderr)
        raise SystemExit(2)
    raise SystemExit(main(sys.argv[1]))
