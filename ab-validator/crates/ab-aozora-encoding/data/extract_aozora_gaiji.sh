#!/usr/bin/env bash
# Re-extract aozora-gaiji-chuki.tsv from the upstream XML mirror.
#
# Run when the upstream `kurema/AozoraGaijiChukiXml` ships a new
# revision (8th edition currently). Output is committed to the repo
# alongside this script.
#
# Steps:
#   1. download Chuki.xml (~4 MB, CC0)
#   2. extract descriptions, source characters, and explicit Unicode codes
#   3. perl-filter: use explicit Unicode for composed glyphs, dedupe,
#      decode XML entities, format codepoint as hex
#   4. prepend the schema-comment header
#
# The committed `aozora-gaiji-chuki.tsv` is the *generated artifact*;
# the source of truth is the upstream XML. xtask gaiji-gen reads the
# TSV, not the XML, so consumers do not need network access at gen
# time.

set -euo pipefail

UPSTREAM="https://raw.githubusercontent.com/kurema/AozoraGaijiChukiXml/master/data/Chuki.xml"
HERE="$(cd "$(dirname "$0")" && pwd)"
OUT="$HERE/aozora-gaiji-chuki.tsv"
TMP="$(mktemp -d)"
trap 'rm -rf "$TMP"' EXIT

echo "→ downloading $UPSTREAM"
curl -sLf --max-time 60 -o "$TMP/Chuki.xml" "$UPSTREAM"

echo "→ awk-extracting raw pairs"
awk '
  /<entry / { in_entry=1; char=""; desc=""; unicode=""; in_characters=0; components=0; next }
  /<\/entry>/ {
    if (char != "" && desc != "") print desc "\t" char "\t" unicode "\t" components;
    in_entry=0; char=""; desc=""; next
  }
  in_entry && /<characters>/ { in_characters=1; next }
  in_entry && /<\/characters>/ { in_characters=0; next }
  in_entry && /<character>/ {
    line = $0;
    sub(/.*<character>/, "", line);
    sub(/<\/character>.*/, "", line);
    if (in_characters) components++;
    if (in_characters || char == "") char = char line;
    next
  }
  in_entry && /<unicode code=/ {
    line = $0;
    sub(/.*code="/, "", line);
    sub(/".*/, "", line);
    unicode = line;
    next
  }
  in_entry && /<description>/ {
    line = $0;
    sub(/.*<description>/, "", line);
    sub(/<\/description>.*/, "", line);
    sub(/^「/, "", line);
    sub(/」$/, "", line);
    desc = line;
    next
  }
' "$TMP/Chuki.xml" > "$TMP/raw.tsv"

echo "→ filter / decode / dedupe"
perl -CSD -E '
  my %seen;
  while (<>) {
    chomp;
    my ($desc, $char, $unicode, $components) = split /\t/, $_, 4;
    next unless defined $desc && defined $char;
    next if $desc eq "" || $char eq "";
    if ($components > 1) {
      next unless defined $unicode && $unicode =~ /\A[0-9A-Fa-f]{4,6}\z/;
      my $codepoint = hex $unicode;
      next if $codepoint > 0x10FFFF || ($codepoint >= 0xD800 && $codepoint <= 0xDFFF);
      $char = chr $codepoint;
    }
    next if $desc =~ /[\x00-\x1F]/;
    # decode XML entities (apply &amp; LAST so it does not double-decode)
    for my $s ($desc, $char) {
      $s =~ s/&lt;/</g;
      $s =~ s/&gt;/>/g;
      $s =~ s/&quot;/"/g;
      $s =~ s/&apos;/'\''/g;
      $s =~ s/&amp;/&/g;
    }
    next unless length($char) == 1;
    next if $seen{$desc}++;
    say "$desc\t" . sprintf("%X", ord $char);
  }
' "$TMP/raw.tsv" > "$TMP/clean.tsv"

echo "→ writing $OUT"
{
  echo "# Aozora Bunko 外字注記辞書 — description → Unicode codepoint mapping."
  echo "#"
  echo "# Source: kurema/AozoraGaijiChukiXml (CC0 1.0), an XML transcription of the"
  echo "# official 青空文庫・外字注記辞書【第八版】 (PDF). Upstream URL:"
  echo "#   $UPSTREAM"
  echo "# License: CC0 1.0 Universal (Public Domain Dedication)."
  echo "#"
  echo "# Filtering applied during extraction:"
  echo "#  * composed glyphs use the explicit Unicode code; omit them when absent"
  echo "#  * otherwise retain exactly 1 Unicode codepoint per entry"
  echo "#  * surrounding 「」 stripped from descriptions"
  echo "#  * XML entities (&lt;, &gt;, &amp;) decoded"
  echo "#  * first-wins on duplicate descriptions"
  echo "#"
  echo "# Format: description<TAB>codepoint_hex"
  echo "# Regenerate from upstream: see extract_aozora_gaiji.sh in this directory."
  cat "$TMP/clean.tsv"
} > "$OUT"

LINES=$(grep -cv '^#' "$OUT")
echo "✓ wrote $OUT ($LINES entries)"
