#!/usr/bin/env bash
set -euo pipefail

usage() {
  printf '%s\n' \
    'usage: parser-rq-campaign-capture.sh --candidate PATH --authorization PATH' \
    '       --provenance PATH --corpus-root DIR --primary-store DIR' \
    '       --lock PATH --staging-root DIR'
}

candidate=
authorization=
provenance=
corpus_root=
primary_store=
lock=
staging_root=

while (( $# )); do
  case "$1" in
    --candidate) candidate=${2:?}; shift 2 ;;
    --authorization) authorization=${2:?}; shift 2 ;;
    --provenance) provenance=${2:?}; shift 2 ;;
    --corpus-root) corpus_root=${2:?}; shift 2 ;;
    --primary-store) primary_store=${2:?}; shift 2 ;;
    --lock) lock=${2:?}; shift 2 ;;
    --staging-root) staging_root=${2:?}; shift 2 ;;
    *) usage >&2; exit 2 ;;
  esac
done

: "${candidate:?missing --candidate}"
: "${authorization:?missing --authorization}"
: "${provenance:?missing --provenance}"
: "${corpus_root:?missing --corpus-root}"
: "${primary_store:?missing --primary-store}"
: "${lock:?missing --lock}"
: "${staging_root:?missing --staging-root}"
: "${PARSER_RQ_CORE_CAPTURE:?set PARSER_RQ_CORE_CAPTURE}"
: "${PARSER_RQ_SOURCE_CAPTURE:?set PARSER_RQ_SOURCE_CAPTURE}"
: "${PARSER_RQ_PREDICATE_CAPTURE:?set PARSER_RQ_PREDICATE_CAPTURE}"
: "${PARSER_RQ_DIAGNOSTIC_CAPTURE:?set PARSER_RQ_DIAGNOSTIC_CAPTURE}"
: "${PARSER_RQ_PUBLICATION_CAPTURE:?set PARSER_RQ_PUBLICATION_CAPTURE}"
: "${PARSER_RQ_RESOURCE_CAPTURE:?set PARSER_RQ_RESOURCE_CAPTURE}"

exec 9>"$lock"
flock -n 9

# The campaign module authenticates provenance and the one-shot authorization
# before any candidate process starts. Lane programs receive only explicit
# candidate/corpus/store coordinates; composition is not caller-delegated.
clojure -M:abc/parser-rq-campaign verify-provenance \
  --candidate "$candidate" --provenance "$provenance"
clojure -M:abc/parser-rq-campaign verify-authorization \
  --candidate "$candidate" --authorization "$authorization"

export PARSER_RQ_CANDIDATE="$candidate"
export PARSER_RQ_AUTHORIZATION="$authorization"
export PARSER_RQ_PROVENANCE="$provenance"
export PARSER_RQ_CORPUS_ROOT="$corpus_root"
export PARSER_RQ_PRIMARY_STORE="$primary_store"
export PARSER_RQ_STAGING_ROOT="$staging_root"

"$PARSER_RQ_CORE_CAPTURE"
"$PARSER_RQ_SOURCE_CAPTURE"
"$PARSER_RQ_PREDICATE_CAPTURE"
"$PARSER_RQ_DIAGNOSTIC_CAPTURE"
"$PARSER_RQ_PUBLICATION_CAPTURE"
"$PARSER_RQ_RESOURCE_CAPTURE"

clojure -M:abc/parser-rq-campaign compose \
  --candidate "$candidate" --authorization "$authorization" \
  --capture-root "$staging_root" --out "$staging_root/measurements.edn"
clojure -M:abc/parser-rq-campaign verify-capture \
  --candidate "$candidate" --authorization "$authorization" \
  --capture-root "$staging_root"
