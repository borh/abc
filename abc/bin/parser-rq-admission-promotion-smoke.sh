#!/usr/bin/env bash
set -euo pipefail

abc_root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$abc_root"
clojure -M:test:kaocha -m kaocha.runner \
  --focus abc.tools.parser-rq-admission-promotion-drift-test \
  --focus abc.tools.parser-rq-campaign-test
