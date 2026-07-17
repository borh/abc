#!/usr/bin/env bash
set -euo pipefail

wrapper="${1:?wrapper path required}"
work_dir="$(mktemp -d)"
trap 'rm -rf "$work_dir"' EXIT

systemctl --user show-environment >/dev/null || {
  echo "prerequisite_error:user_systemd_unavailable" >&2
  exit 1
}
test -f /sys/fs/cgroup/cgroup.controllers || {
  echo "prerequisite_error:cgroup_v2_unavailable" >&2
  exit 1
}

run_case() {
  local name="$1"
  local bytes="$2"
  local output="$work_dir/$name.json"
  systemd-run --user --wait --collect --service-type=exec \
    --unit="parser-rq-smoke-$name-$RANDOM.service" \
    --property=MemoryAccounting=yes \
    --property=MemoryMax=3221225472 \
    --property=MemorySwapMax=0 \
    --property=OOMPolicy=continue \
    --property=Delegate=no \
    python "$wrapper" --output "$output" --closure-timeout 10 -- \
    python -c "value=bytearray($bytes); value[0]=1"
  jq -e '.peak_swap_bytes == 0' "$output" >/dev/null
  jq -c --arg case "$name" '{case:$case,status,peak_cgroup_memory_bytes,right_censored}' "$output"
}

run_case small 33554432
run_case ceiling 3300000000
jq -e '.status == "ceiling_clipped" and .right_censored == true and .peak_cgroup_memory_bytes > 2147483648' \
  "$work_dir/ceiling.json" >/dev/null
