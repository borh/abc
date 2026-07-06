# Warehouse thread scaling benchmark

Date: 2026-05-01

## Purpose

Measure practical `ab-morph-run analyze-aat --warehouse-dir` scaling on the local AMD Ryzen 9 7950X3D before launching a full-corpus warehouse run.

The previous full-corpus attempt used `--jobs 4`; progress output showed that it was both under-parallelized for this machine and affected by static count-based sharding imbalance.

## Machine topology

- Hardware threads: 32
- Physical cores: 16
- L3 groups:
  - CPUs `0-7,16-23`: 96 MiB L3
  - CPUs `8-15,24-31`: 32 MiB L3
- Available memory before benchmark: about 72 GiB

## Input

- Source: `scratch/morph-full-corpus/aats/aozora-rs-adapter`
- Benchmark subset: first 512 AAT JSON files by sorted path, symlinked into `scratch/morph-warehouse-thread-bench/aat-512`
- Analyzers: `vibrato`, `sudachi-a`, `sudachi-c`
- Sudachi dictionary: Nix-provided `.#sudachi-dictionary-full`

## Commands

```sh
AB_SUDACHI_DICT="$(nix path-info .#sudachi-dictionary-full)/share/sudachi/system.dic" \
  time -v taskset -c <cpus> target/release/ab-morph-run analyze-aat \
    --aat-dir scratch/morph-warehouse-thread-bench/aat-512 \
    --analyzer vibrato \
    --analyzer sudachi-a \
    --analyzer sudachi-c \
    --warehouse-dir scratch/morph-warehouse-thread-bench/warehouse-jobs<jobs> \
    --run-id bench-512-jobs<jobs> \
    --jobs <jobs> \
    --progress
```

Affinity choices:

- `jobs=8`: `taskset -c 0-7`, physical cores on the 96 MiB L3 CCD.
- `jobs=16`: `taskset -c 0-15`, all physical cores.
- `jobs=24`: `taskset -c 0-23`, all physical cores plus SMT siblings on the 96 MiB L3 CCD.

## Results

| jobs | cpus | wall time | CPU | max RSS | output size | exit |
| ---: | --- | ---: | ---: | ---: | ---: | ---: |
| 8 | `0-7` | 6:37.08 | 373% | 12,665,516 KiB | 989 MiB | 0 |
| 16 | `0-15` | 5:30.47 | 453% | 19,291,068 KiB | 991 MiB | 0 |
| 24 | `0-23` | 4:35.45 | 612% | 25,832,916 KiB | 991 MiB | 0 |

## Conclusions

- `--jobs 24` is safe on this machine for this workload: peak RSS was about 24.6 GiB, well below available memory.
- `--jobs 24` was the fastest tested setting on this subset.
- Scaling is positive but not linear. The main visible limiter is static count-based sharding: workers that receive large early AAT files lag while other workers finish small files quickly.
- The next full-corpus run should use `--jobs 24` with `taskset -c 0-23`.
- A future runner improvement should replace count-based partitions with dynamic scheduling or size-balanced partitions.
