# Parser Performance Measurement

- index: `/db/ab-validator/aat-corpus/index.json`
- corpus: `/nix/store/sdr1imwrxfldvlwzs2d2fhs11vxncgpx-aozorabunko-corpus`
- timeout limit: `90s`
- selected works: 6

## Summary

| adapter | stage | attempted | ok | timeout | errors | median wall s | max wall s | max RSS KB |
|---|---|---:|---:|---:|---:|---:|---:|---:|
| aozora | full_adapter | 6 | 6 | 0 | 0 | 1.245 | 3.180 | 515808 |
| aozora-epub3 | full_adapter | 6 | 6 | 0 | 0 | 0.955 | 1.400 | 359552 |
| aozora-rs | full_adapter | 6 | 6 | 0 | 0 | 0.190 | 0.750 | 646004 |
| aozora2 | full_adapter | 6 | 4 | 2 | 0 | 16.885 | 77.020 | 156584 |
| aozora2html | full_adapter | 6 | 6 | 0 | 0 | 6.765 | 28.570 | 553312 |
| aozora2html | ruby_parser | 2 | 2 | 0 | 0 | 4.990 | 5.320 | 33112 |
| aozora2html | rust_mapper | 2 | 2 | 0 | 0 | 12.555 | 22.790 | 552500 |

## Notes

- `full_adapter` is the comparable measurement across parser adapters.
- aozora2html `ruby_parser` and `rust_mapper` rows are supplemental stage diagnostics, not a separate adapter comparison axis.
- These measurements are for oracle/comparison guidance; they do not imply an optimization target for aozora2html.
