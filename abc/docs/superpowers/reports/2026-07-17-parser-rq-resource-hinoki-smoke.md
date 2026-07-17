# P3 Hinoki cgroup-v2 integration smoke

On 2026-07-17, the committed P3 wrapper and live smoke script were copied to
`hinoki.hyakutake-barbel.ts.net` and executed through transient systemd user
services. The small allocation produced an exact 45,785,088-byte peak with
zero swap. The safety-ceiling allocation produced an exact capped-execution
peak of 3,221,225,472 bytes, zero swap, and the required right-censored status.

This is volatile integration evidence. The sandboxed pure capture check remains
the byte-identical drift authority, and neither artifact is a P5 full-corpus
qualification observation.
