//! Memory-aware default parallelism for warehouse analyze runs.
//!
//! `--jobs 0` derives a job count from `MemAvailable` so a full-corpus run
//! never trips earlyoom (observed: jobs = nproc = 32 with 4 analyzers reached
//! 68.5 GiB RSS and was SIGTERM'd at the 10% MemAvailable watermark).
//! Constants are calibrated by the subset sweep in the 2026-07-05
//! warehouse-run-perf plan (Task 4); the budget mirrors the summarizer's
//! read-once /proc/meminfo discipline.

pub(crate) const GIB: u64 = 1024 * 1024 * 1024;
/// Fraction of MemAvailable the run may budget (30% headroom for the OS,
/// page cache churn, and concurrent processes).
const MEM_FRACTION_NUM: u64 = 7;
const MEM_FRACTION_DEN: u64 = 10;
/// Fixed per-job overhead. Calibrated 2026-07-06 by the Task 4 subset sweep
/// (2,000-source random sample, 4 analyzers, jobs 4/16/32; peak RSS
/// 15,397,980 / 28,978,108 / 39,760,908 kB). Least-squares slope was
/// 859,500 kB/job (~839 MiB); the fit attributes the whole slope to
/// analyzers (see `PER_ANALYZER_BYTES`), leaving a zero residual, so this
/// floor is the plan's 64 MiB minimum.
const BASE_PER_JOB_BYTES: u64 = 64 * 1024 * 1024;
/// Additional bytes per analyzer per job. Calibrated 2026-07-06 (Task 4
/// sweep, above): fitted slope 859,500 kB/job ÷ 4 analyzers = ~209.8 MiB,
/// rounded up to 210 MiB. Middle sweep point sat +7.7% off the line
/// (< 20%), so the linear fit stands. The fit's ~12.5 GiB intercept is
/// budgeted separately as `FIXED_OVERHEAD_BYTES` below (controller-approved
/// deviation from the original per-job-only formula, 2026-07-06): on hosts
/// with under ~20 GiB MemAvailable the intercept alone would exceed the 70%
/// budget, and the original formula ignored it, risking OOM.
const PER_ANALYZER_BYTES: u64 = 210 * 1024 * 1024;
/// Fixed memory overhead independent of job count — shared dictionary
/// allocations (vibrato cwj+novel, sudachi). Fitted intercept from the
/// 2026-07-06 subset sweep: 13,147,661 kB.
const FIXED_OVERHEAD_BYTES: u64 = 13_147_661 * 1024;

fn per_job_bytes(analyzer_count: usize) -> u64 {
    BASE_PER_JOB_BYTES + analyzer_count as u64 * PER_ANALYZER_BYTES
}

fn fallback_jobs(nproc: usize) -> usize {
    (nproc / 4).clamp(1, 8)
}

pub(crate) fn auto_jobs(nproc: usize, mem_available_bytes: Option<u64>, analyzer_count: usize) -> usize {
    match mem_available_bytes {
        Some(mem) if mem > 0 => {
            let budget = (mem / MEM_FRACTION_DEN * MEM_FRACTION_NUM).saturating_sub(FIXED_OVERHEAD_BYTES);
            let by_memory = (budget / per_job_bytes(analyzer_count)) as usize;
            by_memory.clamp(1, nproc)
        }
        _ => fallback_jobs(nproc),
    }
}

pub(crate) fn resolve_requested(requested: usize, nproc: usize, mem_available_bytes: Option<u64>, analyzer_count: usize) -> usize {
    if requested == 0 {
        auto_jobs(nproc, mem_available_bytes, analyzer_count)
    } else {
        requested
    }
}

fn read_mem_available_bytes() -> Option<u64> {
    let meminfo = std::fs::read_to_string("/proc/meminfo").ok()?;
    let kb = meminfo.lines().find_map(|line| {
        line.strip_prefix("MemAvailable:")?
            .trim()
            .split(' ')
            .next()?
            .parse::<u64>()
            .ok()
    })?;
    Some(kb * 1024)
}

/// Resolve the effective job count for an analyze run, logging the decision.
pub(crate) fn resolve_jobs(requested: usize, analyzer_count: usize) -> usize {
    let nproc = std::thread::available_parallelism()
        .map(std::num::NonZeroUsize::get)
        .unwrap_or(1);
    let mem = read_mem_available_bytes();
    let jobs = resolve_requested(requested, nproc, mem, analyzer_count);
    match (requested, mem) {
        (0, Some(m)) => eprintln!(
            "auto-jobs: {jobs} (nproc={nproc}, MemAvailable={:.1} GiB, {analyzer_count} analyzers, per-job={:.1} GiB, 70% budget, fixed-overhead={:.1} GiB)",
            m as f64 / GIB as f64,
            per_job_bytes(analyzer_count) as f64 / GIB as f64,
            FIXED_OVERHEAD_BYTES as f64 / GIB as f64,
        ),
        (0, None) => eprintln!("auto-jobs: {jobs} (MemAvailable unreadable; fallback max(1, min(nproc/4, 8)) with nproc={nproc})"),
        (n, Some(m)) => {
            let budget_jobs = auto_jobs(nproc, Some(m), analyzer_count);
            if n > budget_jobs {
                eprintln!(
                    "warning: --jobs {n} exceeds the memory budget ({budget_jobs} jobs for MemAvailable={:.1} GiB, {analyzer_count} analyzers); honoring the explicit request",
                    m as f64 / GIB as f64,
                );
            }
        }
        _ => {}
    }
    jobs
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn budget_scales_with_memory_and_analyzers() {
        // Calibrated constants (2026-07-06 sweep): base 64 MiB, 210 MiB/analyzer,
        // fixed overhead 13,147,661 kB = 13,463,204,864 B.
        // 64 GiB available → raw budget = 68,719,476,736 / 10 × 7 = 48,103,633,711 B;
        // minus fixed overhead: 48,103,633,711 − 13,463,204,864 = 34,640,428,847 B.
        // 4 analyzers → per_job = 64 + 4×210 = 904 MiB = 947,912,704 B;
        // 34,640,428,847 / 947,912,704 = 36.54 → 36 jobs (< nproc 64, budget binds).
        let mem = 64 * GIB;
        assert_eq!(auto_jobs(64, Some(mem), 4), 36);
        // 8 analyzers under the same memory → per_job = 64 + 8×210 = 1744 MiB
        // = 1,828,716,544 B; 34,640,428,847 / 1,828,716,544 = 18.94 → 18 jobs.
        assert_eq!(auto_jobs(64, Some(mem), 8), 18);
    }

    #[test]
    fn constrained_memory_clamps_to_one_job() {
        // Safety property (this is the fix): on hosts under ~20 GiB
        // MemAvailable, the fixed dictionary overhead alone can exceed the
        // 70% budget, so the post-overhead budget must saturate to 0 rather
        // than underflow, yielding by_memory = 0 → clamp(1, nproc) = 1.
        // 16 GiB available → raw budget = 17,179,869,184 / 10 × 7 =
        // 12,025,908,426 B, which is less than the 13,463,204,864 B fixed
        // overhead, so budget.saturating_sub(overhead) = 0 regardless of
        // analyzer count.
        let mem = 16 * GIB;
        assert_eq!(auto_jobs(32, Some(mem), 4), 1);
        assert_eq!(auto_jobs(32, Some(mem), 8), 1);
    }

    #[test]
    fn budget_clamps_to_nproc() {
        // Plenty of memory → nproc wins.
        assert_eq!(auto_jobs(8, Some(512 * GIB), 4), 8);
    }

    #[test]
    fn budget_clamps_to_one() {
        // Tiny memory → never below 1.
        assert_eq!(auto_jobs(32, Some(GIB), 4), 1);
    }

    #[test]
    fn missing_meminfo_uses_fallback() {
        // max(1, min(nproc/4, 8))
        assert_eq!(auto_jobs(32, None, 4), 8);
        assert_eq!(auto_jobs(64, None, 4), 8);
        assert_eq!(auto_jobs(4, None, 4), 1);
        assert_eq!(auto_jobs(2, None, 4), 1);
    }

    #[test]
    fn zero_mem_available_uses_fallback() {
        assert_eq!(auto_jobs(32, Some(0), 4), 8);
    }

    #[test]
    fn explicit_request_is_honored() {
        assert_eq!(resolve_requested(10, 32, Some(64 * GIB), 4), 10);
        // Over budget: still honored (warning is logged by the caller wrapper).
        assert_eq!(resolve_requested(32, 32, Some(8 * GIB), 4), 32);
    }
}
