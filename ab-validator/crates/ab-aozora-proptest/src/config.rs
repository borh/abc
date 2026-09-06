//! Shared property-test configuration, controlled by `AOZORA_PROPTEST_CASES`.

use std::env;

use proptest::prelude::ProptestConfig;
use proptest::test_runner::FileFailurePersistence;

/// Uses 128 cases unless `AOZORA_PROPTEST_CASES` parses as a case count.
/// Regressions persist beside each test in `proptest-regressions/`.
#[must_use]
pub fn default_config() -> ProptestConfig {
    ProptestConfig {
        cases: env::var("AOZORA_PROPTEST_CASES")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or(128),
        max_shrink_iters: 10_000,
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource(
            "proptest-regressions",
        ))),
        ..ProptestConfig::default()
    }
}
