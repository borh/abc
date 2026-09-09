//! Shared property-test configuration.

use std::env;

use proptest::prelude::ProptestConfig;
use proptest::test_runner::FileFailurePersistence;

/// The settings every property suite in the workspace runs under.
///
/// Draws 128 cases unless `AOZORA_PROPTEST_CASES` specifies an explicit count,
/// allowing extended test runs without modifying test source code.
/// Failing inputs persist to a `proptest-regressions/` file beside the test
/// that found them, so a shrunk counterexample becomes a checked-in
/// regression rather than a one-off console line.
#[must_use]
pub fn default_config() -> ProptestConfig {
    ProptestConfig {
        cases: env::var("AOZORA_PROPTEST_CASES")
            .ok()
            .and_then(|requested| requested.parse().ok())
            .unwrap_or(128),
        max_shrink_iters: 10_000,
        failure_persistence: Some(Box::new(FileFailurePersistence::WithSource(
            "proptest-regressions",
        ))),
        ..ProptestConfig::default()
    }
}

#[cfg(test)]
mod tests {
    use super::{default_config, env};

    #[test]
    fn case_count_falls_back_to_the_default_when_the_variable_is_absent_or_junk() {
        // Reading the process environment makes this test order-dependent if
        // it mutated the variable, so it only asserts the fallback that holds
        // whenever the variable is unset or unparseable.
        if env::var("AOZORA_PROPTEST_CASES").is_err() {
            assert_eq!(default_config().cases, 128);
        }
    }
}
