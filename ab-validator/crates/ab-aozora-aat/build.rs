fn main() {
    // Bakes the source revision into --version so gate evidence and the
    // registry row identify the measured code. Gate builds MUST set
    // AB_AOZORA_GIT_REV (Global Constraints); absent -> "unknown", never a
    // build failure (dev builds). rerun-if-env-changed makes cargo rebuild
    // when the rev changes despite an otherwise-clean cache.
    println!("cargo:rerun-if-env-changed=AB_AOZORA_GIT_REV");
    let rev = std::env::var("AB_AOZORA_GIT_REV").unwrap_or_else(|_| "unknown".into());
    println!("cargo:rustc-env=AB_AOZORA_GIT_REV={rev}");
}
