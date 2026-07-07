//! Aozora ruby oracle: adjudicate analyzer readings against editor ruby.
// `reading_norm` has no callers yet; the `ruby` module (Task 4) will call
// `normalize`. Suppress dead-code warnings until that lands.
#[allow(dead_code)]
pub(crate) mod reading_norm;
// `ruby`'s items (RubyBase, ruby_bases, morpheme_reading) have no callers yet;
// Task 6 wires them into the pipeline. Suppress dead-code warnings until then.
#[allow(dead_code)]
pub(crate) mod ruby;
