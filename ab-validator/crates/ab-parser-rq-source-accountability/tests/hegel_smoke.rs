use ab_parser_rq_source_accountability::hegel_probe_value;
use hegel::generators;

#[hegel::test]
fn hegel_native_engine_shrinks_counterexamples(tc: hegel::TestCase) {
    let value = tc.draw(generators::integers::<u16>());
    assert_eq!(hegel_probe_value(value), value);
}
