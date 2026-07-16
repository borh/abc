use ab_parser_rq_source_accountability::interval::{
    Interval, intersect, normalize, subtract, total_len,
};
use hegel::generators;

fn iv(start: usize, end: usize) -> Interval {
    Interval::new(start, end, end).unwrap()
}

fn draw_intervals(tc: &hegel::TestCase) -> Vec<Interval> {
    let count = tc.draw(generators::integers::<u8>().max_value(32));
    (0..count)
        .map(|_| {
            let bound = tc.draw(generators::integers::<u16>());
            let first = tc.draw(generators::integers::<u16>().max_value(bound));
            let second = tc.draw(generators::integers::<u16>().max_value(bound));
            Interval::new(
                usize::from(first.min(second)),
                usize::from(first.max(second)),
                usize::from(bound),
            )
            .unwrap()
        })
        .collect()
}

#[test]
fn subtraction_retains_residual_bytes() {
    let source = vec![Interval::new(0, 10, 10).unwrap()];
    let excused = vec![Interval::new(4, 5, 10).unwrap()];
    assert_eq!(subtract(&source, &excused), vec![iv(0, 4), iv(5, 10)]);
}

#[test]
fn construction_rejects_reversed_and_out_of_bounds_intervals() {
    assert!(Interval::new(2, 1, 2).is_err());
    assert!(Interval::new(0, 3, 2).is_err());
}

#[test]
fn normalization_drops_empty_intervals_and_merges_adjacency() {
    assert_eq!(
        normalize(vec![iv(4, 4), iv(2, 4), iv(0, 2)]),
        vec![iv(0, 4)]
    );
}

#[cfg(target_pointer_width = "64")]
#[test]
fn total_length_rejects_u64_overflow() {
    let maximal = Interval::new(0, usize::MAX, usize::MAX).unwrap();
    assert!(total_len(&[maximal, maximal]).is_err());
}

#[hegel::test]
fn normalization_is_order_and_duplicate_invariant(tc: hegel::TestCase) {
    let intervals = draw_intervals(&tc);
    let mut reversed_twice = intervals.clone();
    reversed_twice.reverse();
    reversed_twice.extend(intervals.clone());
    assert_eq!(normalize(intervals), normalize(reversed_twice));
}

#[hegel::test]
fn normalization_is_idempotent_and_disjoint(tc: hegel::TestCase) {
    let once = normalize(draw_intervals(&tc));
    assert_eq!(once, normalize(once.clone()));
    assert!(once.windows(2).all(|pair| pair[0].end() < pair[1].start()));
}

#[hegel::test]
fn subtraction_partitions_source(tc: hegel::TestCase) {
    let source = normalize(draw_intervals(&tc));
    let removed = normalize(draw_intervals(&tc));
    let kept = subtract(&source, &removed);
    let cut = intersect(&source, &removed);
    assert_eq!(
        total_len(&source).unwrap(),
        total_len(&kept).unwrap() + total_len(&cut).unwrap()
    );
    assert!(intersect(&kept, &cut).is_empty());
}
