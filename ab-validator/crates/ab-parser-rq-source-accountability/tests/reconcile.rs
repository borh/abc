use ab_parser_rq_source_accountability::{Interval, reconcile};

fn iv(start: usize, end: usize) -> Interval {
    Interval::new(start, end, 100).unwrap()
}

#[test]
fn one_byte_authorization_does_not_excuse_a_large_gap() {
    let result = reconcile(&[iv(0, 10)], &[iv(4, 5)]);
    assert_eq!(result.diagnosed, vec![iv(4, 5)]);
    assert_eq!(result.silent, vec![iv(0, 4), iv(5, 10)]);
    assert_eq!(result.silent_drops, 2);
}

#[test]
fn adjacent_constructs_are_one_maximal_non_census_witness() {
    let result = reconcile(&[iv(0, 4), iv(4, 8)], &[]);
    assert_eq!(result.silent, vec![iv(0, 8)]);
    assert_eq!(result.silent_drops, 1);
}

#[test]
fn partition_conserves_integer_bytes() {
    let result = reconcile(&[iv(0, 10), iv(20, 30)], &[iv(3, 7), iv(5, 25)]);
    let length = |xs: &[Interval]| xs.iter().map(|x| x.end() - x.start()).sum::<usize>();
    assert_eq!(length(&result.diagnosed) + length(&result.silent), 20);
    assert_eq!(result.diagnosed, vec![iv(3, 10), iv(20, 25)]);
    assert_eq!(result.silent, vec![iv(0, 3), iv(25, 30)]);
}
