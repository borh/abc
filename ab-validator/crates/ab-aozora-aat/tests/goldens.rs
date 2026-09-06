//! Exact AAT serialization snapshots. Spans cover decoded source bytes while
//! text values normalize line endings. The terminal fixture retains attribution
//! and colophon groups separately, in source order.
use std::fs;

#[test]
fn aat_output_matches_hand_verified_goldens() {
    let data = concat!(env!("CARGO_MANIFEST_DIR"), "/tests/data");
    for entry in fs::read_dir(data).unwrap() {
        let path = entry.unwrap().path();
        let name = path.file_name().unwrap().to_str().unwrap();
        let golden = format!(
            "{}/tests/goldens/{name}.expected.json",
            env!("CARGO_MANIFEST_DIR")
        );
        let expected = fs::read(&golden).unwrap();
        let actual = ab_aozora_aat::aat_json_from_bytes(&fs::read(&path).unwrap()).unwrap();
        assert_eq!(actual, expected, "golden drift: {name}");
    }
}
