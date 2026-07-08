pub mod align;
pub mod first_diff;
pub mod frequency;
pub mod hashing;
pub mod visible_text;

pub use align::{
    AlignmentConfig, AlignmentKind, AlignmentRegion, AlignmentResult, AlignmentSummary,
    ComparisonEvidence, ComparisonToken, MoveDetection, NearMatch, ScoringConfig,
    algorithm_config_hash, align_pair,
};
pub use first_diff::{FirstDifference, first_difference};
pub use frequency::{DEFAULT_MAX_EXAMPLES, FrequencyEntry, FrequencyTable};
pub use hashing::{
    canonical_json_string, hash_bytes, hash_json, hash_json_canonical, hash_string_sequence,
    hash_string_sequence_raw,
};
pub use visible_text::{remove_unicode_whitespace, sentence_like_runs, sentence_like_tokens};
