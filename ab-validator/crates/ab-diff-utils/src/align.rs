use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum ComparisonEvidence {
    SourceSpanAligned,
    TokenSequenceAligned,
    HashOnly,
    FirstDifferenceOnly,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ComparisonToken {
    pub ordinal: usize,
    pub text: String,
    pub normalized: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum AlignmentKind {
    Equal,
    Insertion,
    Deletion,
    Substitution,
    LikelyMovedBlock,
    UnclassifiedMismatch,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentRegion {
    pub kind: AlignmentKind,
    pub left_range: [usize; 2],
    pub right_range: [usize; 2],
    pub left_text_sample: String,
    pub right_text_sample: String,
    pub truncated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentSummary {
    pub total_regions: usize,
    pub equal_regions: usize,
    pub insertion_regions: usize,
    pub deletion_regions: usize,
    pub substitution_regions: usize,
    pub likely_moved_block_regions: usize,
    pub unclassified_mismatch_regions: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentResult {
    pub summary: AlignmentSummary,
    pub regions: Vec<AlignmentRegion>,
    pub truncated: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ScoringConfig {
    pub r#match: i32,
    pub gap: i32,
    pub substitution: i32,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum NearMatch {
    Disabled,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum MoveDetection {
    Disabled,
    ExactNormalizedSequenceV1,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct AlignmentConfig {
    pub anchor_ngram_size: usize,
    pub max_tokens_per_window: usize,
    pub max_chars_per_window: usize,
    pub near_match: NearMatch,
    pub move_detection: MoveDetection,
    pub scoring: ScoringConfig,
}

impl Default for AlignmentConfig {
    fn default() -> Self {
        Self {
            anchor_ngram_size: 3,
            max_tokens_per_window: 512,
            max_chars_per_window: 8192,
            near_match: NearMatch::Disabled,
            move_detection: MoveDetection::ExactNormalizedSequenceV1,
            scoring: ScoringConfig {
                r#match: 2,
                gap: -1,
                substitution: -1,
            },
        }
    }
}

#[must_use]
pub fn algorithm_config_hash(config: &AlignmentConfig) -> String {
    let value = serde_json::to_value(config).expect("alignment config must serialize");
    crate::hashing::hash_json_canonical(&value).expect("alignment config must canonicalize")
}

#[must_use]
pub fn align_pair(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> AlignmentResult {
    let mut regions = align_partitioned(left, right, config);
    if config.move_detection == MoveDetection::ExactNormalizedSequenceV1 {
        mark_exact_moves(&mut regions);
        regions = coalesce_regions(regions);
    }
    summarize(regions)
}

const DEFAULT_MAX_SAMPLE_CHARS: usize = 160;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Edit {
    Equal(usize, usize),
    Insert(usize),
    Delete(usize),
    Substitute(usize, usize),
}

#[derive(Debug, Clone, Copy)]
struct RegionBounds {
    left_start: usize,
    left_end: usize,
    right_start: usize,
    right_end: usize,
}

fn align_partitioned(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> Vec<AlignmentRegion> {
    let anchors = unique_anchors(left, right, config.anchor_ngram_size);
    if anchors.is_empty() {
        return align_window(left, right, 0, left.len(), 0, right.len(), config);
    }

    let mut regions = Vec::new();
    let mut left_cursor = 0;
    let mut right_cursor = 0;
    for (left_anchor, right_anchor) in anchors {
        let left_anchor_end = left_anchor + config.anchor_ngram_size;
        let right_anchor_end = right_anchor + config.anchor_ngram_size;
        if left_anchor < left_cursor
            || right_anchor < right_cursor
            || left_anchor_end > left.len()
            || right_anchor_end > right.len()
        {
            continue;
        }
        regions.extend(align_window(
            left,
            right,
            left_cursor,
            left_anchor,
            right_cursor,
            right_anchor,
            config,
        ));
        regions.push(make_region(
            AlignmentKind::Equal,
            left,
            right,
            RegionBounds {
                left_start: left_anchor,
                left_end: left_anchor_end,
                right_start: right_anchor,
                right_end: right_anchor_end,
            },
            false,
        ));
        left_cursor = left_anchor_end;
        right_cursor = right_anchor_end;
    }
    regions.extend(align_window(
        left,
        right,
        left_cursor,
        left.len(),
        right_cursor,
        right.len(),
        config,
    ));
    coalesce_regions(regions)
}

fn align_window(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    left_start: usize,
    left_end: usize,
    right_start: usize,
    right_end: usize,
    config: &AlignmentConfig,
) -> Vec<AlignmentRegion> {
    if left_start == left_end && right_start == right_end {
        return Vec::new();
    }

    let left_window = &left[left_start..left_end];
    let right_window = &right[right_start..right_end];
    if window_exceeds_limits(left_window, right_window, config) {
        return vec![make_region(
            AlignmentKind::UnclassifiedMismatch,
            left,
            right,
            RegionBounds {
                left_start,
                left_end,
                right_start,
                right_end,
            },
            true,
        )];
    }

    let script = edit_script(left_window, right_window, config);
    coalesce_script(left, right, &script, left_start, right_start)
}

fn window_exceeds_limits(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> bool {
    let token_count = left.len().max(right.len());
    let char_count: usize = left
        .iter()
        .chain(right.iter())
        .map(|token| token.text.chars().count())
        .sum();
    token_count > config.max_tokens_per_window || char_count > config.max_chars_per_window
}

fn edit_script(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    config: &AlignmentConfig,
) -> Vec<Edit> {
    let n = left.len();
    let m = right.len();
    let mut score = vec![vec![0_i32; m + 1]; n + 1];
    for i in 1..=n {
        score[i][0] = score[i - 1][0] + config.scoring.gap;
    }
    for j in 1..=m {
        score[0][j] = score[0][j - 1] + config.scoring.gap;
    }

    for i in 1..=n {
        for j in 1..=m {
            let diagonal = score[i - 1][j - 1]
                + if left[i - 1].normalized == right[j - 1].normalized {
                    config.scoring.r#match
                } else {
                    config.scoring.substitution
                };
            let insert = score[i - 1][j] + config.scoring.gap;
            let delete = score[i][j - 1] + config.scoring.gap;
            score[i][j] = diagonal.max(insert).max(delete);
        }
    }

    let mut edits = Vec::new();
    let mut i = n;
    let mut j = m;
    while i > 0 || j > 0 {
        if i > 0 && j > 0 {
            let diagonal = score[i - 1][j - 1]
                + if left[i - 1].normalized == right[j - 1].normalized {
                    config.scoring.r#match
                } else {
                    config.scoring.substitution
                };
            if score[i][j] == diagonal {
                if left[i - 1].normalized == right[j - 1].normalized {
                    edits.push(Edit::Equal(i - 1, j - 1));
                } else {
                    edits.push(Edit::Substitute(i - 1, j - 1));
                }
                i -= 1;
                j -= 1;
                continue;
            }
        }
        if i > 0 && score[i][j] == score[i - 1][j] + config.scoring.gap {
            edits.push(Edit::Insert(i - 1));
            i -= 1;
        } else {
            edits.push(Edit::Delete(j - 1));
            j -= 1;
        }
    }
    edits.reverse();
    edits
}

fn coalesce_script(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    script: &[Edit],
    left_offset: usize,
    right_offset: usize,
) -> Vec<AlignmentRegion> {
    let mut regions = Vec::new();
    let mut left_cursor = left_offset;
    let mut right_cursor = right_offset;
    for edit in script {
        let (kind, left_start, left_end, right_start, right_end) = match *edit {
            Edit::Equal(left_idx, right_idx) => {
                let left_idx = left_offset + left_idx;
                let right_idx = right_offset + right_idx;
                left_cursor = left_idx + 1;
                right_cursor = right_idx + 1;
                (
                    AlignmentKind::Equal,
                    left_idx,
                    left_idx + 1,
                    right_idx,
                    right_idx + 1,
                )
            }
            Edit::Insert(left_idx) => {
                let left_idx = left_offset + left_idx;
                left_cursor = left_idx + 1;
                (
                    AlignmentKind::Insertion,
                    left_idx,
                    left_idx + 1,
                    right_cursor,
                    right_cursor,
                )
            }
            Edit::Delete(right_idx) => {
                let right_idx = right_offset + right_idx;
                right_cursor = right_idx + 1;
                (
                    AlignmentKind::Deletion,
                    left_cursor,
                    left_cursor,
                    right_idx,
                    right_idx + 1,
                )
            }
            Edit::Substitute(left_idx, right_idx) => {
                let left_idx = left_offset + left_idx;
                let right_idx = right_offset + right_idx;
                left_cursor = left_idx + 1;
                right_cursor = right_idx + 1;
                (
                    AlignmentKind::Substitution,
                    left_idx,
                    left_idx + 1,
                    right_idx,
                    right_idx + 1,
                )
            }
        };
        regions.push(make_region(
            kind,
            left,
            right,
            RegionBounds {
                left_start,
                left_end,
                right_start,
                right_end,
            },
            false,
        ));
    }
    coalesce_regions(regions)
}

fn make_region(
    kind: AlignmentKind,
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    bounds: RegionBounds,
    truncated: bool,
) -> AlignmentRegion {
    let (left_text_sample, left_sample_truncated) =
        sample_text(&left[bounds.left_start..bounds.left_end]);
    let (right_text_sample, right_sample_truncated) =
        sample_text(&right[bounds.right_start..bounds.right_end]);
    AlignmentRegion {
        kind,
        left_range: [bounds.left_start, bounds.left_end],
        right_range: [bounds.right_start, bounds.right_end],
        left_text_sample,
        right_text_sample,
        truncated: truncated || left_sample_truncated || right_sample_truncated,
    }
}

fn sample_text(tokens: &[ComparisonToken]) -> (String, bool) {
    let text = tokens
        .iter()
        .map(|token| token.text.as_str())
        .collect::<String>();
    let mut sample = String::new();
    let mut truncated = false;
    for (idx, ch) in text.chars().enumerate() {
        if idx == DEFAULT_MAX_SAMPLE_CHARS {
            truncated = true;
            break;
        }
        sample.push(ch);
    }
    (sample, truncated)
}

fn coalesce_regions(regions: Vec<AlignmentRegion>) -> Vec<AlignmentRegion> {
    let mut coalesced: Vec<AlignmentRegion> = Vec::new();
    for region in regions {
        if let Some(previous) = coalesced.last_mut()
            && previous.kind == region.kind
            && previous.left_range[1] == region.left_range[0]
            && previous.right_range[1] == region.right_range[0]
            && !previous.truncated
            && !region.truncated
        {
            previous.left_range[1] = region.left_range[1];
            previous.right_range[1] = region.right_range[1];
            previous.left_text_sample.push_str(&region.left_text_sample);
            previous
                .right_text_sample
                .push_str(&region.right_text_sample);
            continue;
        }
        coalesced.push(region);
    }
    coalesced
}

fn summarize(regions: Vec<AlignmentRegion>) -> AlignmentResult {
    let mut summary = AlignmentSummary {
        total_regions: regions.len(),
        equal_regions: 0,
        insertion_regions: 0,
        deletion_regions: 0,
        substitution_regions: 0,
        likely_moved_block_regions: 0,
        unclassified_mismatch_regions: 0,
    };
    let mut truncated = false;
    for region in &regions {
        truncated |= region.truncated;
        match region.kind {
            AlignmentKind::Equal => summary.equal_regions += 1,
            AlignmentKind::Insertion => summary.insertion_regions += 1,
            AlignmentKind::Deletion => summary.deletion_regions += 1,
            AlignmentKind::Substitution => summary.substitution_regions += 1,
            AlignmentKind::LikelyMovedBlock => summary.likely_moved_block_regions += 1,
            AlignmentKind::UnclassifiedMismatch => summary.unclassified_mismatch_regions += 1,
        }
    }
    AlignmentResult {
        summary,
        regions,
        truncated,
    }
}

fn unique_anchors(
    left: &[ComparisonToken],
    right: &[ComparisonToken],
    ngram_size: usize,
) -> Vec<(usize, usize)> {
    if ngram_size == 0 {
        return Vec::new();
    }

    let mut left_counts = std::collections::BTreeMap::<Vec<String>, Vec<usize>>::new();
    let mut right_counts = std::collections::BTreeMap::<Vec<String>, Vec<usize>>::new();
    for (idx, window) in left.windows(ngram_size).enumerate() {
        left_counts
            .entry(
                window
                    .iter()
                    .map(|token| token.normalized.clone())
                    .collect(),
            )
            .or_default()
            .push(idx);
    }
    for (idx, window) in right.windows(ngram_size).enumerate() {
        right_counts
            .entry(
                window
                    .iter()
                    .map(|token| token.normalized.clone())
                    .collect(),
            )
            .or_default()
            .push(idx);
    }

    let mut anchors = Vec::new();
    for (ngram, left_positions) in left_counts {
        if left_positions.len() == 1
            && let Some(right_positions) = right_counts.get(&ngram)
            && right_positions.len() == 1
        {
            anchors.push((left_positions[0], right_positions[0]));
        }
    }
    anchors.sort();
    non_crossing_anchors(anchors)
}

fn non_crossing_anchors(anchors: Vec<(usize, usize)>) -> Vec<(usize, usize)> {
    let mut result = Vec::new();
    let mut last_right = None;
    for (left_idx, right_idx) in anchors {
        if last_right.is_none_or(|prev| right_idx > prev) {
            result.push((left_idx, right_idx));
            last_right = Some(right_idx);
        }
    }
    result
}

fn mark_exact_moves(regions: &mut [AlignmentRegion]) {
    let insertions: Vec<(usize, String)> = regions
        .iter()
        .enumerate()
        .filter(|(_, region)| region.kind == AlignmentKind::Insertion)
        .map(|(idx, region)| (idx, region.left_text_sample.clone()))
        .collect();
    let deletions: Vec<(usize, String)> = regions
        .iter()
        .enumerate()
        .filter(|(_, region)| region.kind == AlignmentKind::Deletion)
        .map(|(idx, region)| (idx, region.right_text_sample.clone()))
        .collect();
    for (insert_idx, inserted) in &insertions {
        for (delete_idx, deleted) in &deletions {
            if !inserted.is_empty() && inserted == deleted {
                regions[*insert_idx].kind = AlignmentKind::LikelyMovedBlock;
                regions[*delete_idx].kind = AlignmentKind::LikelyMovedBlock;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn tok(ordinal: usize, text: &str) -> ComparisonToken {
        ComparisonToken {
            ordinal,
            text: text.to_owned(),
            normalized: text.to_owned(),
        }
    }

    #[test]
    fn equal_sequences_emit_one_equal_region() {
        let tokens = vec![tok(0, "メロス"), tok(1, "は"), tok(2, "激怒した。")];
        let result = align_pair(&tokens, &tokens, &AlignmentConfig::default());
        assert_eq!(result.summary.total_regions, 1);
        assert_eq!(result.summary.equal_regions, 1);
        assert_eq!(result.regions[0].kind, AlignmentKind::Equal);
        assert_eq!(result.regions[0].left_range, [0, 3]);
        assert_eq!(result.regions[0].right_range, [0, 3]);
    }

    #[test]
    fn tail_insertion_is_single_insertion_region() {
        let left = vec![
            tok(0, "勇者は、ひどく赤面した。"),
            tok(1, "（古伝説と、シルレルの詩から。）"),
        ];
        let right = vec![tok(0, "勇者は、ひどく赤面した。")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert_eq!(result.summary.insertion_regions, 1);
        assert_eq!(
            result.regions.last().unwrap().kind,
            AlignmentKind::Insertion
        );
        assert_eq!(
            result.regions.last().unwrap().left_text_sample,
            "（古伝説と、シルレルの詩から。）"
        );
        assert_eq!(result.regions.last().unwrap().right_text_sample, "");
    }

    #[test]
    fn one_token_difference_is_substitution_not_delete_plus_insert() {
        let left = vec![tok(0, "メロス"), tok(1, "怒った")];
        let right = vec![tok(0, "メロス"), tok(1, "激怒した")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert_eq!(result.summary.substitution_regions, 1);
        assert_eq!(result.summary.insertion_regions, 0);
        assert_eq!(result.summary.deletion_regions, 0);
    }

    #[test]
    fn oversized_window_becomes_truncated_unclassified_region() {
        let config = AlignmentConfig {
            max_tokens_per_window: 1,
            max_chars_per_window: 4,
            ..AlignmentConfig::default()
        };
        let left = vec![tok(0, "abcdef"), tok(1, "ghijkl")];
        let right = vec![tok(0, "mnopqr"), tok(1, "stuvwx")];
        let result = align_pair(&left, &right, &config);
        assert!(result.truncated);
        assert_eq!(result.regions[0].kind, AlignmentKind::UnclassifiedMismatch);
        assert!(result.regions[0].truncated);
        assert_eq!(result.regions[0].left_text_sample, "abcdefghijkl");
    }

    #[test]
    fn repeated_tokens_keep_stable_order() {
        let left = vec![tok(0, "A"), tok(1, "B"), tok(2, "A"), tok(3, "C")];
        let right = vec![tok(0, "A"), tok(1, "A"), tok(2, "B"), tok(3, "C")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert!(
            result
                .regions
                .iter()
                .any(|region| region.kind == AlignmentKind::Equal)
        );
        assert_eq!(result.regions.last().unwrap().kind, AlignmentKind::Equal);
        assert_eq!(result.regions.last().unwrap().left_text_sample, "C");
    }

    #[test]
    fn unique_anchor_splits_oversized_outer_window() {
        let config = AlignmentConfig {
            anchor_ngram_size: 1,
            max_tokens_per_window: 2,
            max_chars_per_window: 32,
            ..AlignmentConfig::default()
        };
        let left = vec![tok(0, "L1"), tok(1, "L2"), tok(2, "ANCHOR"), tok(3, "L3")];
        let right = vec![tok(0, "R1"), tok(1, "R2"), tok(2, "ANCHOR"), tok(3, "R3")];
        let result = align_pair(&left, &right, &config);
        assert!(result.regions.iter().any(|region| {
            region.kind == AlignmentKind::Equal && region.left_text_sample == "ANCHOR"
        }));
        assert!(result.regions.len() >= 3);
    }

    #[test]
    fn exact_unmatched_sequence_can_be_marked_likely_moved() {
        let left = vec![tok(0, "A"), tok(1, "X"), tok(2, "B")];
        let right = vec![tok(0, "A"), tok(1, "B"), tok(2, "X")];
        let result = align_pair(&left, &right, &AlignmentConfig::default());
        assert!(
            result
                .regions
                .iter()
                .any(|region| region.kind == AlignmentKind::LikelyMovedBlock)
        );
    }

    proptest! {
        #[test]
        fn equal_normalized_sequences_are_one_equal_region(values in proptest::collection::vec("[ぁ-んァ-ン一-龯]{1,4}", 1..20)) {
            let tokens: Vec<_> = values.iter().enumerate().map(|(i, value)| tok(i, value)).collect();
            let result = align_pair(&tokens, &tokens, &AlignmentConfig::default());
            prop_assert_eq!(result.summary.total_regions, 1);
            prop_assert_eq!(result.summary.equal_regions, 1);
            prop_assert!(!result.truncated);
        }
    }
}
