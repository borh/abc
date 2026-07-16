use anyhow::{Result, anyhow};

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub struct Interval {
    start: usize,
    end: usize,
}

impl Interval {
    pub fn new(start: usize, end: usize, bound: usize) -> Result<Self> {
        if start > end {
            return Err(anyhow!("interval start {start} exceeds end {end}"));
        }
        if end > bound {
            return Err(anyhow!("interval end {end} exceeds bound {bound}"));
        }
        Ok(Self { start, end })
    }

    pub const fn start(self) -> usize {
        self.start
    }

    pub const fn end(self) -> usize {
        self.end
    }
}

pub fn normalize(mut intervals: Vec<Interval>) -> Vec<Interval> {
    intervals.retain(|interval| interval.start < interval.end);
    intervals.sort_unstable_by_key(|interval| (interval.start, interval.end));

    let mut normalized: Vec<Interval> = Vec::with_capacity(intervals.len());
    for interval in intervals {
        if let Some(last) = normalized.last_mut()
            && interval.start <= last.end
        {
            last.end = last.end.max(interval.end);
            continue;
        }
        normalized.push(interval);
    }
    normalized
}

pub fn subtract(source: &[Interval], removed: &[Interval]) -> Vec<Interval> {
    let mut kept = Vec::new();
    let mut removed_index = 0;

    for source_interval in source {
        let mut cursor = source_interval.start;
        while removed_index < removed.len() && removed[removed_index].end <= cursor {
            removed_index += 1;
        }

        let mut index = removed_index;
        while index < removed.len() && removed[index].start < source_interval.end {
            let cut = removed[index];
            if cursor < cut.start {
                kept.push(Interval {
                    start: cursor,
                    end: cut.start.min(source_interval.end),
                });
            }
            cursor = cursor.max(cut.end);
            if cursor >= source_interval.end {
                break;
            }
            index += 1;
        }

        if cursor < source_interval.end {
            kept.push(Interval {
                start: cursor,
                end: source_interval.end,
            });
        }
    }
    kept
}

pub fn intersect(left: &[Interval], right: &[Interval]) -> Vec<Interval> {
    let mut intersection = Vec::new();
    let (mut left_index, mut right_index) = (0, 0);

    while left_index < left.len() && right_index < right.len() {
        let left_interval = left[left_index];
        let right_interval = right[right_index];
        let start = left_interval.start.max(right_interval.start);
        let end = left_interval.end.min(right_interval.end);
        if start < end {
            intersection.push(Interval { start, end });
        }

        if left_interval.end <= right_interval.end {
            left_index += 1;
        } else {
            right_index += 1;
        }
    }
    intersection
}

pub fn total_len(intervals: &[Interval]) -> Result<u64> {
    intervals.iter().try_fold(0_u64, |total, interval| {
        let length = u64::try_from(interval.end - interval.start)
            .map_err(|_| anyhow!("interval length does not fit in u64"))?;
        total
            .checked_add(length)
            .ok_or_else(|| anyhow!("total interval length exceeds u64"))
    })
}
