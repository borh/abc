use crate::interval::{Interval, intersect, normalize, subtract};

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Reconciliation {
    pub diagnosed: Vec<Interval>,
    pub silent: Vec<Interval>,
    /// Count of maximal silent interval witnesses, not a construct census.
    pub silent_drops: u64,
}

/// Partitions uncovered bytes using intervals already authorized by P4.
pub fn reconcile(uncovered: &[Interval], authorized: &[Interval]) -> Reconciliation {
    let uncovered = normalize(uncovered.to_vec());
    let authorized = normalize(authorized.to_vec());
    let diagnosed = normalize(intersect(&uncovered, &authorized));
    let silent = normalize(subtract(&uncovered, &authorized));
    Reconciliation {
        diagnosed,
        silent_drops: silent.len() as u64,
        silent,
    }
}
