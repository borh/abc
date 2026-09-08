//! Whole-document sentinel registry: one position-sorted table.
//!
//! [`NodeRef`] is the unified registry-hit view: inline payloads carry an
//! owned [`Node`]; container discriminants carry `RegionFormat` /
//! `RegionClose`. [`Registry`] holds `(position, NodeRef)` sorted by
//! normalized byte position; `node_at` is one binary search.

use ab_aozora_spec::{NormalizedOffset, Sentinel};

use crate::format::{RegionClose, RegionFormat};

use super::payload::Node;

/// Unified view over a registry hit.
///
/// Each variant tags the sentinel kind that fired; consumers pattern-match the
/// variant once, then handle the inline payload (an owned [`Node`]) or the
/// container payload (a `Copy` [`RegionFormat`] / [`RegionClose`]
/// discriminant) accordingly.
///
/// `Copy` because every inlined payload is `Copy` ([`Node`] flattens its
/// `&str`/list payloads to `StrId`/ranges). No `Eq`.
#[derive(Debug, Clone, Copy, PartialEq)]
#[non_exhaustive]
pub enum NodeRef {
    /// Hit on an inline-sentinel position ([`Sentinel::Inline`]).
    Inline(Node),
    /// Hit on a block-leaf-sentinel position ([`Sentinel::BlockLeaf`]).
    BlockLeaf(Node),
    /// Hit on a block-container-open position ([`Sentinel::BlockOpen`]).
    /// Carries the authoritative open [`RegionFormat`].
    BlockOpen(RegionFormat),
    /// Hit on a block-container-close position ([`Sentinel::BlockClose`]).
    /// Carries the [`RegionClose`] discriminant.
    BlockClose(RegionClose),
}

impl NodeRef {
    /// Sentinel kind that produced this entry.
    #[must_use]
    pub const fn sentinel_kind(self) -> Sentinel {
        match self {
            Self::Inline(_) => Sentinel::Inline,
            Self::BlockLeaf(_) => Sentinel::BlockLeaf,
            Self::BlockOpen(_) => Sentinel::BlockOpen,
            Self::BlockClose(_) => Sentinel::BlockClose,
        }
    }

    /// Cross-cutting [`crate::NodeKind`] tag for this entry.
    #[must_use]
    pub const fn kind(self) -> crate::NodeKind {
        match self {
            Self::Inline(node) | Self::BlockLeaf(node) => node.kind(),
            Self::BlockOpen(_) => crate::NodeKind::ContainerOpen,
            Self::BlockClose(_) => crate::NodeKind::ContainerClose,
        }
    }
}

/// Whole-document owned registry: one position-sorted table.
///
/// `node_at` is one binary search; every entry's sentinel kind is encoded by
/// the [`NodeRef`] variant. Not `Copy` (the table owns a `Vec`).
///
/// The table is a flat sorted `Vec` searched by [`slice::binary_search_by_key`]
/// rather than a cache-optimised layout. An earlier version used an Eytzinger
/// (BFS-of-BST) layout on the theory that its prefetcher-friendly probe
/// sequence would beat midpoint probing. Measured on this workload it did not:
/// the flat table was 3.5x faster at 1,000 entries, 2.4x at 16,000, 1.5x at
/// 64,000, and only lost by 8% at 256,000, far past the largest registry a
/// single Aozora work produces. The Eytzinger walk compares with a three-armed
/// `Ordering` match, so it pays a mispredicted branch per probe, and at these
/// sizes that costs more than the cache misses the layout avoids.
#[derive(Debug, Clone)]
pub struct Registry {
    /// Lookup table keyed by normalized byte position, ascending. Entries
    /// arrive in strictly increasing position order.
    table: Vec<(u32, NodeRef)>,
}

impl Registry {
    /// Construct from a position-sorted slice of `(position, NodeRef)`.
    ///
    /// # Panics
    ///
    /// Panics in debug builds if `entries` is not sorted ascending by
    /// position. In release builds unsorted input silently breaks `node_at`,
    /// which cannot detect it: verify the caller instead. The pipeline's
    /// recorder appends in increasing position order, so the assertion guards
    /// a future second producer rather than the present one.
    #[must_use]
    pub fn from_sorted_slice(entries: &[(u32, NodeRef)]) -> Self {
        debug_assert!(
            entries.windows(2).all(|w| w[0].0 <= w[1].0),
            "registry entries must be sorted ascending by position"
        );
        Self {
            table: entries.to_vec(),
        }
    }

    /// Empty registry.
    #[must_use]
    pub const fn empty() -> Self {
        Self { table: Vec::new() }
    }

    /// True iff the registry holds no entries.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }

    /// Total number of entries across all sentinel kinds. O(1).
    #[must_use]
    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// Look up the entry at the given normalized-text byte position.
    #[must_use]
    pub fn node_at(&self, pos: NormalizedOffset) -> Option<NodeRef> {
        let target = pos.get();
        let index = self
            .table
            .binary_search_by_key(&target, |&(position, _)| position)
            .ok()?;
        self.table.get(index).map(|&(_, entry)| entry)
    }

    /// Iterate `(position, NodeRef)` in ascending position order.
    pub fn iter_sorted(&self) -> impl Iterator<Item = (u32, NodeRef)> + '_ {
        self.table.iter().copied()
    }

    /// Iterate entries whose [`NodeRef::sentinel_kind`] matches `kind`.
    pub fn iter_kind(&self, kind: Sentinel) -> impl Iterator<Item = (u32, NodeRef)> + '_ {
        self.iter_sorted()
            .filter(move |(_, nr)| nr.sentinel_kind() == kind)
    }

    /// Count entries whose sentinel kind matches `kind`. O(n).
    #[must_use]
    pub fn count_kind(&self, kind: Sentinel) -> usize {
        self.iter_kind(kind).count()
    }
}

impl Default for Registry {
    fn default() -> Self {
        Self::empty()
    }
}

/// Resolved container pair with normalized positions and original marker spans.
///
/// Lifetime-free `Copy` side-table entry: the pipeline emits one per balanced
/// `［＃ここから…］` / `［＃ここで…終わり］` pair. Editor surfaces (LSP
/// `linkedEditingRange` / `documentHighlight` against container markers) consume
/// this directly instead of re-deriving the pairing from independent open /
/// close registry entries.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContainerPair {
    /// The open container format. The builder constructs the pair from the
    /// open-stack pop, so `kind` reflects the open marker authoritatively
    /// (the close side is a discriminant; see [`RegionClose`]).
    pub kind: RegionFormat,
    /// Normalized byte offset of the open sentinel (`U+E003`).
    pub open: NormalizedOffset,
    /// Normalized byte offset of the close sentinel (`U+E004`).
    pub close: NormalizedOffset,
    /// Original opening marker in sanitized-source byte coordinates.
    pub source_open: crate::Span,
    /// Source event that ends this region, in sanitized-source byte coordinates.
    pub source_end: ContainerEnd,
}

/// The source event terminating a region; replacement is not a closing marker.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ContainerEnd {
    /// An explicit matching closing marker.
    ClosingMarker(crate::Span),
    /// A supplied following layout instruction replaces this scope.
    SourceReplacement(crate::Span),
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::format::EnclosureKind;

    #[test]
    fn empty_registry_reports_empty() {
        let r = Registry::empty();
        assert!(r.is_empty(), "empty registry is empty");
        assert_eq!(r.len(), 0, "empty registry has zero entries");
    }

    #[test]
    fn every_entry_resolves_to_its_own_payload() {
        // Guards the failure modes a flat binary search can hide: an
        // off-by-one that reads the neighbouring row, and a gap position that
        // resolves to the row below it instead of missing. Payloads cycle
        // through four variants so a neighbour read returns a visibly
        // different one rather than an identical copy.
        let payloads = [
            NodeRef::Inline(Node::PageBreak),
            NodeRef::BlockLeaf(Node::BodyEnd),
            NodeRef::BlockOpen(RegionFormat::Bold { padded: true }),
            NodeRef::BlockClose(RegionClose::Framed(EnclosureKind::Rule)),
        ];
        for count in [0usize, 1, 2, 3, 7, 8, 64, 257] {
            // Positions are gapped by two, so every odd position is a
            // known miss sitting between two known hits.
            let entries: Vec<(u32, NodeRef)> = (0..count)
                .map(|i| {
                    let position = u32::try_from(i).expect("count fits in u32") * 2;
                    (position, payloads[i % payloads.len()])
                })
                .collect();
            let registry = Registry::from_sorted_slice(&entries);
            assert_eq!(registry.len(), count, "count={count}");

            for &(position, expected) in &entries {
                assert_eq!(
                    registry.node_at(NormalizedOffset::new(position)),
                    Some(expected),
                    "count={count} position={position}"
                );
                assert_eq!(
                    registry.node_at(NormalizedOffset::new(position + 1)),
                    None,
                    "count={count} gap after {position}"
                );
            }

            let walked: Vec<(u32, NodeRef)> = registry.iter_sorted().collect();
            assert_eq!(walked, entries, "iter_sorted must replay input order");
        }
    }

    #[test]
    fn node_at_dispatches_to_variant() {
        let r = Registry::from_sorted_slice(&[
            (10u32, NodeRef::Inline(Node::PageBreak)),
            (20u32, NodeRef::BlockLeaf(Node::PageBreak)),
            (
                30u32,
                NodeRef::BlockOpen(RegionFormat::Framed(EnclosureKind::Rule)),
            ),
            (
                40u32,
                NodeRef::BlockClose(RegionClose::Framed(EnclosureKind::Rule)),
            ),
        ]);
        assert!(matches!(
            r.node_at(NormalizedOffset::new(30)),
            Some(NodeRef::BlockOpen(RegionFormat::Framed(
                EnclosureKind::Rule
            )))
        ));
        assert_eq!(r.count_kind(Sentinel::Inline), 1, "one inline entry");
        assert_eq!(r.count_kind(Sentinel::BlockOpen), 1, "one open entry");
        assert!(
            r.node_at(NormalizedOffset::new(99)).is_none(),
            "miss returns None"
        );
    }
}
