//! Flat backing store for the owned AST.
//!
//! Content runs, segment slices, and co-applied formatting attributes
//! are stored as half-open ranges
//! into flat `Vec`s held by [`NodeStore`], alongside the [`StrInterner`] that
//! owns every interned string. `StrId` / range payloads on the owned nodes
//! resolve against this store.

use std::num::NonZeroU32;
use std::slice;

use crate::{ForwardAttr, PartialLayout};

/// Handle to source evidence for one partially interpreted layout instruction.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct PartialLayoutId(NonZeroU32);

use super::intern::{StrId, StrInterner};
use super::payload::{Content, Illustration, Segment, TranscribedNotes};

/// Document-owned illustration metadata, kept out of the common node payload.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct IllustrationId(NonZeroU32);

/// Document-owned associations between separately transcribed notes and targets.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TranscribedNotesId(NonZeroU32);

/// Half-open run of [`Content`] in [`NodeStore::resolve_content_range`];
/// `len >= 1` (a content run is never empty).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContentRange {
    /// Index of the first [`Content`] in the store's content `Vec`.
    pub start: u32,
    /// Number of [`Content`] entries in the run.
    pub len: u32,
}

/// Half-open run of [`Segment`] in [`NodeStore::resolve_seg_range`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SegRange {
    /// Index of the first [`Segment`] in the store's segment `Vec`.
    pub start: u32,
    /// Number of [`Segment`] entries in the run.
    pub len: u32,
}

/// Attributes applying together to one forward-reference target.
/// Storage order preserves source spelling; it implies no layout nesting.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ForwardAttrs {
    /// One attribute needs no arena allocation.
    One(ForwardAttr),
    /// Multiple attributes share the document's flat pool.
    Many(ForwardAttrRange),
}

impl ForwardAttrs {
    /// The single attribute, when this is not a compound directive.
    #[must_use]
    pub const fn single(self) -> Option<ForwardAttr> {
        match self {
            Self::One(attr) => Some(attr),
            Self::Many(_) => None,
        }
    }
}

/// Range of co-applied attributes in a document store.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ForwardAttrRange {
    start: u32,
    len: u32,
}

/// Owned backing store: the string interner plus the flat content / segment
/// `Vec`s the owned nodes' [`StrId`] / range payloads resolve against.
///
/// Not `Copy` (owns heap storage); not `PartialEq` (the interner's
/// `InternStats` field is not `PartialEq`).
#[derive(Debug, Clone, Default)]
pub struct NodeStore {
    /// String interner backing every [`StrId`] in the tree.
    pub interner: StrInterner,
    /// Flat pool of [`Content`] entries; [`ContentRange`]s index here.
    contents: Vec<Content>,
    /// Flat pool of [`Segment`] entries; [`SegRange`]s index here.
    segments: Vec<Segment>,
    forward_attrs: Vec<ForwardAttr>,
    partial_layouts: Vec<PartialLayout>,
    illustrations: Vec<Illustration>,
    transcribed_notes: Vec<TranscribedNotes>,
}

impl NodeStore {
    pub(crate) fn push_transcribed_notes(&mut self, notes: TranscribedNotes) -> TranscribedNotesId {
        let next = u32::try_from(self.transcribed_notes.len())
            .expect("note group pool exceeds u32")
            .checked_add(1)
            .expect("note group pool exceeds u32");
        self.transcribed_notes.push(notes);
        TranscribedNotesId(NonZeroU32::new(next).unwrap())
    }

    /// Resolve against the document that allocated the note associations.
    #[must_use]
    pub fn resolve_transcribed_notes(&self, id: TranscribedNotesId) -> &TranscribedNotes {
        &self.transcribed_notes[(id.0.get() - 1) as usize]
    }

    pub(crate) fn push_illustration(&mut self, image: Illustration) -> IllustrationId {
        let next = u32::try_from(self.illustrations.len())
            .expect("illustration pool exceeds u32")
            .checked_add(1)
            .expect("illustration pool exceeds u32");
        self.illustrations.push(image);
        IllustrationId(NonZeroU32::new(next).unwrap())
    }

    /// Resolve against the document that allocated the illustration.
    #[must_use]
    pub fn resolve_illustration(&self, id: IllustrationId) -> Illustration {
        self.illustrations[(id.0.get() - 1) as usize]
    }

    pub(crate) fn illustration_mut(&mut self, id: IllustrationId) -> &mut Illustration {
        &mut self.illustrations[(id.0.get() - 1) as usize]
    }

    /// Retain one partial instruction without enlarging every complete payload.
    ///
    /// # Panics
    /// Panics if the document exhausts the native handle space.
    pub fn push_partial_layout(&mut self, partial: PartialLayout) -> PartialLayoutId {
        let index =
            u32::try_from(self.partial_layouts.len()).expect("partial layout pool exceeds u32");
        self.partial_layouts.push(partial);
        PartialLayoutId(
            NonZeroU32::new(
                index
                    .checked_add(1)
                    .expect("partial layout pool exceeds u32"),
            )
            .unwrap(),
        )
    }

    /// Resolve evidence against the document that allocated its handle.
    #[must_use]
    pub fn resolve_partial_layout(&self, id: PartialLayoutId) -> &PartialLayout {
        &self.partial_layouts[(id.0.get() - 1) as usize]
    }
    /// Empty store.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Store a nonempty collection of distinct, co-applied attributes.
    ///
    /// # Panics
    /// Panics for an empty or duplicate collection, or an exhausted pool.
    pub fn push_forward_attrs(&mut self, attrs: &[ForwardAttr]) -> ForwardAttrs {
        assert!(!attrs.is_empty(), "forward attributes must be nonempty");
        for (idx, attr) in attrs.iter().enumerate() {
            assert!(!attrs[..idx].contains(attr), "duplicate forward attribute");
        }
        if let [attr] = attrs {
            return ForwardAttrs::One(*attr);
        }
        let start = u32::try_from(self.forward_attrs.len()).expect("attribute pool exceeds u32");
        let len = u32::try_from(attrs.len()).expect("attribute run exceeds u32");
        self.forward_attrs.extend_from_slice(attrs);
        ForwardAttrs::Many(ForwardAttrRange { start, len })
    }

    /// Resolve attributes against the store that owns their range.
    #[must_use]
    pub fn resolve_forward_attrs<'a>(&'a self, attrs: &'a ForwardAttrs) -> &'a [ForwardAttr] {
        match attrs {
            ForwardAttrs::One(attr) => slice::from_ref(attr),
            ForwardAttrs::Many(range) => {
                &self.forward_attrs[range.start as usize..(range.start + range.len) as usize]
            }
        }
    }

    /// Intern `s` into the store's interner, returning a stable [`StrId`].
    pub fn intern(&mut self, s: &str) -> StrId {
        self.interner.intern(s)
    }

    /// Resolve a [`StrId`] to its interned bytes.
    ///
    /// # Panics
    ///
    /// Panics if `id` was not produced by this store's interner.
    #[must_use]
    pub fn resolve_str(&self, id: StrId) -> &str {
        self.interner.resolve(id)
    }

    /// Append a content run and return the [`ContentRange`] that addresses it.
    ///
    /// # Panics
    ///
    /// Panics if the content pool would exceed `u32::MAX` entries — not
    /// reachable for any realistic document.
    pub fn push_contents(&mut self, items: &[Content]) -> ContentRange {
        let start =
            u32::try_from(self.contents.len()).expect("content pool exceeds u32 entry count");
        let len = u32::try_from(items.len()).expect("content run exceeds u32 length");
        self.contents.extend_from_slice(items);
        ContentRange { start, len }
    }

    /// Append a segment run and return the [`SegRange`] that addresses it.
    ///
    /// # Panics
    ///
    /// Panics if the segment pool would exceed `u32::MAX` entries — not
    /// reachable for any realistic document.
    pub fn push_segments(&mut self, items: &[Segment]) -> SegRange {
        let start =
            u32::try_from(self.segments.len()).expect("segment pool exceeds u32 entry count");
        let len = u32::try_from(items.len()).expect("segment run exceeds u32 length");
        self.segments.extend_from_slice(items);
        SegRange { start, len }
    }

    /// Resolve a [`ContentRange`] to its sub-slice of the content pool.
    ///
    /// # Panics
    ///
    /// Panics if the range falls outside the content pool.
    #[must_use]
    pub fn resolve_content_range(&self, range: ContentRange) -> &[Content] {
        let start = range.start as usize;
        &self.contents[start..start + range.len as usize]
    }

    /// Resolve a [`SegRange`] to its sub-slice of the segment pool.
    ///
    /// # Panics
    ///
    /// Panics if the range falls outside the segment pool.
    #[must_use]
    pub fn resolve_seg_range(&self, range: SegRange) -> &[Segment] {
        let start = range.start as usize;
        &self.segments[start..start + range.len as usize]
    }

    /// Plain-text fast path over a length-1 content run: `Some(text)` iff the
    /// run is exactly one [`Content::Plain`]; `None` for a `Segments` run
    /// or any `len != 1`.
    ///
    /// Consumers reading a single-`Content` field (ruby base/reading,
    /// forward-format target, …) take its plain text through this.
    ///
    /// # Panics
    ///
    /// Panics if `range` falls outside the content pool (via
    /// [`Self::resolve_content_range`]).
    #[must_use]
    pub fn content_range_as_plain(&self, range: ContentRange) -> Option<&str> {
        match self.resolve_content_range(range) {
            [Content::Plain(id)] => Some(self.resolve_str(*id)),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // UNIT TEST 4: ranges resolve to the correct sub-slices.
    #[test]
    fn ranges_resolve_to_correct_sub_slices() {
        let mut store = NodeStore::new();
        let a = store.intern("a");
        let b = store.intern("b");
        let c = store.intern("c");

        // Two separate content runs must not alias.
        let first = store.push_contents(&[Content::Plain(a)]);
        let second = store.push_contents(&[Content::Plain(b), Content::Plain(c)]);

        assert_eq!(first, ContentRange { start: 0, len: 1 });
        assert_eq!(second, ContentRange { start: 1, len: 2 });
        assert_eq!(store.resolve_content_range(first), &[Content::Plain(a)]);
        assert_eq!(
            store.resolve_content_range(second),
            &[Content::Plain(b), Content::Plain(c)]
        );

        // Segment ranges resolve independently.
        let seg = store.push_segments(&[Segment::Text(a), Segment::Text(c)]);
        assert_eq!(seg, SegRange { start: 0, len: 2 });
        assert_eq!(
            store.resolve_seg_range(seg),
            &[Segment::Text(a), Segment::Text(c)]
        );
    }

    #[test]
    fn content_range_as_plain_length_one() {
        let mut store = NodeStore::new();
        let a = store.intern("foo");
        let b = store.intern("bar");

        // Length-1 `Plain` run → the interned text (the 99%+ majority case).
        let plain = store.push_contents(&[Content::Plain(a)]);
        assert_eq!(store.content_range_as_plain(plain), Some("foo"));

        // A `Segments` run → `None` (mixed content; not the length-1 plain fast path).
        let seg = store.push_segments(&[Segment::Text(a)]);
        let mixed = store.push_contents(&[Content::Segments(seg)]);
        assert_eq!(store.content_range_as_plain(mixed), None);

        // A multi-entry run → `None` (only the length-1 fast path is plain).
        let two = store.push_contents(&[Content::Plain(a), Content::Plain(b)]);
        assert_eq!(store.content_range_as_plain(two), None);
    }
}
