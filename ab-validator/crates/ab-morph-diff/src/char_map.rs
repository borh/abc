//! Precomputed char<->byte offset map for O(1) span conversions.
//!
//! Building this once per source text replaces the O(n^2)
//! `source[..byte_offset].chars().count()` / `text.char_indices().nth(i)`
//! patterns that were duplicated across the morphology pipeline and paid
//! O(prefix length) per morpheme.

/// Bidirectional char-index <-> byte-index map for a source text.
///
/// Build once via [`CharByteMap::new`]; lookups are O(1). The map is indexed
/// by byte offset for [`Self::char_count_at_byte`] and by char index for
/// [`Self::byte_offset_at_char`].
///
/// Building the map is O(source.len()). For a 1 MB source (≈330k CJK chars)
/// the two tables cost ≈6.6 MB and are dropped when the map goes out of scope.
#[derive(Clone)]
pub struct CharByteMap<'a> {
    source: &'a str,
    /// `byte_to_char[byte_offset]` = number of chars before `byte_offset`.
    /// Length = `source.len() + 1`. Indexed by byte offset; only entries at
    /// char boundaries are meaningful, but every byte offset is populated
    /// cheaply so callers that have already verified `is_char_boundary` can
    /// index directly.
    byte_to_char: Vec<u32>,
    /// `byte_offset_at_char[char_index]` = byte offset of the start of char
    /// `char_index`. Length = char count + 1. The final entry is `source.len()`.
    char_to_byte: Vec<usize>,
}

impl<'a> CharByteMap<'a> {
    /// Build the char<->byte map for `source`.
    pub fn new(source: &'a str) -> Self {
        let byte_len = source.len();
        let mut byte_to_char = vec![0u32; byte_len + 1];
        // Upper bound on char count = byte_len (all-ASCII); shrink later to fit.
        let mut char_to_byte = Vec::with_capacity(byte_len + 1);

        let mut char_count = 0u32;
        for (byte_index, _) in source.char_indices() {
            byte_to_char[byte_index] = char_count;
            char_count += 1;
            char_to_byte.push(byte_index);
        }
        byte_to_char[byte_len] = char_count;
        // Trailing entry maps the one-past-last char index to source.len().
        char_to_byte.push(byte_len);
        char_to_byte.shrink_to_fit();

        Self {
            source,
            byte_to_char,
            char_to_byte,
        }
    }

    /// Number of chars before `byte_offset`. The caller must ensure
    /// `byte_offset` is a char boundary (use `str::is_char_boundary` first);
    /// this is the same precondition as indexing `&source[..byte_offset]`.
    ///
    /// Equivalent to `source[..byte_offset].chars().count()` but O(1).
    #[inline]
    pub fn char_count_at_byte(&self, byte_offset: usize) -> usize {
        // Indexing is safe because byte_offset <= source.len() == byte_to_char.len()-1
        // when the caller respects the char-boundary precondition.
        self.byte_to_char[byte_offset] as usize
    }

    /// Byte offset of the start of char `char_index`.
    ///
    /// Returns `None` when `char_index` exceeds the char count. Equivalent to
    /// the `text.char_indices().nth(char_index)` pattern but O(1).
    #[inline]
    pub fn byte_offset_at_char(&self, char_index: usize) -> Option<usize> {
        self.char_to_byte.get(char_index).copied()
    }

    /// Total number of chars in the source. O(1).
    #[inline]
    pub fn char_count(&self) -> usize {
        // self.source.chars().count() is what this was before; the trailing
        // byte_to_char entry holds the total.
        self.byte_to_char[self.source.len()] as usize
    }

    /// Total byte length of the source. O(1).
    #[inline]
    pub fn source_byte_len(&self) -> usize {
        self.source.len()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_source_round_trips() {
        let map = CharByteMap::new("");
        assert_eq!(map.char_count(), 0);
        assert_eq!(map.char_count_at_byte(0), 0);
        assert_eq!(map.byte_offset_at_char(0), Some(0));
        assert_eq!(map.byte_offset_at_char(1), None);
    }

    #[test]
    fn ascii_offsets_match_chars_count() {
        let map = CharByteMap::new("abc");
        assert_eq!(map.char_count(), 3);
        assert_eq!(map.char_count_at_byte(0), 0);
        assert_eq!(map.char_count_at_byte(1), 1);
        assert_eq!(map.char_count_at_byte(3), 3);
        assert_eq!(map.byte_offset_at_char(0), Some(0));
        assert_eq!(map.byte_offset_at_char(2), Some(2));
        assert_eq!(map.byte_offset_at_char(3), Some(3));
        assert_eq!(map.byte_offset_at_char(4), None);
    }

    #[test]
    fn multibyte_offsets_match_old_helpers() {
        let source = "今日はabc";
        let map = CharByteMap::new(source);
        // Every char boundary's char count must match source[..b].chars().count()
        for (byte_index, _) in source.char_indices() {
            assert_eq!(
                map.char_count_at_byte(byte_index),
                source[..byte_index].chars().count(),
                "mismatch at byte {byte_index}"
            );
        }
        assert_eq!(map.char_count_at_byte(source.len()), source.chars().count());

        // char -> byte must match char_indices().nth
        for (i, (byte_index, _)) in source.char_indices().enumerate() {
            assert_eq!(map.byte_offset_at_char(i), Some(byte_index), "char {i}");
        }
        assert_eq!(
            map.byte_offset_at_char(source.chars().count()),
            Some(source.len())
        );
    }

    #[test]
    fn matches_legacy_helpers_on_japanese_sample() {
        let source = "吾輩は猫である。";
        let map = CharByteMap::new(source);
        for (byte_index, _) in source.char_indices() {
            assert_eq!(
                map.char_count_at_byte(byte_index),
                source[..byte_index].chars().count()
            );
        }
        for (i, (byte_index, _)) in source.char_indices().enumerate() {
            assert_eq!(map.byte_offset_at_char(i), Some(byte_index));
        }
    }
}
