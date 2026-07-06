#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TextChunk<'a> {
    pub(crate) text: &'a str,
    pub(crate) byte_offset: usize,
    pub(crate) char_offset: usize,
    pub(crate) hard_split: bool,
}

pub(crate) fn semantic_chunks(text: &str, max_bytes: usize) -> Vec<TextChunk<'_>> {
    if text.is_empty() {
        return Vec::new();
    }

    let mut chunks = Vec::new();
    let mut start = 0usize;
    let mut char_offset = 0usize;

    while start < text.len() {
        let (end, hard_split) = choose_chunk_end(text, start, max_bytes);
        let chunk_text = &text[start..end];
        let chunk_chars = chunk_text.chars().count();
        chunks.push(TextChunk {
            text: chunk_text,
            byte_offset: start,
            char_offset,
            hard_split,
        });
        start = end;
        char_offset += chunk_chars;
    }

    chunks
}

fn choose_chunk_end(text: &str, start: usize, max_bytes: usize) -> (usize, bool) {
    let hard_end = next_char_boundary_at_or_before(text, (start + max_bytes).min(text.len()));
    if hard_end == text.len() {
        if let Some(end) = find_first_boundary(text, start, hard_end) {
            return (end, false);
        }
        return (hard_end, false);
    }

    if let Some(end) = find_first_boundary(text, start, hard_end)
        && end > start
    {
        return (end, false);
    }

    (hard_end.max(next_char_boundary_after(text, start)), true)
}

fn find_first_boundary(text: &str, start: usize, hard_end: usize) -> Option<usize> {
    let mut iter = text[start..hard_end].char_indices().peekable();
    while let Some((relative_index, ch)) = iter.next() {
        if !is_chunk_boundary(ch) {
            continue;
        }
        let mut end = start + relative_index + ch.len_utf8();
        while let Some((next_relative_index, next_ch)) = iter.peek().copied() {
            if !is_chunk_boundary(next_ch) {
                break;
            }
            iter.next();
            end = start + next_relative_index + next_ch.len_utf8();
        }
        return Some(end);
    }
    None
}

fn is_chunk_boundary(ch: char) -> bool {
    ch == '\n' || ch == '\r' || matches!(ch, '。' | '！' | '？' | '!' | '?')
}

fn next_char_boundary_at_or_before(text: &str, mut index: usize) -> usize {
    while index > 0 && !text.is_char_boundary(index) {
        index -= 1;
    }
    index
}

fn next_char_boundary_after(text: &str, mut index: usize) -> usize {
    index += 1;
    while index < text.len() && !text.is_char_boundary(index) {
        index += 1;
    }
    index.min(text.len())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn chunking_prefers_sentence_boundaries_under_limit() {
        let chunks = semantic_chunks("吾輩は猫である。名前はまだ無い。", 10_000);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "吾輩は猫である。");
        assert_eq!(chunks[0].byte_offset, 0);
        assert_eq!(chunks[0].char_offset, 0);
        assert!(!chunks[0].hard_split);
        assert_eq!(chunks[1].text, "名前はまだ無い。");
        assert_eq!(chunks[1].byte_offset, "吾輩は猫である。".len());
        assert_eq!(chunks[1].char_offset, "吾輩は猫である。".chars().count());
        assert!(!chunks[1].hard_split);
    }

    #[test]
    fn chunking_marks_hard_split_when_no_sentence_boundary_fits() {
        let chunks = semantic_chunks("abcdef", 3);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "abc");
        assert!(chunks[0].hard_split);
        assert_eq!(chunks[1].text, "def");
        assert!(!chunks[1].hard_split);
    }

    #[test]
    fn chunking_keeps_adjacent_sentence_punctuation_together() {
        let chunks = semantic_chunks("本当！？そう！！！はい。", 10_000);

        assert_eq!(chunks.len(), 3);
        assert_eq!(chunks[0].text, "本当！？");
        assert_eq!(chunks[1].text, "そう！！！");
        assert_eq!(chunks[2].text, "はい。");
    }

    #[test]
    fn chunking_does_not_split_decimal_points() {
        let chunks = semantic_chunks("値は5.4です。値は５．４です。", 10_000);

        assert_eq!(chunks.len(), 2);
        assert_eq!(chunks[0].text, "値は5.4です。");
        assert_eq!(chunks[1].text, "値は５．４です。");
    }
}
