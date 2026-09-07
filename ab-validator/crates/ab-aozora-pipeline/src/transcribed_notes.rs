//! Interpret source statements that associate separately transcribed note lines with a poem.

use ab_aozora_syntax::{
    MarginNotePosition, Span,
    alloc::Allocator,
    ast::{Node, NonEmptySpan, TranscribedNote},
};

struct NamedNote<'a> {
    note: &'a str,
    target: &'a str,
    position: MarginNotePosition,
}

fn quoted(text: &str) -> Option<(&str, &str)> {
    let (value, rest) = text.strip_prefix('「')?.split_once('」')?;
    (!value.is_empty() && !value.contains(['「', '\n', '\r'])).then_some((value, rest))
}

fn named_notes(body: &str) -> Option<Vec<NamedNote<'_>>> {
    let mut rest = body
        .strip_suffix("注記するような形で")?
        .trim_end_matches('、');
    let mut target = None;
    let mut notes = Vec::new();
    while !rest.is_empty() {
        let (note, tail) = quoted(rest)?;
        let mut tail = tail.strip_prefix('は')?;
        if tail.starts_with('「') {
            let (named, following) = quoted(tail)?;
            target = Some(named);
            tail = following.strip_prefix('の')?;
        }
        let (position, following) = if let Some(following) = tail.strip_prefix("右側に") {
            (MarginNotePosition::Right, following)
        } else if let Some(following) = tail.strip_prefix("左側に") {
            (MarginNotePosition::Left, following)
        } else {
            return None;
        };
        if notes
            .iter()
            .any(|existing: &NamedNote<'_>| existing.note == note)
        {
            return None;
        }
        notes.push(NamedNote {
            note,
            target: target?,
            position,
        });
        rest = if following.is_empty() {
            following
        } else {
            following.strip_prefix('、')?
        };
    }
    (!notes.is_empty()).then_some(notes)
}

fn formatting_space(ch: char) -> bool {
    matches!(ch, ' ' | '\t' | '\u{3000}')
}

fn extent(start: usize, end: usize) -> Option<NonEmptySpan> {
    NonEmptySpan::new(Span::new(
        u32::try_from(start).ok()?,
        u32::try_from(end).ok()?,
    ))
}

fn note_line(
    line: &str,
    start: usize,
    named: &[NamedNote<'_>],
) -> Option<Vec<(usize, NonEmptySpan)>> {
    let mut rest = line;
    let mut found = Vec::new();
    loop {
        rest = rest.trim_start_matches(formatting_space);
        if rest.is_empty() {
            break;
        }
        let mut matching = named
            .iter()
            .enumerate()
            .filter(|(_, note)| rest.starts_with(note.note));
        let (index, note) = matching.next()?;
        if matching.next().is_some() || found.iter().any(|(seen, _)| *seen == index) {
            return None;
        }
        let offset = start + line.len() - rest.len();
        found.push((index, extent(offset, offset + note.note.len())?));
        rest = &rest[note.note.len()..];
    }
    (!found.is_empty()).then_some(found)
}

fn poem_targets(line: &str, start: usize, named: &[NamedNote<'_>]) -> Option<Vec<NonEmptySpan>> {
    named
        .iter()
        .map(|note| {
            let mut occurrences = line.match_indices(note.target);
            let (offset, _) = occurrences.next()?;
            if occurrences.next().is_some() {
                return None;
            }
            extent(start + offset, start + offset + note.target.len())
        })
        .collect()
}

/// Require one adjacent target line and complete coverage of all separate note lines.
/// Unsupported or ambiguous groups remain available to the ordinary unknown-marker path.
pub(crate) fn classify(source: &str, marker: Span, alloc: &mut Allocator) -> Option<Node> {
    let marker_start = usize::try_from(marker.start).ok()?;
    let marker_end = usize::try_from(marker.end).ok()?;
    let body = source
        .get(marker_start..marker_end)?
        .strip_prefix("［＃")?
        .strip_suffix('］')?;
    let named = named_notes(body)?;
    let marker_line = source[..marker_start]
        .rfind('\n')
        .map_or(0, |index| index + 1);
    let following_end = source[marker_end..]
        .find('\n')
        .map_or(source.len(), |offset| marker_end + offset);
    if !source[marker_line..marker_start]
        .chars()
        .all(formatting_space)
        || !source[marker_end..following_end]
            .trim_end_matches('\r')
            .chars()
            .all(formatting_space)
    {
        return None;
    }
    let mut cursor = marker_line;
    let mut notes = vec![None; named.len()];
    let mut targets = None;
    let marker_line_end = following_end + usize::from(following_end < source.len());
    let mut apparatus_lines = vec![Span::new(
        u32::try_from(marker_line).ok()?,
        u32::try_from(marker_line_end).ok()?,
    )];
    while cursor > 0 {
        let end = cursor;
        let text_end = source[..end].strip_suffix('\n').map_or(end, str::len);
        let text_end = source[..text_end]
            .strip_suffix('\r')
            .map_or(text_end, str::len);
        let start = source[..text_end].rfind('\n').map_or(0, |index| index + 1);
        let line = &source[start..text_end];
        if let Some(found) = note_line(line, start, &named) {
            for (index, span) in found {
                if notes[index].replace(span).is_some() {
                    return None;
                }
            }
            apparatus_lines.push(Span::new(
                u32::try_from(start).ok()?,
                u32::try_from(end).ok()?,
            ));
        } else if targets.is_none() {
            targets = Some(poem_targets(line, start, &named)?);
        } else {
            return None;
        }
        if notes.iter().all(Option::is_some) && targets.is_some() {
            break;
        }
        cursor = start;
    }
    let targets = targets?;
    let associations: Option<Vec<_>> = named
        .iter()
        .zip(notes)
        .zip(targets)
        .map(|((named, note), target)| {
            Some(TranscribedNote {
                note: note?,
                target,
                position: named.position,
            })
        })
        .collect();
    Some(alloc.transcribed_notes(body, associations?, apparatus_lines))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::Pipeline;
    use ab_aozora_syntax::ast::NodeRef;

    #[test]
    fn two_sided_source_notes_own_external_payloads_and_one_target() {
        let source = "　　　　　　　　　　なごりイ\n飯蛸の手をひろげたる檐端哉\n　　　　　　　　　り檐の花イ\n［＃「なごりイ」は「檐端哉」の右側に、「り檐の花イ」は左側に、注記するような形で］\n";
        let parsed = Pipeline::run_to_completion(source);
        let group = parsed
            .source_nodes
            .iter()
            .find_map(|node| match node.node {
                NodeRef::Inline(Node::TranscribedNotes(id))
                | NodeRef::BlockLeaf(Node::TranscribedNotes(id)) => {
                    Some(parsed.store.resolve_transcribed_notes(id))
                }
                _ => None,
            })
            .expect("one source-owned group");
        assert_eq!(group.notes.len(), 2);
        assert_eq!(group.apparatus_lines.len(), 3);
        for (note, expected, side) in [
            (&group.notes[0], "なごりイ", MarginNotePosition::Right),
            (&group.notes[1], "り檐の花イ", MarginNotePosition::Left),
        ] {
            let target = note.target.span();
            let payload = note.note.span();
            assert_eq!(
                &parsed.sanitized[target.start as usize..target.end as usize],
                "檐端哉"
            );
            assert_eq!(
                &parsed.sanitized[payload.start as usize..payload.end as usize],
                expected
            );
            assert_eq!(note.position, side);
            assert!(payload.end < u32::try_from(source.find("［＃").unwrap()).unwrap());
        }
    }

    #[test]
    fn ambiguous_or_partial_groups_never_reclassify_source_lines() {
        for source in [
            "繩\n綱と綱\n［＃「繩」は「綱」の右側に注記するような形で］",
            "繩と別の本文\n馬つなぐ綱\n［＃「繩」は「綱」の右側に注記するような形で］",
            "繩\n無関係な行\n馬つなぐ綱\n［＃「繩」は「綱」の右側に注記するような形で］",
            "繩\n馬つなぐ綱［＃「繩」は「綱」の右側に注記するような形で］",
            "馬つなぐ綱\n［＃「繩」は「綱」の右側に注記するような形で］",
            "繩　別注\n馬つなぐ綱\n［＃「繩」は「綱」の右側に、「別注」は「不存在」の左側に注記するような形で］",
        ] {
            let parsed = Pipeline::run_to_completion(source);
            assert!(
                !parsed.source_nodes.iter().any(|node| matches!(
                    node.node,
                    NodeRef::Inline(Node::TranscribedNotes(_))
                        | NodeRef::BlockLeaf(Node::TranscribedNotes(_))
                )),
                "{source}"
            );
        }
    }
}
