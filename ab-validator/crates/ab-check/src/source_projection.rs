use unicode_normalization::UnicodeNormalization;

#[must_use]
pub fn comparison_lossy_body(txt: &str) -> String {
    ab_source_syntax::comparison_lossy_body(txt).into_owned()
}

/// Fold accented Latin letters to their base letters for order comparison.
///
/// The parser's sanitize stage composes Aozora accent notation in place
/// (`〔Franc,ois〕` becomes `〔François〕` in the AAT text) while the raw
/// source keeps the notation, so an order comparison must fold BOTH sides
/// with this one function. Composed letters lose their diacritics (NFD,
/// then combining marks dropped) and the non-decomposable specials map to
/// the base letters their notation spells. The fold is global and
/// symmetric: on the source side the notation's base letters survive in
/// order and its accent marks (`'`, `:`, `,`, …) become extra haystack
/// characters the subsequence walk skips; on the projection side the
/// composed letters fold to those same base letters.
#[must_use]
pub fn fold_accent_notation(text: &str) -> String {
    let stripped: String = text
        .nfd()
        .filter(|ch| !unicode_normalization::char::is_combining_mark(*ch))
        .collect();
    // Composed forms NFD cannot take apart; their notation spells the
    // base letters directly (s&s → ß, ae& → æ, o/ → ø, …).
    let mut out = String::with_capacity(stripped.len());
    for ch in stripped.chars() {
        match ch {
            'ß' => out.push_str("ss"),
            'æ' => out.push_str("ae"),
            'Æ' => out.push_str("AE"),
            'œ' => out.push_str("oe"),
            'Œ' => out.push_str("OE"),
            'ø' => out.push('o'),
            'Ø' => out.push('O'),
            'đ' | 'ð' => out.push('d'),
            'Đ' | 'Ð' => out.push('D'),
            'þ' => out.push_str("th"),
            'Þ' => out.push_str("Th"),
            'ı' => out.push('i'),
            _ => out.push(ch),
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn fold_accent_notation_strips_composed_diacritics_to_base_letters() {
        // Projection side: composed letters fold to the base letters the
        // source notation spells.
        assert_eq!(fold_accent_notation("〔François〕"), "〔Francois〕");
        assert_eq!(fold_accent_notation("〔Grüße〕"), "〔Grusse〕");
        // Mixed CJK/Latin spans fold too (span-agnostic global fold).
        assert_eq!(
            fold_accent_notation("〔Unsyô(阿蘇の峰名)〕"),
            "〔Unsyo(阿蘇の峰名)〕"
        );
    }

    #[test]
    fn fold_accent_notation_keeps_source_notation_marks_as_haystack_extras() {
        // Source side: the notation's base letters survive in order; its
        // accent marks stay put and are skipped by the subsequence walk.
        assert_eq!(fold_accent_notation("〔Franc,ois〕"), "〔Franc,ois〕");
        assert_eq!(fold_accent_notation("〔Gru:sse〕"), "〔Gru:sse〕");
        assert_eq!(fold_accent_notation("〔談話〕"), "〔談話〕");
    }

    #[test]
    fn comparison_lossy_body_excludes_unresolved_gaiji_descriptions() {
        let visible = comparison_lossy_body("二二※［＃小書き片仮名ン、237-11］が四");

        assert_eq!(visible, "二二が四");
    }

    #[test]
    fn comparison_lossy_body_projects_explicit_ruby_base_without_marker() {
        let visible =
            comparison_lossy_body("――『｜あのひとにとって、わたし《ルビ》はなんだろう？」");

        assert_eq!(visible, "――『あのひとにとって、わたしはなんだろう？」");
    }

    #[test]
    fn comparison_lossy_body_removes_orphan_ruby_after_unresolved_gaiji() {
        let visible =
            comparison_lossy_body("ことを、※［＃「口＋愛」、第3水準1-15-23］《おくび》にも");

        assert_eq!(visible, "ことを、にも");
    }

    #[test]
    fn comparison_lossy_body_removes_nested_gaiji_in_command() {
        let visible = comparison_lossy_body(
            "豌豆《ゑんどう》［＃「豌豆」は底本では「※［＃「足＋宛」、第3水準1-92-36］豆」］の大さ",
        );

        assert_eq!(visible, "豌豆の大さ");
    }

    #[test]
    fn comparison_lossy_body_removes_unmatched_ruby_delimiters() {
        let visible = comparison_lossy_body(
            "今日｜民族観念［＃「民族観念」に傍点］と呼ぶ。悲憤｜慷慨《こうがい》も知悉《ちしつ》した",
        );

        assert_eq!(visible, "今日民族観念と呼ぶ。悲憤慷慨も知悉した");
    }
}
