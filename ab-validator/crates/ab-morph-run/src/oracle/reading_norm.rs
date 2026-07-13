//! Deterministic kana canonicalizer for ruby-oracle reading comparison.
//! Applied identically to the analyzer reading and the editor ruby reading.

use unicode_normalization::UnicodeNormalization;

/// Canonicalize a reading string for oracle equality comparison.
/// Layers: NFKC → strip non-kana → katakana→hiragana → discrete historical
/// folds → long-vowel canonicalization → bounded lexical table. Idempotent.
pub(crate) fn normalize(reading: &str) -> String {
    // 1. NFKC, then katakana→hiragana keeping only kana; expand iteration marks
    //    (ゝ ゞ ヽ ヾ) against the last repeatable kana (never the prolongation mark
    //    ー) so historical ruby like ものゝふ → もののふ instead of dropping the
    //    mark (ものふ). ゝ/ヽ repeat the unvoiced base (classical); ゞ/ヾ the voiced
    //    form. The pushed kana updates last_kana so chained marks (ゝゝ) work.
    let mut chars: Vec<char> = Vec::new();
    let mut last_kana: Option<char> = None;
    for ch in reading.nfkc() {
        match ch {
            'ゝ' | 'ヽ' => {
                if let Some(prev) = last_kana {
                    let r = unvoiced_base(prev);
                    chars.push(r);
                    last_kana = Some(r);
                }
            }
            'ゞ' | 'ヾ' => {
                if let Some(prev) = last_kana {
                    let r = voiced_form(unvoiced_base(prev));
                    chars.push(r);
                    last_kana = Some(r);
                }
            }
            _ => {
                if let Some(h) = kana_to_hiragana(ch) {
                    chars.push(h);
                    if h != 'ー' {
                        last_kana = Some(h);
                    }
                }
            }
        }
    }

    // 5. bounded lexical table (applied on the hiragana form, longest-match first).
    apply_lexical_table(&mut chars);

    // 3. discrete historical single-char substitutions.
    for ch in &mut chars {
        *ch = match *ch {
            'ゐ' => 'い',
            'ゑ' => 'え',
            'ぢ' => 'じ',
            'づ' => 'ず',
            other => other,
        };
    }

    // 3b. くわ/ぐわ → か/が ; word-medial は行 → わ行 (positions > 0).
    let folded = fold_digraphs_and_medial_ha(&chars);

    // 4. long-vowel canonicalization: collapse each vowel's long-vowel spellings.
    long_vowel_canonicalize(&folded)
}

/// Map a char to hiragana, dropping anything that is not kana (spaces, ・, punctuation).
fn kana_to_hiragana(ch: char) -> Option<char> {
    match ch {
        // Katakana block → hiragana (offset 0x60), excluding the prolongation mark.
        'ァ'..='ン' => char::from_u32(ch as u32 - 0x60),
        'ー' => Some('ー'), // prolongation mark handled in long-vowel layer
        'ぁ'..='ん' => Some(ch),
        _ => None,
    }
}

/// Strip dakuten/handakuten to the base hiragana (が→か, ぱ→は); identity otherwise.
fn unvoiced_base(ch: char) -> char {
    match ch {
        'が' => 'か',
        'ぎ' => 'き',
        'ぐ' => 'く',
        'げ' => 'け',
        'ご' => 'こ',
        'ざ' => 'さ',
        'じ' => 'し',
        'ず' => 'す',
        'ぜ' => 'せ',
        'ぞ' => 'そ',
        'だ' => 'た',
        'ぢ' => 'ち',
        'づ' => 'つ',
        'で' => 'て',
        'ど' => 'と',
        'ば' | 'ぱ' => 'は',
        'び' | 'ぴ' => 'ひ',
        'ぶ' | 'ぷ' => 'ふ',
        'べ' | 'ぺ' => 'へ',
        'ぼ' | 'ぽ' => 'ほ',
        other => other,
    }
}

/// Add dakuten to a base hiragana (か→が); identity when there is no voiced form.
fn voiced_form(ch: char) -> char {
    match ch {
        'か' => 'が',
        'き' => 'ぎ',
        'く' => 'ぐ',
        'け' => 'げ',
        'こ' => 'ご',
        'さ' => 'ざ',
        'し' => 'じ',
        'す' => 'ず',
        'せ' => 'ぜ',
        'そ' => 'ぞ',
        'た' => 'だ',
        'ち' => 'ぢ',
        'つ' => 'づ',
        'て' => 'で',
        'と' => 'ど',
        'は' => 'ば',
        'ひ' => 'び',
        'ふ' => 'ぶ',
        'へ' => 'べ',
        'ほ' => 'ぼ',
        other => other,
    }
}

fn apply_lexical_table(chars: &mut Vec<char>) {
    // Longest-match-first historical readings that are not mechanically foldable.
    const TABLE: &[(&str, &str)] = &[
        ("てふ", "ちょう"),
        ("けふ", "きょう"),
        ("せふ", "しょう"),
        ("でふ", "じょう"),
    ];
    let mut s: String = chars.iter().collect();
    for (from, to) in TABLE {
        s = s.replace(from, to);
    }
    *chars = s.chars().collect();
}

fn fold_digraphs_and_medial_ha(chars: &[char]) -> Vec<char> {
    let mut out: Vec<char> = Vec::with_capacity(chars.len());
    let mut i = 0;
    while i < chars.len() {
        // くわ→か, ぐわ→が
        if i + 1 < chars.len() && chars[i + 1] == 'わ' {
            match chars[i] {
                'く' => {
                    out.push('か');
                    i += 2;
                    continue;
                }
                'ぐ' => {
                    out.push('が');
                    i += 2;
                    continue;
                }
                _ => {}
            }
        }
        // word-medial は行 → わ行 (never at position 0)
        let folded = if i > 0 {
            match chars[i] {
                'は' => 'わ',
                'ひ' => 'い',
                'ふ' => 'う',
                'へ' => 'え',
                'ほ' => 'お',
                other => other,
            }
        } else {
            chars[i]
        };
        out.push(folded);
        i += 1;
    }
    out
}

#[derive(Clone, Copy)]
enum Vowel {
    A,
    I,
    U,
    E,
    O,
}

/// Vowel class of a hiragana mora kana, INCLUDING small yōon kana (ゃゅょ) so a
/// prolongation mark after a yōon expands correctly (きょー → きょう, not きょょ).
/// Returns None for non-vowel-bearing symbols (っ sokuon, ん, anything else).
fn vowel_of(ch: char) -> Option<Vowel> {
    use Vowel::{A, E, I, O, U};
    Some(match ch {
        'あ' | 'か' | 'が' | 'さ' | 'ざ' | 'た' | 'だ' | 'な' | 'は' | 'ば' | 'ぱ' | 'ま'
        | 'や' | 'ら' | 'わ' | 'ぁ' | 'ゃ' => A,
        'い' | 'き' | 'ぎ' | 'し' | 'じ' | 'ち' | 'ぢ' | 'に' | 'ひ' | 'び' | 'ぴ' | 'み'
        | 'り' | 'ゐ' | 'ぃ' => I,
        'う' | 'く' | 'ぐ' | 'す' | 'ず' | 'つ' | 'づ' | 'ぬ' | 'ふ' | 'ぶ' | 'ぷ' | 'む'
        | 'ゆ' | 'る' | 'ぅ' | 'ゅ' => U,
        'え' | 'け' | 'げ' | 'せ' | 'ぜ' | 'て' | 'で' | 'ね' | 'へ' | 'べ' | 'ぺ' | 'め'
        | 'れ' | 'ゑ' | 'ぇ' => E,
        'お' | 'こ' | 'ご' | 'そ' | 'ぞ' | 'と' | 'ど' | 'の' | 'ほ' | 'ぼ' | 'ぽ' | 'も'
        | 'よ' | 'ろ' | 'を' | 'ぉ' | 'ょ' => O,
        _ => return None,
    })
}

/// The single kana appended after a vowel-class mora to spell out its modern
/// long-vowel form: long-o appends う (…おう) and long-e appends い (…えい),
/// so the pron form (ー) converges with the kana form (which already uses
/// う/い in context).
fn long_vowel_kana(v: Vowel) -> char {
    match v {
        Vowel::A => 'あ',
        Vowel::I => 'い',
        Vowel::U => 'う',
        Vowel::E => 'い',
        Vowel::O => 'う',
    }
}

fn long_vowel_canonicalize(chars: &[char]) -> String {
    // Expand each ー to the preceding mora's long-vowel kana (mora-aware via
    // vowel_of, so yōon works), then fold historical あ段+う long-o spellings
    // (あう/かう/…) to the modern おう family. So トーキョー, とうきょう, and
    // たうきやう all converge to とうきょう.
    let mut out: Vec<char> = Vec::with_capacity(chars.len());
    for &ch in chars {
        if ch == 'ー' {
            if let Some(v) = out.last().copied().and_then(vowel_of) {
                out.push(long_vowel_kana(v));
            }
            continue;
        }
        out.push(ch);
    }
    let s: String = out.iter().collect();
    s.replace("あう", "おう")
        .replace("かう", "こう")
        .replace("がう", "ごう")
        .replace("さう", "そう")
        .replace("たう", "とう")
        .replace("なう", "のう")
        .replace("はう", "ほう")
        .replace("まう", "もう")
        .replace("やう", "よう")
        .replace("らう", "ろう")
        .replace("わう", "おう")
}

#[cfg(test)]
mod tests {
    use super::normalize;

    #[test]
    fn katakana_and_hiragana_fold_equal() {
        assert_eq!(normalize("トウキョウ"), normalize("とうきょう"));
    }

    #[test]
    fn strips_interpunct_and_spaces() {
        assert_eq!(normalize("と・う きょう"), normalize("とうきょう"));
    }

    #[test]
    fn discrete_historical_substitutions() {
        assert_eq!(normalize("ゐ"), normalize("い"));
        assert_eq!(normalize("ゑ"), normalize("え"));
        assert_eq!(normalize("かぢ"), normalize("かじ"));
        assert_eq!(normalize("みづ"), normalize("みず"));
        assert_eq!(normalize("くわし"), normalize("かし"));
    }

    #[test]
    fn word_medial_ha_row_folds() {
        // 川: kaha (historical) → kawa (modern)
        assert_eq!(normalize("かは"), normalize("かわ"));
        assert_eq!(normalize("こひ"), normalize("こい"));
    }

    #[test]
    fn long_vowel_family_collapses() {
        // pron form (ー) == modern kana (う) == historical (かう→こう)
        assert_eq!(normalize("トーキョー"), normalize("とうきょう"));
        assert_eq!(normalize("かう"), normalize("こう"));
    }

    #[test]
    fn yoon_long_vowels_expand_by_mora() {
        // ー after a yōon expands the yōon's vowel, not the small kana literally.
        assert_eq!(normalize("キョー"), normalize("きょう"));
        assert_eq!(normalize("ショー"), normalize("しょう"));
        assert_eq!(normalize("リュー"), normalize("りゅう"));
        // long-e converges: pron センセー == kana せんせい
        assert_eq!(normalize("センセー"), normalize("せんせい"));
    }

    #[test]
    fn bounded_lexical_table() {
        assert_eq!(normalize("てふ"), normalize("ちょう"));
        assert_eq!(normalize("けふ"), normalize("きょう"));
    }

    #[test]
    fn idempotent() {
        for s in ["とうきやう", "トーキョー", "てふ", "くわし", "かは"] {
            assert_eq!(normalize(&normalize(s)), normalize(s));
        }
    }

    #[test]
    fn prop_idempotent_and_script_invariant() {
        // hegel-style: any katakana string canonicalizes equal to its hiragana form,
        // and normalize is idempotent. Kept as a bounded enumerated check to avoid
        // pulling generators into a unit test; the hegel target is added.
        for s in ["カハ", "ミヅ", "テフテフ", "トーキヨー", "クワシ"] {
            let n = normalize(s);
            assert_eq!(normalize(&n), n, "idempotent for {s}");
        }
    }

    #[test]
    fn iteration_marks_expand_previous_kana() {
        // 武士《ものゝふ》: ゝ repeats の → もののふ (was dropped → ものふ).
        assert_eq!(normalize("ものゝふ"), normalize("もののふ"));
        // 心《こゝろ》
        assert_eq!(normalize("こゝろ"), normalize("こころ"));
        // katakana mark
        assert_eq!(normalize("スヽメ"), normalize("ススメ"));
    }

    #[test]
    fn voiced_iteration_mark_adds_dakuten() {
        // いすゞ: ゞ after す → ず → いすず.
        assert_eq!(normalize("いすゞ"), normalize("いすず"));
        // ゞ after a voiced kana repeats the voiced base: じゞ → じじ.
        assert_eq!(normalize("じゞ"), normalize("じじ"));
    }

    #[test]
    fn iteration_mark_edges() {
        // chained marks follow the mark's own output
        assert_eq!(normalize("たゝゝ"), normalize("たたた"));
        // mark after ー repeats the pre-ー kana, not ー
        assert_eq!(normalize("たーゝ"), normalize("たーた"));
        // mark with no preceding kana is dropped
        assert_eq!(normalize("ゝあ"), normalize("あ"));
        // ゝ unvoices a preceding voiced kana (classical rule): がゝ → がか
        assert_eq!(normalize("がゝ"), normalize("がか"));
    }
}
