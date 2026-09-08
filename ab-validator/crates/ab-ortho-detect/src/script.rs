/// Convert full-width katakana to hiragana.
///
/// The primary U+30A1–U+30F6 range uses a constant -0x60 codepoint offset.
/// Half-width katakana (U+FF65–U+FF9F) is first converted to full-width
/// via a per-codepoint lookup, then the standard offset applies.
/// Edge cases (ヴ→う゛, small katakana extensions, iteration marks) are
/// handled explicitly.
///
/// # Examples
///
/// ```
/// use ab_ortho_detect::script::kata_to_hira;
/// assert_eq!(kata_to_hira("カタカナ"), "かたかな");
/// assert_eq!(kata_to_hira("吾輩ハ猫デアル"), "吾輩は猫である");
/// assert_eq!(kata_to_hira("今日ヴ"), "今日う゛");
/// ```
#[must_use]
pub fn kata_to_hira(text: &str) -> String {
    let mut out = String::with_capacity(text.len());

    for ch in text.chars() {
        match ch {
            // ヴ → う゛ (two characters for dictionary compatibility)
            // Must precede the range arm: U+30F4 is inside U+30A1–U+30F6.
            '\u{30F4}' => {
                out.push('\u{3046}');
                out.push('\u{309B}');
            }

            // Obsolete kana: ヷ→わ゛, ヸ→ゐ゛, ヹ→ゑ゛, ヺ→を゛
            '\u{30F7}' => {
                out.push('\u{308F}');
                out.push('\u{309B}');
            }
            '\u{30F8}' => {
                out.push('\u{3090}');
                out.push('\u{309B}');
            }
            '\u{30F9}' => {
                out.push('\u{3091}');
                out.push('\u{309B}');
            }
            '\u{30FA}' => {
                out.push('\u{3092}');
                out.push('\u{309B}');
            }

            // Full-width katakana block: U+30A1–U+30F6 → subtract 0x60
            c if ('\u{30A1}'..='\u{30F6}').contains(&c) => {
                let hira = char::from_u32(c as u32 - 0x60).unwrap();
                out.push(hira);
            }

            // Iteration marks: U+30FD ヽ→ゝ, U+30FE ヾ→ゞ
            '\u{30FD}' => out.push('\u{309D}'),
            '\u{30FE}' => out.push('\u{309E}'),

            // Chōonpu: passthrough (ー is U+30FC, outside the U+30A1–U+30F6 range)
            'ー' => out.push('ー'),

            // Katakana Phonetic Extensions: small forms → full-size hiragana
            '\u{31F0}' => out.push('\u{304F}'), // ㇰ→く
            '\u{31F1}' => out.push('\u{3057}'), // ㇱ→し
            '\u{31F2}' => out.push('\u{3059}'), // ㇲ→す
            '\u{31F3}' => out.push('\u{3068}'), // ㇳ→と
            '\u{31F4}' => out.push('\u{306C}'), // ㇴ→ぬ
            '\u{31F5}' => out.push('\u{306F}'), // ㇵ→は
            '\u{31F6}' => out.push('\u{3072}'), // ㇶ→ひ
            '\u{31F7}' => out.push('\u{3075}'), // ㇷ→ふ
            '\u{31F8}' => out.push('\u{3078}'), // ㇸ→へ
            '\u{31F9}' => out.push('\u{307B}'), // ㇹ→ほ
            '\u{31FA}' => out.push('\u{307E}'), // ㇺ→ま
            '\u{31FB}' => out.push('\u{3084}'), // ㇻ→や
            '\u{31FC}' => out.push('\u{308A}'), // ㇼ→り
            '\u{31FD}' => out.push('\u{308B}'), // ㇽ→る
            '\u{31FE}' => out.push('\u{308C}'), // ㇾ→れ
            '\u{31FF}' => out.push('\u{308D}'), // ㇿ→ろ

            // Half-width katakana: first convert to full-width, recurse
            c if ('\u{FF65}'..='\u{FF9F}').contains(&c) => {
                if let Some(fullwidth) = halfwidth_to_fullwidth_katakana(c) {
                    // Recurse: the fullwidth char will hit the main block above
                    let s = fullwidth.to_string();
                    out.push_str(&kata_to_hira(&s));
                } else {
                    out.push(c);
                }
            }

            // Everything else: passthrough (kanji, hiragana, punctuation, etc.)
            other => out.push(other),
        }
    }

    out
}

/// Map half-width katakana codepoint to full-width katakana codepoint.
fn halfwidth_to_fullwidth_katakana(ch: char) -> Option<char> {
    let result = match ch {
        // U+FF65 ･ → U+30FB ・(katakana middle dot)
        '\u{FF65}' => '\u{30FB}',
        // U+FF66–U+FF9F: half-width katakana letters
        '\u{FF66}' => '\u{30F2}', // ｦ→ヲ
        '\u{FF67}' => '\u{30A1}', // ｧ→ァ
        '\u{FF68}' => '\u{30A3}', // ｨ→ィ
        '\u{FF69}' => '\u{30A5}', // ｩ→ゥ
        '\u{FF6A}' => '\u{30A7}', // ｪ→ェ
        '\u{FF6B}' => '\u{30A9}', // ｫ→ォ
        '\u{FF6C}' => '\u{30E3}', // ｬ→ャ
        '\u{FF6D}' => '\u{30E5}', // ｭ→ュ
        '\u{FF6E}' => '\u{30E7}', // ｮ→ョ
        '\u{FF6F}' => '\u{30C3}', // ｯ→ッ
        '\u{FF70}' => '\u{30FC}', // ｰ→ー
        '\u{FF71}' => '\u{30A2}', // ｱ→ア
        '\u{FF72}' => '\u{30A4}', // ｲ→イ
        '\u{FF73}' => '\u{30A6}', // ｳ→ウ
        '\u{FF74}' => '\u{30A8}', // ｴ→エ
        '\u{FF75}' => '\u{30AA}', // ｵ→オ
        '\u{FF76}' => '\u{30AB}', // ｶ→カ
        '\u{FF77}' => '\u{30AD}', // ｷ→キ
        '\u{FF78}' => '\u{30AF}', // ｸ→ク
        '\u{FF79}' => '\u{30B1}', // ｹ→ケ
        '\u{FF7A}' => '\u{30B3}', // ｺ→コ
        '\u{FF7B}' => '\u{30B5}', // ｻ→サ
        '\u{FF7C}' => '\u{30B7}', // ｼ→シ
        '\u{FF7D}' => '\u{30B9}', // ｽ→ス
        '\u{FF7E}' => '\u{30BB}', // ｾ→セ
        '\u{FF7F}' => '\u{30BD}', // ｿ→ソ
        '\u{FF80}' => '\u{30BF}', // ﾀ→タ
        '\u{FF81}' => '\u{30C1}', // ﾁ→チ
        '\u{FF82}' => '\u{30C4}', // ﾂ→ツ
        '\u{FF83}' => '\u{30C6}', // ﾃ→テ
        '\u{FF84}' => '\u{30C8}', // ﾄ→ト
        '\u{FF85}' => '\u{30CA}', // ﾅ→ナ
        '\u{FF86}' => '\u{30CB}', // ﾆ→ニ
        '\u{FF87}' => '\u{30CC}', // ﾇ→ヌ
        '\u{FF88}' => '\u{30CD}', // ﾈ→ネ
        '\u{FF89}' => '\u{30CE}', // ﾉ→ノ
        '\u{FF8A}' => '\u{30CF}', // ﾊ→ハ
        '\u{FF8B}' => '\u{30D2}', // ﾋ→ヒ
        '\u{FF8C}' => '\u{30D5}', // ﾌ→フ
        '\u{FF8D}' => '\u{30D8}', // ﾍ→ヘ
        '\u{FF8E}' => '\u{30DB}', // ﾎ→ホ
        '\u{FF8F}' => '\u{30DE}', // ﾏ→マ
        '\u{FF90}' => '\u{30DF}', // ﾐ→ミ
        '\u{FF91}' => '\u{30E0}', // ﾑ→ム
        '\u{FF92}' => '\u{30E1}', // ﾒ→メ
        '\u{FF93}' => '\u{30E2}', // ﾓ→モ
        '\u{FF94}' => '\u{30E4}', // ﾔ→ヤ
        '\u{FF95}' => '\u{30E6}', // ﾕ→ユ
        '\u{FF96}' => '\u{30E8}', // ﾖ→ヨ
        '\u{FF97}' => '\u{30E9}', // ﾗ→ラ
        '\u{FF98}' => '\u{30EA}', // ﾘ→リ
        '\u{FF99}' => '\u{30EB}', // ﾙ→ル
        '\u{FF9A}' => '\u{30EC}', // ﾚ→レ
        '\u{FF9B}' => '\u{30ED}', // ﾛ→ロ
        '\u{FF9C}' => '\u{30EF}', // ﾜ→ワ
        '\u{FF9D}' => '\u{30F3}', // ﾝ→ン
        '\u{FF9E}' => '\u{309B}', // ﾞ→゛ (voiced mark: maps directly to hiragana combining)
        '\u{FF9F}' => '\u{309C}', // ﾟ→゜ (semi-voiced mark)
        _ => return None,
    };
    Some(result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn converts_fullwidth_katakana() {
        assert_eq!(kata_to_hira("カタカナ"), "かたかな");
    }

    #[test]
    fn converts_katakana_in_mixed_text() {
        assert_eq!(kata_to_hira("吾輩ハ猫デアル"), "吾輩は猫である");
    }

    #[test]
    fn passthrough_kanji_and_hiragana() {
        assert_eq!(kata_to_hira("漢字ひらがな"), "漢字ひらがな");
    }

    #[test]
    fn converts_vu_to_two_chars() {
        assert_eq!(kata_to_hira("今日ヴ"), "今日う゛");
    }

    #[test]
    fn converts_iteration_marks() {
        assert_eq!(kata_to_hira("ヽヽ"), "ゝゝ");
        assert_eq!(kata_to_hira("ヾ"), "ゞ");
    }

    #[test]
    fn converts_small_katakana_extensions() {
        assert_eq!(kata_to_hira("ㇱ"), "し");
        assert_eq!(kata_to_hira("ㇺ"), "ま");
        assert_eq!(kata_to_hira("ㇿ"), "ろ");
    }

    #[test]
    fn converts_halfwidth_katakana() {
        assert_eq!(kata_to_hira("\u{FF71}\u{FF72}"), "あい");
    }

    #[test]
    fn passthrough_chouonpu() {
        assert_eq!(kata_to_hira("カード"), "かーど");
    }

    #[test]
    fn empty_string() {
        assert_eq!(kata_to_hira(""), "");
    }

    #[test]
    fn converts_obsolete_kana() {
        assert_eq!(kata_to_hira("\u{30F7}"), "わ゛");
    }
}
