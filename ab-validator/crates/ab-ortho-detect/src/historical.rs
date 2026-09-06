//! Historical-kana → modern-kana surface modernizer
//!
//! Rewrites 歴史的仮名遣い to 現代仮名遣い at the **surface** level — keep the kanji,
//! rewrite only the historical *kana* — so an analyzer with no historical
//! dictionary (sudachi) can be fed modernized input, and every analyzer sees the
//! identical modernized text for cross-analyzer comparability.
//!
//! ## Modernization rules
//!
//! 1. **POS particle guard** — a 助詞 は/へ/を keeps its historical spelling
//!    (現代仮名遣い keeps the particles). Requires the segmentation oracle's POS.
//! 2. **Digraph / long-vowel tokens** (`pron` carries a `ー`) — reconstruct from
//!    the modern reading: `expand_long_vowel(kata_to_hira(pron))`. `pron` is the
//!    only field that resolves けふ→きょう, てふ→ちょう, かう→こう. This carries
//!    sokuon (ッ→っ) and yōon (ャ→ゃ) for free.
//! 3. **Everything else** — per-char surface rewrite over the token's kana (kanji
//!    pass through unchanged): the context-free obsolete-kana swaps (ゐ→い, ゑ→え,
//!    yotsugana づ→ず, ぢ→じ) plus the word-medial は/ひ/ふ/へ/ほ → わ/い/う/え/お
//!    (medial = not the token-initial char) and っ-before-て/た.
//!
//! No lexical exception table is applied. Segmentation boundaries and modern
//! spelling exceptions (such as retained づ/ぢ) can therefore cause mismatches.

use std::ops::Range;
use std::sync::Arc;

use ab_plaintext::SentenceSpan;

use crate::OrthoDetector;
use crate::script::kata_to_hira;
use crate::types::{OrthoAnnotation, OrthoDetectorId, OrthoNormalization};

/// The minimal token view the modernizer needs from the segmentation oracle.
///
/// Under M2 this is populated from a `kindai-bungo` UniDic morpheme: `surface`
/// is the historical surface, `pron` its modern reading (katakana, `ー` for long
/// vowels), `pos1` the coarse POS used only for the particle guard.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HistToken<'a> {
    /// The token's historical surface text.
    pub surface: &'a str,
    /// UniDic `pron` (modern reading, katakana). `None` if absent (`*`/empty).
    pub pron: Option<&'a str>,
    /// UniDic `pos1` (coarse POS). `None` if absent. Only `助詞` is consulted.
    pub pos1: Option<&'a str>,
}

/// The modern-kana surface for a historical token, or `None` if the token needs
/// no rewrite (already modern, or a protected particle). Callers emit one
/// token-granular annotation per `Some` so each length-changing span exactly
/// covers its offset-map entry.
#[must_use]
pub fn modernize_token(tok: &HistToken<'_>) -> Option<String> {
    let modern = modernize(tok.surface, tok.pron.unwrap_or(""), tok.pos1.unwrap_or(""));
    if modern == tok.surface {
        None
    } else {
        Some(modern)
    }
}

/// The raw rewrite. Returns the surface verbatim when nothing applies (so
/// [`modernize_token`] can decide "no change"). Kept separate for direct testing.
#[must_use]
fn modernize(surface: &str, pron: &str, pos1: &str) -> String {
    // 1. Particle guard: 現代仮名遣い keeps the particles は/へ/を.
    if pos1 == "助詞" && matches!(surface, "は" | "へ" | "を") {
        return surface.to_owned();
    }
    // 2. Digraph / long vowel: reconstruct from the modern reading — but ONLY for
    //    all-kana tokens. `pron` is the reading of the WHOLE token, so applying it
    //    to a kanji-bearing token (日曜, pron ニチヨー) would replace the kanji with
    //    kana (→にちよう). The ground-truth harness measured this path on all-kana
    //    tokens only; kanji-mixed tokens fall through to the kana-run rewrite (3).
    if all_kana(surface) && pron.contains('ー') {
        return expand_long_vowel(&kata_to_hira(pron));
    }
    // 3. Per-char surface rewrite; kanji and already-modern kana pass through.
    let chars: Vec<char> = kata_to_hira(surface).chars().collect();
    let mut out = String::with_capacity(surface.len());
    for i in 0..chars.len() {
        let c = chars[i];
        let next = chars.get(i + 1).copied().unwrap_or(' ');
        let m = match c {
            'ゐ' => 'い',
            'ゑ' => 'え',
            'づ' => 'ず',
            'ぢ' => 'じ',
            'つ' if matches!(next, 'て' | 'た') => 'っ',
            // Word-medial (non-token-initial) は行 → modern vowels.
            'は' if i > 0 => 'わ',
            'ひ' if i > 0 => 'い',
            'ふ' if i > 0 => 'う',
            'へ' if i > 0 => 'え',
            'ほ' if i > 0 => 'お',
            other => other,
        };
        out.push(m);
    }
    out
}

/// Spell a katakana reading's long-vowel `ー` per 現代仮名遣い: after an お-row
/// (or ょ) mora it is `う` (きょー→きょう), after an え-row mora `い` (せー→せい),
/// otherwise the row's own vowel (かー→かあ, きー→きい, くー→くう). Non-`ー`
/// characters pass through; the input is expected to already be hiragana
/// (run [`kata_to_hira`] on `pron` first).
#[must_use]
pub fn expand_long_vowel(hira: &str) -> String {
    let mut out = String::with_capacity(hira.len());
    let mut prev = ' ';
    for c in hira.chars() {
        if c == 'ー' {
            let filler = match vowel_of(prev) {
                'o' => 'う',
                'e' => 'い',
                'a' => 'あ',
                'i' => 'い',
                'u' => 'う',
                _ => continue,
            };
            out.push(filler);
        } else {
            out.push(c);
            prev = c;
        }
    }
    out
}

/// `true` if `c` is a hiragana or katakana kana (including obsolete ゐゑ/ヰヱ,
/// yotsugana づぢ, and the prolonged-sound mark ー). Kanji, punctuation, and
/// latin are excluded.
fn is_kana(c: char) -> bool {
    ('\u{3041}'..='\u{3096}').contains(&c) || ('\u{30A1}'..='\u{30FA}').contains(&c) || c == 'ー'
}

/// `true` if `surface` is non-empty and entirely kana — the gate on the
/// `pron`-reconstruction path, which would otherwise erase a token's kanji.
fn all_kana(surface: &str) -> bool {
    !surface.is_empty() && surface.chars().all(is_kana)
}

/// Coarse hiragana → vowel class for long-vowel expansion. Small-`ゃ/ゅ/ょ`
/// take the vowel of their base (ょ→o) so きょー→きょう resolves.
fn vowel_of(c: char) -> char {
    match c {
        'お' | 'こ' | 'そ' | 'と' | 'の' | 'ほ' | 'も' | 'よ' | 'ろ' | 'ご' | 'ぞ' | 'ど'
        | 'ぼ' | 'ぽ' | 'ょ' => 'o',
        'え' | 'け' | 'せ' | 'て' | 'ね' | 'へ' | 'め' | 'れ' | 'げ' | 'ぜ' | 'で' | 'べ'
        | 'ぺ' => 'e',
        'あ' | 'か' | 'さ' | 'た' | 'な' | 'は' | 'ま' | 'や' | 'ら' | 'わ' | 'が' | 'ざ'
        | 'だ' | 'ば' | 'ぱ' | 'ゃ' => 'a',
        'い' | 'き' | 'し' | 'ち' | 'に' | 'ひ' | 'み' | 'り' | 'ぎ' | 'じ' | 'ぢ' | 'び'
        | 'ぴ' => 'i',
        'う' | 'く' | 'す' | 'つ' | 'ぬ' | 'ふ' | 'む' | 'ゆ' | 'る' | 'ぐ' | 'ず' | 'づ'
        | 'ぶ' | 'ぷ' | 'ゅ' => 'u',
        _ => ' ',
    }
}

/// A stable content hash of the modernization rule set. Bound into
/// [`OrthoDetectorId::HistoricalRewriteV1`](crate::types::OrthoDetectorId) so a
/// rule edit changes the detector (and thus policy) identity — I2-D17's "detector
/// id binds a rules hash" requirement. Computed over a canonical dump of the rule
/// tables (below), so editing the tables here moves the hash; the version tag
/// guards against an accidental table-order permutation hashing the same.
#[must_use]
pub fn rules_hash() -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(RULES_CANON.as_bytes());
    format!("sha256:{:x}", hasher.finalize())
}

/// Canonical, human-auditable dump of every rewrite rule. Kept in lockstep with
/// [`modernize`]/[`expand_long_vowel`]; a test asserts the resulting hash so an
/// edit to one without the other fails CI rather than silently drifting.
const RULES_CANON: &str = concat!(
    "historical-rewrite-rules-v1\n",
    "particle-guard:助詞:は,へ,を\n",
    "digraph:all-kana&pron-ー->expand_long_vowel(kata_to_hira)\n",
    "long-vowel:o->う;e->い;a->あ;i->い;u->う\n",
    "swap:ゐ->い;ゑ->え;づ->ず;ぢ->じ\n",
    "sokuon:つ->っ/_て,た\n",
    "medial-hagyou:は->わ;ひ->い;ふ->う;へ->え;ほ->お\n",
);

/// One token from the historical-segmentation oracle (M2). Byte offsets are
/// relative to the tokenized text (a single sentence), so the detector adds the
/// sentence's `byte_offset` to lift them into source coordinates.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct HistOracleToken {
    /// The token's historical surface text.
    pub surface: String,
    /// Byte range of `surface` within the tokenized sentence text.
    pub byte_span: Range<usize>,
    /// UniDic `pron` (modern reading, katakana). `None` if absent.
    pub pron: Option<String>,
    /// UniDic `pos1` (coarse POS). `None` if absent.
    pub pos1: Option<String>,
}

/// The segmentation/reading oracle the M2 modernizer runs on. Defined here so
/// `ab-ortho-detect` stays free of any concrete analyzer crate; the adapter
/// (`ab-morph-analyzers`) implements it over a `kindai-bungo` `VibratoAnalyzer`.
pub trait HistoricalOracle: Send + Sync {
    /// Tokenize `text` (one sentence) into surface + span + `pron` + `pos1`.
    fn tokenize(&self, text: &str) -> Vec<HistOracleToken>;
}

/// The historical→modern surface detector (M2). Tokenizes each
/// sentence with the historical oracle and emits **one token-granular
/// [`OrthoAnnotation`] per rewritten token** (kind
/// [`OrthoNormalization::HistoricalToModern`]), so every length-changing span
/// (けふ 6B → きょう 9B) exactly covers its offset-map entry and remaps cleanly.
/// Unchanged tokens (already modern, protected particles) emit nothing.
pub struct HistoricalRewriteV1 {
    oracle: Arc<dyn HistoricalOracle>,
    dictionary_hash: String,
}

impl HistoricalRewriteV1 {
    /// Build the detector over a segmentation oracle. `dictionary_hash` is the
    /// SHA-256 of the `kindai-bungo` archive the oracle uses; it is bound into
    /// [`OrthoDetectorId::HistoricalRewriteV1`] so the modernizing dictionary is
    /// pinned into policy identity (I2-D17).
    #[must_use]
    pub fn new(oracle: Arc<dyn HistoricalOracle>, dictionary_hash: String) -> Self {
        Self {
            oracle,
            dictionary_hash,
        }
    }
}

impl OrthoDetector for HistoricalRewriteV1 {
    fn detector_id(&self) -> OrthoDetectorId {
        OrthoDetectorId::HistoricalRewriteV1 {
            dictionary_hash: self.dictionary_hash.clone(),
            rules_hash: rules_hash(),
        }
    }

    fn detect(&self, sentences: &[SentenceSpan<'_>]) -> Vec<OrthoAnnotation> {
        let mut annotations = Vec::new();
        for sentence in sentences {
            for token in self.oracle.tokenize(sentence.text) {
                let hist = HistToken {
                    surface: &token.surface,
                    pron: token.pron.as_deref(),
                    pos1: token.pos1.as_deref(),
                };
                let Some(modern) = modernize_token(&hist) else {
                    continue;
                };
                let start = sentence.byte_offset + token.byte_span.start;
                let end = sentence.byte_offset + token.byte_span.end;
                annotations.push(OrthoAnnotation {
                    source_byte_range: start..end,
                    normalized_text: modern,
                    kind: OrthoNormalization::HistoricalToModern,
                    confidence: None,
                });
            }
        }
        annotations
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn tok<'a>(surface: &'a str, pron: &'a str, pos1: &'a str) -> HistToken<'a> {
        HistToken {
            surface,
            pron: Some(pron),
            pos1: Some(pos1),
        }
    }

    // ---- expand_long_vowel ----

    #[test]
    fn long_vowel_o_row_becomes_u() {
        // けふ reads キョー; kata_to_hira→きょー; ょ is o-row → きょう.
        assert_eq!(expand_long_vowel("きょー"), "きょう");
        assert_eq!(expand_long_vowel("こー"), "こう");
    }

    #[test]
    fn long_vowel_e_row_becomes_i() {
        // せい (先生) reads セー → せー → せい (Sino-Japanese えい spelling).
        assert_eq!(expand_long_vowel("せー"), "せい");
    }

    #[test]
    fn long_vowel_a_i_u_repeat_row_vowel() {
        assert_eq!(expand_long_vowel("かー"), "かあ");
        assert_eq!(expand_long_vowel("にー"), "にい");
        assert_eq!(expand_long_vowel("くー"), "くう");
    }

    #[test]
    fn long_vowel_passthrough_without_mark() {
        assert_eq!(expand_long_vowel("あさ"), "あさ");
    }

    // ---- digraph tokens (pron path) ----

    #[test]
    fn digraph_kefu_becomes_kyou() {
        assert_eq!(
            modernize_token(&tok("けふ", "キョー", "名詞")).as_deref(),
            Some("きょう")
        );
    }

    #[test]
    fn digraph_tefu_becomes_chou() {
        // てふ (butterfly) reads チョー → ちょう.
        assert_eq!(
            modernize_token(&tok("てふ", "チョー", "名詞")).as_deref(),
            Some("ちょう")
        );
    }

    #[test]
    fn digraph_kau_becomes_kou() {
        // かう reads コー → こう.
        assert_eq!(
            modernize_token(&tok("かう", "コー", "動詞")).as_deref(),
            Some("こう")
        );
    }

    // ---- regular surface rewrite ----

    #[test]
    fn medial_hagyou_rewrites() {
        // おもふ → おもう (medial ふ), かは → かわ (medial は, not a particle here).
        assert_eq!(
            modernize_token(&tok("おもふ", "オモウ", "動詞")).as_deref(),
            Some("おもう")
        );
        assert_eq!(
            modernize_token(&tok("かは", "カワ", "名詞")).as_deref(),
            Some("かわ")
        );
    }

    #[test]
    fn kanji_mixed_keeps_kanji_rewrites_kana() {
        // 使ひ → 使い: kanji passes through, medial ひ → い.
        assert_eq!(
            modernize_token(&tok("使ひ", "ツカイ", "動詞")).as_deref(),
            Some("使い")
        );
        // 思ふ → 思う.
        assert_eq!(
            modernize_token(&tok("思ふ", "オモウ", "動詞")).as_deref(),
            Some("思う")
        );
    }

    #[test]
    fn obsolete_kana_swaps() {
        assert_eq!(
            modernize_token(&tok("ゐる", "イル", "動詞")).as_deref(),
            Some("いる")
        );
        assert_eq!(
            modernize_token(&tok("こゑ", "コエ", "名詞")).as_deref(),
            Some("こえ")
        );
    }

    #[test]
    fn yotsugana_swaps() {
        assert_eq!(
            modernize_token(&tok("みづ", "ミズ", "名詞")).as_deref(),
            Some("みず")
        );
        assert_eq!(
            modernize_token(&tok("はぢ", "ハジ", "名詞")).as_deref(),
            Some("はじ")
        );
    }

    // ---- particle guard ----

    #[test]
    fn particle_ha_he_wo_are_protected() {
        // Standalone 助詞 は/へ/を keep historical spelling in 現代仮名遣い.
        assert_eq!(modernize_token(&tok("は", "ワ", "助詞")), None);
        assert_eq!(modernize_token(&tok("へ", "エ", "助詞")), None);
        assert_eq!(modernize_token(&tok("を", "オ", "助詞")), None);
    }

    #[test]
    fn non_particle_ha_at_token_start_is_unchanged() {
        // Token-initial は in a non-particle word is not medial → stays は.
        assert_eq!(modernize_token(&tok("はな", "ハナ", "名詞")), None);
    }

    // ---- no-change detection ----

    #[test]
    fn kanji_token_with_long_vowel_reading_is_not_replaced_by_reading() {
        // Regression: 日曜 (pron ニチヨー) and 蝶々 (pron チョーチョー) must NOT take
        // the pron path — that would erase the kanji (→にちよう / ちょうちょ).
        // Not all-kana ⇒ kana-run rewrite ⇒ no historical kana ⇒ unchanged.
        assert_eq!(modernize_token(&tok("日曜", "ニチヨー", "名詞")), None);
        assert_eq!(modernize_token(&tok("蝶々", "チョーチョー", "名詞")), None);
    }

    #[test]
    fn all_kana_digraph_still_reconstructs() {
        // The gate must not disturb the genuine all-kana digraph case.
        assert_eq!(
            modernize_token(&tok("てふ", "チョー", "名詞")).as_deref(),
            Some("ちょう")
        );
    }

    #[test]
    fn already_modern_token_returns_none() {
        assert_eq!(modernize_token(&tok("ねこ", "ネコ", "名詞")), None);
        assert_eq!(modernize_token(&tok("猫", "ネコ", "名詞")), None);
    }

    #[test]
    fn missing_pron_falls_back_to_surface_rewrite() {
        // No pron → cannot take the digraph path; surface rewrite still applies.
        let t = HistToken {
            surface: "ゐ",
            pron: None,
            pos1: Some("動詞"),
        };
        assert_eq!(modernize_token(&t).as_deref(), Some("い"));
    }

    // ---- rules hash ----

    // ---- detector (token-granular annotations) ----

    /// A stub oracle returning a fixed token stream so the detector is testable
    /// without a dictionary. Spans are byte ranges within the sentence text.
    struct StubOracle(Vec<HistOracleToken>);
    impl HistoricalOracle for StubOracle {
        fn tokenize(&self, _text: &str) -> Vec<HistOracleToken> {
            self.0.clone()
        }
    }

    fn otok(surface: &str, span: Range<usize>, pron: &str, pos1: &str) -> HistOracleToken {
        HistOracleToken {
            surface: surface.to_owned(),
            byte_span: span,
            pron: Some(pron.to_owned()),
            pos1: Some(pos1.to_owned()),
        }
    }

    #[test]
    fn detector_emits_token_granular_annotations_only_for_changes() {
        // Sentence: 今日はけふ  (today, は particle, historical けふ→きょう)
        // bytes: 今(0..3)日(3..6)は(6..9)けふ(9..15)
        let sentence = SentenceSpan {
            text: "今日はけふ",
            byte_offset: 100,
            char_offset: 0,
        };
        let oracle = StubOracle(vec![
            otok("今日", 0..6, "キョウ", "名詞"),
            otok("は", 6..9, "ワ", "助詞"),
            otok("けふ", 9..15, "キョー", "名詞"),
        ]);
        let detector = HistoricalRewriteV1::new(Arc::new(oracle), "sha256:kindaidict".to_owned());
        let anns = detector.detect(&[sentence]);
        // Only けふ changes: 今日 is already modern, は is a protected particle.
        assert_eq!(anns.len(), 1);
        let ann = &anns[0];
        assert_eq!(ann.normalized_text, "きょう");
        assert_eq!(ann.kind, OrthoNormalization::HistoricalToModern);
        // Source coordinates: sentence.byte_offset (100) + token span (9..15).
        assert_eq!(ann.source_byte_range, 109..115);
    }

    #[test]
    fn detector_id_binds_dictionary_and_rules_hashes() {
        let detector = HistoricalRewriteV1::new(
            Arc::new(StubOracle(Vec::new())),
            "sha256:kindaidict".to_owned(),
        );
        match detector.detector_id() {
            OrthoDetectorId::HistoricalRewriteV1 {
                dictionary_hash,
                rules_hash: rh,
            } => {
                assert_eq!(dictionary_hash, "sha256:kindaidict");
                assert_eq!(rh, rules_hash());
            }
            other => panic!("unexpected detector id: {other:?}"),
        }
    }

    #[test]
    fn detector_annotations_round_trip_through_ortho_normalize() {
        // The emitted annotation must apply cleanly and remap back to source.
        let sentence = SentenceSpan {
            text: "けふ",
            byte_offset: 0,
            char_offset: 0,
        };
        let oracle = StubOracle(vec![otok("けふ", 0..6, "キョー", "名詞")]);
        let detector = HistoricalRewriteV1::new(Arc::new(oracle), "sha256:d".to_owned());
        let anns = detector.detect(&[sentence]);
        let (normalized, map) = crate::ortho_normalize("けふ", &anns);
        assert_eq!(normalized, "きょう");
        // きょう is 9 bytes; the whole span remaps to the 6-byte source けふ.
        assert_eq!(map.to_original(0..9).unwrap(), 0..6);
    }

    #[test]
    fn rules_hash_is_stable() {
        // Pin the rule-set identity. If you intentionally change a rule, update
        // RULES_CANON in the same commit and re-pin this value; the golden and
        // the policy hash both depend on it.
        assert_eq!(
            rules_hash(),
            "sha256:ef00d5b53bc124e8ed2bc463b24b5e3ca98cb96fad3ca0c715c6cd00754545ee"
        );
    }
}
