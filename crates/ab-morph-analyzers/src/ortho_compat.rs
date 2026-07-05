use ab_ortho_detect::{OrthoToken, OrthoTokenizer};
use ab_plaintext::{PlainTextDocument, SourceFormat};

use crate::MorphAnalyzer;
use crate::VibratoAnalyzer;

impl OrthoTokenizer for VibratoAnalyzer {
    /// Tokenizes via Vibrato for the proper-noun guard in `HeuristicV1`.
    ///
    /// # Failure mode (TODO: make `OrthoTokenizer::tokenize` return `Result`)
    /// On Vibrato `analyze` error (e.g. dictionary load failure) this returns an
    /// EMPTY token list, which `HeuristicV1` reads as
    /// `proper_noun_char_ratio = 0` and proceeds. The proper-noun guard thus
    /// SILENTLY degenerates to "no proper nouns" on failure. We log to stderr
    /// so the failure is observable, but the contract remains `Vec`-returning
    /// for backwards compatibility with Phase 1's trait shape. A future trait
    /// revision should return `Result<Vec<OrthoToken>, _>` and route errors to
    /// the pipeline `errors_writer`.
    fn tokenize(&self, text: &str) -> Vec<OrthoToken> {
        let doc = PlainTextDocument {
            text_id: String::new(),
            source_format: SourceFormat::AozoraHonbun,
            text: text.to_owned(),
        };
        match self.analyze(&doc) {
            Ok(analysis) => analysis
                .morphemes
                .iter()
                .map(|m| OrthoToken {
                    surface: m.surface.clone(),
                    pos2: m
                        .features
                        .get("pos2")
                        .and_then(|opt| opt.as_deref())
                        .map(String::from),
                })
                .collect(),
            Err(e) => {
                eprintln!(
                    "warn: ortho-detect proper-noun guard: Vibrato analyze failed \
                     ({e}); treating as zero tokens (guard degenerates to no-proper-noun)"
                );
                Vec::new()
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Compile-time assertion that `VibratoAnalyzer` satisfies the
    /// `OrthoTokenizer: Send + Sync` supertrait bound.
    #[test]
    fn vibrato_is_send_sync() {
        fn assert_send_sync<T: Send + Sync>() {}
        assert_send_sync::<VibratoAnalyzer>();
    }
}
