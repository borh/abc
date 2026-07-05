mod chunking;
mod error;
mod features;
mod ortho_compat;
mod span_builder;
pub mod sudachi;
pub mod vaporetto;
pub mod vibrato;

pub use error::AnalyzerError;
pub use sudachi::{SudachiAnalyzer, SudachiMode};
pub use vaporetto::VaporettoAnalyzer;
pub use vibrato::VibratoAnalyzer;

use ab_morph_diff::Analysis;
use ab_plaintext::PlainTextDocument;

pub trait MorphAnalyzer {
    fn analyzer_id(&self) -> &str;

    /// Run the analyzer on a source document and return a structured analysis.
    ///
    /// # Errors
    ///
    /// Returns an error when tokenization or conversion fails.
    fn analyze(&self, document: &PlainTextDocument) -> Result<Analysis, AnalyzerError>;
}

#[cfg(test)]
mod integration_tests {
    use ab_plaintext::from_aozora_honbun_bytes;

    use super::*;

    #[test]
    #[ignore = "requires dictionary symlink"]
    fn analyzes_aozora_plaintext_with_both_adapters() {
        let source = "title\n--------------------\nmeta\n--------------------\n｜吾輩《わがはい》は猫である。\n底本：x\n";
        let document = from_aozora_honbun_bytes("wagahai", source.as_bytes());

        assert!(document.text.starts_with("吾輩は猫である。"));

        let vibrato = VibratoAnalyzer::unidic_cwj_default().unwrap();
        let Some(sudachi_dictionary) = std::env::var_os("AB_SUDACHI_DICT") else {
            println!("skipping Sudachi smoke path: AB_SUDACHI_DICT is not set");
            return;
        };
        let sudachi =
            SudachiAnalyzer::from_dictionary_path(SudachiMode::C, sudachi_dictionary).unwrap();

        let vibrato_analysis = vibrato.analyze(&document).unwrap();
        let sudachi_analysis = sudachi.analyze(&document).unwrap();

        assert_eq!(vibrato_analysis.text_id, "wagahai");
        assert_eq!(sudachi_analysis.text_id, "wagahai");
        assert_eq!(vibrato_analysis.source_text, document.text);
        assert_eq!(sudachi_analysis.source_text, document.text);
        assert!(!vibrato_analysis.morphemes.is_empty());
        assert!(!sudachi_analysis.morphemes.is_empty());
    }
}
