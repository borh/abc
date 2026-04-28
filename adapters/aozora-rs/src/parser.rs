use std::time::{Duration, Instant};

use anyhow::{Result, anyhow};
use aozora_rs_core::{Retokenized, parse_meta, retokenize, scopenize, tokenize};
use winnow::LocatingSlice;

use crate::source::{BodySelection, starts_with_separator, trim_colophon};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ParseBodyStrategy {
    ParserMeta,
    SeparatorFallback,
}

#[derive(Debug, Clone, Copy)]
pub struct ParseBodyDecision<'a> {
    pub parser_body: &'a str,
    pub strategy: ParseBodyStrategy,
}

#[derive(Debug)]
pub struct ParsedSource<'a> {
    pub body: BodySelection<'a>,
    pub parse_body: ParseBodyDecision<'a>,
    pub retokenized: Vec<Retokenized<'a>>,
    pub warnings: Vec<String>,
    pub warnings_summary: ParserWarnings,
    pub tokenized_count: usize,
    pub retokenized_count: usize,
    pub timings: ParseTimings,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ParseTimings {
    pub body_selection: Duration,
    pub tokenize: Duration,
    pub scopenize: Duration,
    pub retokenize: Duration,
}

pub fn parse_with_aozora_rs(body: BodySelection<'_>) -> Result<ParsedSource<'_>> {
    let mut warnings = Vec::new();
    let mut warnings_summary = ParserWarnings::default();
    let parse_body = if body.found_separators {
        ParseBodyDecision {
            parser_body: body.validation_body,
            strategy: ParseBodyStrategy::SeparatorFallback,
        }
    } else {
        let mut parsed_body = body.validation_body;
        let meta_ok = match parse_meta(&mut parsed_body) {
            Ok(_) => true,
            Err(error) => {
                warnings.push(format!("meta parse warning: {error}"));
                warnings_summary.meta_parse_warning = true;
                false
            }
        };
        let parsed_body = trim_colophon(parsed_body);
        if meta_ok && !starts_with_separator(parsed_body) {
            ParseBodyDecision {
                parser_body: parsed_body,
                strategy: ParseBodyStrategy::ParserMeta,
            }
        } else {
            ParseBodyDecision {
                parser_body: body.validation_body,
                strategy: ParseBodyStrategy::SeparatorFallback,
            }
        }
    };

    let tokenize_start = Instant::now();
    let mut input = LocatingSlice::new(parse_body.parser_body);
    let tokenized = tokenize(&mut input).map_err(|()| anyhow!("aozora-rs-core tokenize failed"))?;
    let tokenize = tokenize_start.elapsed();
    let tokenized_count = tokenized.len();

    let scopenize_start = Instant::now();
    let ((scopenized, flat_tokens), scopenize_errors) = scopenize(tokenized).into_tuple();
    let scopenize = scopenize_start.elapsed();

    let retokenize_start = Instant::now();
    let (retokenized, retokenize_errors) = retokenize(flat_tokens, scopenized).into_tuple();
    let retokenize = retokenize_start.elapsed();
    let retokenized_count = retokenized.len();

    warnings_summary.scopenize_errors = scopenize_errors.len();
    warnings_summary.retokenize_errors = retokenize_errors.len();
    warnings.extend(
        scopenize_errors
            .into_iter()
            .map(|error| format!("{error:?}")),
    );
    warnings.extend(retokenize_errors.into_iter().map(|error| error.to_string()));

    Ok(ParsedSource {
        body,
        parse_body,
        retokenized,
        warnings,
        warnings_summary,
        tokenized_count,
        retokenized_count,
        timings: ParseTimings {
            body_selection: body.elapsed,
            tokenize,
            scopenize,
            retokenize,
        },
    })
}

#[derive(Debug, Clone, Default)]
pub struct ParserWarnings {
    pub meta_parse_warning: bool,
    pub scopenize_errors: usize,
    pub retokenize_errors: usize,
}

impl ParserWarnings {
    pub fn has_parser_failures(&self) -> bool {
        self.scopenize_errors > 0 || self.retokenize_errors > 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::BodySelection;
    use std::time::Duration;

    #[test]
    fn parses_ruby_and_records_counts_and_timings() {
        let body = "吾輩《わがはい》は猫である。\n";
        let selection = BodySelection {
            validation_body: body,
            found_separators: false,
            elapsed: Duration::ZERO,
        };

        let parsed = parse_with_aozora_rs(selection).unwrap();
        assert_eq!(parsed.body.validation_body, body);
        assert_eq!(parsed.parse_body.parser_body, body);
        assert_eq!(
            parsed.parse_body.strategy,
            ParseBodyStrategy::SeparatorFallback
        );
        assert!(parsed.tokenized_count > 0);
        assert!(parsed.retokenized_count > 0);
        assert!(parsed.timings.tokenize >= Duration::ZERO);
        assert!(parsed.timings.scopenize >= Duration::ZERO);
        assert!(parsed.timings.retokenize >= Duration::ZERO);
    }

    #[test]
    fn does_not_parse_meta_again_after_separator_selection() {
        let body = "　　　　　○\n本文\n";
        let selection = BodySelection {
            validation_body: body,
            found_separators: true,
            elapsed: Duration::ZERO,
        };

        let parsed = parse_with_aozora_rs(selection).unwrap();
        assert_eq!(parsed.parse_body.parser_body, body);
        assert_eq!(
            parsed.parse_body.strategy,
            ParseBodyStrategy::SeparatorFallback
        );
    }
}
