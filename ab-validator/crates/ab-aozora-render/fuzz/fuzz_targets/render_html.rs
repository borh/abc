//! Fuzz target — `aozora_render::render_html` on arbitrary
//! UTF-8.
//!
//! Arbitrary bytes are decoded as UTF-8 (invalid sequences skip this
//! iteration). The source is lexed via `aozora_pipeline` and rendered
//! to HTML via `aozora_render`. Targets renderer panics and the round-trip "no PUA sentinel survives in
//! the rendered HTML" invariant.

#![no_main]

use aozora_pipeline::lex;
use aozora_render::render_html;
use libfuzzer_sys::fuzz_target;

/// PUA sentinel codepoints embedded by the lexer that the renderer
/// must consume — none should survive into rendered HTML.
const PUA_SENTINELS: [char; 4] = ['\u{E001}', '\u{E002}', '\u{E003}', '\u{E004}'];

fuzz_target!(|data: &[u8]| {
    let Ok(src) = core::str::from_utf8(data) else {
        return;
    };
    // Literal private-use source characters may appear in rendered text.
    if src.chars().any(|c| PUA_SENTINELS.contains(&c)) {
        return;
    }
    let lex_out = lex(src);
    let html = render_html(&lex_out);
    for sentinel in PUA_SENTINELS {
        assert!(
            !html.contains(sentinel),
            "PUA sentinel {sentinel:?} leaked into rendered HTML for src bytes = {data:?}\n  html = {html:?}",
        );
    }
});
