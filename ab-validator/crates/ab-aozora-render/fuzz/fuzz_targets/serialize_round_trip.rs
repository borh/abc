//! Fuzz target — `aozora_render::serialize` idempotency.
//!
//! Arbitrary UTF-8 source is lexed once and serialized back. The
//! result is then re-lexed and re-serialized; the two outputs must
//! be byte-equal (I3 fixed-point invariant: `serialize` is idempotent
//! on its own output).

#![no_main]

use aozora_pipeline::lex;
use aozora_render::serialize;
use libfuzzer_sys::fuzz_target;

fuzz_target!(|data: &[u8]| {
    let Ok(src) = core::str::from_utf8(data) else {
        return;
    };
    let lex1 = lex(src);
    let first = serialize(&lex1);

    let lex2 = lex(&first);
    let second = serialize(&lex2);

    assert!(
        first == second,
        "I3 fixed-point broken for src bytes = {data:?}\n  first  = {first:?}\n  second = {second:?}",
    );
});
