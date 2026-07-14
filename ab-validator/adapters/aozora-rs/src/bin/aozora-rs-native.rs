use std::io::{self, Read, Write};

use anyhow::Result;
use aozora_rs_adapter::native_retokenized_dump_json;

fn main() -> Result<()> {
    let mut bytes = Vec::new();
    io::stdin().read_to_end(&mut bytes)?;
    io::stdout().write_all(&native_retokenized_dump_json(&bytes)?)?;
    Ok(())
}
