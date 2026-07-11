//! Permanent stdin→AAT harness-edge binary (executable-boundary contract,
//! docs/superpowers/specs/2026-07-10-consolidated-parser-phase2-absorption-design.md).
//! ab-check spawns `<adapter> --mode aat` and probes `<adapter> --version`.
//! Exit 0 = success, 1 = fatal. Exit 2 is reserved by the wire contract but
//! deliberately not emitted: the frozen adapter never exits 2 and the
//! conformance harness treats nonzero as adapter error.
use std::env;
use std::io::{self, Read, Write};
use std::process::ExitCode;

const USAGE: &str = "usage: ab-aozora [--mode aat] [--version]  \
(source bytes on stdin; one AAT JSON document on stdout)";

fn main() -> ExitCode {
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--version" => {
                println!("{}", ab_aozora_aat::adapter_version());
                return ExitCode::SUCCESS;
            }
            "--mode" => match args.next().as_deref() {
                Some("aat") => {}
                other => {
                    eprintln!(
                        "ab-aozora: unsupported --mode {:?} (only \"aat\")\n{USAGE}",
                        other.unwrap_or("<missing>")
                    );
                    return ExitCode::FAILURE;
                }
            },
            other => {
                eprintln!("ab-aozora: unknown argument {other:?}\n{USAGE}");
                return ExitCode::FAILURE;
            }
        }
    }
    let mut bytes = Vec::new();
    if let Err(err) = io::stdin().read_to_end(&mut bytes) {
        eprintln!("ab-aozora: failed to read stdin: {err}");
        return ExitCode::FAILURE;
    }
    // aat_json_from_bytes builds the full document in memory: on Err
    // nothing has been written to stdout (no-partial-output contract).
    match ab_aozora_aat::aat_json_from_bytes(&bytes) {
        Ok(out) => {
            if let Err(err) = io::stdout().write_all(&out) {
                eprintln!("ab-aozora: failed to write stdout: {err}");
                return ExitCode::FAILURE;
            }
            ExitCode::SUCCESS
        }
        Err(err) => {
            eprintln!("ab-aozora: {err:#}");
            ExitCode::FAILURE
        }
    }
}
