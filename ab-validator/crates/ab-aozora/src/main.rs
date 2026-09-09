//! Permanent stdin→AAT harness-edge binary (executable-boundary contract).
//! ab-check spawns `<adapter> --mode aat` and probes `<adapter> --version`.
//! Exit 0 = success, 1 = fatal. Exit 2 is reserved by the wire contract but
//! is never emitted because the frozen adapter never exits 2 and the
//! conformance harness treats any non-zero exit code as an adapter error.
use std::env;
use std::io::{self, Read, Write};
use std::process::ExitCode;

const USAGE: &str = "usage: ab-aozora [--mode aat|diagnostics] [--version]  \
(source bytes on stdin; one JSON document on stdout)";

#[derive(Clone, Copy)]
enum Mode {
    Aat,
    Diagnostics,
}

fn main() -> ExitCode {
    let mut mode = Mode::Aat;
    let mut args = env::args().skip(1);
    while let Some(arg) = args.next() {
        match arg.as_str() {
            "--version" => {
                println!("{}", ab_aat::adapter_version());
                return ExitCode::SUCCESS;
            }
            "--mode" => match args.next().as_deref() {
                Some("aat") => mode = Mode::Aat,
                Some("diagnostics") => mode = Mode::Diagnostics,
                other => {
                    eprintln!(
                        "ab-aozora: unsupported --mode {:?} (aat | diagnostics)\n{USAGE}",
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
    // Both functions build the full document in memory: on Err nothing has
    // been written to stdout (no-partial-output contract).
    let result = match mode {
        Mode::Aat => ab_aat::aat_json_from_bytes(&bytes),
        Mode::Diagnostics => ab_aat::diagnostics_json_from_bytes(&bytes),
    };
    match result {
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
