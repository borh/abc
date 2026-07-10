use std::io::Write;
use std::process::{Command, Stdio};

/// Runs `bin args… < input`, captures (exit_code, stdout_bytes).
fn run(bin: &str, args: &[&str], input: &[u8]) -> (i32, Vec<u8>) {
    let mut child = Command::new(bin)
        .args(args)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::null())
        .spawn()
        .unwrap_or_else(|e| panic!("spawn {bin}: {e}"));
    child.stdin.as_mut().unwrap().write_all(input).unwrap();
    let out = child.wait_with_output().unwrap();
    (out.status.code().unwrap_or(-1), out.stdout)
}

#[test]
fn inspect_matches_pinned_upstream_on_samples() {
    let upstream = std::env::var("AB_UPSTREAM_AOZORA_BIN")
        .expect("set AB_UPSTREAM_AOZORA_BIN to the pinned upstream aozora binary");
    let shim = env!("CARGO_BIN_EXE_ab-aozora-cli");
    let samples: &[&[u8]] = &[
        "表題\r\n著者\r\n\r\n-------------------------------------------------------\r\n【テキスト中に現れる記号について】\r\n《》：ルビ\r\n-------------------------------------------------------\r\n吾輩《わがはい》は猫である。※［＃「けものへん＋苗」、第3水準1-87-64］\r\n［＃５字下げ］一［＃「一」は中見出し］\r\n底本：「テスト」\r\n".as_bytes(),
        b"plain ascii only\n",
        "壊れた《ルビ\r\n".as_bytes(),
    ];
    for kind in ["nodes", "diagnostics", "gaiji"] {
        for (i, sample) in samples.iter().enumerate() {
            let (up_code, up_out) = run(&upstream, &["inspect", kind, "-"], sample);
            let (sh_code, sh_out) = run(shim, &["inspect", kind, "-"], sample);
            assert_eq!(
                up_code, sh_code,
                "exit code diverged: kind={kind} sample={i}"
            );
            assert_eq!(up_out, sh_out, "stdout diverged: kind={kind} sample={i}");
        }
    }
}

#[test]
fn version_flag_identifies_the_shim() {
    let shim = env!("CARGO_BIN_EXE_ab-aozora-cli");
    let (code, out) = run(shim, &["--version"], b"");
    assert_eq!(code, 0);
    let text = String::from_utf8(out).unwrap();
    assert!(text.contains("ab-aozora-cli"), "got: {text}");
    assert!(text.contains("1a4f864"), "got: {text}");
}
