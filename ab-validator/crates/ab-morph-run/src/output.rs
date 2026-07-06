use std::fs::{self, File, OpenOptions};
use std::io::{BufRead, BufReader, BufWriter, Write};
use std::path::Path;

use anyhow::{Context, Result};

pub(crate) fn open_output_writer(path: &Path, append: bool) -> Result<Box<dyn Write + Send>> {
    if let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    {
        fs::create_dir_all(parent)
            .with_context(|| format!("failed to create {}", parent.display()))?;
    }

    let file = if append {
        OpenOptions::new()
            .create(true)
            .append(true)
            .open(path)
            .with_context(|| format!("failed to open {}", path.display()))?
    } else {
        File::create(path).with_context(|| format!("failed to create {}", path.display()))?
    };

    if path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
        Ok(Box::new(
            zstd::stream::write::Encoder::new(file, 3)?.auto_finish(),
        ))
    } else {
        Ok(Box::new(BufWriter::new(file)))
    }
}

pub(crate) fn read_jsonl_or_zst_to_string(path: &Path) -> Result<String> {
    if path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
        let bytes = fs::read(path).with_context(|| format!("failed to read {}", path.display()))?;
        let decoded = zstd::decode_all(bytes.as_slice())?;
        Ok(String::from_utf8(decoded)?)
    } else {
        fs::read_to_string(path).with_context(|| format!("failed to read {}", path.display()))
    }
}

pub(crate) fn for_each_jsonl_or_zst_line(
    path: &Path,
    mut visit: impl FnMut(&str) -> Result<()>,
) -> Result<()> {
    let file = File::open(path).with_context(|| format!("failed to open {}", path.display()))?;
    let mut reader: Box<dyn BufRead> =
        if path.extension().and_then(|ext| ext.to_str()) == Some("zst") {
            Box::new(BufReader::new(zstd::stream::read::Decoder::new(file)?))
        } else {
            Box::new(BufReader::new(file))
        };

    let mut line = String::new();
    loop {
        line.clear();
        let read = reader.read_line(&mut line)?;
        if read == 0 {
            break;
        }
        let line = line.trim_end_matches(['\r', '\n']);
        if !line.trim().is_empty() {
            visit(line)?;
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use std::time::{SystemTime, UNIX_EPOCH};

    use super::*;

    #[test]
    fn writes_plain_output_for_jsonl_path() {
        let dir = temp_dir("plain");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }

        assert_eq!(fs::read_to_string(&path).unwrap(), "one\n");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn writes_zstd_output_for_zst_path() {
        let dir = temp_dir("zst");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl.zst");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }

        let bytes = fs::read(&path).unwrap();
        let decoded = zstd::decode_all(bytes.as_slice()).unwrap();
        assert_eq!(decoded, b"one\n");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn appends_zstd_output_as_concatenated_frames() {
        let dir = temp_dir("zst-append");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl.zst");

        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\n").unwrap();
            writer.flush().unwrap();
        }
        {
            let mut writer = open_output_writer(&path, true).unwrap();
            writer.write_all(b"two\n").unwrap();
            writer.flush().unwrap();
        }

        let text = read_jsonl_or_zst_to_string(&path).unwrap();
        assert_eq!(text, "one\ntwo\n");
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn streams_plain_jsonl_lines_without_materializing_file() {
        let dir = temp_dir("stream-plain");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl");
        fs::write(&path, "one\n\n two \n").unwrap();

        let mut rows = Vec::new();
        for_each_jsonl_or_zst_line(&path, |line| {
            rows.push(line.to_owned());
            Ok(())
        })
        .unwrap();

        assert_eq!(rows, vec!["one", " two "]);
        let _ = fs::remove_dir_all(dir);
    }

    #[test]
    fn streams_zstd_jsonl_lines() {
        let dir = temp_dir("stream-zst");
        fs::create_dir_all(&dir).unwrap();
        let path = dir.join("rows.jsonl.zst");
        {
            let mut writer = open_output_writer(&path, false).unwrap();
            writer.write_all(b"one\ntwo\n").unwrap();
            writer.flush().unwrap();
        }

        let mut rows = Vec::new();
        for_each_jsonl_or_zst_line(&path, |line| {
            rows.push(line.to_owned());
            Ok(())
        })
        .unwrap();

        assert_eq!(rows, vec!["one", "two"]);
        let _ = fs::remove_dir_all(dir);
    }

    fn temp_dir(label: &str) -> std::path::PathBuf {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        std::env::temp_dir().join(format!(
            "ab-morph-run-output-{label}-{}-{unique}",
            std::process::id()
        ))
    }
}
