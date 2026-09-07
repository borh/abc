use ab_source_syntax::{SourceRegions, aozora_body_range};

#[test]
fn explicit_body_end_starts_source_apparatus() {
    for newline in ["\n", "\r\n"] {
        for marker in ["［＃本文終わり］", "　［＃本文終わり］　"] {
            let source = format!(
                "作品{newline}作者{newline}{newline}本文{newline}{newline}{marker}{newline}翻訳の底本：原書{newline}※利用条件{newline}"
            );
            let (body, tail) = aozora_body_range(&source);
            assert_eq!(&source[body], "本文");
            assert_eq!(
                &source[tail..],
                format!("{marker}{newline}翻訳の底本：原書{newline}※利用条件{newline}")
            );
            let regions = SourceRegions::derive(&source).unwrap();
            assert!(source[regions.tail()].contains(marker));
        }
    }
}

#[test]
fn prose_mentions_do_not_end_body() {
    for line in [
        "「［＃本文終わり］」という記号",
        "［＃本文終わり］の説明",
        "翻訳の底本：という語",
    ] {
        let source = format!("前\n{line}\n後\n");
        assert_eq!(aozora_body_range(&source), (0..source.len(), source.len()));
    }
}
