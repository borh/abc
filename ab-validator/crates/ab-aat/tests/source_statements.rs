//! Supplied editorial statements retain their meaning and source position.

use ab_aat::aat_json_from_bytes;
use ab_aozora_facade::Document;
use serde_json::Value;

#[test]
fn source_statements_preserve_assertion_kind_and_exact_marker() {
    for (statement, kind) in [
        ("「註」略", "omission"),
        ("未完", "incompleteness"),
        ("現代語訳「月は明るい。」", "explanation"),
        ("「Ｏ」は覆面の英字です。", "explanation"),
        (
            "この作品は表題と副題のみで、本文はありません。",
            "explanation",
        ),
        (
            "「雲隠れ」の帖は冒頭の晶子詞のみで本文はありません。",
            "explanation",
        ),
        ("劇場名", "explanation"),
        ("ホテル名", "explanation"),
        ("お手伝いさん", "explanation"),
        ("夫人", "explanation"),
        ("スカーフ", "explanation"),
        ("長男", "explanation"),
        ("三男", "explanation"),
        ("次男", "explanation"),
        ("小説家", "explanation"),
        ("長女", "explanation"),
        ("父", "explanation"),
        ("母", "explanation"),
        ("甥", "explanation"),
        ("次女", "explanation"),
        ("省略", "omission"),
        ("Ａ、Ｂ、Ｃの図省略", "omission"),
        ("図は省略", "omission"),
        ("紙片の図、図省略", "omission"),
        ("図省略", "omission"),
        ("「年表」省略", "omission"),
        ("「Ｂ圖」省略", "omission"),
        ("王家の紙幣の図、図省略", "omission"),
        ("図が入るが省略。底本43ページ", "omission"),
        ("図が入るが省略。底本44ページ", "omission"),
        (
            "この後、改ページに続いて「VI.　文例」の章があるが、著作権の状態が不明なため、省略する。",
            "omission",
        ),
        (
            "目次のページ数および「解題（大内兵衛）」「追記」は省略しました",
            "omission",
        ),
    ] {
        let marker = format!("［＃{statement}］");
        let source = format!("前{marker}後");
        assert_eq!(Document::new(source.as_str()).parse().to_source(), source);
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        let content = aat["blocks"][0]["content"].as_array().unwrap();
        let note = content
            .iter()
            .find(|node| node["kind"] == "editorial_note")
            .unwrap();
        assert_eq!(note["note_kind"], kind);
        assert_eq!(note["text"], statement);
        assert_eq!(note["span"]["byte_start"], "前".len());
        assert_eq!(note["span"]["byte_end"], "前".len() + marker.len());
        assert_eq!(content.first().unwrap()["value"], "前");
        assert_eq!(content.last().unwrap()["value"], "後");
        let facts = aat["meta"]["interpretation_facts"].as_array().unwrap();
        assert_eq!(
            facts
                .iter()
                .filter(|fact| fact["kind"] == "editorial-note")
                .count(),
            1
        );
    }
}

#[test]
fn statement_like_prose_and_unknown_qualifiers_remain_distinct() {
    for source in [
        "未完",
        "この作品は表題と副題のみで、本文はありません。",
        "父",
        "［＃父？］",
        "［＃父、太字］",
        "［＃父に傍点］",
        "［＃ここから劇場名］",
        "［＃未知の説明］",
        "［＃「Ｏ」は覆面の英字ですか。］",
        "［＃「０」は覆面の英字です。］",
        "［＃人物］",
        "［＃本文はありません］",
        "［＃この作品は表題と副題のみで、本文はありませんか。］",
        "［＃「雲隠れ」の帖は冒頭の晶子詞のみで本文はありません。以下太字］",
        "［＃未完のため省略］",
        "［＃「註」一部略］",
        "［＃図を省略しない］",
        "［＃図を省略する場合］",
        "［＃「省略」とある］",
        "［＃図省略か］",
        "［＃図が入るが省略。底本不明ページ］",
        "［＃現代語訳「」］",
        "［＃現代語訳「月は明るい］",
        "［＃現代語訳「月は明るい」か］",
        "［＃「現代語訳」は見出しらしい］",
    ] {
        let aat: Value =
            serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
        assert!(
            aat["blocks"][0]["content"]
                .as_array()
                .unwrap()
                .iter()
                .all(|node| node["kind"] != "editorial_note")
        );
    }
}

#[test]
fn supplied_translation_preserves_ruby_as_note_content() {
    let marker = "［＃現代語訳「松籟《しょうらい》を聞かせる。」］";
    let source = format!("前{marker}後");
    assert_eq!(Document::new(source.as_str()).parse().to_source(), source);
    let aat: Value =
        serde_json::from_slice(&aat_json_from_bytes(source.as_bytes()).unwrap()).unwrap();
    let content = aat["blocks"][0]["content"].as_array().unwrap();
    let note = content
        .iter()
        .find(|node| node["kind"] == "editorial_note")
        .unwrap();
    assert_eq!(note["note_kind"], "explanation");
    let ruby = note["annotation_content"]
        .as_array()
        .unwrap()
        .iter()
        .find(|node| node["kind"] == "ruby")
        .unwrap();
    assert_eq!(ruby["base"], "松籟");
    assert_eq!(ruby["reading"], "しょうらい");
    assert_eq!(ruby["span"]["byte_start"], source.find("松籟").unwrap());
    assert_eq!(content.first().unwrap()["value"], "前");
    assert_eq!(content.last().unwrap()["value"], "後");
    assert!(
        aat["meta"]["interpretation_facts"]
            .as_array()
            .unwrap()
            .iter()
            .any(|fact| fact["kind"] == "editorial-note"
                && fact["source_span"]["start"] == "前".len()
                && fact["source_span"]["end"] == "前".len() + marker.len())
    );
}
