//! Source role declarations remain metadata while their operands stay visible.

use ab_aozora_pipeline::lex;
use ab_aozora_render::{render_html, serialize};

#[test]
fn supplied_note_roles_are_metadata_not_ruby_readings() {
    for (source, role) in [
        ("前（１）［＃「（１）」は注釈番号］後", "annotation-number"),
        ("前（一）［＃（一）は自注］後", "author-note"),
    ] {
        let parsed = lex(source);
        let html = render_html(&parsed);
        assert!(
            html.contains(&format!("data-source-role=\"{role}\"")),
            "{html}"
        );
        assert!(!html.contains("<ruby"), "{html}");
        assert!(
            !html.contains("注釈番号") && !html.contains("自注"),
            "{html}"
        );
        let canonical = serialize(&parsed);
        assert_eq!(serialize(&lex(&canonical)), canonical);
        assert_eq!(render_html(&lex(&canonical)), html);
    }
}
