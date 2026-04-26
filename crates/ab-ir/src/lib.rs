use serde_json::json;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Block {
    Paragraph {
        content: Vec<Inline>,
    },
    Heading {
        level: u8,
        style: &'static str,
        content: Vec<Inline>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Inline {
    Text {
        value: String,
        provenance: Provenance,
    },
    Ruby {
        base: Vec<Inline>,
        reading: String,
        placement: RubyPlacement,
        provenance: Provenance,
    },
    GaijiRef(GaijiRef),
    Style {
        style_type: &'static str,
        content: Vec<Inline>,
        provenance: Provenance,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GaijiRef {
    pub source: String,
    pub description: String,
    pub description_format: Option<String>,
    pub kind: GaijiKind,
    pub resolved: Option<String>,
    pub provenance: Provenance,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum GaijiKind {
    UnicodeCodepoint { value: char },
    JisCode { plane: Option<u8>, row: u8, cell: u8 },
    JisLevel { level: u8, row: u8, cell: u8 },
    Composition { description: String },
    DakutenVariant { base: String, mark: DakutenMark },
    Unknown,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DakutenMark {
    Voicing,
    SemiVoicing,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RubyPlacement {
    Right,
    Left,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Provenance {
    Parser,
    ParserNormalized,
    RegexSupplement,
    RegexFallback,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedText {
    pub visible_text: String,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProvenanceCounts {
    pub parser: usize,
    pub parser_normalized: usize,
    pub regex_supplement: usize,
    pub regex_fallback: usize,
}

impl Inline {
    pub fn text(value: impl Into<String>) -> Self {
        Self::text_with_provenance(value, Provenance::Parser)
    }

    pub fn text_with_provenance(value: impl Into<String>, provenance: Provenance) -> Self {
        Self::Text {
            value: value.into(),
            provenance,
        }
    }

    pub fn ruby(base: impl Into<String>, reading: impl Into<String>) -> Self {
        Self::ruby_with_provenance(base, reading, Provenance::Parser)
    }

    pub fn ruby_with_provenance(
        base: impl Into<String>,
        reading: impl Into<String>,
        provenance: Provenance,
    ) -> Self {
        Self::Ruby {
            base: vec![Self::text(base)],
            reading: reading.into(),
            placement: RubyPlacement::Right,
            provenance,
        }
    }

    pub fn ruby_with_base(
        base: Vec<Inline>,
        reading: impl Into<String>,
        placement: RubyPlacement,
    ) -> Self {
        Self::ruby_with_base_and_provenance(base, reading, placement, Provenance::Parser)
    }

    pub fn ruby_with_base_and_provenance(
        base: Vec<Inline>,
        reading: impl Into<String>,
        placement: RubyPlacement,
        provenance: Provenance,
    ) -> Self {
        Self::Ruby {
            base,
            reading: reading.into(),
            placement,
            provenance,
        }
    }

    pub fn gaiji(
        description: impl Into<String>,
        resolved: impl Into<String>,
        description_format: Option<&'static str>,
    ) -> Self {
        Self::gaiji_with_provenance(
            description,
            resolved,
            description_format,
            Provenance::Parser,
        )
    }

    pub fn gaiji_with_provenance(
        description: impl Into<String>,
        resolved: impl Into<String>,
        description_format: Option<&'static str>,
        provenance: Provenance,
    ) -> Self {
        Self::GaijiRef(GaijiRef {
            source: String::new(),
            description: description.into(),
            resolved: Some(resolved.into()),
            description_format: description_format.map(str::to_owned),
            kind: GaijiKind::Unknown,
            provenance,
        })
    }

    pub fn gaiji_ref(gaiji: GaijiRef) -> Self {
        Self::GaijiRef(gaiji)
    }

    pub fn style(style_type: &'static str, content: Vec<Inline>) -> Self {
        Self::Style {
            style_type,
            content,
            provenance: Provenance::Parser,
        }
    }
}

pub fn blocks_to_aat_json(blocks: &[Block]) -> Vec<serde_json::Value> {
    blocks
        .iter()
        .map(|block| match block {
            Block::Paragraph { content } => json!({
                "kind": "paragraph",
                "content": inline_to_aat_json(content)
            }),
            Block::Heading {
                level,
                style,
                content,
            } => json!({
                "kind": "heading",
                "level": level,
                "style": style,
                "content": inline_to_aat_json(content)
            }),
        })
        .collect()
}

pub fn visible_projection(blocks: &[Block]) -> ProjectedText {
    let mut visible_text = String::new();
    for block in blocks {
        for child in block_content(block) {
            collect_visible(child, &mut visible_text);
        }
    }
    ProjectedText { visible_text }
}

fn inline_to_aat_json(content: &[Inline]) -> Vec<serde_json::Value> {
    content
        .iter()
        .map(|node| match node {
            Inline::Text { value, provenance } => {
                with_provenance(json!({ "kind": "text", "value": value }), *provenance)
            }
            Inline::Ruby {
                base,
                reading,
                provenance,
                ..
            } => with_provenance(
                json!({ "kind": "ruby", "base": inline_visible_text(base), "reading": reading }),
                *provenance,
            ),
            Inline::GaijiRef(gaiji) => {
                let mut value = json!({
                    "kind": "gaiji",
                    "description": gaiji.description,
                    "resolved": gaiji.resolved,
                    "jis_code": null,
                    "unresolved_reason": if gaiji.resolved.is_some() { None } else { Some("unresolved") }
                });
                if let Some(format) = &gaiji.description_format {
                    value["x-description-format"] = serde_json::Value::String(format.clone());
                }
                with_provenance(value, gaiji.provenance)
            }
            Inline::Style {
                style_type,
                content,
                provenance,
            } => with_provenance(
                json!({
                    "kind": "style",
                    "style_type": style_type,
                    "content": inline_to_aat_json(content)
                }),
                *provenance,
            ),
        })
        .collect()
}

fn collect_visible(value: &Inline, out: &mut String) {
    match value {
        Inline::Text { value, .. } => out.push_str(value),
        Inline::Ruby { base, .. } => {
            for child in base {
                collect_visible(child, out);
            }
        }
        Inline::GaijiRef(gaiji) => {
            if let Some(resolved) = &gaiji.resolved {
                out.push_str(resolved);
            }
        }
        Inline::Style { content, .. } => {
            for child in content {
                collect_visible(child, out);
            }
        }
    }
}

pub fn provenance_counts(blocks: &[Block]) -> ProvenanceCounts {
    let mut counts = ProvenanceCounts::default();
    for block in blocks {
        for child in block_content(block) {
            collect_provenance(child, &mut counts);
        }
    }
    counts
}

fn collect_provenance(value: &Inline, counts: &mut ProvenanceCounts) {
    match inline_provenance(value) {
        Provenance::Parser => counts.parser += 1,
        Provenance::ParserNormalized => counts.parser_normalized += 1,
        Provenance::RegexSupplement => counts.regex_supplement += 1,
        Provenance::RegexFallback => counts.regex_fallback += 1,
    }
    if let Inline::Style { content, .. } = value {
        for child in content {
            collect_provenance(child, counts);
        }
    } else if let Inline::Ruby { base, .. } = value {
        for child in base {
            collect_provenance(child, counts);
        }
    }
}

fn inline_provenance(value: &Inline) -> Provenance {
    match value {
        Inline::Text { provenance, .. }
        | Inline::Ruby { provenance, .. }
        | Inline::GaijiRef(GaijiRef { provenance, .. })
        | Inline::Style { provenance, .. } => *provenance,
    }
}

fn inline_visible_text(content: &[Inline]) -> String {
    let mut out = String::new();
    for child in content {
        collect_visible(child, &mut out);
    }
    out
}

fn with_provenance(mut value: serde_json::Value, provenance: Provenance) -> serde_json::Value {
    if provenance != Provenance::Parser {
        value["x-provenance"] = serde_json::Value::String(provenance.as_str().to_owned());
    }
    value
}

impl Provenance {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Parser => "parser",
            Self::ParserNormalized => "parser_normalized",
            Self::RegexSupplement => "regex_supplement",
            Self::RegexFallback => "regex_fallback",
        }
    }
}

pub fn block_content(block: &Block) -> &[Inline] {
    match block {
        Block::Paragraph { content } | Block::Heading { content, .. } => content,
    }
}

pub fn block_content_mut(block: &mut Block) -> &mut Vec<Inline> {
    match block {
        Block::Paragraph { content } | Block::Heading { content, .. } => content,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn projects_blocks_to_aat_json_shape() {
        let blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("吾輩", "わがはい"),
                Inline::text("は猫である。"),
            ],
        }];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["content"][0]["kind"], "ruby");
        assert_eq!(json[0]["content"][0]["base"], "吾輩");
        assert_eq!(json[0]["content"][1]["value"], "は猫である。");
    }

    #[test]
    fn visible_projection_uses_ruby_base_and_resolved_gaiji() {
        let blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("吾輩", "わがはい"),
                Inline::gaiji("「口＋世」、U+546D", "呻", None),
            ],
        }];

        assert_eq!(visible_projection(&blocks).visible_text, "吾輩呻");
    }

    #[test]
    fn gaiji_ref_preserves_kind_source_resolution_and_provenance() {
        let gaiji = GaijiRef {
            source: "※［＃「口＋愛」、第3水準1-15-23］".to_owned(),
            description: "「口＋愛」、第3水準1-15-23".to_owned(),
            description_format: Some("composition+jis-level".to_owned()),
            kind: GaijiKind::JisLevel {
                level: 3,
                row: 15,
                cell: 23,
            },
            resolved: Some("㖊".to_owned()),
            provenance: Provenance::ParserNormalized,
        };

        let inline = Inline::gaiji_ref(gaiji.clone());

        assert_eq!(inline, Inline::GaijiRef(gaiji));
    }

    #[test]
    fn gaiji_compat_constructor_builds_unknown_gaiji_ref() {
        let inline = Inline::gaiji("「口＋世」、U+546D", "呻", Some("aozora-description"));

        assert_eq!(
            inline,
            Inline::GaijiRef(GaijiRef {
                source: String::new(),
                description: "「口＋世」、U+546D".to_owned(),
                description_format: Some("aozora-description".to_owned()),
                kind: GaijiKind::Unknown,
                resolved: Some("呻".to_owned()),
                provenance: Provenance::Parser,
            })
        );
    }

    #[test]
    fn structured_ruby_base_can_hold_resolved_gaiji() {
        let ruby = Inline::ruby_with_base(
            vec![Inline::gaiji_ref(GaijiRef {
                source: "※［＃「口＋世」、U+546D］".to_owned(),
                description: "「口＋世」、U+546D".to_owned(),
                description_format: Some("aozora-description".to_owned()),
                kind: GaijiKind::UnicodeCodepoint { value: '呻' },
                resolved: Some("呻".to_owned()),
                provenance: Provenance::Parser,
            })],
            "うめ",
            RubyPlacement::Right,
        );
        let blocks = vec![Block::Paragraph {
            content: vec![ruby],
        }];

        assert_eq!(visible_projection(&blocks).visible_text, "呻");
    }

    #[test]
    fn structured_ruby_base_records_left_placement() {
        let ruby = Inline::ruby_with_base(
            vec![Inline::text("左")],
            "ひだり",
            RubyPlacement::Left,
        );

        assert!(matches!(
            ruby,
            Inline::Ruby {
                placement: RubyPlacement::Left,
                ..
            }
        ));
    }

    #[test]
    fn non_parser_provenance_is_projected_as_extension_metadata() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby_with_provenance(
                "",
                "わがはい",
                Provenance::RegexSupplement,
            )],
        }];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["content"][0]["x-provenance"], "regex_supplement");
        assert_eq!(provenance_counts(&blocks).regex_supplement, 1);
    }
}
