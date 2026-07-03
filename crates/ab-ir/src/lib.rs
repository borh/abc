use serde_json::json;

pub mod aat_view;
mod semantic_summary;
pub use semantic_summary::{SemanticSummary, SemanticSummaryNode, SourceSpan, semantic_summary};

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
    /// Indentation block (`jisage_block`). `level` is the column-count indent
    /// applied to `content`.
    Jisage {
        level: u8,
        content: Vec<Inline>,
    },
    CaptionBlock {
        content: Vec<Inline>,
    },
    /// Split-line note (warichu / warigaki). The whole inline run is treated
    /// as the upper column; the lower column is unused for the single-line
    /// form but the structure leaves room for splitting later.
    Warichu {
        content: Vec<Inline>,
    },
    /// Inline figure: image source plus caption inlines.
    Figure {
        source: String,
        content: Vec<Inline>,
    },
    /// Explicit page or line break.
    Break {
        kind: BreakKind,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BreakKind {
    /// `［＃改ページ］` / `［＃ページの左右中央］`.
    Page,
    /// `［＃改行］`.
    Line,
}

impl BreakKind {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            BreakKind::Page => "page",
            BreakKind::Line => "line",
        }
    }
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
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    },
    TextMeta {
        value: String,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    },
    GaijiRef(GaijiRef),
    Style {
        style_type: &'static str,
        content: Vec<Inline>,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    },
    Scope {
        kind: &'static str,
        content: Vec<Inline>,
        provenance: Provenance,
    },
    FontSize {
        size_type: &'static str,
        level: u8,
        content: Vec<Inline>,
        provenance: Provenance,
    },
    Warigaki {
        upper: Vec<Inline>,
        lower: Vec<Inline>,
        provenance: Provenance,
    },
    FigureRef {
        filename: String,
        alt: String,
        width: Option<u64>,
        height: Option<u64>,
        caption: Vec<Inline>,
        provenance: Provenance,
    },
    Accent {
        code: &'static str,
        name: &'static str,
        resolved: String,
        provenance: Provenance,
    },
    EditorNote {
        note: String,
        provenance: Provenance,
    },
    Raw {
        source: String,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    },
}

/// Visitor for walking the Inline tree. Every method has a default no-op
/// implementation so visitors only override the variants they care about.
pub trait InlineVisitor {
    // Leaf nodes — enter only
    fn enter_text(&mut self, _value: &str, _provenance: &Provenance) {}
    fn enter_text_meta(&mut self, _value: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {}
    fn enter_gaiji_ref(&mut self, _gaiji: &GaijiRef) {}
    fn enter_accent(&mut self, _resolved: &str, _provenance: &Provenance) {}
    fn enter_editor_note(&mut self, _note: &str, _provenance: &Provenance) {}
    fn enter_raw(&mut self, _source: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {}

    // Container nodes — enter/leave pair
    fn enter_ruby(
        &mut self,
        _base: &[Inline],
        _reading: &str,
        _placement: &RubyPlacement,
        _provenance: &Provenance,
    ) {
    }
    fn leave_ruby(&mut self) {}
    fn enter_style(
        &mut self,
        _style_type: &str,
        _content: &[Inline],
        _attrs: &[StyleAttr],
        _provenance: &Provenance,
    ) {
    }
    fn leave_style(&mut self) {}
    fn enter_scope(&mut self, _kind: &str, _content: &[Inline], _provenance: &Provenance) {}
    fn leave_scope(&mut self) {}
    fn enter_font_size(
        &mut self,
        _size_type: &str,
        _level: u8,
        _content: &[Inline],
        _provenance: &Provenance,
    ) {
    }
    fn leave_font_size(&mut self) {}
    fn enter_warigaki(&mut self, _upper: &[Inline], _lower: &[Inline], _provenance: &Provenance) {}
    fn leave_warigaki(&mut self) {}
    fn enter_figure_ref(&mut self, _source: &str, _caption: &[Inline], _provenance: &Provenance) {}
    fn leave_figure_ref(&mut self) {}
}

/// Walk an Inline tree with a visitor. Monomorphized per concrete V.
pub fn walk_inline<V: InlineVisitor>(visitor: &mut V, node: &Inline) {
    match node {
        Inline::Text { value, provenance } => {
            visitor.enter_text(value, provenance);
        }
        Inline::TextMeta {
            value,
            attrs,
            provenance,
        } => {
            visitor.enter_text_meta(value, attrs, provenance);
        }
        Inline::GaijiRef(gaiji) => {
            visitor.enter_gaiji_ref(gaiji);
        }
        Inline::Ruby {
            base,
            reading,
            placement,
            provenance,
            ..
        } => {
            visitor.enter_ruby(base, reading, placement, provenance);
            for child in base {
                walk_inline(visitor, child);
            }
            visitor.leave_ruby();
        }
        Inline::Style {
            style_type,
            content,
            attrs,
            provenance,
            ..
        } => {
            visitor.enter_style(style_type, content, attrs, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_style();
        }
        Inline::Scope {
            kind,
            content,
            provenance,
            ..
        } => {
            visitor.enter_scope(kind, content, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_scope();
        }
        Inline::FontSize {
            size_type,
            level,
            content,
            provenance,
            ..
        } => {
            visitor.enter_font_size(size_type, *level, content, provenance);
            for child in content {
                walk_inline(visitor, child);
            }
            visitor.leave_font_size();
        }
        Inline::Warigaki {
            upper,
            lower,
            provenance,
            ..
        } => {
            visitor.enter_warigaki(upper, lower, provenance);
            for child in upper.iter().chain(lower.iter()) {
                walk_inline(visitor, child);
            }
            visitor.leave_warigaki();
        }
        Inline::FigureRef {
            filename,
            caption,
            provenance,
            ..
        } => {
            visitor.enter_figure_ref(filename, caption, provenance);
            for child in caption {
                walk_inline(visitor, child);
            }
            visitor.leave_figure_ref();
        }
        Inline::Accent {
            resolved,
            provenance,
            ..
        } => {
            visitor.enter_accent(resolved, provenance);
        }
        Inline::EditorNote { note, provenance } => {
            visitor.enter_editor_note(note, provenance);
        }
        Inline::Raw {
            source,
            attrs,
            provenance,
        } => {
            visitor.enter_raw(source, attrs, provenance);
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StyleAttr {
    pub key: &'static str,
    pub value: StyleAttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum StyleAttrValue {
    Text(&'static str),
    String(String),
    Integer(i64),
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
    UnicodeCodepoint {
        value: char,
    },
    UnicodeSequence {
        values: Vec<char>,
    },
    JisCode {
        plane: Option<u8>,
        row: u8,
        cell: u8,
    },
    JisLevel {
        level: u8,
        row: u8,
        cell: u8,
    },
    Composition {
        description: String,
    },
    DakutenVariant {
        base: String,
        mark: DakutenMark,
    },
    Alternative {
        source_kind: Box<GaijiKind>,
    },
    Image {
        path: String,
    },
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
    SourceSupplement,
    SourceFallback,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectedText {
    pub visible_text: String,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct ProvenanceCounts {
    pub parser: usize,
    pub parser_normalized: usize,
    pub source_supplement: usize,
    pub source_fallback: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AatProjection {
    pub blocks: Vec<serde_json::Value>,
    pub warnings: Vec<ProjectionWarning>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProjectionWarning {
    pub syntax_id: &'static str,
    pub message: String,
}

impl Inline {
    #[must_use]
    pub fn text(value: impl Into<String>) -> Self {
        Self::text_with_provenance(value, Provenance::Parser)
    }

    #[must_use]
    pub fn text_with_provenance(value: impl Into<String>, provenance: Provenance) -> Self {
        Self::Text {
            value: value.into(),
            provenance,
        }
    }

    #[must_use]
    pub fn ruby(base: impl Into<String>, reading: impl Into<String>) -> Self {
        Self::ruby_with_provenance(base, reading, Provenance::Parser)
    }

    #[must_use]
    pub fn ruby_with_provenance(
        base: impl Into<String>,
        reading: impl Into<String>,
        provenance: Provenance,
    ) -> Self {
        Self::Ruby {
            base: vec![Self::text(base)],
            reading: reading.into(),
            placement: RubyPlacement::Right,
            attrs: Vec::new(),
            provenance,
        }
    }

    #[must_use]
    pub fn ruby_with_base(
        base: Vec<Inline>,
        reading: impl Into<String>,
        placement: RubyPlacement,
    ) -> Self {
        Self::ruby_with_base_and_provenance(base, reading, placement, Provenance::Parser)
    }

    #[must_use]
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
            attrs: Vec::new(),
            provenance,
        }
    }

    #[must_use]
    pub fn ruby_with_attrs(
        base: Vec<Inline>,
        reading: impl Into<String>,
        placement: RubyPlacement,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    ) -> Self {
        Self::Ruby {
            base,
            reading: reading.into(),
            placement,
            attrs,
            provenance,
        }
    }

    #[must_use]
    pub fn text_with_attrs(
        value: impl Into<String>,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    ) -> Self {
        Self::TextMeta {
            value: value.into(),
            attrs,
            provenance,
        }
    }

    #[must_use]
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

    #[must_use]
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

    #[must_use]
    pub fn gaiji_ref(gaiji: GaijiRef) -> Self {
        Self::GaijiRef(gaiji)
    }

    #[must_use]
    pub fn style(style_type: &'static str, content: Vec<Inline>) -> Self {
        Self::Style {
            style_type,
            content,
            attrs: Vec::new(),
            provenance: Provenance::Parser,
        }
    }

    #[must_use]
    pub fn style_with_attrs(
        style_type: &'static str,
        content: Vec<Inline>,
        attrs: Vec<StyleAttr>,
        provenance: Provenance,
    ) -> Self {
        Self::Style {
            style_type,
            content,
            attrs,
            provenance,
        }
    }

    #[must_use]
    pub fn scope(kind: &'static str, content: Vec<Inline>, provenance: Provenance) -> Self {
        Self::Scope {
            kind,
            content,
            provenance,
        }
    }

    #[must_use]
    pub fn font_size(
        size_type: &'static str,
        level: u8,
        content: Vec<Inline>,
        provenance: Provenance,
    ) -> Self {
        Self::FontSize {
            size_type,
            level,
            content,
            provenance,
        }
    }

    #[must_use]
    pub fn warigaki(upper: Vec<Inline>, lower: Vec<Inline>, provenance: Provenance) -> Self {
        Self::Warigaki {
            upper,
            lower,
            provenance,
        }
    }
}

impl Block {
    #[must_use]
    pub fn jisage(level: u8, content: Vec<Inline>) -> Self {
        Self::Jisage { level, content }
    }

    #[must_use]
    pub fn warichu(content: Vec<Inline>) -> Self {
        Self::Warichu { content }
    }

    #[must_use]
    pub fn caption_block(content: Vec<Inline>) -> Self {
        Self::CaptionBlock { content }
    }

    #[must_use]
    pub fn figure(source: impl Into<String>, content: Vec<Inline>) -> Self {
        Self::Figure {
            source: source.into(),
            content,
        }
    }

    #[must_use]
    pub fn page_break() -> Self {
        Self::Break {
            kind: BreakKind::Page,
        }
    }

    #[must_use]
    pub fn line_break() -> Self {
        Self::Break {
            kind: BreakKind::Line,
        }
    }
}

#[must_use]
pub fn blocks_to_aat_json(blocks: &[Block]) -> Vec<serde_json::Value> {
    blocks_to_aat_projection(blocks).blocks
}

#[must_use]
pub fn blocks_to_aat_projection(blocks: &[Block]) -> AatProjection {
    let mut warnings = Vec::new();
    let blocks = blocks
        .iter()
        .map(|block| match block {
            Block::Paragraph { content } => json!({
                "kind": "paragraph",
                "content": inline_to_aat_json(content, &mut warnings)
            }),
            Block::Heading {
                level,
                style,
                content,
            } => json!({
                "kind": "heading",
                "level": level,
                "style": style,
                "content": inline_to_aat_json(content, &mut warnings)
            }),
            Block::Jisage { level, content } => json!({
                "kind": "jisage_block",
                "x-indent": level,
                "children": [{
                    "kind": "paragraph",
                    "content": inline_to_aat_json(content, &mut warnings)
                }]
            }),
            Block::CaptionBlock { content } => json!({
                "kind": "caption_block",
                "children": [{
                    "kind": "paragraph",
                    "content": inline_to_aat_json(content, &mut warnings)
                }]
            }),
            Block::Warichu { content } => json!({
                "kind": "paragraph",
                "x-warichu": true,
                "content": [{
                    "kind": "warigaki",
                    "upper": inline_to_aat_json(content, &mut warnings),
                    "lower": []
                }]
            }),
            Block::Figure { source, content } => json!({
                "kind": "paragraph",
                "x-figure": true,
                "content": std::iter::once(json!({
                    "kind": "Image",
                    "source": source,
                    "description": "",
                    "resolved": null
                }))
                .chain(inline_to_aat_json(content, &mut warnings))
                .collect::<Vec<_>>()
            }),
            Block::Break { kind, .. } => json!({
                "kind": "paragraph",
                "x-break-kind": kind.as_str(),
                "content": []
            }),
        })
        .collect();
    AatProjection { blocks, warnings }
}

#[must_use]
pub fn visible_projection(blocks: &[Block]) -> ProjectedText {
    let mut visible_text = String::new();
    for block in blocks {
        for child in block_content(block) {
            collect_visible(child, &mut visible_text);
        }
    }
    ProjectedText { visible_text }
}

fn inline_to_aat_json(
    content: &[Inline],
    warnings: &mut Vec<ProjectionWarning>,
) -> Vec<serde_json::Value> {
    content
        .iter()
        .flat_map(|node| inline_node_to_aat_json(node, warnings))
        .collect()
}

fn inline_node_to_aat_json(
    node: &Inline,
    warnings: &mut Vec<ProjectionWarning>,
) -> Vec<serde_json::Value> {
    match node {
        Inline::Text { value, .. } if value.is_empty() => Vec::new(),
        Inline::Ruby { base, reading, .. } if contains_unresolved_gaiji(base) => {
            warnings.push(ProjectionWarning {
                syntax_id: "gaiji_ruby.unresolved_base",
                message: format!(
                    "AAT cannot represent ruby reading {reading:?} over an unresolved gaiji base"
                ),
            });
            inline_to_aat_json(base, warnings)
        }
        Inline::Ruby {
            base,
            reading,
            placement,
            attrs,
            provenance,
            ..
        } => vec![with_provenance(
            style_json_with_attrs(
                json!({
                    "kind": "ruby",
                    "base": inline_visible_text(base),
                    "reading": reading,
                    "direction": placement.as_str(),
                    "base_content": inline_to_aat_json_without_warnings(base),
                }),
                attrs,
            ),
            *provenance,
        )],
        Inline::Text { .. }
        | Inline::TextMeta { .. }
        | Inline::GaijiRef(_)
        | Inline::Style { .. }
        | Inline::Scope { .. }
        | Inline::FontSize { .. }
        | Inline::Warigaki { .. }
        | Inline::FigureRef { .. }
        | Inline::Accent { .. }
        | Inline::EditorNote { .. }
        | Inline::Raw { .. } => {
            vec![inline_node_to_aat_json_without_warnings(node)]
        }
    }
}

fn inline_to_aat_json_without_warnings(content: &[Inline]) -> Vec<serde_json::Value> {
    content
        .iter()
        .map(inline_node_to_aat_json_without_warnings)
        .collect()
}

fn inline_node_to_aat_json_without_warnings(node: &Inline) -> serde_json::Value {
    match node {
        Inline::Text { value, provenance } => {
            with_provenance(json!({ "kind": "text", "value": value }), *provenance)
        }
        Inline::TextMeta {
            value,
            attrs,
            provenance,
        } => with_provenance(
            style_json_with_attrs(json!({ "kind": "text", "value": value }), attrs),
            *provenance,
        ),
        Inline::Ruby {
            base,
            reading,
            placement,
            attrs,
            provenance,
            ..
        } => with_provenance(
            style_json_with_attrs(
                json!({
                    "kind": "ruby",
                    "base": inline_visible_text(base),
                    "reading": reading,
                    "direction": placement.as_str(),
                    "base_content": inline_to_aat_json_without_warnings(base),
                }),
                attrs,
            ),
            *provenance,
        ),
        Inline::GaijiRef(gaiji) => gaiji_to_aat_json(gaiji),
        Inline::Style {
            style_type,
            content,
            attrs,
            provenance,
        } => with_provenance(
            style_json_with_attrs(
                json!({
                    "kind": "style",
                    "style_type": style_type,
                    "content": inline_to_aat_json_without_warnings(content)
                }),
                attrs,
            ),
            *provenance,
        ),
        Inline::Scope {
            kind,
            content,
            provenance,
        } => with_provenance(
            json!({
                "kind": kind,
                "content": inline_to_aat_json_without_warnings(content)
            }),
            *provenance,
        ),
        Inline::FontSize {
            size_type,
            level,
            content,
            provenance,
        } => with_provenance(
            json!({
                "kind": "font_size",
                "size_type": size_type,
                "level": level,
                "content": inline_to_aat_json_without_warnings(content)
            }),
            *provenance,
        ),
        Inline::Warigaki {
            upper,
            lower,
            provenance,
        } => with_provenance(
            json!({
                "kind": "warigaki",
                "upper": inline_to_aat_json_without_warnings(upper),
                "lower": inline_to_aat_json_without_warnings(lower),
            }),
            *provenance,
        ),
        Inline::FigureRef {
            filename,
            alt,
            width,
            height,
            caption,
            provenance,
        } => {
            let mut value = json!({
                "kind": "figure",
                "filename": filename,
                "alt": alt,
                "css_class": "",
                "caption": inline_to_aat_json_without_warnings(caption)
            });
            if let Some(width) = width {
                value["width"] = json!(width);
            }
            if let Some(height) = height {
                value["height"] = json!(height);
            }
            with_provenance(value, *provenance)
        }
        Inline::Accent {
            code,
            name,
            resolved,
            provenance,
        } => with_provenance(
            json!({
                "kind": "accent",
                "code": code,
                "name": name,
                "resolved": resolved
            }),
            *provenance,
        ),
        Inline::EditorNote { note, provenance } => with_provenance(
            json!({
                "kind": "text",
                "value": "",
                "x-editor-note": note
            }),
            *provenance,
        ),
        Inline::Raw {
            source,
            attrs,
            provenance,
        } => with_provenance(
            style_json_with_attrs(
                json!({
                    "kind": "raw",
                    "source": source
                }),
                attrs,
            ),
            *provenance,
        ),
    }
}

fn style_json_with_attrs(mut value: serde_json::Value, attrs: &[StyleAttr]) -> serde_json::Value {
    for attr in attrs {
        value[attr.key] = match &attr.value {
            StyleAttrValue::Text(value) => json!(value),
            StyleAttrValue::String(value) => json!(value),
            StyleAttrValue::Integer(value) => json!(value),
        };
    }
    value
}

fn gaiji_to_aat_json(gaiji: &GaijiRef) -> serde_json::Value {
    let resolved_has_visible_value = gaiji
        .resolved
        .as_deref()
        .is_some_and(|value| !value.is_empty());
    let mut value = json!({
        "kind": "gaiji",
        "description": gaiji.description,
        "resolved": gaiji.resolved,
        "jis_code": gaiji_jis_code(gaiji),
        "unresolved_reason": if resolved_has_visible_value { None } else { Some("unresolved") }
    });
    if let Some(format) = &gaiji.description_format {
        value["x-description-format"] = serde_json::Value::String(format.clone());
    }
    with_provenance(value, gaiji.provenance)
}

fn gaiji_jis_code(gaiji: &GaijiRef) -> Option<String> {
    match gaiji.kind {
        GaijiKind::JisCode {
            plane: Some(plane),
            row,
            cell,
        } => Some(format!("{plane}-{row}-{cell}")),
        GaijiKind::JisCode {
            plane: None,
            row,
            cell,
        } => Some(format!("{row}-{cell}")),
        GaijiKind::JisLevel { level, row, cell } => Some(format!("{level}-{row}-{cell}")),
        _ => None,
    }
}

fn contains_unresolved_gaiji(content: &[Inline]) -> bool {
    struct GaijiChecker {
        unresolved_only: bool,
        found: bool,
    }

    impl InlineVisitor for GaijiChecker {
        fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
            if !self.unresolved_only || gaiji.resolved.is_none() {
                self.found = true;
            }
        }
    }

    let mut checker = GaijiChecker {
        unresolved_only: true,
        found: false,
    };
    for node in content {
        walk_inline(&mut checker, node);
        if checker.found {
            return true;
        }
    }
    false
}

struct VisibleCollector {
    out: String,
}

impl InlineVisitor for VisibleCollector {
    fn enter_text(&mut self, value: &str, _provenance: &Provenance) {
        self.out.push_str(value);
    }
    fn enter_text_meta(&mut self, value: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {
        self.out.push_str(value);
    }
    fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
        if let Some(resolved) = &gaiji.resolved {
            self.out.push_str(resolved);
        }
    }
    fn enter_accent(&mut self, resolved: &str, _provenance: &Provenance) {
        self.out.push_str(resolved);
    }
}

fn collect_visible(value: &Inline, out: &mut String) {
    struct Vis<'a>(&'a mut String);

    impl InlineVisitor for Vis<'_> {
        fn enter_text(&mut self, value: &str, _provenance: &Provenance) {
            self.0.push_str(value);
        }
        fn enter_text_meta(&mut self, value: &str, _attrs: &[StyleAttr], _provenance: &Provenance) {
            self.0.push_str(value);
        }
        fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
            if let Some(resolved) = &gaiji.resolved {
                self.0.push_str(resolved);
            }
        }
        fn enter_accent(&mut self, resolved: &str, _provenance: &Provenance) {
            self.0.push_str(resolved);
        }
    }

    walk_inline(&mut Vis(out), value);
}

#[must_use]
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
    struct Vis<'a>(&'a mut ProvenanceCounts);

    impl InlineVisitor for Vis<'_> {
        fn enter_text(&mut self, _value: &str, provenance: &Provenance) {
            self.0.increment(provenance);
        }
        fn enter_text_meta(&mut self, _value: &str, _attrs: &[StyleAttr], provenance: &Provenance) {
            self.0.increment(provenance);
        }
        fn enter_gaiji_ref(&mut self, gaiji: &GaijiRef) {
            self.0.increment(&gaiji.provenance);
        }
        fn enter_ruby(
            &mut self,
            _base: &[Inline],
            _reading: &str,
            _placement: &RubyPlacement,
            provenance: &Provenance,
        ) {
            self.0.increment(provenance);
        }
        fn enter_style(
            &mut self,
            _style_type: &str,
            _content: &[Inline],
            _attrs: &[StyleAttr],
            provenance: &Provenance,
        ) {
            self.0.increment(provenance);
        }
        fn enter_scope(&mut self, _kind: &str, _content: &[Inline], provenance: &Provenance) {
            self.0.increment(provenance);
        }
        fn enter_font_size(
            &mut self,
            _size_type: &str,
            _level: u8,
            _content: &[Inline],
            provenance: &Provenance,
        ) {
            self.0.increment(provenance);
        }
        fn enter_warigaki(
            &mut self,
            _upper: &[Inline],
            _lower: &[Inline],
            provenance: &Provenance,
        ) {
            self.0.increment(provenance);
        }
        fn enter_figure_ref(
            &mut self,
            _source: &str,
            _caption: &[Inline],
            provenance: &Provenance,
        ) {
            self.0.increment(provenance);
        }
        fn enter_accent(&mut self, _resolved: &str, provenance: &Provenance) {
            self.0.increment(provenance);
        }
        fn enter_editor_note(&mut self, _note: &str, provenance: &Provenance) {
            self.0.increment(provenance);
        }
        fn enter_raw(&mut self, _source: &str, _attrs: &[StyleAttr], provenance: &Provenance) {
            self.0.increment(provenance);
        }
    }

    walk_inline(&mut Vis(counts), value);
}

impl ProvenanceCounts {
    fn increment(&mut self, provenance: &Provenance) {
        match provenance {
            Provenance::Parser => self.parser += 1,
            Provenance::ParserNormalized => self.parser_normalized += 1,
            Provenance::SourceSupplement => self.source_supplement += 1,
            Provenance::SourceFallback => self.source_fallback += 1,
        }
    }
}

fn inline_visible_text(content: &[Inline]) -> String {
    let mut visitor = VisibleCollector { out: String::new() };
    for child in content {
        walk_inline(&mut visitor, child);
    }
    visitor.out
}

fn with_provenance(mut value: serde_json::Value, provenance: Provenance) -> serde_json::Value {
    if provenance != Provenance::Parser {
        value["x-provenance"] = serde_json::Value::String(provenance.as_str().to_owned());
    }
    value
}

impl Provenance {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Parser => "parser",
            Self::ParserNormalized => "parser_normalized",
            Self::SourceSupplement => "source_supplement",
            Self::SourceFallback => "source_fallback",
        }
    }
}

impl RubyPlacement {
    #[must_use]
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Right => "right",
            Self::Left => "left",
        }
    }
}

#[must_use]
pub fn block_content(block: &Block) -> &[Inline] {
    match block {
        Block::Paragraph { content }
        | Block::Heading { content, .. }
        | Block::Jisage { content, .. }
        | Block::CaptionBlock { content }
        | Block::Warichu { content }
        | Block::Figure { content, .. } => content,
        Block::Break { .. } => &[],
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
        assert_eq!(json[0]["content"][0]["direction"], "right");
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
    fn gaiji_kind_can_model_ivs_alternative_image_and_dakuten() {
        let ivs = GaijiKind::UnicodeSequence {
            values: vec!['葛', '\u{E0100}'],
        };
        let alternative = GaijiKind::Alternative {
            source_kind: Box::new(GaijiKind::Composition {
                description: "「口＋愛」".to_owned(),
            }),
        };
        let image = GaijiKind::Image {
            path: "gaiji/1-15/1-15-23.png".to_owned(),
        };
        let dakuten = Inline::gaiji_ref(GaijiRef {
            source: "※［＃濁点付きワ］".to_owned(),
            description: "濁点付きワ".to_owned(),
            description_format: Some("dakuten-variant".to_owned()),
            kind: GaijiKind::DakutenVariant {
                base: "ワ".to_owned(),
                mark: DakutenMark::Voicing,
            },
            resolved: Some("ワ゛".to_owned()),
            provenance: Provenance::Parser,
        });

        assert_eq!(
            ivs,
            GaijiKind::UnicodeSequence {
                values: vec!['葛', '\u{E0100}']
            }
        );
        assert_eq!(
            alternative,
            GaijiKind::Alternative {
                source_kind: Box::new(GaijiKind::Composition {
                    description: "「口＋愛」".to_owned()
                })
            }
        );
        assert_eq!(
            image,
            GaijiKind::Image {
                path: "gaiji/1-15/1-15-23.png".to_owned()
            }
        );
        assert!(matches!(
            dakuten,
            Inline::GaijiRef(GaijiRef {
                kind: GaijiKind::DakutenVariant {
                    mark: DakutenMark::Voicing,
                    ..
                },
                ..
            })
        ));
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
        let ruby = Inline::ruby_with_base(vec![Inline::text("左")], "ひだり", RubyPlacement::Left);

        assert!(matches!(
            ruby,
            Inline::Ruby {
                placement: RubyPlacement::Left,
                ..
            }
        ));
    }

    #[test]
    fn aat_projection_flattens_resolved_gaiji_ruby_base() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby_with_base(
                vec![Inline::gaiji_ref(GaijiRef {
                    source: "※［＃「口＋世」、U+546D］".to_owned(),
                    description: "「口＋世」、U+546D".to_owned(),
                    description_format: None,
                    kind: GaijiKind::UnicodeCodepoint { value: '呻' },
                    resolved: Some("呻".to_owned()),
                    provenance: Provenance::Parser,
                })],
                "うめ",
                RubyPlacement::Right,
            )],
        }];

        let projection = blocks_to_aat_projection(&blocks);

        assert!(projection.warnings.is_empty());
        assert_eq!(projection.blocks[0]["content"][0]["kind"], "ruby");
        assert_eq!(projection.blocks[0]["content"][0]["base"], "呻");
        assert_eq!(projection.blocks[0]["content"][0]["reading"], "うめ");
        assert_eq!(projection.blocks[0]["content"][0]["direction"], "right");
    }

    #[test]
    fn aat_projection_warns_and_emits_gaiji_for_unresolved_gaiji_ruby_base() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby_with_base(
                vec![Inline::gaiji_ref(GaijiRef {
                    source: "※［＃「口＋愛」、第3水準1-15-23］".to_owned(),
                    description: "「口＋愛」、第3水準1-15-23".to_owned(),
                    description_format: None,
                    kind: GaijiKind::JisLevel {
                        level: 3,
                        row: 15,
                        cell: 23,
                    },
                    resolved: None,
                    provenance: Provenance::Parser,
                })],
                "おくび",
                RubyPlacement::Right,
            )],
        }];

        let projection = blocks_to_aat_projection(&blocks);

        assert_eq!(projection.blocks[0]["content"][0]["kind"], "gaiji");
        assert_eq!(
            projection.blocks[0]["content"][0]["description"],
            "「口＋愛」、第3水準1-15-23"
        );
        assert_eq!(projection.warnings.len(), 1);
        assert_eq!(
            projection.warnings[0].syntax_id,
            "gaiji_ruby.unresolved_base"
        );
        assert!(projection.warnings[0].message.contains("おくび"));
    }

    #[test]
    fn semantic_summary_records_ruby_gaiji_gaiji_ruby_and_projection_warnings() {
        let blocks = vec![Block::Paragraph {
            content: vec![
                Inline::ruby("吾輩", "わがはい"),
                Inline::ruby_with_base(
                    vec![Inline::gaiji_ref(GaijiRef {
                        source: "※［＃「口＋愛」、第3水準1-15-23］".to_owned(),
                        description: "「口＋愛」、第3水準1-15-23".to_owned(),
                        description_format: None,
                        kind: GaijiKind::JisLevel {
                            level: 3,
                            row: 15,
                            cell: 23,
                        },
                        resolved: None,
                        provenance: Provenance::Parser,
                    })],
                    "おくび",
                    RubyPlacement::Right,
                ),
            ],
        }];
        let projection = blocks_to_aat_projection(&blocks);

        let summary = semantic_summary(&blocks, &projection.warnings);

        assert_eq!(summary.syntax["ruby.basic"].len(), 2);
        assert_eq!(summary.syntax["ruby.basic"][0].kind, "ruby");
        assert_eq!(summary.syntax["ruby.basic"][0].value["reading"], "わがはい");
        assert_eq!(summary.syntax["ruby.basic"][0].provenance, "parser");
        assert_eq!(summary.syntax["gaiji.marker"].len(), 1);
        assert_eq!(
            summary.syntax["gaiji.marker"][0].value["description"],
            "「口＋愛」、第3水準1-15-23"
        );
        assert_eq!(summary.syntax["gaiji_ruby.inline_base"].len(), 1);
        assert_eq!(
            summary.syntax["gaiji_ruby.inline_base"][0].value["reading"],
            "おくび"
        );
        assert_eq!(summary.syntax["projection.warning"].len(), 1);
        assert_eq!(
            summary.syntax["projection.warning"][0].value["syntax_id"],
            "gaiji_ruby.unresolved_base"
        );
        assert_eq!(
            summary.syntax["projection.warning"][0].provenance,
            "projection"
        );
    }

    #[test]
    fn jisage_block_projects_to_jisage_block_with_x_indent() {
        let blocks = vec![Block::jisage(3, vec![Inline::text("インデント本文")])];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["kind"], "jisage_block");
        assert_eq!(json[0]["x-indent"], 3);
        assert_eq!(json[0]["children"][0]["kind"], "paragraph");
        assert_eq!(
            json[0]["children"][0]["content"][0]["value"],
            "インデント本文"
        );
    }

    #[test]
    fn warichu_projects_to_paragraph_with_warigaki() {
        let blocks = vec![Block::warichu(vec![Inline::text("注釈")])];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["x-warichu"], true);
        assert_eq!(json[0]["content"][0]["kind"], "warigaki");
        assert_eq!(json[0]["content"][0]["upper"][0]["value"], "注釈");
        assert!(
            json[0]["content"][0]["lower"]
                .as_array()
                .unwrap()
                .is_empty()
        );
    }

    #[test]
    fn figure_projects_with_image_then_caption() {
        let blocks = vec![Block::figure(
            "fig01.png",
            vec![Inline::text("キャプション")],
        )];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["kind"], "paragraph");
        assert_eq!(json[0]["x-figure"], true);
        assert_eq!(json[0]["content"][0]["kind"], "Image");
        assert_eq!(json[0]["content"][0]["source"], "fig01.png");
        assert_eq!(json[0]["content"][1]["kind"], "text");
        assert_eq!(json[0]["content"][1]["value"], "キャプション");
    }

    #[test]
    fn breaks_project_with_x_break_kind() {
        let blocks = vec![Block::page_break(), Block::line_break()];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["x-break-kind"], "page");
        assert_eq!(json[1]["x-break-kind"], "line");
        assert!(json[0]["content"].as_array().unwrap().is_empty());
    }

    #[test]
    fn semantic_summary_records_jisage_warichu_figure_and_break() {
        let blocks = vec![
            Block::jisage(2, vec![Inline::text("二字下げ")]),
            Block::warichu(vec![Inline::text("割り注")]),
            Block::figure("fig.png", vec![Inline::text("写真")]),
            Block::page_break(),
            Block::line_break(),
        ];

        let summary = semantic_summary(&blocks, &[]);

        assert_eq!(summary.syntax["indentation.jisage_block"].len(), 1);
        assert_eq!(
            summary.syntax["indentation.jisage_block"][0].value["indent"],
            2
        );
        assert_eq!(
            summary.syntax["indentation.jisage_block"][0].value["text"],
            "二字下げ"
        );
        assert_eq!(summary.syntax["warichu.basic"].len(), 1);
        assert_eq!(summary.syntax["figure.image_caption"].len(), 1);
        assert_eq!(
            summary.syntax["figure.image_caption"][0].value["source"],
            "fig.png"
        );
        assert_eq!(summary.syntax["break.page_line"].len(), 1);
        assert_eq!(summary.syntax["break.line_explicit"].len(), 1);
    }

    #[test]
    fn visible_projection_walks_through_jisage_warichu_and_figure() {
        let blocks = vec![
            Block::jisage(1, vec![Inline::text("AA")]),
            Block::warichu(vec![Inline::text("BB")]),
            Block::figure("f.png", vec![Inline::text("CC")]),
            Block::page_break(),
        ];

        assert_eq!(visible_projection(&blocks).visible_text, "AABBCC");
    }

    #[test]
    fn non_parser_provenance_is_projected_as_extension_metadata() {
        let blocks = vec![Block::Paragraph {
            content: vec![Inline::ruby_with_provenance(
                "",
                "わがはい",
                Provenance::SourceSupplement,
            )],
        }];

        let json = blocks_to_aat_json(&blocks);

        assert_eq!(json[0]["content"][0]["x-provenance"], "source_supplement");
        assert_eq!(provenance_counts(&blocks).source_supplement, 1);
    }
}
