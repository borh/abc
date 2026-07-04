//! `aozora-epub3-adapter` library: map AozoraEpub3-JDK21-rendered XHTML to AAT.

pub mod decode;
pub mod model;
pub mod source_derived;
pub mod xhtml_mapper;

pub use model::{
    parse_failure_envelope, DecodedSource, MappingInput, XhtmlDocument, XhtmlDocumentKind,
    ADAPTER_NAME, ADAPTER_VERSION, CLI_VERSION,
};

pub use xhtml_mapper::{map_to_aat, map_to_html};