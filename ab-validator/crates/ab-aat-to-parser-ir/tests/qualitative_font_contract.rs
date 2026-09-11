//! Qualitative font-size wire values exclude invented numeric stages.

use serde_json::{Value, json};

#[test]
fn font_sums_reject_numeric_fields_on_qualitative_comparisons() {
    for (schema, definition, kind) in [
        (
            include_str!("../../../data/aat-schema.json"),
            "formatting_attribute",
            "font_size",
        ),
        (
            include_str!("../../../schemas/parser-ir.schema.json"),
            "layoutScope",
            "font-size",
        ),
    ] {
        let schema: Value = serde_json::from_str(schema).unwrap();
        let projection = json!({"$schema":schema["$schema"], "$defs":schema["$defs"], "$ref":format!("#/$defs/{definition}")});
        let validator = jsonschema::validator_for(&projection).unwrap();
        let mut qualitative = json!({"kind":kind, "size_type":"qualitative", "direction":"smaller", "qualifier":"やや"});
        if kind == "font-size" {
            qualitative["source"] = json!("aat-inline");
        }
        assert!(validator.is_valid(&qualitative), "{qualitative}");
        for (field, value) in [
            ("level", json!(1)),
            ("size", json!("small")),
            ("qualifier", json!("one-stage")),
        ] {
            let mut malformed = qualitative.clone();
            malformed[field] = value;
            assert!(!validator.is_valid(&malformed), "{malformed}");
        }
        let mut absent = qualitative.clone();
        absent.as_object_mut().unwrap().remove("direction");
        assert!(!validator.is_valid(&absent));
        for size_type in ["small", "large"] {
            let mut numeric = qualitative.clone();
            numeric["size_type"] = json!(size_type);
            numeric["level"] = json!(2);
            assert!(!validator.is_valid(&numeric));
            numeric.as_object_mut().unwrap().remove("direction");
            numeric.as_object_mut().unwrap().remove("qualifier");
            assert!(validator.is_valid(&numeric), "{numeric}");
        }
    }
}
