"""Source-region coverage contract helpers shared by report scripts."""

from __future__ import annotations

from typing import Any

SOURCE_REGION_SCHEMA_ID = "https://w3id.org/abc/schemas/source-region-coverage.schema.json"
SOURCE_REGION_SCHEMA_VERSION = "aozora-source-region-coverage-v1"
SOURCE_REGION_POLICY_ID = "https://w3id.org/abc/policies/source-region-publication-v0"
SOURCE_REGION_POLICY_VERSION = "0.2.0"
SCHEMA_ID = SOURCE_REGION_SCHEMA_ID
SCHEMA_VERSION = SOURCE_REGION_SCHEMA_VERSION
POLICY_ID = SOURCE_REGION_POLICY_ID
POLICY_VERSION = SOURCE_REGION_POLICY_VERSION

REQUIRED_COUNTERS = {
    "body_typed_occurrences",
    "body_raw_preserved_occurrences",
    "source_apparatus_occurrences",
    "front_matter_occurrences",
    "back_matter_occurrences",
    "body_end_boundary_occurrences",
    "terminal_provenance_occurrences",
    "colophon_metadata_occurrences",
    "letter_address_origin_occurrences",
    "malformed_source_occurrences",
    "unsupported_body_markup_occurrences",
    "unknown_region_occurrences",
    "unknown_unreviewed_occurrences",
}
REQUIRED_LEGACY_COUNTERS = {
    "typed_occurrences",
    "raw_preserved_occurrences",
    "out_of_body_occurrences",
    "malformed_noise_occurrences",
    "unsupported_occurrences",
    "needs_research_occurrences",
}
REQUIRED_CLASSES = {
    "notation_legend",
    "notation_placeholder",
    "body_end_boundary",
    "terminal_provenance",
    "colophon_metadata",
    "letter_address_origin",
    "malformed_source",
}
MEASURED_CLASS_COUNTERS = {
    "body_end_boundary": "body_end_boundary_occurrences",
    "terminal_provenance": "terminal_provenance_occurrences",
    "colophon_metadata": "colophon_metadata_occurrences",
    "letter_address_origin": "letter_address_origin_occurrences",
}
ALLOWED_TARGET_CLASSES = {
    "tei_policy_projection",
    "tei_plus_abc_extension",
    "custom_sidecar",
    "diagnostic",
    "unsupported_gap",
}
ALLOWED_MEASUREMENT_STATUSES = {"measured", "needs_measurement_split"}


def coverage_passes_source_authority(summary: dict[str, Any]) -> bool:
    """Return whether a source-region summary is green enough for report gates."""
    region = summary.get("source_region_coverage", {})
    if not isinstance(region, dict):
        return False
    required_counters_present = all(
        isinstance(region.get(counter), int) for counter in REQUIRED_COUNTERS
    )
    return bool(
        summary.get("schema_version") == SOURCE_REGION_SCHEMA_VERSION
        and summary.get("gate_status") == "SOURCE_AUTHORITY_GATE_PASS"
        and required_counters_present
        and (summary.get("unallowlisted_unknown_markers_total") or 0) == 0
        and (region.get("unsupported_body_markup_occurrences") or 0) == 0
        and (region.get("unknown_region_occurrences") or 0) == 0
        and (region.get("unknown_unreviewed_occurrences") or 0) == 0
    )
