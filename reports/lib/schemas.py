"""Load and validate JSON schemas and instances."""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any

import jsonschema


def load_schema(path: Path) -> dict[str, Any]:
    """Load a JSON Schema from a file path."""
    return json.loads(path.read_text(encoding="utf-8"))


def validate_instance(instance: Any, schema: dict[str, Any]) -> None:
    """Validate an instance against a JSON Schema. Raises on failure."""
    jsonschema.validate(instance, schema)


def load_and_validate(path: Path, schema: dict[str, Any]) -> Any:
    """Load a JSON file and validate it against a JSON Schema."""
    instance = json.loads(path.read_text(encoding="utf-8"))
    jsonschema.validate(instance, schema)
    return instance
