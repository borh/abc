from __future__ import annotations

import re

MAX_POINTER_DEPTH = 12
MAX_ARRAY_DEPTH = 6


class MappingContractError(ValueError):
    pass


def _deref(schema: dict, node: dict) -> dict:
    ref = node.get("$ref")
    if not ref:
        return node
    prefix = "#/$defs/"
    if not ref.startswith(prefix):
        raise MappingContractError(f"unsupported ref: {ref}")
    return schema["$defs"][ref[len(prefix):]]


def _kind_values(node: dict) -> list[str]:
    kind = node.get("properties", {}).get("kind", {})
    if "const" in kind:
        return [kind["const"]]
    enum = kind.get("enum")
    if isinstance(enum, list):
        return [value for value in enum if isinstance(value, str)]
    return []


def _collect_paths(
    schema: dict,
    node: dict,
    prefix: str,
    out: set[str],
    depth: int = 0,
) -> None:
    if depth > MAX_POINTER_DEPTH:
        return
    node = _deref(schema, node)
    if "oneOf" in node:
        for child in node["oneOf"]:
            resolved = _deref(schema, child)
            for kind in _kind_values(resolved):
                if not prefix:
                    continue
                out.add(f"{prefix}.{kind}")
                _collect_paths(schema, resolved, f"{prefix}.{kind}", out, depth + 1)
            _collect_paths(schema, resolved, prefix, out, depth + 1)
        return
    if node.get("type") == "array":
        out.add(prefix)
        if prefix.count("[]") >= MAX_ARRAY_DEPTH:
            return
        _collect_paths(schema, node["items"], f"{prefix}[]", out, depth + 1)
        return
    if node.get("type") == "object" or "properties" in node:
        if prefix:
            out.add(prefix)
        for name, child in node.get("properties", {}).items():
            if name == "kind":
                continue
            child_prefix = f"{prefix}.{name}" if prefix else name
            out.add(child_prefix)
            _collect_paths(schema, child, child_prefix, out, depth + 1)


def allowed_aat_pointers(aat_schema: dict) -> set[str]:
    out: set[str] = set()
    _collect_paths(aat_schema, aat_schema, "", out)
    return out


_INDEX_RE = re.compile(r"\[[0-9]+\]")


def fold_pointer(pointer: str) -> str:
    return _INDEX_RE.sub("[]", pointer)


def validate_mapping_contract(mapping: dict, aat_schema: dict) -> None:
    allowed = allowed_aat_pointers(aat_schema)
    for rule in mapping.get("transform_rule_descriptions", []):
        pointer = rule.get("aat_pointer")
        if pointer is None:
            continue
        folded = fold_pointer(pointer)
        if folded not in allowed:
            raise MappingContractError(
                f"{rule.get('rule_id')} has non-schema AAT pointer {pointer!r}"
            )
