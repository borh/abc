"""Resolve and validate AAT corpus run sets used by fidelity reports."""

from __future__ import annotations

import json
import os
import string
from pathlib import Path
from typing import Any

JsonObject = dict[str, Any]

DEFAULT_DB_ROOT = "/db/ab-validator"
DEFAULT_RUN_SET = Path(__file__).resolve().parents[1] / "aat-fidelity" / "run-sets" / "current.json"
DEFAULT_LOCK_PATHS = ("flake.lock", "ab-validator/flake.lock")


def load_run_set(path: str | os.PathLike[str] | None = None) -> JsonObject:
    """Load a run-set JSON file.

    The path defaults to AB_AAT_RUN_SET, then the checked-in current run set.
    The returned value carries private path metadata for relative path resolution.
    """
    selected = Path(path or os.environ.get("AB_AAT_RUN_SET", DEFAULT_RUN_SET))
    run_set = json.loads(selected.read_text(encoding="utf-8"))
    if not isinstance(run_set, dict):
        raise ValueError(f"AAT run set must be a JSON object: {selected}")
    run_set["_run_set_path"] = str(selected)
    run_set["_run_set_dir"] = str(selected.parent)
    return run_set


def adapter_aat_dirs(run_set: JsonObject, order: list[str] | None = None) -> dict[str, str]:
    """Return adapter label -> resolved AAT directory."""
    adapters = _adapters(run_set)
    labels = order or list(adapters)
    return {label: _adapter_path(run_set, label, "aat_dir") for label in labels}


def adapter_aat_globs(run_set: JsonObject, order: list[str] | None = None) -> dict[str, str]:
    """Return adapter label -> resolved `*.json` glob."""
    return {
        label: str(Path(path) / "*.json")
        for label, path in adapter_aat_dirs(run_set, order=order).items()
    }


def validate_run_set(
    run_set: JsonObject,
    *,
    repo_root: str | os.PathLike[str] | None = None,
    require_paths: bool = False,
) -> list[str]:
    """Return validation errors for a run set and its descriptors.

    This is intentionally a semantic validator, not a JSON Schema substitute. It
    checks the coherence that has caused drift: selected AAT directories,
    descriptor adapter ids, and flake input revisions.
    """
    errors: list[str] = []
    root = Path(repo_root or Path.cwd())
    lock_nodes = _load_lock_nodes(root)

    for label, entry in _adapters(run_set).items():
        if not isinstance(entry, dict):
            errors.append(f"adapter {label}: entry must be an object")
            continue
        aat_dir = _adapter_path(run_set, label, "aat_dir")
        if require_paths and not Path(aat_dir).is_dir():
            errors.append(f"adapter {label}: AAT directory missing: {aat_dir}")

        descriptor_path = _optional_adapter_path(run_set, label, "run_descriptor")
        descriptor: JsonObject | None = None
        if descriptor_path:
            path = Path(descriptor_path)
            if path.exists():
                loaded = json.loads(path.read_text(encoding="utf-8"))
                if isinstance(loaded, dict):
                    descriptor = loaded
                else:
                    errors.append(f"adapter {label}: run descriptor must be an object: {path}")
            elif require_paths:
                errors.append(f"adapter {label}: run descriptor missing: {path}")

            # Coherence: the run_descriptor (metadata.json) must sit at the root of the
            # SAME dump as the resolved aat_dir. A stale `<label>_aat_dir_env` override
            # can otherwise swap in a different dump while the descriptor still resolves
            # to the run-set default (which matches flake.lock), so validation would pass
            # on the wrong data. See 2026-07-09-fidelity-workflow-integration.md.
            dump_root = Path(descriptor_path).parent
            try:
                Path(aat_dir).resolve().relative_to(dump_root.resolve())
            except ValueError:
                errors.append(
                    f"adapter {label}: aat_dir is not under the run_descriptor dump root "
                    f"(dump-swap / stale *_AAT_DIR override?): aat_dir={aat_dir} "
                    f"descriptor_root={dump_root}"
                )

        expected = entry.get("expected", {})
        if isinstance(expected, dict):
            errors.extend(_validate_expected(label, expected, descriptor, lock_nodes))
    return errors


def _adapters(run_set: JsonObject) -> JsonObject:
    adapters = run_set.get("adapters")
    if not isinstance(adapters, dict):
        raise ValueError("AAT run set must contain an adapters object")
    return adapters


def _adapter_path(run_set: JsonObject, label: str, field: str) -> str:
    value = _optional_adapter_path(run_set, label, field)
    if not value:
        raise ValueError(f"adapter {label}: missing {field}")
    return value


def _optional_adapter_path(run_set: JsonObject, label: str, field: str) -> str | None:
    entry = _adapters(run_set)[label]
    if not isinstance(entry, dict):
        return None
    env_name = entry.get(f"{field}_env")
    if isinstance(env_name, str) and os.environ.get(env_name):
        return os.environ[env_name]
    value = entry.get(field)
    if not isinstance(value, str) or not value:
        return None
    return _resolve_path(run_set, value)


def _resolve_path(run_set: JsonObject, value: str) -> str:
    env = dict(os.environ)
    env.setdefault("AB_DB_ROOT", DEFAULT_DB_ROOT)
    expanded = string.Template(value).safe_substitute(env)
    path = Path(os.path.expanduser(expanded))
    if not path.is_absolute():
        base = Path(str(run_set.get("_run_set_dir", ".")))
        path = base / path
    return str(path)


def _load_lock_nodes(repo_root: Path) -> dict[str, dict[str, str]]:
    nodes: dict[str, dict[str, str]] = {}
    for rel_path in DEFAULT_LOCK_PATHS:
        path = repo_root / rel_path
        if not path.exists():
            continue
        data = json.loads(path.read_text(encoding="utf-8"))
        for name, node in data.get("nodes", {}).items():
            locked = node.get("locked", {})
            if isinstance(locked, dict):
                nodes[f"{rel_path}:{name}"] = {
                    "rev": str(locked.get("rev", "")),
                    "narHash": str(locked.get("narHash", "")),
                }
    return nodes


def _validate_expected(
    label: str,
    expected: JsonObject,
    descriptor: JsonObject | None,
    lock_nodes: dict[str, dict[str, str]],
) -> list[str]:
    errors: list[str] = []
    expected_adapter = expected.get("adapter_id")
    if descriptor is not None and isinstance(expected_adapter, str):
        actual_adapter = descriptor.get("adapter_id")
        adapter_version_contains = expected.get("adapter_version_contains")
        actual_version = descriptor.get("adapter_version")
        if actual_adapter == expected_adapter:
            pass
        elif isinstance(adapter_version_contains, str) and isinstance(actual_version, str):
            if adapter_version_contains not in actual_version:
                errors.append(
                    f"adapter {label}: descriptor adapter_version {actual_version!r} "
                    f"does not contain expected {adapter_version_contains!r}"
                )
        else:
            errors.append(
                f"adapter {label}: descriptor adapter_id {actual_adapter!r} "
                f"does not match expected {expected_adapter!r}"
            )

    source = expected.get("source")
    if isinstance(source, dict):
        flake_input = source.get("flake_input")
        rev = source.get("rev")
        nar_hash = source.get("narHash")
        if isinstance(flake_input, str):
            matches = {
                name: locked
                for name, locked in lock_nodes.items()
                if name.endswith(f":{flake_input}")
            }
            if not matches:
                errors.append(
                    f"adapter {label}: flake input not found in flake.lock: {flake_input}"
                )
            for lock_name, locked in sorted(matches.items()):
                if isinstance(rev, str) and locked.get("rev") != rev:
                    errors.append(
                        f"adapter {label}: {lock_name} rev {locked.get('rev')} "
                        f"does not match expected {rev}"
                    )
                if isinstance(nar_hash, str) and locked.get("narHash") != nar_hash:
                    errors.append(
                        f"adapter {label}: {lock_name} narHash {locked.get('narHash')} "
                        f"does not match expected {nar_hash}"
                    )
        if descriptor is not None:
            descriptor_source = descriptor.get("source", {})
            if isinstance(descriptor_source, dict):
                actual_rev = descriptor_source.get("rev")
                if isinstance(rev, str) and actual_rev and actual_rev != rev:
                    errors.append(
                        f"adapter {label}: descriptor source rev {actual_rev!r} "
                        f"does not match expected {rev!r}"
                    )
    return errors
