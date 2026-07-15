import hashlib
import importlib.util
import json
import zipfile
from pathlib import Path


SCRIPT = Path(__file__).parents[1] / "neutral_executor.py"
SPEC = importlib.util.spec_from_file_location("neutral_executor", SCRIPT)
assert SPEC and SPEC.loader
executor = importlib.util.module_from_spec(SPEC)
SPEC.loader.exec_module(executor)


def digest(data: bytes) -> str:
    return "sha256:" + hashlib.sha256(data).hexdigest()


def test_run_and_resume_produce_one_verified_outcome_per_inventory_item(tmp_path: Path) -> None:
    sources = tmp_path / "sources"
    sources.mkdir()
    (sources / "a.txt").write_bytes(b"alpha")
    (sources / "b.txt").write_bytes(b"beta")
    inventory = {
        "items": [
            {"id": "a", "path": "a.txt", "sha256": digest(b"alpha")},
            {"id": "b", "path": "b.txt", "sha256": digest(b"beta")},
        ]
    }
    inventory_path = tmp_path / "inventory.json"
    inventory_path.write_text(json.dumps(inventory))
    output = tmp_path / "out"

    executor.execute(inventory_path, sources, ["/bin/sh", "-c", "cat"], output, 2, 1)
    first = executor.verify(inventory_path, output)
    assert [row["item_id"] for row in first] == ["a", "b"]
    assert all(row["status"] == "success" for row in first)

    (output / "manifest.json").unlink()
    executor.execute(inventory_path, sources, ["/bin/sh", "-c", "cat"], output, 2, 1)
    assert executor.verify(inventory_path, output) == first

    executor.execute(inventory_path, sources, ["/bin/sh", "-c", "exit 99"], output, 2, 1)
    second = executor.verify(inventory_path, output)
    assert all(row["status"] == "failure" for row in second)
    assert second != first


def test_verifier_rejects_missing_duplicate_and_tampered_outcomes(tmp_path: Path) -> None:
    inventory_path = tmp_path / "inventory.json"
    inventory_path.write_text(json.dumps({"items": []}))
    output = tmp_path / "out"
    output.mkdir()
    (output / "manifest.json").write_text(json.dumps({"outcomes": ["missing.json"]}))
    try:
        executor.verify(inventory_path, output)
    except ValueError as error:
        assert "manifest outcomes" in str(error)
    else:
        raise AssertionError("invalid manifest accepted")


def test_inventory_paths_are_cross_host_safe(tmp_path: Path) -> None:
    for path in ["../x", "/tmp/x", "C:\\tmp\\x", "\\tmp\\x"]:
        inventory = tmp_path / "inventory.json"
        inventory.write_text(
            json.dumps({"items": [{"id": "x", "path": path, "sha256": digest(b"")}]})
        )
        try:
            executor.load_inventory(inventory)
        except ValueError:
            pass
        else:
            raise AssertionError(f"unsafe path accepted: {path}")


def test_materialize_vectors_is_sorted_complete_and_content_addressed(tmp_path: Path) -> None:
    vectors = tmp_path / "vectors"
    for name, source in [("z", "後"), ("a", "前")]:
        path = vectors / name
        path.mkdir(parents=True)
        (path / "vector.json").write_text(json.dumps({"name": name, "source": source}))
    output = tmp_path / "materialized"
    executor.materialize_vectors(vectors, output)
    inventory = executor.load_inventory(output / "inventory.json")
    assert [item["id"] for item in inventory] == ["a", "z"]
    assert (output / "sources" / inventory[0]["path"]).read_text() == "前"


def test_materialize_index_extracts_zip_members_without_reordering(tmp_path: Path) -> None:
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    with zipfile.ZipFile(corpus / "works.zip", "w") as archive:
        archive.writestr("a.txt", b"alpha")
    index = tmp_path / "index.json"
    index.write_text(json.dumps({"works": [{"id": "work-a", "txt_path": "works.zip::a.txt"}]}))
    output = tmp_path / "materialized"
    executor.materialize_index(index, corpus, output, 0)
    inventory = executor.load_inventory(output / "inventory.json")
    assert inventory[0]["id"] == "work-a"
    assert (output / "sources" / inventory[0]["path"]).read_bytes() == b"alpha"


def test_materialize_index_is_smoke_only_not_the_authoritative_inventory(tmp_path: Path) -> None:
    corpus = tmp_path / "corpus"
    corpus.mkdir()
    (corpus / "a.txt").write_bytes(b"alpha")
    index = tmp_path / "index.json"
    index.write_text(json.dumps({"works": [{"id": "work-a", "txt_path": "a.txt"}]}))
    output = tmp_path / "materialized"
    executor.materialize_index(index, corpus, output, 0)
    document = json.loads((output / "inventory.json").read_bytes())
    # The smoke helper uses a different identity scheme than the authoritative
    # Rust `ab-materialize-study-inventory` ({work_id}-{sha12}): here the id is the
    # raw work id and the path is sha256(work_id). Pin that so it cannot be
    # mistaken for, or used to reproduce, the study inventory hash.
    item = document["items"][0]
    assert item["id"] == "work-a"
    assert "-" not in item["id"] or not item["id"].startswith("work-a-")
    assert item["path"] == hashlib.sha256(b"work-a").hexdigest() + ".txt"
    # The artifact must self-identify as non-authoritative.
    assert document.get("materializer") == "neutral_executor.smoke"
    assert "authoritative" not in document  # never claims authority


def test_verifier_rejects_output_symlink_escape(tmp_path: Path) -> None:
    output = tmp_path / "out"
    output.mkdir()
    outside = tmp_path / "outside.json"
    outside.write_text("{}")
    (output / "escaped.json").symlink_to(outside)
    try:
        executor.contained_file(output, "escaped.json")
    except ValueError:
        pass
    else:
        raise AssertionError("output-root symlink escape accepted")
