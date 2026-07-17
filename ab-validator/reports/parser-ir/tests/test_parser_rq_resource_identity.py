import importlib.util
import pathlib


MODULE = pathlib.Path(__file__).parents[1] / "parser-rq-resource-identity.py"


def load_module():
    spec = importlib.util.spec_from_file_location("parser_rq_resource_identity", MODULE)
    assert spec and spec.loader
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def test_local_import_closure_is_transitive_and_stable(tmp_path):
    (tmp_path / "entry.py").write_text("import helper\n", encoding="utf-8")
    (tmp_path / "helper.py").write_text("VALUE = 1\n", encoding="utf-8")
    identity = load_module()
    closure = identity.discover_local_import_closure(tmp_path / "entry.py", (tmp_path,))
    assert closure == (tmp_path / "entry.py", tmp_path / "helper.py")


def test_identity_changes_with_dependency_bytes(tmp_path):
    entry = tmp_path / "entry.py"
    helper = tmp_path / "helper.py"
    entry.write_text("import helper\n", encoding="utf-8")
    helper.write_text("VALUE = 1\n", encoding="utf-8")
    identity = load_module()
    before = identity.build_identity(entry, pathlib.Path("/proc/self/exe"), "drv")
    helper.write_text("VALUE = 2\n", encoding="utf-8")
    after = identity.build_identity(entry, pathlib.Path("/proc/self/exe"), "drv")
    assert before["wrapper_identity_hash"] != after["wrapper_identity_hash"]
