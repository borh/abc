#!/usr/bin/env python
"""Compatibility wrapper for the promoted TEI-EAJ comparator."""

import importlib.util
import pathlib


TOOL_PATH = pathlib.Path(__file__).resolve().parents[2] / "tools" / "tei_eaj_compare.py"


def load_tool():
    spec = importlib.util.spec_from_file_location("tei_eaj_compare_tool", TOOL_PATH)
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


if __name__ == "__main__":
    load_tool().main()
