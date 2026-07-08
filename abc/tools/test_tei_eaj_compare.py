#!/usr/bin/env python
import importlib.util
import json
import pathlib
import tempfile
import textwrap
import unittest


TOOL_DIR = pathlib.Path(__file__).resolve().parent


def load_probe():
    spec = importlib.util.spec_from_file_location(
        "tei_eaj_compare", TOOL_DIR / "tei_eaj_compare.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


def load_reports():
    spec = importlib.util.spec_from_file_location(
        "tei_eaj_aozora_reports", TOOL_DIR / "tei_eaj_aozora_reports.py"
    )
    module = importlib.util.module_from_spec(spec)
    spec.loader.exec_module(module)
    return module


probe = load_probe()
reports = load_reports()


TEI_TEMPLATE = """\
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt>
        <title>{title}</title>
      </titleStmt>
      <sourceDesc><p>source</p></sourceDesc>
    </fileDesc>
  </teiHeader>
  <text>
    <body>{body}</body>
  </text>
</TEI>
"""

TEI_WITH_WORK_ID_1567 = """\
<?xml version="1.0" encoding="UTF-8"?>
<TEI xmlns="http://www.tei-c.org/ns/1.0">
  <teiHeader>
    <fileDesc>
      <titleStmt><title>走れメロス</title></titleStmt>
      <publicationStmt><idno type="aozora-work-id">001567</idno></publicationStmt>
      <sourceDesc><p>source</p></sourceDesc>
    </fileDesc>
  </teiHeader>
  <text><body><p>メロス</p></body></text>
</TEI>
"""


class TeiEajCompareTest(unittest.TestCase):
    def write_xml(self, root, relpath, title, body):
        path = root / relpath
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(TEI_TEMPLATE.format(title=title, body=body), encoding="utf-8")
        return path

    def test_discovers_finished_and_draft_tei_eaj_files_with_levels(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            self.write_xml(
                root, "data/complete/tei_lib_lv4/1567_tei.xml", "走れメロス", "<p>one</p>"
            )
            self.write_xml(root, "data/draft/tei_lib_lv3/86_tei.xml", "羅生門", "<p>two</p>")
            self.write_xml(root, "data/etc/misc.xml", "Other", "<p>three</p>")

            records = probe.discover_tei_eaj_files(root)

        self.assertEqual(
            [
                ("data/complete/tei_lib_lv4/1567_tei.xml", "complete", "Level 4", "走れメロス"),
                ("data/draft/tei_lib_lv3/86_tei.xml", "draft", "Level 3", "羅生門"),
                ("data/etc/misc.xml", "etc", None, "Other"),
            ],
            [
                (record["relpath"], record["state"], record["level"], record["title"])
                for record in records
            ],
        )

    def test_compares_abc_melos_against_all_melos_variants_and_flags_no_draft(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc = self.write_xml(
                root,
                "abc-melos.xml",
                "走れメロス",
                "<p>メロス<ruby><rb>邪智暴虐</rb><rt>じゃちぼうぎゃく</rt></ruby></p><note>source note</note>",
            )
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_tei.xml",
                "走れメロス",
                "<p>メロス邪智暴虐</p>",
            )
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_header_updated.xml",
                "走れメロス",
                "<p>メロス邪智暴虐</p>",
            )
            self.write_xml(root, "data/draft/tei_lib_lv3/86_tei.xml", "羅生門", "<p>羅生門</p>")

            report = probe.build_report(abc, root, source_rev="probe-rev")
            markdown = probe.render_markdown(report)

        self.assertEqual(3, report["tei_eaj_file_count"])
        self.assertEqual(2, report["melos_file_count"])
        self.assertEqual(2, report["complete_melos_file_count"])
        self.assertEqual(0, report["draft_melos_file_count"])
        self.assertIn("No draft Melos TEI files were found", markdown)
        self.assertEqual(
            [
                "data/complete/tei_lib_lv4/1567_header_updated.xml",
                "data/complete/tei_lib_lv4/1567_tei.xml",
            ],
            [comparison["relpath"] for comparison in report["melos_comparisons"]],
        )

    def test_melos_report_resolves_abc_from_counterpart_directory_when_abc_omitted(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc_dir = root / "generated" / "artifacts"
            abc = abc_dir / "works" / "melos" / "tei" / "tei.xml"
            abc.parent.mkdir(parents=True)
            abc.write_text(TEI_WITH_WORK_ID_1567, encoding="utf-8")
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_tei.xml",
                "走れメロス",
                "<p>メロス</p>",
            )

            report = probe.build_report(
                None,
                root,
                source_rev="probe-rev",
                abc_tei_dirs=[abc_dir],
            )

        self.assertEqual(abc.as_posix(), report["abc_path"])
        self.assertEqual(1, report["melos_file_count"])
        self.assertEqual(1, len(report["melos_comparisons"]))

    def test_report_wrapper_does_not_default_to_paper_directory(self):
        context = reports.ReportContext(
            compare_script=pathlib.Path("tei_eaj_compare.py"),
            tei_eaj_root=pathlib.Path("tei-eaj"),
            source_rev="rev",
            abc_melos=None,
            abc_tei=[],
            abc_tei_dir=[],
        )

        self.assertEqual([], reports.abc_all_work_inputs(context))

    def test_alignment_probe_command_consumes_workset_without_paper_default(self):
        context = reports.ReportContext(
            compare_script=pathlib.Path("tei_eaj_compare.py"),
            tei_eaj_root=pathlib.Path("tei-eaj"),
            source_rev="rev",
            abc_melos=None,
            abc_tei=[],
            abc_tei_dir=[pathlib.Path("target/soranoha/full/artifacts").as_posix()],
            alignment_probe_bin="probe-bin",
        )

        command = reports.alignment_probe_command(
            context,
            pathlib.Path("workset.json"),
            pathlib.Path("alignment-probe.json"),
            pathlib.Path("alignment-probe.md"),
            max_rows=4,
        )

        command_text = " ".join(str(part) for part in command)
        self.assertEqual("probe-bin", command[0])
        self.assertIn("tei-eaj-alignment-probe", command)
        self.assertIn("--workset", command)
        self.assertIn("--max-rows", command)
        self.assertNotIn("paper", command_text)

    def test_default_alignment_probe_outputs_use_aozora_report_names(self):
        report_dir = pathlib.Path("out/reports/tei-eaj-aozora")

        summary_json, report_md = reports.default_alignment_probe_outputs(report_dir)

        self.assertEqual(report_dir / "tei-eaj-aozora-alignment-probe.json", summary_json)
        self.assertEqual(report_dir / "tei-eaj-aozora-alignment-probe.md", report_md)

    def test_attaches_alignment_probes_to_workset_export(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            workset_path = root / "workset.json"
            probe_path = root / "alignment-probe.json"
            workset_path.write_text(
                json.dumps(
                    {
                        "schema_version": "tei-eaj-aozora-workset-export-v1",
                        "files": [
                            {
                                "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
                                "comparison_status": "compared",
                            }
                        ],
                    }
                ),
                encoding="utf-8",
            )
            probe_path.write_text(
                json.dumps(
                    {
                        "schema_version": "tei-eaj-alignment-probe-report-v1",
                        "rows": [
                            {
                                "tei_eaj_file": "data/complete/tei_lib_lv4/1567_tei.xml",
                                "alignment_probe": {
                                    "schema_version": "alignment-probe-v1",
                                    "diagnosis_counts": {"tail_addition": 1},
                                },
                            }
                        ],
                    }
                ),
                encoding="utf-8",
            )

            reports.attach_alignment_probes_to_workset(workset_path, probe_path)
            updated = json.loads(workset_path.read_text(encoding="utf-8"))

        self.assertEqual(
            {"tail_addition": 1},
            updated["files"][0]["alignment_probe"]["diagnosis_counts"],
        )

    def test_body_base_text_ignores_ruby_readings_and_parentheses(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            path = root / "sample.xml"
            path.write_text(
                textwrap.dedent(
                    """\
                    <?xml version="1.0" encoding="UTF-8"?>
                    <TEI xmlns="http://www.tei-c.org/ns/1.0" xmlns:eaj="http://www.example.org/ns/ejaTEI">
                      <text><body><p>お<eaj:ruby><eaj:rb>世嗣</eaj:rb><eaj:rp>（</eaj:rp><eaj:rt>よつぎ</eaj:rt><eaj:rp>）</eaj:rp></eaj:ruby>を</p></body></text>
                    </TEI>
                    """
                ),
                encoding="utf-8",
            )

            features = probe.analyze_file(path)

        self.assertEqual("お世嗣を", features["body_base_text_no_ws"])
        self.assertEqual(1, features["counts"]["ruby"])
        self.assertEqual(1, features["counts"]["rb"])
        self.assertEqual(1, features["counts"]["rt"])
        self.assertEqual(2, features["counts"]["rp"])

    def test_body_base_text_ignores_tei_eaj_span_ruby_and_notes(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            path = root / "sample.xml"
            path.write_text(
                textwrap.dedent(
                    """\
                    <?xml version="1.0" encoding="UTF-8"?>
                    <TEI xmlns="http://www.tei-c.org/ns/1.0">
                      <text><body><p>高瀬舟<span type="rp">（</span><span type="rt">たかせぶね</span><span type="rp">）</span><span rend="notes">［＃校訂注］</span>は小舟である<note>source note</note></p></body></text>
                    </TEI>
                    """
                ),
                encoding="utf-8",
            )

            features = probe.analyze_file(path)

        self.assertEqual(
            "高瀬舟（たかせぶね）［＃校訂注］は小舟であるsourcenote",
            features["body_text_no_ws"],
        )
        self.assertEqual("高瀬舟は小舟である", features["body_base_text_no_ws"])
        self.assertEqual(1, features["counts"]["rt"])
        self.assertEqual(2, features["counts"]["rp"])

    def test_extracts_aozora_work_ids_from_tei_eaj_file_names(self):
        self.assertEqual("1567", probe.tei_eaj_work_id("data/complete/tei_lib_lv4/1567_tei.xml"))
        self.assertEqual(
            "1567", probe.tei_eaj_work_id("data/complete/tei_lib_lv4/1567_header_updated.xml")
        )
        self.assertEqual("104", probe.tei_eaj_work_id("data/complete/tei_lib_lv4/104_15099.xml"))
        self.assertEqual("4244", probe.tei_eaj_work_id("data/draft/tei_lib_lv4/4244-3_tei.xml"))
        self.assertIsNone(probe.tei_eaj_work_id("data/etc/Curriculum vitae.xml"))

    def test_corrects_file_id_named_tei_eaj_files_with_abc_index_titles(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc_dir = root / "publication" / "tei-by-work-id"
            abc_dir.mkdir(parents=True)
            (root / "publication" / "inspection-index.tsv").write_text(
                "\n".join(
                    [
                        "work_id\tsource_slug\ttitle\treading\tstatus\ttei_path",
                        "000104\tslug\t長崎小品\tながさきしょうひん\tpassed\tignored",
                        "002575\tslug\t鈴木三重吉宛書簡―明治三十九年\tすずき\tpassed\tignored",
                    ]
                )
                + "\n",
                encoding="utf-8",
            )
            self.write_xml(abc_dir, "000104.xml", "長崎小品", "<p>薄暗き硝子戸棚の中。</p>")
            self.write_xml(
                abc_dir,
                "002575.xml",
                "鈴木三重吉宛書簡―明治三十九年",
                "<p>明治三十九年一月一日午前零時。</p>",
            )
            self.write_xml(
                root,
                "data/complete/tei_lib_lv3/15099_tei.xml",
                "長崎小品",
                "<p>薄暗き<span type='rp'>（</span><span type='rt'>うすぐらき</span><span type='rp'>）</span>硝子戸棚の中。</p>",
            )
            self.write_xml(
                root,
                "data/draft/tei_lib_lv4/2571_tei.xml",
                "鈴木三重吉宛書簡―明治三十九年",
                "<p>明治三十九年一月一日午前零時。</p>",
            )

            report = probe.build_all_work_report([], [abc_dir], root, source_rev="probe-rev")
            export = probe.workset_export(report)

        rows = {row["tei_eaj_file"]: row for row in export["files"]}
        self.assertEqual("104", rows["data/complete/tei_lib_lv3/15099_tei.xml"]["work_id"])
        self.assertEqual("2575", rows["data/draft/tei_lib_lv4/2571_tei.xml"]["work_id"])
        self.assertEqual(
            "title_unique_index_correction",
            rows["data/complete/tei_lib_lv3/15099_tei.xml"]["work_id_method"],
        )
        self.assertEqual(2, export["summary"]["compared_file_count"])
        self.assertEqual(2, export["summary"]["base_text_equal_count"])

    def test_all_work_report_compares_each_matching_tei_eaj_variant(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc = self.write_xml(root, "abc/001567.xml", "走れメロス", "<p>メロス邪智暴虐</p>")
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_tei.xml",
                "走れメロス",
                "<p>メロス邪智暴虐</p>",
            )
            self.write_xml(
                root,
                "data/draft/tei_lib_lv4/1567_header_updated.xml",
                "走れメロス",
                "<p>メロス別本文</p>",
            )
            self.write_xml(
                root, "data/complete/tei_lib_lv3/86_tei.xml", "二人小町", "<p>二人小町</p>"
            )
            self.write_xml(root, "data/draft/tei_lib_lv2/01.xml", "源氏物語 第1冊", "<p>桐壺</p>")

            report = probe.build_all_work_report([f"1567={abc}"], [], root, source_rev="probe-rev")
            markdown = probe.render_all_work_markdown(report)

        self.assertEqual(4, report["tei_eaj_file_count"])
        self.assertEqual(1, report["abc_counterpart_count"])
        self.assertEqual(2, report["compared_file_count"])
        self.assertEqual(1, report["missing_counterpart_count"])
        self.assertEqual(1, report["no_work_id_count"])
        self.assertEqual(2, report["uncompared_file_count"])
        self.assertEqual(1, report["base_text_equal_count"])
        self.assertEqual(1, report["base_text_mismatch_count"])
        self.assertCountEqual(
            [
                ("1567", "data/complete/tei_lib_lv4/1567_tei.xml", "equal"),
                ("1567", "data/draft/tei_lib_lv4/1567_header_updated.xml", "mismatch"),
                ("86", "data/complete/tei_lib_lv3/86_tei.xml", None),
                (None, "data/draft/tei_lib_lv2/01.xml", None),
            ],
            [
                (row["work_id"], row["relpath"], row["base_text_relation"])
                for row in report["all_work_rows"]
            ],
        )
        self.assertIn("Compared TEI-EAJ files: 2", markdown)
        self.assertIn("Missing ABC counterparts: 1", markdown)
        self.assertIn("TEI-EAJ files without candidate work IDs: 1", markdown)

    def test_discovers_abc_tei_counterparts_from_directories(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc = self.write_xml(root, "abc/melos.xml", "走れメロス", "<p>メロス</p>")
            abc.write_text(
                textwrap.dedent(
                    """\
                    <?xml version="1.0" encoding="UTF-8"?>
                    <TEI xmlns="http://www.tei-c.org/ns/1.0">
                      <teiHeader>
                        <fileDesc>
                          <titleStmt><title>走れメロス</title></titleStmt>
                          <publicationStmt><idno type="aozora-work-id">001567</idno></publicationStmt>
                          <sourceDesc><p>source</p></sourceDesc>
                        </fileDesc>
                      </teiHeader>
                      <text><body><p>メロス</p></body></text>
                    </TEI>
                    """
                ),
                encoding="utf-8",
            )
            self.write_xml(root, "abc/no-work-id.xml", "No ID", "<p>ignored</p>")
            self.write_xml(
                root, "data/complete/tei_lib_lv4/1567_tei.xml", "走れメロス", "<p>メロス</p>"
            )

            report = probe.build_all_work_report([], [root / "abc"], root)

        self.assertEqual(1, report["abc_counterpart_count"])
        self.assertEqual("1567", report["all_work_rows"][0]["work_id"])
        self.assertTrue(report["all_work_rows"][0]["base_text_equal"])

    def test_classifies_base_text_subset_relations(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc = self.write_xml(
                root, "abc/001567.xml", "走れメロス", "<p>メロス邪智暴虐（古伝説）</p>"
            )
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_tei.xml",
                "走れメロス",
                "<p>メロス邪智暴虐</p>",
            )

            report = probe.build_all_work_report([f"1567={abc}"], [], root)
            row = report["all_work_rows"][0]

        self.assertFalse(row["base_text_equal"])
        self.assertEqual("tei_eaj_subset_of_abc", row["base_text_relation"])
        self.assertEqual({"tei_eaj_subset_of_abc": 1}, report["base_text_relation_counts"])

    def test_markdown_summarizes_counterparts_instead_of_listing_thousands(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc_dir = root / "publication" / "tei-by-work-id"
            abc_dir.mkdir(parents=True)
            index_rows = ["work_id\tsource_slug\ttitle\treading\tstatus\ttei_path"]
            for work_id in range(1, 151):
                padded = f"{work_id:06d}"
                title = f"作品{work_id}"
                index_rows.append(f"{padded}\tslug\t{title}\treading\tpassed\tignored")
                self.write_xml(abc_dir, f"{padded}.xml", title, f"<p>{title}</p>")
            (root / "publication" / "inspection-index.tsv").write_text(
                "\n".join(index_rows) + "\n",
                encoding="utf-8",
            )
            self.write_xml(root, "data/complete/tei_lib_lv3/1_tei.xml", "作品1", "<p>作品1</p>")

            report = probe.build_all_work_report([], [abc_dir], root)
            markdown = probe.render_all_work_markdown(report)

        self.assertIn("ABC counterpart works discovered: 150", markdown)
        self.assertIn("ABC counterpart table omitted", markdown)
        self.assertNotIn("000150.xml", markdown)

    def test_renders_machine_readable_workset_export(self):
        with tempfile.TemporaryDirectory() as td:
            root = pathlib.Path(td)
            abc = self.write_xml(root, "abc/001567.xml", "走れメロス", "<p>メロス邪智暴虐</p>")
            self.write_xml(
                root,
                "data/complete/tei_lib_lv4/1567_tei.xml",
                "走れメロス",
                "<p>メロス邪智暴虐</p>",
            )
            self.write_xml(
                root, "data/complete/tei_lib_lv3/86_tei.xml", "二人小町", "<p>二人小町</p>"
            )
            self.write_xml(root, "data/draft/tei_lib_lv2/01.xml", "源氏物語 第1冊", "<p>桐壺</p>")

            report = probe.build_all_work_report([f"1567={abc}"], [], root, source_rev="probe-rev")
            export = probe.workset_export(report)
            rendered = json.loads(probe.render_workset_json(report))

        self.assertEqual("tei-eaj-aozora-workset-export-v1", export["schema_version"])
        self.assertEqual(export, rendered)
        self.assertEqual(
            {
                "tei_eaj_file_count": 3,
                "tei_eaj_work_id_count": 2,
                "abc_counterpart_count": 1,
                "compared_file_count": 1,
                "missing_counterpart_count": 1,
                "no_work_id_count": 1,
                "uncompared_file_count": 2,
                "base_text_equal_count": 1,
                "base_text_mismatch_count": 0,
                "base_text_relation_counts": {"equal": 1},
            },
            export["summary"],
        )
        self.assertEqual(["86", "1567"], export["candidate_work_ids"])
        self.assertEqual(["86"], export["missing_abc_counterpart_work_ids"])
        self.assertEqual(["data/draft/tei_lib_lv2/01.xml"], export["no_work_id_files"])
        self.assertEqual(
            ["missing_abc_counterpart", "compared", "no_tei_eaj_work_id"],
            [row["comparison_status"] for row in export["files"]],
        )
        self.assertEqual(
            ["missing_abc_counterpart", "equal", None],
            [row["base_text_relation"] for row in export["files"]],
        )


if __name__ == "__main__":
    unittest.main()
