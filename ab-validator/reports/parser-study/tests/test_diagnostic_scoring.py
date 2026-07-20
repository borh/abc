import sys
from pathlib import Path

_CONFORMANCE = Path(__file__).resolve().parents[2] / "parser-conformance"
sys.path.insert(0, str(_CONFORMANCE))

from diagnostics_scoring import (  # noqa: E402
    ActualDiagnostic,
    DiagnosticCaseScore,
    ExpectedDiagnostic,
    aggregate_cases,
    score_case,
)


def test_score_case_counts_exact_relevant_and_extra_diagnostic():
    expected = ExpectedDiagnostic("error", 9, 18)
    actual = [
        ActualDiagnostic("unclosed-bracket", "error", 9, 18),
        ActualDiagnostic("unrelated", "warning", 0, 3),
    ]
    assert score_case(expected, actual) == DiagnosticCaseScore(
        presence=1,
        stable_code=1,
        severity=1,
        relevant_span=1,
        false_positive=1,
        false_negative=0,
    )


def test_score_case_discloses_false_negative_without_imputing_fields():
    expected = ExpectedDiagnostic("warning_or_error", 18, 21)
    assert score_case(expected, []) == DiagnosticCaseScore(
        presence=0,
        stable_code=0,
        severity=0,
        relevant_span=0,
        false_positive=0,
        false_negative=1,
    )


def test_aggregate_cases_adds_each_field_without_changing_order_semantics():
    exact = score_case(
        ExpectedDiagnostic("error", 9, 18),
        [ActualDiagnostic("code", "error", 9, 18)],
    )
    missing = score_case(ExpectedDiagnostic("error", 4, 8), [])
    totals = aggregate_cases([exact, missing])
    assert totals.presence == 1
    assert totals.false_negative == 1
