# aozora2html parse_incomplete Classification Report

Date: 2026-07-03
Source AAT dir: `aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`

## Summary

| Class | Reports | Example message |
|---|---:|---|
| ruby_structural | 57 | aozora2html parser aborted: エラー(158行目):字下げを閉じようとしましたが、字下げ中ではありません. 処理を停止します |
| invalid_xhtml | 30 | invalid XHTML: expected '"' not '<' at 846:1028 |
| ruby_internal_error | 17 | aozora2html parser aborted: warning: Git tree '/home/bor/Projects/ab-validator' is dirty /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/style_stack.rb:30:in 'Aozora2Html::StyleStack#last_command': undefine... |
| other | 1 | aozora2html parser aborted: warning: Git tree '/home/bor/Projects/ab-validator' is dirty 警告(25行目):1バイトの「(」が使われています 警告(25行目):1バイトの「)」が使われています 警告(40行目):1バイトの「(」が使われています 警告(40行目):1バイトの「)」が使われています 警告(57行目):1バイトの「(」が使われています 警告(57行目):1バイトの「)」が... |
| **Total** | **105** | |

## Remediation scope (deferred)

- `ruby_structural`: upstream aozora2html parser rejects edge-case markup; investigate wrapper pre-normalization (e.g., CRLF) as a follow-up, not here.
- `invalid_xhtml`: mapper strictness; a `roxmltree` -> `html5ever` migration is a separate plan, not this one.
- `ruby_internal_error`: upstream aozora2html 3.0.1 bugs; file upstream reports only.
- `other`: inspect the example message before routing.
