# aozora2html parse_incomplete Classification Report

Date: 2026-07-03
Source AAT dir: `aozora2html-full-20260703T020301Z/aat/aozora2html-adapter`

## Summary

| Class | Reports | Example message |
|---|---:|---|
| ruby_structural | 57 | aozora2html parser aborted: エラー(158行目):字下げを閉じようとしましたが、字下げ中ではありません. 
処理を停止します |
| invalid_xhtml | 30 | invalid XHTML: expected '"' not '<' at 846:1028 |
| ruby_internal_error | 17 | aozora2html parser aborted: warning: Git tree '/home/bor/Projects/ab-validator' is dirty
/db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/style_stack.rb:30:in 'Aozora2Html::StyleStack#last_command': undefined method '[]' for nil (NoMethodError)

      @stack.last[0]
                 ^^^
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:963:in 'Aozora2Html#exec_inline_end_command'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:691:in 'Aozora2Html#dispatch_aozora_command'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:420:in 'Aozora2Html#parse_body'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:344:in 'block in Aozora2Html#parse'
	from <internal:kernel>:168:in 'Kernel#loop'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:335:in 'Aozora2Html#parse'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:193:in 'block in Aozora2Html#process'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:192:in 'Kernel#catch'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:192:in 'Aozora2Html#process'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/bin/aozora2html:69:in 'block in <top (required)>'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/tmpdir.rb:105:in 'Dir.mktmpdir'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/bin/aozora2html:42:in '<top (required)>'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/rubygems.rb:319:in 'Kernel#load'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/rubygems.rb:319:in 'Gem.activate_and_load_bin_path'
	from /db/ab-validator/gems/aozora2html-3.0.1/bin/aozora2html:25:in '<main>'
ERROR: line: 22 |
| other | 1 | aozora2html parser aborted: warning: Git tree '/home/bor/Projects/ab-validator' is dirty
警告(25行目):1バイトの「(」が使われています
警告(25行目):1バイトの「)」が使われています
警告(40行目):1バイトの「(」が使われています
警告(40行目):1バイトの「)」が使われています
警告(57行目):1バイトの「(」が使われています
警告(57行目):1バイトの「)」が使われています
警告(58行目):1バイトの「(」が使われています
警告(58行目):1バイトの「)」が使われています
警告(71行目):1バイトの「(」が使われています
警告(71行目):1バイトの「)」が使われています
警告(74行目):1バイトの「(」が使われています
警告(74行目):1バイトの「)」が使われています
警告(140行目):1バイトの「(」が使われています
警告(140行目):1バイトの「)」が使われています
警告(165行目):1バイトの「(」が使われています
警告(165行目):1バイトの「)」が使われています
警告(169行目):1バイトの「(」が使われています
警告(169行目):1バイトの「)」が使われています
警告(175行目):1バイトの「(」が使われています
警告(175行目):1バイトの「)」が使われています
警告(194行目):1バイトの「(」が使われています
警告(194行目):1バイトの「)」が使われています
警告(314行目):1バイトの「(」が使われています
警告(314行目):1バイトの「)」が使われています
警告(323行目):1バイトの「(」が使われています
警告(323行目):1バイトの「)」が使われています
警告(325行目):1バイトの「(」が使われています
警告(325行目):1バイトの「)」が使われています
警告(325行目):1バイトの「(」が使われています
警告(325行目):1バイトの「)」が使われています
警告(326行目):1バイトの「(」が使われています
警告(326行目):1バイトの「)」が使われています
警告(326行目):1バイトの「(」が使われています
警告(326行目):1バイトの「)」が使われています
警告(327行目):1バイトの「(」が使われています
警告(327行目):1バイトの「(」が使われています
警告(327行目):1バイトの「)」が使われています
警告(327行目):1バイトの「)」が使われています
警告(337行目):1バイトの「(」が使われています
警告(337行目):1バイトの「)」が使われています
警告(337行目):1バイトの「(」が使われています
警告(337行目):1バイトの「)」が使われています
警告(344行目):1バイトの「(」が使われています
警告(344行目):1バイトの「)」が使われています
警告(345行目):1バイトの「(」が使われています
警告(345行目):1バイトの「)」が使われています
警告(345行目):1バイトの「(」が使われています
警告(345行目):1バイトの「)」が使われています
警告(345行目):1バイトの「(」が使われています
警告(345行目):1バイトの「)」が使われています
警告(354行目):1バイトの「(」が使われています
警告(354行目):1バイトの「)」が使われています
警告(355行目):1バイトの「(」が使われています
警告(355行目):1バイトの「)」が使われています
警告(355行目):1バイトの「(」が使われています
警告(355行目):1バイトの「)」が使われています
警告(355行目):1バイトの「(」が使われています
警告(355行目):1バイトの「)」が使われています
警告(370行目):1バイトの「(」が使われています
警告(370行目):1バイトの「)」が使われています
警告(370行目):1バイトの「(」が使われています
警告(370行目):1バイトの「)」が使われています
警告(370行目):1バイトの「(」が使われています
警告(370行目):1バイトの「)」が使われています
警告(371行目):1バイトの「(」が使われています
警告(371行目):1バイトの「)」が使われています
警告(372行目):1バイトの「(」が使われています
警告(372行目):1バイトの「)」が使われています
警告(372行目):1バイトの「(」が使われています
警告(372行目):1バイトの「(」が使われています
警告(372行目):1バイトの「)」が使われています
警告(372行目):1バイトの「)」が使われています
警告(379行目):1バイトの「(」が使われています
警告(379行目):1バイトの「)」が使われています
警告(380行目):1バイトの「(」が使われています
警告(380行目):1バイトの「)」が使われています
警告(380行目):1バイトの「(」が使われています
警告(380行目):1バイトの「)」が使われています
警告(380行目):1バイトの「(」が使われています
警告(380行目):1バイトの「)」が使われています
警告(381行目):1バイトの「(」が使われています
警告(381行目):1バイトの「)」が使われています
警告(381行目):1バイトの「(」が使われています
警告(381行目):1バイトの「)」が使われています
警告(382行目):1バイトの「(」が使われています
警告(382行目):1バイトの「)」が使われています
警告(402行目):1バイトの「(」が使われています
警告(402行目):1バイトの「)」が使われています
警告(403行目):1バイトの「(」が使われています
警告(403行目):1バイトの「)」が使われています
警告(411行目):1バイトの「(」が使われています
警告(411行目):1バイトの「)」が使われています
警告(413行目):1バイトの「!」が使われています
警告(421行目):1バイトの「(」が使われています
警告(421行目):1バイトの「)」が使われています
警告(423行目):1バイトの「(」が使われています
警告(423行目):1バイトの「)」が使われています
警告(433行目):1バイトの「!」が使われています
警告(440行目):1バイトの「!」が使われています
警告(455行目):1バイトの「(」が使われています
警告(455行目):1バイトの「)」が使われています
警告(455行目):1バイトの「(」が使われています
警告(455行目):1バイトの「)」が使われています
警告(455行目):1バイトの「(」が使われています
警告(455行目):1バイトの「)」が使われています
警告(455行目):1バイトの「(」が使われています
警告(455行目):1バイトの「)」が使われています
警告(473行目):1バイトの「(」が使われています
警告(473行目):1バイトの「)」が使われています
警告(473行目):1バイトの「(」が使われています
警告(473行目):1バイトの「)」が使われています
警告(475行目):1バイトの「(」が使われています
警告(475行目):1バイトの「)」が使われています
警告(475行目):1バイトの「(」が使われています
警告(475行目):1バイトの「)」が使われています
警告(475行目):1バイトの「(」が使われています
警告(475行目):1バイトの「)」が使われています
警告(476行目):1バイトの「(」が使われています
警告(476行目):1バイトの「)」が使われています
警告(504行目):1バイトの「(」が使われています
警告(504行目):1バイトの「)」が使われています
警告(504行目):1バイトの「(」が使われています
警告(504行目):1バイトの「)」が使われています
警告(504行目):1バイトの「(」が使われています
警告(504行目):1バイトの「)」が使われています
警告(505行目):1バイトの「(」が使われています
警告(505行目):1バイトの「)」が使われています
警告(506行目):1バイトの「(」が使われています
警告(506行目):1バイトの「)」が使われています
警告(507行目):1バイトの「(」が使われています
警告(507行目):1バイトの「)」が使われています/db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/string_refinements.rb:32:in 'String#encode': "\xEB\x81" from Windows-31J to UTF-8 (Encoding::UndefinedConversionError)
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/string_refinements.rb:32:in 'to_utf8'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/i18n.rb:42:in 'Aozora2Html::I18n.t'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html/utils.rb:151:in 'Aozora2Html::Utils.illegal_char_check'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:441:in 'Aozora2Html#parse_body'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:344:in 'block in Aozora2Html#parse'
	from <internal:kernel>:168:in 'Kernel#loop'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:335:in 'Aozora2Html#parse'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:193:in 'block in Aozora2Html#process'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:192:in 'Kernel#catch'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/lib/aozora2html.rb:192:in 'Aozora2Html#process'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/bin/aozora2html:69:in 'block in <top (required)>'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/tmpdir.rb:105:in 'Dir.mktmpdir'
	from /db/ab-validator/gems/aozora2html-3.0.1/gems/aozora2html-3.0.1/bin/aozora2html:42:in '<top (required)>'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/rubygems.rb:319:in 'Kernel#load'
	from /nix/store/ks57gbw6jia5ssk0rd407ic2r8qvw9nn-ruby-3.4.9/lib/ruby/3.4.0/rubygems.rb:319:in 'Gem.activate_and_load_bin_path'
	from /db/ab-validator/gems/aozora2html-3.0.1/bin/aozora2html:25:in '<main>'

警告(507行目):1バイトの「(」が使われています
警告(507行目):1バイトの「)」が使われています
警告(507行目):1バイトの「(」が使われています
警告(507行目):1バイトの「)」が使われています
警告(507行目):1バイトの「(」が使われています
警告(507行目):1バイトの「)」が使われています
警告(508行目):1バイトの「(」が使われています
警告(508行目):1バイトの「)」が使われています
警告(508行目):1バイトの「(」が使われています
警告(508行目):1バイトの「)」が使われています
警告(516行目):1バイトの「(」が使われています
警告(516行目):1バイトの「)」が使われています
警告(516行目):1バイトの「(」が使われています
警告(516行目):1バイトの「)」が使われています
警告(516行目):1バイトの「(」が使われています
警告(516行目):1バイトの「)」が使われています
警告(520行目):1バイトの「(」が使われています
警告(520行目):1バイトの「)」が使われています
警告(521行目):1バイトの「(」が使われています
警告(521行目):1バイトの「)」が使われています
警告(521行目):1バイトの「(」が使われています
警告(521行目):1バイトの「)」が使われています
警告(526行目):1バイトの「(」が使われています
警告(526行目):1バイトの「)」が使われています
警告(526行目):1バイトの「(」が使われています
警告(526行目):1バイトの「)」が使われています
警告(526行目):1バイトの「(」が使われています
警告(526行目):1バイトの「)」が使われています
警告(526行目):1バイトの「(」が使われています
警告(526行目):1バイトの「)」が使われています
警告(527行目):1バイトの「(」が使われています
警告(527行目):1バイトの「)」が使われています
警告(532行目):1バイトの「(」が使われています
警告(532行目):1バイトの「)」が使われています
警告(533行目):1バイトの「(」が使われています
警告(533行目):1バイトの「)」が使われています
警告(533行目):1バイトの「(」が使われています
警告(533行目):1バイトの「)」が使われています
警告(533行目):1バイトの「(」が使われています
警告(533行目):1バイトの「)」が使われています
警告(535行目):1バイトの「(」が使われています
警告(535行目):1バイトの「)」が使われています
警告(535行目):1バイトの「(」が使われています
警告(535行目):1バイトの「)」が使われています
警告(551行目):1バイトの「(」が使われています
警告(551行目):1バイトの「)」が使われています
警告(551行目):1バイトの「(」が使われています
警告(551行目):1バイトの「)」が使われています
警告(588行目):1バイトの「(」が使われています
警告(588行目):1バイトの「)」が使われています
警告(588行目):1バイトの「(」が使われています
警告(588行目):1バイトの「)」が使われています
警告(589行目):1バイトの「(」が使われています
警告(589行目):1バイトの「)」が使われています
警告(589行目):1バイトの「(」が使われています
警告(589行目):1バイトの「)」が使われています
警告(594行目):1バイトの「(」が使われています
警告(594行目):1バイトの「)」が使われています
警告(594行目):1バイトの「(」が使われています
警告(594行目):1バイトの「)」が使われています
警告(594行目):1バイトの「(」が使われています
警告(594行目):1バイトの「)」が使われています
警告(594行目):1バイトの「(」が使われています
警告(594行目):1バイトの「)」が使われています
警告(595行目):1バイトの「(」が使われています
警告(595行目):1バイトの「)」が使われています
警告(595行目):1バイトの「(」が使われています
警告(595行目):1バイトの「)」が使われています
警告(599行目):1バイトの「(」が使われています
警告(599行目):1バイトの「)」が使われています
警告(599行目):1バイトの「(」が使われています
警告(599行目):1バイトの「)」が使われています
警告(601行目):1バイトの「(」が使われています
警告(601行目):1バイトの「)」が使われています
警告(605行目):1バイトの「(」が使われています
警告(605行目):1バイトの「)」が使われています
警告(605行目):1バイトの「(」が使われています
警告(605行目):1バイトの「)」が使われています
警告(605行目):1バイトの「(」が使われています
警告(605行目):1バイトの「)」が使われています
警告(605行目):1バイトの「(」が使われています
警告(605行目):1バイトの「)」が使われています
警告(606行目):1バイトの「(」が使われています
警告(606行目):1バイトの「)」が使われています
警告(626行目):1バイトの「(」が使われています
警告(626行目):1バイトの「)」が使われています
ERROR: line: 685 |
| **Total** | **105** | |

## Remediation scope (deferred)

- `ruby_structural`: upstream aozora2html parser rejects edge-case markup; investigate wrapper pre-normalization (e.g., CRLF) as a follow-up, not here.
- `invalid_xhtml`: mapper strictness; a `roxmltree` -> `html5ever` migration is a separate plan, not this one.
- `ruby_internal_error`: upstream aozora2html 3.0.1 bugs; file upstream reports only.
- `other`: inspect the example message before routing.
