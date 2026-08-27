# Japanese statutory term citations — Q1–Q5 and the old-law transition

Status: CITATIONS FOR OWNER ADOPTION, not adopted law. This document
answers the statutory portions of Q1–Q5 of
`aozora-rights-source-contract.md` plus the old-law/current-law
transition question named in the 2026-08-27 owner decision record
(soranoha ledger). It quotes the law and the Agency for Cultural
Affairs' (文化庁) published guidance verbatim, with retrieval dates. It
performs no assessment of any work, says nothing about what Aozora
Bunko did (Q6–Q8 remain open), and binds nothing until the owner
adopts it.

All retrievals 2026-08-27 (UTC). Retrieval channels: the e-Gov law
API (laws.e-gov.go.jp, v1 article endpoint and v2
`elm=SupplProvision`) for consolidated statute text, and 文化庁
publications read directly (PDF pages rendered, not paraphrased from
search results).

## Sources

- S-A 著作権法 (昭和45年法律第48号), consolidated text, e-Gov law id
  `345AC0000000048`. Articles retrieved individually:
  https://laws.e-gov.go.jp/api/1/articles;lawId=345AC0000000048;article=第五十一条
  (same pattern for 28, 52, 53, 57, 58); supplementary provisions via
  https://laws.e-gov.go.jp/api/2/law_data/345AC0000000048?elm=SupplProvision
- S-B 連合国及び連合国民の著作権の特例に関する法律 (昭和27年法律第302号),
  e-Gov law id `327AC0000000302`, art. 4 via the v1 article endpoint.
- S-C 文化庁著作権課「著作物等の保護期間の延長に関するQ&A」(平成30年12月),
  https://www.bunka.go.jp/seisaku/chosakuken/hokaisei/kantaiheiyo_chosakuken/1411890.html
  and the identical PDF
  https://www.bunka.go.jp/seisaku/chosakuken/hokaisei/kantaiheiyo_chosakuken/pdf/r1410925_01.pdf
  (問1–問13; the PDF was read page by page).
- S-D 文化庁掲載のTPP整備法条文（抄）
  「環太平洋パートナーシップ協定の締結及び環太平洋パートナーシップに関する
  包括的及び先進的な協定の締結に伴う関係法律の整備に関する法律（抄）」,
  https://www.bunka.go.jp/seisaku/chosakuken/hokaisei/kantaiheiyo_hokaisei/pdf/r1408266_02.pdf
  (the amendment act 平成28年法律第108号 as amended by 平成30年法律第70号;
  第8条 and 附則, read page by page).
- Note: e-Gov does not carry the TPP整備法 as a standalone law
  (`/api/2/laws?law_title=環太平洋パートナーシップ` returns zero), and the
  consolidated 著作権法 XML truncates before amendment-act 附則 blocks in
  API retrieval — hence S-D for the 整備法 text.

## Q1 — the applicable term and its transition rule

Principle (S-A, 第51条):

> 第五十一条（保護期間の原則）
> １ 著作権の存続期間は、著作物の創作の時に始まる。
> ２ 著作権は、この節に別段の定めがある場合を除き、著作者の死後（共同著作物にあつては、最終に死亡した著作者の死後。次条第一項において同じ。）七十年を経過するまでの間、存続する。

The 70 was 50 until the TPP整備法 amendment (S-D, 第8条):

> 第五十一条第二項中「五十年」を「七十年」に改める。
> 第五十二条第一項中「公表後五十年」を「公表後七十年」に改め、同項ただし書中「五十年」を「七十年」に改める。
> 第五十三条第一項中「五十年」を「七十年」に改める。
> 第五十七条中「五十年、著作物の公表後五十年若しくは創作後五十年」を「七十年」に改める。

Effective date: the amendment took effect on the day the CPTPP entered
into force for Japan (S-D, 附則第1条: この法律は、環太平洋パートナーシップに
関する包括的及び先進的な協定が日本国について効力を生ずる日…から施行する),
which was 平成30 (2018) 年12月30日 (S-C 問3).

Non-revival — the transition rule itself (S-D, 附則第7条第1項,
著作権法の一部改正に伴う経過措置):

> 第七条 第八条の規定による改正後の著作権法（次項及び第三項において「新著作権法」という。）第五十一条第二項、第五十二条第一項、第五十三条第一項、第五十七条並びに第百一条第二項第一号及び第二号の規定は、施行日の前日において現に第八条の規定による改正前の著作権法（以下この項において「旧著作権法」という。）による著作権又は著作隣接権が存する著作物、実演及びレコードについて適用し、同日において旧著作権法による著作権又は著作隣接権が消滅している著作物、実演及びレコードについては、なお従前の例による。

文化庁's statement of the principle and the frontier (S-C 問4, 問3):

> 一度保護が切れた著作物等については，その保護を後になって遡って復活させるという措置は採らない
> 改正法の施行日である平成30（2018）年12月30日の前日において著作権等が消滅していない著作物等についてのみ保護期間が延長される（TPP整備法附則第7条）
> 原則として昭和43年（1968年）以降に亡くなった方の著作物の保護期間が延長されることとなります。

問3's worked example: 藤田嗣治 died 昭和43 (1968); the 50-year term
would have ended 2018-12-31, the amendment arrived 2018-12-30 while
the right subsisted, so the term became 70 years (to 2038-12-31).

## Q2 — when within a year a term expires

S-A, 第57条:

> 第五十七条（保護期間の計算方法）
> 第五十一条第二項、第五十二条第一項、第五十三条第一項又は第五十四条第一項の場合において、著作者の死後七十年又は著作物の公表後七十年若しくは創作後七十年の期間の終期を計算するときは、著作者が死亡した日又は著作物が公表され若しくは創作された日のそれぞれ属する年の翌年から起算する。

S-C 問2: すべての期間は、死亡、公表、創作した年の「翌年の1月1日」から
起算します（第57条）— with the example that a 1989 death is protected
to 2059-12-31 under the 70-year term.

## Q3 — anonymous, pseudonymous, and corporate works

S-A, 第52条 (paragraph 1 verbatim; paragraph 2 as retrieved):

> 第五十二条（無名又は変名の著作物の保護期間）
> １ 無名又は変名の著作物の著作権は、その著作物の公表後七十年を経過するまでの間、存続する。（ただし書: 公表後七十年の満了前に著作者の死後七十年の経過が明らかであれば、その時点で消滅。）
> ２ 前項の規定は、次の各号のいずれかに該当するときは、適用しない: 一 変名がその者のものとして周知であるとき; 二 期間内に第七十五条第一項の実名の登録があつたとき; 三 期間内に著作者が実名又は周知の変名を著作者名として表示して公表したとき。

S-A, 第53条:

> 第五十三条（団体名義の著作物の保護期間）
> １ 法人その他の団体が著作の名義を有する著作物の著作権は、その著作物の公表後七十年（その著作物がその創作後七十年以内に公表されなかつたときは、その創作後七十年）を経過するまでの間、存続する。
> ２ 前項の規定は、法人その他の団体が著作の名義を有する著作物の著作者である個人が同項の期間内にその実名又は周知の変名を著作者名として表示してその著作物を公表したときは、適用しない。
> ３ 第十五条第二項の規定により法人その他の団体が著作者である著作物の著作権の存続期間に関しては、第一項の著作物に該当する著作物以外の著作物についても、当該団体が著作の名義を有するものとみなして同項の規定を適用する。

## Q4 — foreign works: comparison of terms and wartime additions

S-A, 第58条:

> 第五十八条（保護期間の特例）
> 文学的及び美術的著作物の保護に関するベルヌ条約により創設された国際同盟の加盟国、著作権に関する世界知的所有権機関条約の締約国又は世界貿易機関の加盟国である外国をそれぞれ文学的及び美術的著作物の保護に関するベルヌ条約、著作権に関する世界知的所有権機関条約又は世界貿易機関を設立するマラケシュ協定の規定に基づいて本国とする著作物（第六条第一号に該当するものを除く。）で、その本国において定められる著作権の存続期間が第五十一条から第五十四条までに定める著作権の存続期間より短いものについては、その本国において定められる著作権の存続期間による。

S-C 問5 adds the reciprocity reading: foreign works are extended to
70 in principle; works from a country with a shorter term are
protected only for that country's term (第58条). The comparison can
only shorten, never lengthen, the Japanese term.

Wartime additions (S-B, 第4条):

> 第四条（著作権の存続期間に関する特例）
> １ 昭和十六年十二月七日に連合国及び連合国民が有していた著作権は、著作権法に規定する当該著作権に相当する権利の存続期間に、昭和十六年十二月八日から日本国と当該連合国との間に日本国との平和条約が効力を生ずる日の前日までの期間（当該期間において連合国及び連合国民以外の者が当該著作権を有していた期間があるときは、その期間を除く。）に相当する期間を加算した期間継続する。
> ２ 昭和十六年十二月八日から日本国と当該連合国との間に日本国との平和条約が効力を生ずる日の前日までの期間において、連合国又は連合国民が取得した著作権（前条の規定により有効に取得されたものとして保護される著作権を含む。）は、著作権法に規定する当該著作権に相当する権利の存続期間に、当該連合国又は連合国民がその著作権を取得した日から日本国と当該連合国との間に日本国との平和条約が効力を生ずる日の前日までの期間（当該期間において連合国及び連合国民以外の者が当該著作権を有していた期間があるときは、その期間を除く。）に相当する期間を加算した期間継続する。

S-C 問7 gives the operational figure: e.g. 3,794 days for the United
States and Australia (1941-12-08 to the day before the peace treaty's
entry into force). 問8–問9: works inside a wartime-addition period on
2018-12-29 were subsisting rights, so they too were extended — legally
70 years plus the addition.

## Q5 — composition of the original author's and translator's terms

S-A, 第28条:

> 第二十八条（二次的著作物の利用に関する原著作者の権利）
> 二次的著作物の原著作物の著作者は、当該二次的著作物の利用に関し、この款に規定する権利で当該二次的著作物の著作者が有するものと同一の種類の権利を専有する。

A translation is a derivative work; its use implicates two
independently-running rights: the translator's own copyright (第51条
on the translator's death) and the original author's rights over the
derivative's use (第28条, running on the original author's term).
Using a translation freely therefore requires BOTH terms to have
expired — the statutory citation behind the composition rule S2
records as Aozora's operating practice.

## The old-law transition (旧著作権法, 明治32年法律第39号)

Current-law supplementary provisions (S-A, 附則, 昭和45年):

> 附則第二条（適用範囲についての経過措置）
> 改正後の著作権法（以下「新法」という。）中著作権に関する規定は、この法律の施行の際現に改正前の著作権法（以下「旧法」という。）による著作権の全部が消滅している著作物については、適用しない。
>
> 附則第七条（著作物の保護期間についての経過措置）
> この法律の施行前に公表された著作物の著作権の存続期間については、当該著作物の旧法による著作権の存続期間が新法第二章第四節の規定による期間より長いときは、なお従前の例による。

文化庁's transition table (S-C 問10; the 現行法 columns as amended at
each date, 施行 1971-01-01 / 1997-03-25 / 2004-01-01 / 2018-12-30):

| 種類 | 公表名義 | 旧法 | 1971 現行法 | …1996/2003 改正 | 2018 改正後 |
|---|---|---|---|---|---|
| 映画・写真以外の著作物 | 実名（生前公表） | 死後38年間 | 死後50年間 | 死後50年間 | 死後70年間 |
| 同 | 実名（死後公表） | 死後38年間 ※ | 死後50年間 | 死後50年間 | 死後70年間 |
| 同 | 無名・変名 | 公表後38年間 | 公表後50年間 | 公表後50年間 | 公表後70年間 |
| 同 | 団体名義 | 公表後33年間 | 公表後50年間 | 公表後50年間 | 公表後70年間 |
| 写真の著作物 | — | 発行又は創作後13年間 | 公表後50年間 | 死後50年間 (1996改正〜) | 死後70年間 |

(※ the PDF table's second row is rendered near-identically to the
first at this resolution; consult S-C 問10 directly before relying on
the 死後公表 cell. Film rows omitted here — no films in scope.)

The governing rule as 文化庁 states it (S-C 問10):

> 法改正により保護期間の長さが変更される場合は，それぞれの改正法の施行の際，現に著作権が消滅していないもののみが，変更された保護期間の適用を受けます。なお，旧法の時代の著作物の保護期間については，変更後の保護期間と比べて，旧法に定められた保護期間の方が長い場合は，その長い保護期間が適用されます。

Worked example (S-C 問10): an author who died 昭和45 (1970), work
published under real name in their lifetime — old law: protected to
2008 (死後38年); still protected at 1971-01-01, so 死後50年 → 2020;
still protected at 2018-12-30, so 死後70年 → 2040.

## Derivations (not citations — consequences the owner adopts or rejects with the above)

D-1 For 映画・写真以外 works of natural persons where a death-based
term applies (実名 or 周知の変名, per 第52条第2項), the old-law term
(死後38年) is strictly shorter than every current-law term, so 附則7条
never selects it; and any work already expired under the old law at
1971-01-01 (附則2条) belongs to an author who died before 1933, which
the death-year test below already covers. For this class the
old-law/current-law transition adds NO case the single death-year test
misses.

D-2 The death-year frontier: an author (and for the admission rule,
every contributor) who died on or before 1967-12-31 had the 死後50年
term end by 2017-12-31 (第57条: counted from the following year), i.e.
before 施行日の前日 2018-12-29, so TPP整備法附則第7条 leaves the work
expired — public domain today. An author who died in 1968 or later was
in term on 2018-12-29 and carries 死後70年. This is the statutory
basis of the census predicate's "parseable CE year ≤ 1967" and of
F3's observed 1967 frontier.

D-3 The frontier in D-2 is SUFFICIENT only under all of: (a) a
death-based term applies — real name or 周知の変名 or 実名登録
(第52条第2項); a non-周知 pseudonym or 団体名義 switches to a
publication-based term (公表後70年 now; for those, publication on or
before 1967-12-31 gives the analogous expired-before-extension
argument); (b) no 戦時加算 — the addition attaches to rights HELD OR
ACQUIRED BY Allied nationals in the treaty window (S-B 第4条), a fact
about right-holders that catalog fields do not carry; the census
predicate's no-翻訳者/no-原題 filter removes the overwhelmingly
typical translated-work case but is lexical, not probative; (c) for
translations and other derivatives, BOTH chains (第28条) satisfy the
frontier — the predicate's "every contributor" quantifier is the
matching test; (d) 第58条 can only shorten a foreign work's Japanese
term, so it never defeats an expiry conclusion, but it also never
rescues a domestic conclusion misapplied to a foreign work with
wartime additions.

D-4 What none of this establishes: who assessed Aozora's flags,
under which rule, or when (Q6); jurisdiction scoping of any external
assertion (Q7–Q8); and the effective date of any third-party
assertion (the contract's *Three distinct times*). Those remain open
exactly as the source contract states.
