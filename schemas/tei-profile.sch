<?xml version="1.0" encoding="UTF-8"?>
<sch:schema xmlns:sch="http://purl.oclc.org/dsdl/schematron"
            queryBinding="xslt2">
  <sch:title>ABC TEI v0 Schematron</sch:title>
  <sch:ns prefix="tei" uri="http://www.tei-c.org/ns/1.0"/>

  <sch:pattern id="abc-tei-header-title">
    <sch:rule context="tei:teiHeader">
      <sch:assert role="error"
                  test="tei:fileDesc/tei:titleStmt/tei:title[not(@type = 'sub')]">
        ABC TEI requires a main title in teiHeader/fileDesc/titleStmt.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-tei-header-source-work-id">
    <sch:rule context="tei:teiHeader">
      <sch:assert role="error"
                  test=".//tei:idno[@type = ('aozora-work-id', 'source-work-id')]">
        ABC TEI requires an Aozora work ID or source work identifier.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-ruby-complete">
    <sch:rule context="tei:ruby">
      <sch:assert role="error"
                  test="tei:rb and tei:rt">
        ABC TEI ruby requires both rb and rt components.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-gaiji-reference">
    <sch:rule context="tei:g">
      <sch:assert role="error"
                  test="@ref or @corresp or @ana">
        ABC TEI gaiji requires a declaration reference, source marker reference, or resolution status.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-figure-accessibility">
    <sch:rule context="tei:figure">
      <sch:report role="warning"
                  test="not(tei:figDesc or tei:head or tei:p)">
        ABC TEI figure should preserve a figDesc, head, or caption paragraph when available.
      </sch:report>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-source-span-reference">
    <sch:rule context="*[@source]">
      <sch:assert role="error"
                  test="every $s in tokenize(normalize-space(@source), '\s+') satisfies starts-with($s, '#')">
        ABC TEI source span references must point to local span identifiers.
      </sch:assert>
    </sch:rule>
  </sch:pattern>

  <sch:pattern id="abc-transcription-vs-annotation">
    <sch:rule context="tei:text//tei:w | tei:text//tei:m | tei:text//tei:pc">
      <sch:assert role="warning"
                  test="ancestor::tei:TEI/tei:teiHeader//tei:encodingDesc//tei:tagsDecl//tei:namespace[@name = 'http://www.tei-c.org/ns/1.0']/tei:tagUsage/@gi = local-name()">
        ABC TEI linguistic enrichment in the transcription layer must be declared in the header.
      </sch:assert>
    </sch:rule>
  </sch:pattern>
</sch:schema>
