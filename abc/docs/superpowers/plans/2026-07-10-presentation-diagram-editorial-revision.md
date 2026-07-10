# Presentation Diagram Editorial Revision Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Produce cleaner, larger-type academic presentation SVGs while preserving the existing semantic architecture, fifteen-coordinate evidence, and ownership palette.

**Architecture:** Projection data remains the source of semantic nodes, edges, copy, and backing. The DOT renderer performs only visual deduplication and routing, while SVG normalization enforces graph-body typography, scale, accessibility, and resource closure. Generated artifacts are accepted only after pinned regeneration and original-size raster inspection.

**Tech Stack:** Clojure, Kaocha, Graphviz DOT, `clojure.data.xml`, Nix, librsvg, ImageMagick.

## Global Constraints

- Preserve the exact semantic node and edge sets in Task 1; ports, ordering, invisible constraints, and bundled routes are non-semantic.
- Preserve the six-color palette and current ownership-color assignments.
- Retain all fifteen coordinate records and backing references.
- Render fourteen coordinate bullets plus the `Output format` family heading.
- Use `Versioned identity contract` and `Identity-bearing inputs determine ArtifactID.` exactly.
- Remove the selective-rebuild claim from projection and generated artifacts.
- Enforce graph-body source-size floors of 34 bold and 24 non-bold; preserve chrome at 52/22/16.
- Preserve the 1920×1080 canvas, 1728×745 graph region, outer scale ≥1, and descendant scale ≥1.
- Preserve accurate namespaced `<title>`/`<desc>`, embedded Noto Sans CJK JP, and resource closure.
- Require stable DOT/SVG bytes after pinned regeneration.

---

### Task 1: Lock Semantic Topology and Revise Projection Copy

**Files:**
- Modify: `abc/docs/architecture-presentation.edn`
- Modify: `abc/src/abc/tools/diagram/presentation_figures.clj`
- Modify: `abc/test/abc/tools/diagram/presentation_figures_test.clj`

**Interfaces:**
- Consumes: `presentation-model/validated-model` and existing presentation metadata.
- Produces: revised graphs with unchanged semantic topology, exact editorial copy, and theme sizes 34/24.

- [ ] **Step 1: Add failing topology and copy tests**

Add to `presentation_figures_test.clj`:

```clojure
(def expected-topology
  {:reproducibility
   {:nodes #{:sources :source :parsing :publication :analysis :output
             :artifact-id :manifest :views}
    :edges #{[:sources :source] [:sources :parsing]
             [:sources :publication] [:sources :analysis] [:sources :output]
             [:source :artifact-id] [:parsing :artifact-id]
             [:publication :artifact-id] [:analysis :artifact-id]
             [:output :artifact-id] [:artifact-id :manifest]
             [:manifest :views]}}
   :publication
   {:nodes #{:source :parser-process :parser-ir :manifest :outputs :aat-detail}
    :edges #{[:source :parser-process] [:parser-process :parser-ir]
             [:parser-ir :manifest] [:manifest :outputs]
             [:parser-process :aat-detail] [:aat-detail :parser-ir]}}})

(defn- semantic-topology [graph]
  {:nodes (set (map :id (:nodes graph)))
   :edges (set (map (juxt :from :to) (:edges graph)))})

(deftest editorial-revision-preserves-semantic-topology
  (doseq [[id expected] expected-topology]
    (is (= expected (semantic-topology (graph id))) (name id))))

(deftest reproducibility-editorial-contract
  (let [g (graph :reproducibility)
        group (some #(when (= :identity-contract (:id %)) %) (:groups g))
        nodes (into {} (map (juxt :id identity) (:nodes g)))]
    (is (= "Versioned identity contract" (:label group)))
    (is (= "Identity-bearing inputs determine ArtifactID." (:footer g)))
    (is (not (str/includes? (:footer g) "rebuilds only dependent layers")))
    (is (= [{:id "output_format_spec_hash" :label "Output format"}]
           (:coordinates (:output nodes))))
    (is (= 34 (get-in g [:theme :primary-size])))
    (is (= 24 (get-in g [:theme :secondary-size])))
    (is (= #{"#000000" "#F5F7FA" "#A7B0BE"
             "#48CAE4" "#F2B84B" "#7BC47F"}
           (set (map (get-in g [:theme])
                     [:canvas :text :secondary :identity :evidence :output]))))
    (is (= "Sources and evidence enter a versioned identity contract; ArtifactID identifies a validated manifest from which scholarly views derive."
           (:description g)))
    (is (= "Sources · metadata · parser evidence"
           (:subtitle (:sources nodes))))
    (is (= "SHA-256 of canonical manifest identity"
           (:subtitle (:artifact-id nodes))))))

(deftest publication-copy-is-presentation-brief
  (let [g (graph :publication)
        subtitles (into {} (map (juxt :id :subtitle) (:nodes g)))]
    (is (= "A stable source-to-Parser-IR-to-manifest publication path, with the current AAT producer implementation shown as subordinate detail."
           (:description g)))
    (is (= "Text · metadata" (:source subtitles)))
    (is (= "Versioned evidence · configuration" (:parser-process subtitles)))
    (is (= "Stable publication interchange" (:parser-ir subtitles)))
    (is (= "Identity · provenance · validation · content" (:manifest subtitles)))
    (is (= "TEI · text · RDF · Linked Art · IIIF · annotation · analysis"
           (:outputs subtitles)))
    (is (nil? (:aat-detail subtitles)))))
```

- [ ] **Step 2: Run tests and verify RED**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-figures-test
```

Expected: topology passes; heading, footer, theme, and copy assertions fail.

- [ ] **Step 3: Implement exact presentation copy**

In `architecture-presentation.edn`, replace the two subtitle strings and the
publication manifest aggregate label exactly:

```clojure
:subtitle "Identity, provenance, and scholarly derivatives"
:subtitle "Validated transformation from source to scholarly outputs"
:manifest {:stages [:manifest] :label "Validated manifest"}
```

In `presentation_figures.clj`, set `:primary-size 34` and `:secondary-size 24`, preserving every color. Use these exact values:

```clojure
{:id :identity-contract :label "Versioned identity contract"}
{:id :record :label "Scholarly record"}
{:id :derived :label "Derived views"}
:description "Sources and evidence enter a versioned identity contract; ArtifactID identifies a validated manifest from which scholarly views derive."
:footer "Identity-bearing inputs determine ArtifactID."

:sources "Sources · metadata · parser evidence"
:artifact-id "SHA-256 of canonical manifest identity"
:manifest "Identity · provenance · validation · content hash"
:views "TEI · text · RDF · Linked Art · IIIF · annotation · analysis"

{:id :producer :label "Source and parser evidence · ab-validator"}
{:id :abc :label "Publication contract · ABC"}
{:id :scholarship :label "Scholarly outputs"}
:description "A stable source-to-Parser-IR-to-manifest publication path, with the current AAT producer implementation shown as subordinate detail."

:source "Text · metadata"
:parser-process "Versioned evidence · configuration"
:parser-ir "Stable publication interchange"
:manifest "Identity · provenance · validation · content"
:outputs "TEI · text · RDF · Linked Art · IIIF · annotation · analysis"
:aat-detail nil
```

Remove the old ArtifactID `:subtitle-wrap 42`. Retain every id, role, edge, backing map, `:primary-order`, current-inset path, and approved title.

- [ ] **Step 4: Run focused tests and verify GREEN**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test \
  --focus abc.tools.diagram.presentation-figures-test
```

Expected: all tests pass, including strict canonical backing validation.

- [ ] **Step 5: Commit**

```sh
git add abc/docs/architecture-presentation.edn \
  abc/src/abc/tools/diagram/presentation_figures.clj \
  abc/test/abc/tools/diagram/presentation_figures_test.clj
git commit -m "feat(diagram): tighten academic presentation copy"
```

---

### Task 2: Deduplicate Coordinate Rendering and Simplify Routes

**Files:**
- Modify: `abc/src/abc/tools/diagram/graphviz.clj`
- Modify: `abc/src/abc/tools/diagram/presentation_figures.clj`
- Modify: `abc/test/abc/tools/diagram/graphviz_test.clj`
- Modify: `abc/test/abc/tools/diagram/presentation_figures_test.clj`

**Interfaces:**
- Consumes: optional node `:coordinate-columns`, graph `:concentrate?`, and edge ports.
- Produces: deterministic DOT with render-only deduplication and routing metadata.

- [ ] **Step 1: Write failing renderer/layout tests**

Add to `graphviz_test.clj`:

```clojure
(deftest single-coordinate-equal-to-heading-renders-once
  (let [node {:id :output :label "Output format"
              :coordinates [{:id "output_format_spec_hash"
                             :label "Output format"}]
              :role :coordinate-family :group :a :backing {}}
        out (graphviz/dot (assoc sample :nodes [node] :edges []
                                :primary-order [] :concentrate? true))]
    (is (= 1 (count (re-seq #"Output format" out))))
    (is (str/includes? out "concentrate=\"true\""))))

(deftest coordinate-columns-and-edge-ports-are-layout-only
  (let [out (graphviz/dot (-> sample
                              (assoc-in [:nodes 1 :coordinate-columns] 2)
                              (assoc-in [:edges 0 :tail-port] :n)))]
    (is (re-find #"Coordinate alpha.*Coordinate beta.*</TR>.*Coordinate gamma"
                 out))
    (is (str/includes? out "tailport=\"n\""))))

(deftest ownership-role-colors-remain-stable
  (doseq [[role color] [[:coordinate-family "#48CAE4"]
                        [:identity "#48CAE4"]
                        [:evidence "#F2B84B"]
                        [:validation "#F2B84B"]
                        [:output "#7BC47F"]
                        [:source "#A7B0BE"]
                        [:contract "#A7B0BE"]]]
    (let [node {:id role :label (name role) :role role :backing {}}
          out (graphviz/dot (assoc sample :nodes [node] :edges []
                                  :primary-order []))]
      (is (str/includes? out (str "color=\"" color "\"")) (name role)))))
```

Add `[abc.tools.diagram.graphviz :as graphviz]` to the namespace `:require`, then
add:

```clojure
(deftest reproducibility-layout-hints-do-not-change-topology
  (let [g (graph :reproducibility)
        family (into {} (map (juxt :id identity)
                             (filter #(= :coordinate-family (:role %))
                                     (:nodes g))))]
    (is (:concentrate? g))
    (is (= 2 (:coordinate-columns (:source family))))
    (is (= 2 (:coordinate-columns (:parsing family))))
    (is (= 2 (:coordinate-columns (:analysis family))))
    (is (= (:reproducibility expected-topology) (semantic-topology g)))
    (is (every? #(= :w (:head-port %))
                (filter #(= :artifact-id (:to %)) (:edges g))))))

(deftest all-fifteen-coordinate-labels-render-once
  (let [g (graph :reproducibility)
        labels (map :label
                    (mapcat :coordinates
                            (filter #(= :coordinate-family (:role %))
                                    (:nodes g))))
        out (graphviz/dot g)]
    (is (= 15 (count labels)))
    (is (= 15 (count (distinct labels))))
    (doseq [label labels]
      (is (= 1 (count (re-seq
                       (re-pattern (java.util.regex.Pattern/quote label))
                       out)))
          label))))
```

- [ ] **Step 2: Run tests and verify RED**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-figures-test
```

- [ ] **Step 3: Implement render-only coordinate suppression**

Add before `html-label`:

```clojure
(defn- visible-coordinates [{:keys [label coordinates]}]
  (if (and (= 1 (count coordinates))
           (= label (:label (first coordinates))))
    []
    coordinates))
```

Destructure `coordinate-columns` and `:as node` in `html-label`; change only its coordinate row source:

```clojure
(for [row (partition-all (or coordinate-columns 3)
                         (visible-coordinates node))]
  (str "<TR>"
       (apply str
              (for [{:keys [label]} row]
                (str "<TD ALIGN=\"LEFT\"><FONT COLOR=\""
                     (:secondary theme) "\" POINT-SIZE=\""
                     (:secondary-size theme) "\">· "
                     (escape-html label) "</FONT></TD>")))
       "</TR>"))
```

The output coordinate remains in projection and backing.

- [ ] **Step 4: Implement deterministic layout hints**

In `edge-line`, add:

```clojure
(when-let [tail-port (:tail-port edge)]
  {:tailport (name tail-port)})
```

Destructure `concentrate?` in `dot` and construct graph attributes with:

```clojure
(merge {:bgcolor "transparent"
        :fontname "Noto Sans CJK JP"
        :fontcolor (:text theme)
        :nodesep "0.02"
        :ranksep "0.10"
        :pad "0.05"
        :margin "0"
        :rankdir direction
        :splines "polyline"}
       (when concentrate? {:concentrate "true"}))
```

In `family-node`, build the existing map with `cond->` so the three dense families
request two columns without changing their coordinates:

```clojure
(cond-> {:id family
         :label family-label
         :role :coordinate-family
         :group :identity-contract
         :coordinates coordinates
         :backing {:coordinates (mapv :id coordinates) :adrs owners}}
  (#{:source :parsing :analysis} family)
  (assoc :coordinate-columns 2))
```

Retain the existing family-label map as `family-label` in the surrounding `let`.
In the reproducibility graph, add `:concentrate? true`; add `:tail-port :n` to
the five sources→family edges and `:head-port :w` to the five
family→ArtifactID edges. Do not add, remove, or redirect an edge.

- [ ] **Step 5: Run tests and verify GREEN**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-figures-test
```

Expected: topology is unchanged and visible DOT label text contains `Output format` once.

- [ ] **Step 6: Commit**

```sh
git add abc/src/abc/tools/diagram/graphviz.clj \
  abc/src/abc/tools/diagram/presentation_figures.clj \
  abc/test/abc/tools/diagram/graphviz_test.clj \
  abc/test/abc/tools/diagram/presentation_figures_test.clj
git commit -m "feat(diagram): simplify presentation graph layout"
```

---

### Task 3: Raise Graph-Body Typography Floors

**Files:**
- Modify: `abc/src/abc/tools/diagram/presentation_svg.clj`
- Modify: `abc/test/abc/tools/diagram/presentation_svg_test.clj`

**Interfaces:**
- Consumes: normalized SVG with outer/descendant scales ≥1.
- Produces: body floors 34/24 with exact chrome 52/22/16 and accurate title/description.

- [ ] **Step 1: Write the failing boundary test**

Change body text in `raw-svg` and `raw-svg-with-inner-transform` from 30 to 34. Replace the legibility test with:

```clojure
(deftest graph-content-must-meet-revised-body-floors
  (let [raw (str/replace
             raw-svg
             "<text font-size=\"34\">Example</text>"
             (str "<text font-size=\"33\" font-weight=\"bold\">Bold</text>"
                  "<text font-size=\"23\">Small</text>"
                  "<path fill=\"none\" stroke=\"#48CAE4\" "
                  "stroke-width=\"1\" d=\"M0,0 L10,10\"/>"))
        problems (svg/svg-problems
                  (svg/normalize-svg graph raw (.getBytes "woff2" "UTF-8")))]
    (is (some #(str/includes? % "bold graph text is smaller than 34 px") problems))
    (is (some #(str/includes? % "graph text is smaller than 24 px") problems))
    (is (some #(str/includes? % "graph stroke is thinner than 2 px") problems))))
```

In the normalization test, assert:

```clojure
(is (= "52" (element-attr title "font-size")))
(is (= "22" (element-attr subtitle "font-size")))
(is (= "16" (element-attr footer "font-size")))
```

Existing assertions continue to require exact `<title>` and `<desc>` content.

- [ ] **Step 2: Run tests and verify RED**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-svg-test
```

Expected: 33/23 assertions fail under the old 30/22 validator.

- [ ] **Step 3: Raise only graph-body thresholds**

Change `graph-node-problems` exactly:

```clojure
(and (bold? font-weight) (< size 34.0))
[(str "bold graph text is smaller than 34 px: " font-size)]

(< size 24.0)
[(str "graph text is smaller than 24 px: " font-size)]
```

Do not change chrome validation, the graph region, or either scale guard.

- [ ] **Step 4: Run tests and verify GREEN**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-svg-test \
  --focus abc.tools.diagram.graphviz-test
```

- [ ] **Step 5: Commit**

```sh
git add abc/src/abc/tools/diagram/presentation_svg.clj \
  abc/test/abc/tools/diagram/presentation_svg_test.clj
git commit -m "fix(diagram): enforce revised presentation type floors"
```

---

### Task 4: Regenerate, Inspect, and Verify

**Files:**
- Regenerate: `abc/docs/figures/soranoha-reproducibility-architecture.{dot,svg}`
- Regenerate: `abc/docs/figures/soranoha-publication-pipeline.{dot,svg}`

**Interfaces:**
- Consumes: Tasks 1–3 projection, DOT, and SVG contracts.
- Produces: reviewed, byte-current 1920×1080 academic figures.

- [ ] **Step 1: Run pinned generation as the fit-scale gate**

```sh
nix run .#abc-presentation-diagrams
```

Expected: four files are written; neither graph throws `fit scale below 1`. If it does, stop. Do not lower 34/24, enlarge the graph region, or weaken scale validation. Adjust only approved prose length or Task 2 routing/column hints, rerun their focused tests, and regenerate.

- [ ] **Step 2: Check exact editorial and accessibility output**

```sh
rg -n 'Versioned identity contract|Identity-bearing inputs determine ArtifactID\.' \
  abc/docs/figures/soranoha-reproducibility-architecture.{dot,svg}
test "$(rg -o 'Output format' abc/docs/figures/soranoha-reproducibility-architecture.dot | wc -l)" -eq 1
! rg -n 'rebuilds only dependent layers|rebuilds only' abc/docs/figures
rg -n 'data-graph-scale="1\.[0-9]+"' abc/docs/figures/*.svg
rg -n '<[^>]*title[^>]*>|<[^>]*desc[^>]*>' abc/docs/figures/*.svg
```

Expected: exact copy is present, visible DOT text contains `Output format` once, no unsupported claim remains, scales are ≥1, and each SVG contains title/description.

- [ ] **Step 3: Run focused and pinned structural checks**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test \
  --focus abc.tools.diagram.presentation-figures-test \
  --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-svg-test \
  --focus abc.tools.diagram.presentation-registry-test
cd ..
nix run .#abc-presentation-diagrams -- --check
nix build ./abc#checks.x86_64-linux.presentation-diagram-drift --no-link
```

Expected: all tests pass; SVGs are byte-current and rasterize nonblank at 1920×1080.

- [ ] **Step 4: Produce and inspect original-size proofs**

```sh
nix shell nixpkgs#librsvg -c rsvg-convert --width 1920 --height 1080 \
  abc/docs/figures/soranoha-reproducibility-architecture.svg \
  --output /tmp/soranoha-reproducibility-editorial-revision.png
nix shell nixpkgs#librsvg -c rsvg-convert --width 1920 --height 1080 \
  abc/docs/figures/soranoha-publication-pipeline.svg \
  --output /tmp/soranoha-publication-editorial-revision.png
```

Inspect at original size. Confirm all fifteen coordinate labels are readable; node/group boundaries are distinct; no text or shape clips; no route crosses a label; the AAT inset remains subordinate; and `<desc>` accurately summarizes the visible figure. Spatial failures return to Task 2 without topology or palette changes.

- [ ] **Step 5: Commit reviewed generated artifacts**

```sh
git add abc/docs/figures/soranoha-reproducibility-architecture.dot \
  abc/docs/figures/soranoha-reproducibility-architecture.svg \
  abc/docs/figures/soranoha-publication-pipeline.dot \
  abc/docs/figures/soranoha-publication-pipeline.svg
git commit -m "docs(diagram): regenerate cleaner academic figures"
```

- [ ] **Step 6: Run full verification**

```sh
cd abc
bin/kaocha
clojure -M:abc/adr-governance
clojure -M:abc/diagrams --check
nix build .#checks.x86_64-linux.clj-kondo --no-link
nix build .#checks.x86_64-linux.presentation-diagram-drift --no-link
nix build .#checks.x86_64-linux.diagram-drift --no-link
cd ..
just nix-format-check
just validate-migration
git diff --check
git diff --exit-code -- abc/docs/figures
```

Expected: zero test failures; all Nix checks succeed; ADR/Mermaid diagrams remain current; root validation ends `all checks passed!`; repeat generation produces no diff.

## Final Review Checklist

- [ ] Fifteen coordinate records remain; fourteen bullets plus the output heading are visible.
- [ ] Semantic topology matches Task 1; routing metadata does not enter the comparison.
- [ ] Palette and ownership colors are unchanged.
- [ ] Exact heading/footer are present and selective-rebuild wording is absent.
- [ ] Body sizes meet 34/24; chrome remains 52/22/16; every scale is ≥1.
- [ ] No overlap, clipping, or unintended edge/label collision is visible at 1920×1080.
- [ ] Accessible title/description accurately summarize each figure.
- [ ] Both SVGs are byte-current and rasterize nonblank under pinned Nix inputs.
- [ ] Full Kaocha and `just validate-migration` pass.
