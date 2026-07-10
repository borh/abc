# Academic Presentation Diagrams Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Generate two clean, professional, source-validated SVG figures for a black-background academic presentation: **Soranoha Reproducibility Architecture** and **Soranoha Publication Pipeline**.

**Architecture:** Add a checked presentation projection over the existing architecture stages, manifest schema, and ADR ownership data. Pure Clojure builders produce enriched graph values and deterministic DOT; a separate effect-owning SVG pipeline invokes Nix-pinned Graphviz, embeds a pinned Noto Sans CJK JP subset, normalizes the SVG onto an exact 1920×1080 black canvas, and drift-checks committed DOT/SVG artifacts without changing the existing Mermaid registry.

**Tech Stack:** Clojure 1.12, EDN, `clojure.data.xml`, `babashka.process`, Graphviz 12.2.1 from the locked Nixpkgs input, Noto Sans CJK Sans 2.004, a combined Python environment containing fonttools 4.61.1 plus Brotli, librsvg, ImageMagick, Kaocha, Nix flakes, SVG 1.1-compatible XML.

## Global Constraints

- Work from the monorepo root, but run Clojure commands from `abc/`.
- Use `bin/kaocha`; do not invent another Clojure test entry point.
- Keep `clojure -M:abc/diagrams --check` green; Mermaid remains unchanged.
- The presentation canvas is exactly `viewBox="0 0 1920 1080"` with an explicit `#000000` rectangle.
- Maintain a minimum 96-pixel safe margin.
- Theme colors are limited to `#000000`, `#F5F7FA`, `#A7B0BE`, `#48CAE4`, `#F2B84B`, and `#7BC47F` plus `none`/`transparent`.
- At 1920×1080, titles are at least 52 px, primary labels 30 px, secondary labels 22 px, citations 16 px, and strokes 2 px.
- Preserve SVG text; do not convert labels to paths.
- Embed the used Noto Sans CJK JP glyph subset as a `data:font/woff2;base64,...` resource.
- Every emitted SVG element, including the root and hand-built title/background/footer elements, must resolve to the namespace URI `http://www.w3.org/2000/svg`.
- The pinned Nix gate must rasterize both SVGs with librsvg and verify 1920×1080 PNG output; non-empty XML is not sufficient.
- Every presentation node has canonical backing. Every semantic connector is backed by an identity-contract relation, canonical edge, or reachable canonical path.
- The current parser inset must resolve the live path `:aozora-snapshot -> :aat -> :parser-ir`; generation fails when it stops resolving.
- Presentation data, DOT, and SVG must never enter `manifest_identity_object` or ArtifactID computation.
- Generate all candidates in a temporary directory, validate both figures, then replace each committed artifact with `ATOMIC_MOVE` and `REPLACE_EXISTING`.
- Never hand-edit generated `.dot` or `.svg` files.
- Preserve unrelated worktree changes.

---

## File Structure

- `docs/architecture-presentation.edn` — presentation-only labels, identity families, aggregate backing sets, and current-parser inset path.
- `src/abc/tools/diagram/presentation_model.clj` — load canonical sources, graph reachability, metadata completeness, backing and citation validation.
- `src/abc/tools/diagram/presentation_figures.clj` — pure builders for the two enriched graph values.
- `src/abc/tools/diagram/graphviz.clj` — deterministic enriched graph value → DOT renderer.
- `src/abc/tools/diagram/presentation_svg.clj` — Graphviz execution, font subsetting, SVG normalization and structural validation.
- `src/abc/tools/diagram/presentation_registry.clj` — two-figure registry, in-memory validation, drift checking, atomic replacement, CLI.
- `test/abc/tools/diagram/presentation_model_test.clj` — metadata, reachability, citation, and stale-inset tests.
- `test/abc/tools/diagram/presentation_figures_test.clj` — semantic content and canonical-backing tests for both figures.
- `test/abc/tools/diagram/graphviz_test.clj` — deterministic DOT and escaping tests.
- `test/abc/tools/diagram/presentation_svg_test.clj` — canvas, font, accessibility, palette, resource, and SVG normalization tests.
- `test/abc/tools/diagram/presentation_registry_test.clj` — all-or-nothing validation, drift reporting, and atomic write behavior.
- `docs/figures/*.dot`, `docs/figures/*.svg` — committed generated presentation artifacts.
- `deps.edn` — `:abc/presentation-diagrams` CLI alias.
- `flake.nix` — pinned runtime app/dev-shell inputs and `presentation-diagram-drift` check.
- `docs/architecture.md` — links, audience/purpose distinction, and regeneration instructions.
- `test/abc/tools/schema_test.clj` — explicit regression that presentation fields are rejected inside manifest identity.

---

### Task 0: Rendering-Risk Preflight Spike

**Files:** None. This blocking preflight runs against the locked toolchain before Task 1 changes repository files.

**Interfaces:**
- Consumes: the root flake's locked Nixpkgs input.
- Produces: verified constraints for Graphviz HTML labels and colors, WOFF2 subsetting and variable weights, namespace-qualified XML assembly, and real rasterization.

- [ ] **Step 1: Verify Graphviz HTML labels, palette output, and rasterization**

Run from the monorepo root:

```sh
nix shell --inputs-from . nixpkgs#graphviz nixpkgs#librsvg -c bash -euo pipefail -c '
dot_source='"'"'digraph g { graph [bgcolor="transparent",rankdir="LR",splines="polyline"]; node [fontname="Noto Sans CJK JP"]; subgraph cluster_current { label="Current producer implementation"; color="#A7B0BE"; fontcolor="#A7B0BE"; fontsize="22"; style="rounded,dashed"; a [label=<<TABLE BORDER="0" CELLBORDER="0" CELLPADDING="3"><TR><TD><FONT POINT-SIZE="30"><B>AAT evidence</B></FONT></TD></TR><TR><TD><FONT COLOR="#A7B0BE" POINT-SIZE="22">current detail</FONT></TD></TR></TABLE>>,shape="rect",style="rounded",penwidth="2",color="#F2B84B",fontcolor="#F5F7FA"]; } b [label=<<TABLE BORDER="0" CELLBORDER="0" CELLPADDING="3"><TR><TD><FONT POINT-SIZE="30"><B>Parser-IR</B></FONT></TD></TR></TABLE>>,shape="rect",style="rounded",penwidth="2",color="#48CAE4",fontcolor="#F5F7FA"]; a -> b [label="mapping compatibility",color="#F2B84B",fontcolor="#A7B0BE",fontsize="22",penwidth="2",style="dashed"]; }'"'"'
svg=$(printf "%s" "$dot_source" | dot -Tsvg)
colors=$(printf "%s" "$svg" | grep -Eo '"'"'(fill|stroke|color)="[^"]+"'"'"' | sort -u)
printf "%s\n" "$colors"
while IFS= read -r declaration; do
  value=${declaration#*=\"}; value=${value%\"}; value=${value^^}
  case "$value" in
    "#000000"|"#F5F7FA"|"#A7B0BE"|"#48CAE4"|"#F2B84B"|"#7BC47F"|"NONE"|"TRANSPARENT") ;;
    *) echo "unexpected Graphviz color: $value" >&2; exit 1 ;;
  esac
done <<<"$colors"
printf "%s" "$svg" | rsvg-convert --width 1920 --height 1080 >/tmp/soranoha-graphviz-spike.png
test -s /tmp/soranoha-graphviz-spike.png
rm -f /tmp/soranoha-graphviz-spike.png
'
```

Expected: HTML-table labels rasterize and reported colors are only lowercase forms of the approved palette plus `none`/`transparent`. Any additional named or hex color blocks implementation until the DOT theme explicitly overrides its source.

- [ ] **Step 2: Verify WOFF2 encoding and the retained weight axis**

```sh
font=$(nix build --inputs-from . nixpkgs#noto-fonts-cjk-sans \
  --no-link --print-out-paths)/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc
font_env='let f = builtins.getFlake (toString ./.); pkgs = import f.inputs.nixpkgs { system = builtins.currentSystem; }; in pkgs.python3.withPackages (ps: [ ps.fonttools ps.brotli ])'
rm -f /tmp/soranoha-font-spike.woff2 /tmp/soranoha-font-spike.ttx
nix shell --impure --expr "$font_env" -c pyftsubset "$font" \
  --font-number=0 --text='Soranoha 再現可能性 AAT Parser-IR · 400 700' \
  --flavor=woff2 --layout-features='*' \
  --output-file=/tmp/soranoha-font-spike.woff2
nix shell --impure --expr "$font_env" -c ttx -q -f \
  -o /tmp/soranoha-font-spike.ttx -t fvar /tmp/soranoha-font-spike.woff2
rg -n '<AxisTag>wght</AxisTag>|<MinValue>100.0</MinValue>|<MaxValue>900.0</MaxValue>' \
  /tmp/soranoha-font-spike.ttx
test -s /tmp/soranoha-font-spike.woff2
rm -f /tmp/soranoha-font-spike.woff2 /tmp/soranoha-font-spike.ttx
```

Expected: WOFF2 encoding succeeds and the subset retains the `wght` axis spanning 100–900, covering 400 and 700. `python3Packages.fonttools` alone is not acceptable: its wrapper lacks the optional Brotli module needed for WOFF2.

- [ ] **Step 3: Verify namespace-qualified `data.xml` construction**

Run from `abc/`:

```sh
clojure -M -e '
(require (quote [clojure.data.xml :as xml]))
(let [uri "http://www.w3.org/2000/svg"
      q #(xml/qname uri %)
      root (xml/element (q "svg") {:viewBox "0 0 1920 1080"}
                        (xml/element (q "title") {} "Spike")
                        (xml/element (q "rect")
                                     {:x "0" :y "0" :width "1920"
                                      :height "1080" :fill "#000000"}))
      parsed (xml/parse-str (xml/emit-str root))]
  (assert (= uri (xml/qname-uri (:tag parsed))))
  (assert (every? #(= uri (xml/qname-uri (:tag %)))
                  (filter :tag (tree-seq map? :content parsed))))
  (println (xml/emit-str root)))'
```

Expected: both assertions pass. The emitted prefix is immaterial; every element QName must resolve to the SVG namespace. Never use bare `:svg` plus a literal `:xmlns` attribute.

- [ ] **Step 4: Record the preflight outcome**

Record command outputs in the execution report. If any step fails, stop before Task 1 and revise the rendering design; do not build the pure model around an unproven renderer.

---

### Task 1: Canonical Presentation Metadata and Validation

**Files:**
- Create: `abc/docs/architecture-presentation.edn`
- Create: `abc/src/abc/tools/diagram/presentation_model.clj`
- Create: `abc/test/abc/tools/diagram/presentation_model_test.clj`

**Interfaces:**
- Consumes: `architecture-graph/load-stages`, `architecture-graph/manifest-identity-required`, `abc.tools.adr/parse-all`, `schemas/manifest.schema.json`.
- Produces:
  - `load-metadata [] -> map`
  - `canonical-context [] -> {:stages map :stage-edges set :coordinates set :coordinate-owners map :adr-nums set}`
  - `reachable? [stage-edges from to] -> boolean`
  - `backing-problems [context backing] -> vector<string>`
  - `problems [metadata context] -> vector<string>`
  - `validated-model [] -> {:metadata map :context map}`, throwing `ExceptionInfo` with `:problems` on failure.

- [ ] **Step 1: Write failing model tests**

Create `abc/test/abc/tools/diagram/presentation_model_test.clj`:

```clojure
(ns abc.tools.diagram.presentation-model-test
  (:require [abc.tools.diagram.presentation-model :as model]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(deftest committed-presentation-metadata-is-total
  (is (= [] (model/problems (model/load-metadata)
                            (model/canonical-context)))))

(deftest coordinate-and-stage-metadata-are-closed
  (let [metadata (model/load-metadata)
        context (model/canonical-context)]
    (testing "a missing coordinate fails"
      (is (some #(str/includes? % "coordinate metadata mismatch")
                (model/problems
                 (update metadata :coordinates dissoc "manifest_schema_hash")
                 context))))
    (testing "an extra stage fails"
      (is (some #(str/includes? % "stage metadata mismatch")
                (model/problems
                 (assoc-in metadata [:stages :imaginary]
                           {:label "Imaginary" :role :source})
                 context))))))

(deftest reachability-follows-canonical-stage-edges
  (let [edges (:stage-edges (model/canonical-context))]
    (is (model/reachable? edges :aozora-snapshot :analysis))
    (is (not (model/reachable? edges :analysis :aozora-snapshot)))))

(deftest aggregate-backing-and-citations-must-be-canonical
  (let [context (model/canonical-context)]
    (is (seq (model/backing-problems
              context
              {:stages [:missing] :adrs [1]})))
    (is (seq (model/backing-problems
              context
              {:stages [:manifest] :adrs [30]})))
    (is (= [] (model/backing-problems
               context
               {:stages [:manifest] :adrs [1 10 23 27 28]})))))

(deftest current-parser-inset-must-resolve-the-live-path
  (let [metadata (assoc-in (model/load-metadata)
                           [:figures :publication :current-inset :path]
                           [:aozora-snapshot :parser-ir])]
    (is (some #(str/includes? % "current parser inset path")
              (model/problems metadata (model/canonical-context))))))

(deftest validated-model-throws-actionable-data
  (let [bad-metadata (update (model/load-metadata) :coordinates dissoc
                             "manifest_schema_hash")]
    (with-redefs [model/load-metadata (constantly bad-metadata)]
      (try
        (model/validated-model)
        (is false "expected invalid presentation metadata")
        (catch clojure.lang.ExceptionInfo ex
          (is (seq (:problems (ex-data ex)))))))))
```

- [ ] **Step 2: Run the focused test and confirm the missing namespace failure**

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test
```

Expected: FAIL because `abc.tools.diagram.presentation-model` does not exist.

- [ ] **Step 3: Add the complete presentation metadata document**

Create `abc/docs/architecture-presentation.edn`:

```clojure
{:version 1
 :coordinates
 {"manifest_schema_hash" {:family :publication :label "Manifest schema"}
  "corpus_snapshot_hash" {:family :source :label "Corpus snapshot"}
  "work_content_hash" {:family :source :label "Work text"}
  "metadata_record_hash" {:family :source :label "Bibliographic metadata"}
  "parser_build_hash" {:family :parsing :label "Parser build"}
  "parser_config_hash" {:family :parsing :label "Parser configuration"}
  "aat_parser_ir_mapping_hash" {:family :parsing :label "AAT–Parser-IR mapping"}
  "parser_ir_schema_hash" {:family :parsing :label "Parser-IR schema"}
  "tei_profile_hash" {:family :publication :label "TEI profile"}
  "tokenizer_build_hash" {:family :analysis :label "Tokenizer build"}
  "tokenizer_dictionary_hash" {:family :analysis :label "Dictionary"}
  "tokenizer_profile_hash" {:family :analysis :label "Tokenizer profile"}
  "analysis_recipe_hash" {:family :analysis :label "Analysis recipe"}
  "annotation_policy_hash" {:family :analysis :label "Annotation policy"}
  "output_format_spec_hash" {:family :output :label "Output format"}}
 :stages
 {:aozora-snapshot {:label "Aozora source" :role :source}
  :aat {:label "Parser and AAT evidence" :role :evidence}
  :parser-ir {:label "Parser-IR" :role :contract}
  :metadata {:label "Bibliographic and person metadata" :role :source}
  :manifest {:label "Validated artifact manifest" :role :identity}
  :tei {:label "TEI and visible text" :role :output}
  :rdf {:label "RDF / PROV-O / Linked Art" :role :output}
  :iiif {:label "IIIF applicability" :role :output}
  :tokenized {:label "Tokenized text" :role :output}
  :analysis {:label "Analytical datasets" :role :output}
  :annotation {:label "Annotation views" :role :output}}
 :figures
 {:reproducibility
  {:title "Soranoha Reproducibility Architecture"
   :subtitle "Exact sources, evidence, contracts, and recipes produce traceable scholarly views"
   :aggregates
   {:sources {:stages [:aozora-snapshot :metadata :aat]
              :label "Sources and evidence"}
    :identity {:stages [:manifest]
               :label "Canonical identity contract"}
    :manifest {:stages [:manifest]
               :label "Validated manifest"}
    :views {:stages [:tei :rdf :iiif :tokenized :analysis :annotation]
            :label "Scholarly views"}}}
  :publication
  {:title "Soranoha Publication Pipeline"
   :subtitle "Validated source transformation with explicit evidence, contracts, and provenance"
   :aggregates
   {:source {:stages [:aozora-snapshot :metadata] :label "Aozora source"}
    :parser-process {:stages [:aat] :label "Validated parser process"}
    :parser-ir {:stages [:parser-ir] :label "Parser-IR"}
    :manifest {:stages [:manifest] :label "Identity and provenance manifest"}
    :outputs {:stages [:tei :rdf :iiif :tokenized :analysis :annotation]
              :label "Scholarly outputs"}}
   :current-inset
   {:label "Current producer implementation"
    :path [:aozora-snapshot :aat :parser-ir]}}}}
```

- [ ] **Step 4: Implement canonical context, reachability, and closed validation**

Create `abc/src/abc/tools/diagram/presentation_model.clj` with these public functions and private helpers:

```clojure
(ns abc.tools.diagram.presentation-model
  (:require [abc.tools.adr :as adr]
            [abc.tools.diagram.architecture-graph :as architecture]
            [abc.tools.files :as files]
            [abc.tools.json :as json]
            [clojure.set :as set]))

(def metadata-path "docs/architecture-presentation.edn")
(def manifest-schema-path "schemas/manifest.schema.json")
(def adr-dir "docs/adr")
(def coordinate-families #{:source :parsing :publication :analysis :output})
(def stage-roles #{:source :evidence :contract :identity :output})

(defn load-metadata [] (files/read-edn metadata-path))

(defn- stage-map [stage-doc]
  (into {} (map (juxt :id identity)) (:stages stage-doc)))

(defn- stage-edges [stages]
  (into #{}
        (mapcat (fn [{:keys [id inputs]}]
                  (map (fn [input] [input id]) inputs)))
        (vals stages)))

(defn canonical-context []
  (let [stage-doc (architecture/load-stages)
        stages (stage-map stage-doc)]
    {:stage-doc stage-doc
     :stages stages
     :stage-edges (stage-edges stages)
     :coordinates (architecture/manifest-identity-required
                   (json/read-json-file manifest-schema-path))
     :coordinate-owners (get-in stage-doc
                                [:manifest-identity-contract :coordinates])
     :adr-nums (set (map :num (adr/parse-all adr-dir)))}))

(defn reachable? [edges from to]
  (loop [frontier [from] seen #{}]
    (cond
      (empty? frontier) false
      (= to (peek frontier)) true
      (seen (peek frontier)) (recur (pop frontier) seen)
      :else
      (let [node (peek frontier)
            nexts (for [[a b] edges :when (= a node)] b)]
        (recur (into (pop frontier) nexts) (conj seen node))))))

(defn- path-resolves? [edges path]
  (and (<= 2 (count path))
       (every? #(contains? edges %) (partition 2 1 path))))

(defn- canonical-stage-adrs [context stage-ids]
  (set (mapcat #(get-in context [:stages % :adr]) stage-ids)))

(defn backing-problems [context {:keys [stages adrs]}]
  (let [stage-ids (set (keys (:stages context)))
        stages (vec stages)
        missing (sort (remove stage-ids stages))
        allowed-adrs (canonical-stage-adrs context stages)
        invalid-adrs (sort (remove allowed-adrs adrs))]
    (cond-> []
      (empty? stages) (conj "aggregate backing must name at least one stage")
      (seq missing) (conj (format "aggregate backing references missing stages %s"
                                  missing))
      (seq invalid-adrs) (conj (format "aggregate citations are not canonical for backing stages: %s"
                                       invalid-adrs)))))

(defn- keyset-problem [label expected actual]
  (when (not= expected actual)
    (format "%s metadata mismatch: missing=%s extra=%s"
            label
            (sort (set/difference expected actual))
            (sort (set/difference actual expected)))))

(defn problems [metadata context]
  (let [coordinate-problem
        (keyset-problem "coordinate" (:coordinates context)
                        (set (keys (:coordinates metadata))))
        stage-problem
        (keyset-problem "stage" (set (keys (:stages context)))
                        (set (keys (:stages metadata))))
        aggregates (mapcat (comp vals :aggregates val) (:figures metadata))
        inset-path (get-in metadata [:figures :publication :current-inset :path])]
    (vec
     (concat
      (keep identity [coordinate-problem stage-problem])
      (for [[coordinate {:keys [family label]}] (:coordinates metadata)
            :when (or (not (coordinate-families family))
                      (not (string? label))
                      (empty? label))]
        (format "coordinate %s has invalid presentation family or label" coordinate))
      (for [[stage {:keys [role label]}] (:stages metadata)
            :when (or (not (stage-roles role))
                      (not (string? label))
                      (empty? label))]
        (format "stage %s has invalid presentation role or label" stage))
      (mapcat #(backing-problems context %) aggregates)
      (when-not (path-resolves? (:stage-edges context) inset-path)
        [(format "current parser inset path does not resolve: %s" inset-path)])))))

(defn validated-model []
  (let [metadata (load-metadata)
        context (canonical-context)
        failures (problems metadata context)]
    (when (seq failures)
      (throw (ex-info "academic presentation model is invalid"
                      {:problems failures})))
    {:metadata metadata :context context}))
```

The committed aggregate maps omit explicit `:adrs`; `backing-problems` treats that as an empty citation selection. Later figure builders derive citations from canonical stage ADRs and coordinate ownership.

- [ ] **Step 5: Run and format the focused tests**

Run:

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test
cljfmt check src/abc/tools/diagram/presentation_model.clj \
  test/abc/tools/diagram/presentation_model_test.clj
```

Expected: the focused namespace passes; cljfmt exits 0.

- [ ] **Step 6: Commit the checked presentation model**

```sh
git add abc/docs/architecture-presentation.edn \
  abc/src/abc/tools/diagram/presentation_model.clj \
  abc/test/abc/tools/diagram/presentation_model_test.clj
git commit -m "feat(diagram): validate academic presentation model"
```

---

### Task 2: Pure Reproducibility and Publication Figure Projections

**Files:**
- Create: `abc/src/abc/tools/diagram/presentation_figures.clj`
- Create: `abc/test/abc/tools/diagram/presentation_figures_test.clj`

**Interfaces:**
- Consumes: `presentation-model/validated-model` and `presentation-model/reachable?`.
- Produces:
  - `theme -> presentation theme map`
  - `validate-graph! [context graph] -> graph`, throwing on missing backing or undeclared edge endpoints
  - `reproducibility-graph [model] -> enriched graph map`
  - `publication-graph [model] -> enriched graph map`
  - `graphs [] -> vector of both graph maps`
  - graph nodes shaped as `{:id keyword :label string :subtitle string? :role keyword :group keyword? :backing map}`
  - graph edges shaped as `{:from keyword :to keyword :label string? :role keyword :style keyword :backing map}`.

- [ ] **Step 1: Write failing projection tests**

Create `abc/test/abc/tools/diagram/presentation_figures_test.clj`:

```clojure
(ns abc.tools.diagram.presentation-figures-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-model :as model]
            [clojure.test :refer [deftest is testing]]))

(defn- graph [id]
  (some #(when (= id (:id %)) %) (figures/graphs)))

(deftest reproducibility-figure-has-approved-title-and-five-coordinate-families
  (let [g (graph :reproducibility)
        family-nodes (filter #(= :coordinate-family (:role %)) (:nodes g))]
    (is (= "Soranoha Reproducibility Architecture" (:title g)))
    (is (= #{:source :parsing :publication :analysis :output}
           (set (map :id family-nodes))))
    (is (= 15 (reduce + (map #(count (:coordinates %)) family-nodes))))
    (is (some #(= "ArtifactID = SHA-256(JCS(manifest_identity_object))"
                  (:label %))
              (:nodes g)))))

(deftest publication-figure-keeps-stable-path-and-current-inset-separate
  (let [g (graph :publication)]
    (is (= "Soranoha Publication Pipeline" (:title g)))
    (is (= [:source :parser-process :parser-ir :manifest :outputs]
           (:primary-order g)))
    (is (= [:aozora-snapshot :aat :parser-ir]
           (get-in g [:current-inset :path])))
    (is (= "Current producer implementation"
           (get-in g [:current-inset :label])))
    (is (some #(= :aat-detail (:id %)) (:nodes g)))
    (is (= :dashed
           (:style (some #(when (= :current-inset (:id %)) %) (:groups g)))))))

(deftest every-node-and-semantic-edge-has-canonical-backing
  (doseq [g (figures/graphs)
          item (concat (:nodes g) (:edges g))]
    (is (map? (:backing item)) (str (:id g) " " item))))

(deftest graph-validation-rejects-phantom-edge-endpoints
  (let [context (:context (model/validated-model))]
    (is (thrown-with-msg?
         clojure.lang.ExceptionInfo #"undeclared node"
         (figures/validate-graph!
          context
          {:id :broken
           :nodes [{:id :declared :backing {:stages [:manifest]}}]
           :edges [{:from :declared :to :typo
                    :backing {:stages [:manifest]}}]})))))

(deftest publication-output-summary-is-backed-by-live-output-stages
  (let [g (graph :publication)
        outputs (some #(when (= :outputs (:id %)) %) (:nodes g))]
    (is (= #{:tei :rdf :iiif :tokenized :analysis :annotation}
           (set (get-in outputs [:backing :stages]))))))

(deftest existing-mermaid-graph-shape-remains-valid
  (testing "the enriched fields are additive"
    (doseq [g (figures/graphs)]
      (is (string? (:direction g)))
      (is (sequential? (:nodes g)))
      (is (sequential? (:edges g))))))
```

- [ ] **Step 2: Run the focused test and confirm it fails on the missing builder**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-figures-test
```

Expected: FAIL because `presentation-figures` does not exist.

- [ ] **Step 3: Implement the two complete graph projections**

Create `abc/src/abc/tools/diagram/presentation_figures.clj`. Keep data assembly explicit so visual review can trace every item:

```clojure
(ns abc.tools.diagram.presentation-figures
  (:require [abc.tools.diagram.presentation-model :as model]))

(def theme
  {:canvas "#000000"
   :text "#F5F7FA"
   :secondary "#A7B0BE"
   :identity "#48CAE4"
   :evidence "#F2B84B"
   :output "#7BC47F"
   :title-size 52
   :primary-size 30
   :secondary-size 22
   :citation-size 16
   :stroke-width 2
   :safe-margin 96})

(defn- aggregate [metadata figure id]
  (get-in metadata [:figures figure :aggregates id]))

(defn- stage-adrs [context stage-ids]
  (vec (sort (set (mapcat #(get-in context [:stages % :adr]) stage-ids)))))

(defn- aggregate-node [metadata context figure id role group subtitle]
  (let [{:keys [label stages]} (aggregate metadata figure id)]
    {:id id :label label :subtitle subtitle :role role :group group
     :backing {:stages stages :adrs (stage-adrs context stages)}}))

(defn- family-node [metadata context family]
  (let [coordinates (->> (:coordinates metadata)
                         (keep (fn [[coordinate value]]
                                 (when (= family (:family value))
                                   {:id coordinate :label (:label value)})))
                         (sort-by :id)
                         vec)
        owners (vec (sort (set (mapcat #(get (:coordinate-owners context) (:id %))
                                       coordinates))))]
    {:id family
     :label ({:source "Source identity"
              :parsing "Parsing identity"
              :publication "Publication contracts"
              :analysis "Linguistic analysis"
              :output "Output format"} family)
     :role :coordinate-family
     :group :identity-contract
     :coordinates coordinates
     :backing {:coordinates (mapv :id coordinates) :adrs owners}}))

(defn- path-valid? [context path]
  (and (<= 2 (count path))
       (every? (fn [[from to]]
                 (model/reachable? (:stage-edges context) from to))
               (partition 2 1 path))))

(defn- item-backing-problems [context {:keys [coordinates stages path
                                               reachable-targets adrs]}]
  (let [coordinate-set (:coordinates context)
        stage-set (set (keys (:stages context)))
        missing-coordinates (sort (remove coordinate-set coordinates))
        missing-stages (sort (remove stage-set stages))
        missing-targets (sort (remove stage-set reachable-targets))
        allowed-adrs (set (concat
                           (mapcat #(get-in context [:stages % :adr]) stages)
                           (mapcat #((:coordinate-owners context) %) coordinates)))
        invalid-adrs (sort (remove allowed-adrs adrs))
        path-source (first path)
        unreachable (sort (remove #(and path-source
                                        (model/reachable? (:stage-edges context)
                                                          path-source %))
                                  reachable-targets))]
    (cond-> []
      (seq missing-coordinates)
      (conj (str "unknown backing coordinates " missing-coordinates))
      (seq missing-stages)
      (conj (str "unknown backing stages " missing-stages))
      (seq missing-targets)
      (conj (str "unknown reachable targets " missing-targets))
      (seq invalid-adrs)
      (conj (str "citations are not canonical for backing " invalid-adrs))
      (and path (not (path-valid? context path)))
      (conj (str "backing path does not resolve " path))
      (seq unreachable)
      (conj (str "backing targets are unreachable " unreachable)))))

(defn validate-graph! [context graph]
  (let [node-ids (set (map :id (:nodes graph)))]
    (doseq [{:keys [from to] :as edge} (:edges graph)
            endpoint [from to]
            :when (not (node-ids endpoint))]
      (throw (ex-info "presentation edge references undeclared node"
                      {:figure (:id graph) :edge edge
                       :endpoint endpoint :node-ids node-ids}))))
  (doseq [item (concat (:nodes graph) (:edges graph))]
    (when-not (map? (:backing item))
      (throw (ex-info "presentation item has no canonical backing"
                      {:figure (:id graph) :item item})))
    (when-let [failures (seq (item-backing-problems context (:backing item)))]
      (throw (ex-info "presentation item backing is invalid"
                      {:figure (:id graph) :item item :problems failures}))))
  graph)

(defn reproducibility-graph [{:keys [metadata context]}]
  (let [graph
        {:id :reproducibility
   :direction "LR"
   :title (get-in metadata [:figures :reproducibility :title])
   :subtitle (get-in metadata [:figures :reproducibility :subtitle])
   :description "Sources and computational evidence enter a canonical identity contract, producing a validated manifest and traceable scholarly views."
   :theme theme
   :groups [{:id :inputs :label "Sources and computational evidence"}
            {:id :identity-contract :label "15-coordinate identity contract"}
            {:id :record :label "Durable scholarly record"}
            {:id :derived :label "Derived scholarly views"}]
   :nodes (vec
           (concat
            [(aggregate-node metadata context :reproducibility :sources
                             :evidence :inputs
                             "Corpus, bibliographic metadata, and parser evidence")]
            (map #(family-node metadata context %)
                 [:source :parsing :publication :analysis :output])
            [{:id :artifact-id
              :label "ArtifactID = SHA-256(JCS(manifest_identity_object))"
              :subtitle "Canonical identity; distinct from the output byte hash"
              :role :identity-formula :group :identity-contract
              :backing {:coordinates (vec (sort (:coordinates context)))
                        :adrs (vec (sort (set (mapcat val (:coordinate-owners context)))))}}
             (aggregate-node metadata context :reproducibility :manifest
                             :identity :record
                             "Identity, provenance, validation status, and content hash")
             (aggregate-node metadata context :reproducibility :views
                             :output :derived
                             "TEI, visible text, RDF, Linked Art, IIIF, annotation, and analysis")]))
   :edges [{:from :sources :to :source :role :identity-input :style :solid
            :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                    "metadata_record_hash"]}}
           {:from :sources :to :parsing :role :identity-input :style :solid
            :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                    "aat_parser_ir_mapping_hash"
                                    "parser_ir_schema_hash"]}}
           {:from :sources :to :publication :role :identity-input :style :solid
            :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
           {:from :sources :to :analysis :role :identity-input :style :solid
            :backing {:coordinates ["tokenizer_build_hash"
                                    "tokenizer_dictionary_hash"
                                    "tokenizer_profile_hash"
                                    "analysis_recipe_hash"
                                    "annotation_policy_hash"]}}
           {:from :sources :to :output :role :identity-input :style :solid
            :backing {:coordinates ["output_format_spec_hash"]}}
           {:from :source :to :artifact-id :role :identity :style :solid
            :backing {:coordinates ["corpus_snapshot_hash" "work_content_hash"
                                    "metadata_record_hash"]}}
           {:from :parsing :to :artifact-id :role :identity :style :solid
            :backing {:coordinates ["parser_build_hash" "parser_config_hash"
                                    "aat_parser_ir_mapping_hash"
                                    "parser_ir_schema_hash"]}}
           {:from :publication :to :artifact-id :role :identity :style :solid
            :backing {:coordinates ["manifest_schema_hash" "tei_profile_hash"]}}
           {:from :analysis :to :artifact-id :role :identity :style :solid
            :backing {:coordinates ["tokenizer_build_hash"
                                    "tokenizer_dictionary_hash"
                                    "tokenizer_profile_hash"
                                    "analysis_recipe_hash"
                                    "annotation_policy_hash"]}}
           {:from :output :to :artifact-id :role :identity :style :solid
            :backing {:coordinates ["output_format_spec_hash"]}}
           {:from :artifact-id :to :manifest :role :identity :style :thick
            :backing {:stages [:manifest] :adrs (stage-adrs context [:manifest])}}
           {:from :manifest :to :views :role :derived-view :style :solid
            :backing {:path [:manifest :tei]
                      :reachable-targets [:tei :rdf :iiif :tokenized :analysis :annotation]}}]
   :footer "Changing an identity-bearing input produces a new ArtifactID and rebuilds only dependent layers."
         :primary-order [:sources :artifact-id :manifest :views]}]
    (validate-graph! context graph)))

(defn publication-graph [{:keys [metadata context]}]
  (let [figure :publication
        node (fn [id role group subtitle]
               (aggregate-node metadata context figure id role group subtitle))
        graph
        {:id :publication
     :direction "LR"
     :title (get-in metadata [:figures figure :title])
     :subtitle (get-in metadata [:figures figure :subtitle])
     :description "A stable source-to-publication contract with the current AAT producer implementation shown as subordinate detail."
     :theme theme
     :groups [{:id :producer :label "Sources and parser evidence · ab-validator"}
              {:id :abc :label "Publication contracts and materialization · ABC"}
              {:id :scholarship :label "Scholarly publication and analysis"}
              {:id :current-inset :label "Current producer implementation"
               :style :dashed}]
     :nodes [(node :source :source :producer "Authoritative text and metadata")
             (node :parser-process :evidence :producer "Versioned parser evidence and configuration")
             (node :parser-ir :contract :abc "Stable publication-side interchange contract")
             (node :manifest :identity :abc "Exact identity, provenance, validation, and content")
             (node :outputs :output :scholarship "TEI, text, RDF, Linked Art, IIIF, annotations, and analysis")
             {:id :aat-detail
              :label "AAT evidence + mapping gate"
              :subtitle "Current implementation detail"
              :role :implementation-detail :group :current-inset
              :backing {:stages [:aat]
                        :adrs (stage-adrs context [:aat])}}]
     :edges [{:from :source :to :parser-process :label "parse + measure"
              :role :evidence :style :solid
              :backing {:path [:aozora-snapshot :aat]}}
             {:from :parser-process :to :parser-ir :label "compatibility gate"
              :role :validation :style :solid
              :backing {:path [:aat :parser-ir]}}
             {:from :parser-ir :to :manifest :label "identity + schema gate"
              :role :validation :style :thick
              :backing {:path [:parser-ir :manifest]}}
             {:from :manifest :to :outputs :label "validated materialization"
              :role :derived-view :style :solid
              :backing {:path [:manifest :tei]
                        :reachable-targets [:tei :rdf :iiif :tokenized
                                            :analysis :annotation]}}
             {:from :parser-process :to :aat-detail
              :label "current detail" :role :implementation-detail
              :style :dashed :backing {:stages [:aat]}}
             {:from :aat-detail :to :parser-ir
              :label "mapping compatibility" :role :implementation-detail
              :style :dashed :backing {:path [:aat :parser-ir]}}]
     :current-inset (get-in metadata [:figures figure :current-inset])
     :identity-spine [:source :parser-process :parser-ir :manifest :outputs]
         :primary-order [:source :parser-process :parser-ir :manifest :outputs]
         :footer "Stable contract shown prominently; current AAT implementation shown as a dashed inset."}]
    (validate-graph! context graph)))

(defn graphs []
  (let [validated (model/validated-model)]
    [(reproducibility-graph validated)
     (publication-graph validated)]))
```

The two final `validate-graph!` calls make endpoint and backing checks execute in
production rather than remaining test-only assertions.

- [ ] **Step 4: Run the focused model and projection tests**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test \
  --focus abc.tools.diagram.presentation-figures-test
cljfmt check src/abc/tools/diagram/presentation_figures.clj \
  test/abc/tools/diagram/presentation_figures_test.clj
```

Expected: both namespaces pass; cljfmt exits 0.

- [ ] **Step 5: Commit the two checked graph projections**

```sh
git add abc/src/abc/tools/diagram/presentation_figures.clj \
  abc/test/abc/tools/diagram/presentation_figures_test.clj
git commit -m "feat(diagram): project academic architecture figures"
```

---

### Task 3: Deterministic Graphviz DOT Renderer

**Files:**
- Create: `abc/src/abc/tools/diagram/graphviz.clj`
- Create: `abc/test/abc/tools/diagram/graphviz_test.clj`

**Interfaces:**
- Consumes: enriched graph maps from Task 2.
- Produces:
  - `escape-html [value] -> string`
  - `escape-dot [value] -> string`
  - `dot [graph] -> deterministic DOT string ending in one newline`.

- [ ] **Step 1: Write failing deterministic-renderer tests**

Create `abc/test/abc/tools/diagram/graphviz_test.clj`:

```clojure
(ns abc.tools.diagram.graphviz-test
  (:require [abc.tools.diagram.graphviz :as graphviz]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def sample
  {:id :sample :direction "LR" :title "Sample" :subtitle "Subtitle"
   :theme {:canvas "#000000" :text "#F5F7FA" :secondary "#A7B0BE"
           :identity "#48CAE4" :evidence "#F2B84B" :output "#7BC47F"
           :primary-size 30 :secondary-size 22 :stroke-width 2}
   :groups [{:id :b :label "Second"} {:id :a :label "First"}]
   :nodes [{:id :z :label "Zed" :role :output :group :b :backing {}}
           {:id :a :label "A \"quoted\" label" :subtitle "sub"
            :role :identity :group :a :backing {}}]
   :edges [{:from :z :to :a :label "later" :role :validation :style :dashed
            :backing {}}
           {:from :a :to :z :role :identity :style :thick :backing {}}]})

(deftest dot-is-stable-sorted-and-escaped
  (let [out (graphviz/dot sample)]
    (is (= out (graphviz/dot sample)))
    (is (str/starts-with? out "// GENERATED"))
    (is (< (.indexOf out "cluster_a") (.indexOf out "cluster_b")))
    (is (< (.indexOf out "\"a\"") (.indexOf out "\"z\"")))
    (is (str/includes? out "A &quot;quoted&quot; label"))
    (is (str/includes? out "label=<<TABLE"))
    (is (str/includes? out "style=\"dashed\""))
    (is (str/ends-with? out "\n"))))

(deftest dot-rejects-duplicate-node-ids
  (is (thrown-with-msg?
       clojure.lang.ExceptionInfo #"duplicate presentation node ids"
       (graphviz/dot (assoc sample :nodes
                            [{:id :a :label "A" :role :source}
                             {:id :a :label "B" :role :output}])))))
```

- [ ] **Step 2: Run the focused test and verify the missing namespace failure**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.graphviz-test
```

Expected: FAIL because `abc.tools.diagram.graphviz` does not exist.

- [ ] **Step 3: Implement deterministic DOT rendering**

Create `abc/src/abc/tools/diagram/graphviz.clj`. Implement these exact rendering rules:

```clojure
(ns abc.tools.diagram.graphviz
  (:require [clojure.string :as str]))

(defn escape-html [value]
  (-> (str value)
      (str/replace "&" "&amp;")
      (str/replace "\"" "&quot;")
      (str/replace "<" "&lt;")
      (str/replace ">" "&gt;")))

(defn escape-dot [value]
  (-> (str value)
      (str/replace "\\" "\\\\")
      (str/replace "\"" "\\\"")
      (str/replace "\n" "\\n")))

(defn- id-string [value]
  (-> (name value) (str/replace #"[^A-Za-z0-9_]" "_")))

(defn- attr-value [value]
  (if (and (map? value) (contains? value :html))
    (:html value)
    (str "\"" (escape-dot value) "\"")))

(defn- attrs [m]
  (str "["
       (str/join ","
                 (for [[k v] (sort-by (comp name key) m)]
                   (str (name k) "=" (attr-value v))))
       "]"))

(defn- role-style [theme role]
  (cond
    (#{:identity :identity-formula :coordinate-family} role)
    {:color (:identity theme) :fontcolor (:text theme)}
    (#{:evidence :validation} role)
    {:color (:evidence theme) :fontcolor (:text theme)}
    (= :output role)
    {:color (:output theme) :fontcolor (:text theme)}
    :else
    {:color (:secondary theme) :fontcolor (:text theme)}))

(defn- html-label [{:keys [label subtitle coordinates]} theme]
  (str "<"
       "<TABLE BORDER=\"0\" CELLBORDER=\"0\" CELLPADDING=\"3\">"
       "<TR><TD><FONT POINT-SIZE=\"" (:primary-size theme) "\"><B>"
       (escape-html label) "</B></FONT></TD></TR>"
       (when subtitle
         (str "<TR><TD><FONT COLOR=\"" (:secondary theme)
              "\" POINT-SIZE=\"" (:secondary-size theme) "\">"
              (escape-html subtitle) "</FONT></TD></TR>"))
       (apply str
              (for [{:keys [label]} coordinates]
                (str "<TR><TD ALIGN=\"LEFT\"><FONT COLOR=\""
                     (:secondary theme) "\" POINT-SIZE=\""
                     (:secondary-size theme) "\">· "
                     (escape-html label) "</FONT></TD></TR>")))
       "</TABLE>>"))

(defn- node-line [node theme]
  (let [style (role-style theme (:role node))]
    (str "    \"" (id-string (:id node)) "\" "
         (attrs (merge {:label {:html (html-label node theme)}
                        :shape (if (= :validation (:role node)) "diamond" "rect")
                        :style "rounded"
                        :penwidth (:stroke-width theme)}
                       style)) ";")))

(defn- edge-line [edge theme]
  (let [style (role-style theme (:role edge))]
    (str "  \"" (id-string (:from edge)) "\" -> \""
         (id-string (:to edge)) "\" "
         (attrs (merge
                 {:color (:color style)
                  :fontcolor (:secondary theme)
                  :fontname "Noto Sans CJK JP"
                  :fontsize (:secondary-size theme)
                  :penwidth (if (= :thick (:style edge)) 4
                                (:stroke-width theme))
                  :style (if (= :dashed (:style edge)) "dashed" "solid")}
                 (when-let [label (:label edge)]
                   {:label label}))) ";")))

(defn- id-collisions [nodes]
  (->> nodes
       (group-by (comp id-string :id))
       (keep (fn [[rendered matches]]
               (when (< 1 (count matches))
                 [rendered (mapv :id matches)])))
       (into {})))

(defn dot [{:keys [id direction theme groups nodes edges primary-order]}]
  (when-let [collisions (not-empty (id-collisions nodes))]
    (throw (ex-info "duplicate presentation node ids"
                    {:collisions collisions})))
  (let [grouped (group-by :group nodes)
        group-lines
        (mapcat
         (fn [{:keys [id label style]}]
           (concat
            [(str "  subgraph \"cluster_" (id-string id) "\" {")
             (str "    label=\"" (escape-dot label) "\";")
             (str "    color=\"" (:secondary theme) "\";")
             "    fontname=\"Noto Sans CJK JP\";"
             (str "    fontcolor=\"" (:secondary theme) "\";")
             (str "    fontsize=\"" (:secondary-size theme) "\";")
             (str "    style=\"rounded"
                  (when (= :dashed style) ",dashed") "\";")]
            (map #(node-line % theme)
                 (sort-by (comp id-string :id) (get grouped id)))
            ["  }"]))
         (sort-by (comp id-string :id) groups))
        ungrouped (sort-by (comp id-string :id) (get grouped nil))
        order-lines
        (for [[from to] (partition 2 1 primary-order)]
          (str "  \"" (id-string from) "\" -> \"" (id-string to)
               "\" [style=\"invis\",weight=\"100\"];"))]
    (str
     "// GENERATED by clojure -M:abc/presentation-diagrams — do not edit\n"
     "digraph \"" (id-string id) "\" {\n"
     "  graph " (attrs {:bgcolor "transparent"
                         :fontname "Noto Sans CJK JP"
                         :fontcolor (:text theme)
                         :nodesep "0.55" :ranksep "0.85"
                         :pad "0.05" :margin "0"
                         :rankdir direction :splines "polyline"}) ";\n"
     "  node " (attrs {:fontname "Noto Sans CJK JP"
                        :margin "0.20,0.14"}) ";\n"
     "  edge " (attrs {:arrowsize "0.75"}) ";\n"
     (str/join "\n" group-lines) "\n"
     (when (seq ungrouped)
       (str (str/join "\n" (map #(node-line % theme) ungrouped)) "\n"))
     (when (seq order-lines) (str (str/join "\n" order-lines) "\n"))
     (str/join "\n" (map #(edge-line % theme)
                           (sort-by (juxt (comp id-string :from)
                                          (comp id-string :to)
                                          :label)
                                    edges)))
     "\n}\n")))
```

The `attr-value` branch above emits HTML labels without surrounding quotes and
keeps all ordinary attributes quoted. Extend the sample assertion with
`(is (str/includes? out "label=<<TABLE"))` so invalid quoted HTML labels cannot
regress.

- [ ] **Step 4: Run focused renderer and projection tests**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-figures-test
cljfmt check src/abc/tools/diagram/graphviz.clj \
  test/abc/tools/diagram/graphviz_test.clj
```

Expected: both test namespaces pass; cljfmt exits 0.

- [ ] **Step 5: Commit the deterministic DOT renderer**

```sh
git add abc/src/abc/tools/diagram/graphviz.clj \
  abc/test/abc/tools/diagram/graphviz_test.clj
git commit -m "feat(diagram): render presentation graphs as stable DOT"
```

---

### Task 4: Self-Contained SVG Rendering and Validation

**Files:**
- Create: `abc/src/abc/tools/diagram/presentation_svg.clj`
- Create: `abc/test/abc/tools/diagram/presentation_svg_test.clj`

**Interfaces:**
- Consumes: DOT strings and graph metadata from Tasks 2–3; environment variables `ABC_GRAPHVIZ_DOT`, `ABC_FONTTOOLS_SUBSET`, and `ABC_PRESENTATION_FONT`.
- Produces:
  - `normalize-svg [graph raw-svg woff2-bytes] -> SVG string`
  - `svg-problems [svg-string] -> vector<string>`
  - `render-svg! [graph dot-string temp-dir] -> SVG string`.

- [ ] **Step 1: Write failing pure SVG normalization tests**

Create `abc/test/abc/tools/diagram/presentation_svg_test.clj`:

```clojure
(ns abc.tools.diagram.presentation-svg-test
  (:require [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-svg :as svg]
            [clojure.data.xml :as xml]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]))

(def raw-svg
  "<svg xmlns=\"http://www.w3.org/2000/svg\" width=\"200pt\" height=\"100pt\" viewBox=\"0 0 200 100\"><g id=\"graph0\"><text font-size=\"30\">Example</text><path fill=\"none\" stroke=\"#48CAE4\" d=\"M0,0 L10,10\"/></g></svg>")

(def graph
  {:id :test
   :title "Test Figure"
   :subtitle "Test subtitle"
   :description "Accessible description"
   :footer "Citation footer"
   :theme figures/theme})

(deftest normalization-produces-self-contained-slide-svg
  (let [out (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))
        root (xml/parse-str out)
        elements (filter :tag (tree-seq map? :content root))]
    (is (str/includes? out "viewBox=\"0 0 1920 1080\""))
    (is (str/includes? out "fill=\"#000000\""))
    (is (= svg/svg-namespace (xml/qname-uri (:tag root))))
    (is (every? #(= svg/svg-namespace (xml/qname-uri (:tag %))) elements))
    (is (some #(and (= "title" (xml/qname-local (:tag %)))
                    (= ["Test Figure"] (:content %)))
              elements))
    (is (some #(and (= "desc" (xml/qname-local (:tag %)))
                    (= ["Accessible description"] (:content %)))
              elements))
    (is (str/includes? out "data:font/woff2;base64,"))
    (is (str/includes? out "font-size=\"52\""))
    (is (= [] (svg/svg-problems out)))))

(deftest validation-rejects-external-resources-and-unknown-colors
  (is (seq (svg/svg-problems
            (str/replace
             (svg/normalize-svg graph raw-svg (.getBytes "woff2" "UTF-8"))
             "</svg>"
             "<image href=\"https://example.org/x.png\" fill=\"#FF00FF\"/></svg>")))))

(deftest malformed-graphviz-svg-is-actionable
  (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Graphviz SVG"
                        (svg/normalize-svg graph "not xml" (byte-array 0)))))

(deftest validator-rejects-a-null-namespace-svg-root
  (is (some #(str/includes? % "root is not in the SVG namespace")
            (svg/svg-problems
             "<svg viewBox=\"0 0 1920 1080\"><title>x</title></svg>"))))
```

- [ ] **Step 2: Run the focused test and confirm the missing namespace failure**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-svg-test
```

Expected: FAIL because `presentation-svg` does not exist.

- [ ] **Step 3: Implement SVG parsing, normalization, and structural checks**

Create `abc/src/abc/tools/diagram/presentation_svg.clj` with:

```clojure
(ns abc.tools.diagram.presentation-svg
  (:require [babashka.process :as process]
            [clojure.data.xml :as xml]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import [java.nio.file Files]
           [java.util Base64]))

(def allowed-colors
  #{"#000000" "#F5F7FA" "#A7B0BE" "#48CAE4" "#F2B84B" "#7BC47F"
    "none" "transparent"})

(def svg-namespace "http://www.w3.org/2000/svg")

(defn- parse-xml [label value]
  (try
    ;; Graphviz emits a remote SVG 1.1 DOCTYPE. The figure is self-contained;
    ;; remove it before parsing so validation never performs external lookup.
    (xml/parse-str (str/replace value #"(?is)<!DOCTYPE[^>]*>" ""))
    (catch Exception cause
      (throw (ex-info (str label " is not valid Graphviz SVG") {} cause)))))

(defn- parse-view-box [root]
  (let [parts (mapv parse-double
                    (str/split (get-in root [:attrs :viewBox]) #"\s+"))]
    (when-not (= 4 (count parts))
      (throw (ex-info "Graphviz SVG has no four-number viewBox"
                      {:viewBox (get-in root [:attrs :viewBox])})))
    parts))

(defn- element [tag attrs & content]
  (apply xml/element (xml/qname svg-namespace (name tag)) attrs content))

(defn- font-style [woff2-bytes]
  (str "@font-face{font-family:'Noto Sans CJK JP';font-style:normal;"
       "font-weight:400 700;src:url(data:font/woff2;base64,"
       (.encodeToString (Base64/getEncoder) woff2-bytes)
       ") format('woff2');}text{font-family:'Noto Sans CJK JP',sans-serif;}"))

(defn normalize-svg [graph raw-svg woff2-bytes]
  (let [raw-root (parse-xml "Graphviz SVG" raw-svg)
        [_ _ raw-width raw-height] (parse-view-box raw-root)
        graph-x 96.0 graph-y 190.0 graph-width 1728.0 graph-height 760.0
        scale (min (/ graph-width raw-width) (/ graph-height raw-height))
        tx (+ graph-x (/ (- graph-width (* raw-width scale)) 2.0))
        ty (+ graph-y (/ (- graph-height (* raw-height scale)) 2.0))
        graph-content (:content raw-root)
        root
        (element :svg
                 {:width "1920" :height "1080"
                  :viewBox "0 0 1920 1080"
                  :role "img"
                  :aria-labelledby "figure-title figure-description"}
                 (element :title {:id "figure-title"} (:title graph))
                 (element :desc {:id "figure-description"} (:description graph))
                 (element :style {} (font-style woff2-bytes))
                 (element :rect {:x "0" :y "0" :width "1920" :height "1080"
                                 :fill "#000000"})
                 (element :text {:x "96" :y "82" :fill "#F5F7FA"
                                 :font-size "52" :font-weight "700"
                                 :class "figure-title"}
                          (:title graph))
                 (element :text {:x "96" :y "126" :fill "#A7B0BE"
                                 :font-size "22" :font-weight "400"
                                 :class "figure-subtitle"}
                          (:subtitle graph))
                 (apply element :g
                        {:transform (format "translate(%.4f %.4f) scale(%.6f)"
                                            tx ty scale)}
                        graph-content)
                 (element :text {:x "96" :y "1034" :fill "#A7B0BE"
                                 :font-size "16" :font-weight "400"
                                 :class "figure-citation"}
                          (:footer graph)))]
    (str (xml/emit-str root) "\n")))

(defn- elements [root]
  (tree-seq #(and (map? %) (seq (:content %))) :content root))

(defn- tag-name [node]
  (some-> (:tag node) name))

(defn- attr [node wanted]
  (some (fn [[key value]]
          (when (= wanted (name key)) value))
        (:attrs node)))

(defn- font-size-problem [node]
  (when-let [raw-size (attr node "font-size")]
    (let [size (parse-double raw-size)
          class (attr node "class")]
      (cond
        (= "figure-title" class)
        (when (< size 52.0) "figure title is smaller than 52 px")

        (= "figure-citation" class)
        (when (not= size 16.0) "figure citation must be exactly 16 px")

        (< size 22.0)
        (str "presentation label is smaller than 22 px: " raw-size)))))

(defn svg-problems [svg-string]
  (try
    (let [root (parse-xml "presentation SVG" svg-string)
          nodes (filter map? (elements root))
          first-rect (first (filter #(= "rect" (tag-name %)) nodes))
          style-text (apply str (mapcat :content
                                       (filter #(= "style" (tag-name %)) nodes)))
          attribute-colors
          (for [node nodes
                key-name ["fill" "stroke" "color"]
                :let [value (attr node key-name)]
                :when value]
            (if (str/starts-with? value "#")
              (str/upper-case value)
              value))
          hex-colors (set (map str/upper-case
                               (re-seq #"#[0-9A-Fa-f]{6}" svg-string)))
          external (for [node nodes
                         [key value] (:attrs node)
                         :when (and (= "href" (name key))
                                    (not (str/starts-with? value "data:"))
                                    (not (str/starts-with? value "#")))]
                     value)
          font-problems (keep font-size-problem
                              (filter #(= "text" (tag-name %)) nodes))
          wrong-namespace
          (for [node nodes
                :when (and (:tag node)
                           (not= svg-namespace (xml/qname-uri (:tag node))))]
            (tag-name node))]
      (vec
       (concat
        (when-not (= "0 0 1920 1080" (attr root "viewBox"))
          ["presentation SVG must use viewBox 0 0 1920 1080"])
        (when-not (= svg-namespace (xml/qname-uri (:tag root)))
          ["presentation SVG root is not in the SVG namespace"])
        (for [tag wrong-namespace]
          (str "presentation element is not in the SVG namespace: " tag))
        (when-not (some #(= "title" (tag-name %)) nodes)
          ["presentation SVG needs title"])
        (when-not (some #(= "desc" (tag-name %)) nodes)
          ["presentation SVG needs description"])
        (when-not (and first-rect
                       (= "0" (attr first-rect "x"))
                       (= "0" (attr first-rect "y"))
                       (= "1920" (attr first-rect "width"))
                       (= "1080" (attr first-rect "height"))
                       (= "#000000" (str/upper-case (attr first-rect "fill"))))
          ["presentation SVG needs a full 1920x1080 black background"])
        (when-not (str/includes? style-text "data:font/woff2;base64,")
          ["presentation SVG must embed its WOFF2 font subset"])
        (for [color attribute-colors :when (not (allowed-colors color))]
          (str "unapproved SVG color " color))
        (for [color hex-colors :when (not (allowed-colors color))]
          (str "unapproved SVG color " color))
        (for [resource external] (str "external SVG resource " resource))
        font-problems)))
    (catch clojure.lang.ExceptionInfo ex [(ex-message ex)])))
```

- [ ] **Step 4: Implement external rendering and font subsetting**

Add to the same namespace:

```clojure
(defn- required-env [name]
  (or (System/getenv name)
      (throw (ex-info (str name " is required; run in the ABC Nix environment")
                      {:environment name}))))

(defn- run-command! [args]
  (let [{:keys [exit out err]} @(process/process args {:out :string :err :string})]
    (when-not (zero? exit)
      (throw (ex-info (str "presentation renderer command failed: "
                           (str/join " " args))
                      {:exit exit :stdout out :stderr err :command args})))
    out))

(defn- glyph-text [graph]
  (str/join "\n"
            (remove nil?
                    (concat [(:title graph) (:subtitle graph)
                             (:description graph) (:footer graph) "·"]
                            (map :label (:groups graph))
                            (mapcat (juxt :label :subtitle) (:nodes graph))
                            (mapcat (fn [node]
                                      (map :label (:coordinates node)))
                                    (:nodes graph))
                            (map :label (:edges graph))))))

(defn render-svg! [graph dot-string temp-dir]
  (let [dot-path (io/file temp-dir (str (name (:id graph)) ".dot"))
        raw-path (io/file temp-dir (str (name (:id graph)) ".raw.svg"))
        glyph-path (io/file temp-dir (str (name (:id graph)) ".glyphs.txt"))
        font-path (io/file temp-dir (str (name (:id graph)) ".woff2"))]
    (spit dot-path dot-string)
    (spit glyph-path (glyph-text graph))
    (run-command! [(required-env "ABC_GRAPHVIZ_DOT") "-Tsvg"
                   (.getPath dot-path) "-o" (.getPath raw-path)])
    (run-command! [(required-env "ABC_FONTTOOLS_SUBSET")
                   (required-env "ABC_PRESENTATION_FONT")
                   "--font-number=0"
                   (str "--text-file=" (.getPath glyph-path))
                   "--flavor=woff2"
                   "--layout-features=*"
                   (str "--output-file=" (.getPath font-path))])
    (let [svg (normalize-svg graph (slurp raw-path)
                             (Files/readAllBytes (.toPath font-path)))
          failures (svg-problems svg)]
      (when (seq failures)
        (throw (ex-info "rendered presentation SVG is invalid"
                        {:figure (:id graph) :problems failures})))
      svg)))
```

- [ ] **Step 5: Run pure SVG tests and Clojure formatting**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-svg-test
cljfmt check src/abc/tools/diagram/presentation_svg.clj \
  test/abc/tools/diagram/presentation_svg_test.clj
```

Expected: focused tests pass without requiring Graphviz because they exercise `normalize-svg` directly; cljfmt exits 0.

- [ ] **Step 6: Commit the self-contained SVG pipeline**

```sh
git add abc/src/abc/tools/diagram/presentation_svg.clj \
  abc/test/abc/tools/diagram/presentation_svg_test.clj
git commit -m "feat(diagram): normalize self-contained presentation SVGs"
```

---

### Task 5: Presentation Registry, CLI, and Pinned Nix Runtime

**Files:**
- Create: `abc/src/abc/tools/diagram/presentation_registry.clj`
- Create: `abc/test/abc/tools/diagram/presentation_registry_test.clj`
- Modify: `abc/deps.edn`
- Modify: `abc/flake.nix`

**Interfaces:**
- Consumes: `presentation-figures/graphs`, `graphviz/dot`, `presentation-svg/render-svg!`.
- Produces:
  - `registry -> vector<{:id :dot-path :svg-path}>`
  - `run! [{:keys [check?]}] -> {:ok? boolean :problems vector :drifts vector :wrote vector}`
  - CLI `clojure -M:abc/presentation-diagrams [--check]`.

- [ ] **Step 1: Write failing registry tests using injected pure renderers**

Create `abc/test/abc/tools/diagram/presentation_registry_test.clj`:

```clojure
(ns abc.tools.diagram.presentation-registry-test
  (:require [abc.test-fs :refer [with-temp-dir]]
            [abc.tools.diagram.presentation-registry :as registry]
            [babashka.fs :as fs]
            [clojure.test :refer [deftest is]]))

(def sample-graphs
  [{:id :one :title "One"} {:id :two :title "Two"}])

(deftest check-reports-every-stale-artifact
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          result (registry/run-with!
                  entries sample-graphs {:check? true}
                  {:dot-render #(str "dot-" (name (:id %)) "\n")
                   :svg-render (fn [g _ _] (str "svg-" (name (:id g)) "\n"))})]
      (is (false? (:ok? result)))
      (is (= 4 (count (:drifts result)))))))

(deftest invalid-second-figure-writes-neither-figure
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          result (registry/run-with!
                  entries sample-graphs {:check? false}
                  {:dot-render #(str "dot-" (name (:id %)) "\n")
                   :svg-render (fn [g _ _]
                                 (when (= :two (:id g))
                                   (throw (ex-info "bad second figure" {})))
                                 "svg-one\n")})]
      (is (false? (:ok? result)))
      (is (empty? (fs/list-dir dir))))))

(deftest successful-write-then-check-is-current
  (with-temp-dir [dir]
    (let [entries [{:id :one :dot-path (str (fs/file dir "one.dot"))
                    :svg-path (str (fs/file dir "one.svg"))}
                   {:id :two :dot-path (str (fs/file dir "two.dot"))
                    :svg-path (str (fs/file dir "two.svg"))}]
          renderers {:dot-render #(str "dot-" (name (:id %)) "\n")
                     :svg-render (fn [g _ _]
                                   (str "svg-" (name (:id g)) "\n"))}]
      (is (:ok? (registry/run-with! entries sample-graphs
                                    {:check? false} renderers)))
      (is (:ok? (registry/run-with! entries sample-graphs
                                    {:check? true} renderers))))))
```

- [ ] **Step 2: Run the focused test and verify the missing registry failure**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-registry-test
```

Expected: FAIL because `presentation-registry` does not exist.

- [ ] **Step 3: Implement render-all-before-replace registry behavior**

Create `abc/src/abc/tools/diagram/presentation_registry.clj`:

```clojure
(ns abc.tools.diagram.presentation-registry
  (:refer-clojure :exclude [run!])
  (:require [abc.tools.diagram.graphviz :as graphviz]
            [abc.tools.diagram.presentation-figures :as figures]
            [abc.tools.diagram.presentation-svg :as svg]
            [babashka.fs :as fs])
  (:import [java.nio.file Files StandardCopyOption]
           [java.nio.file.attribute FileAttribute]))

(def registry
  [{:id :reproducibility
    :dot-path "docs/figures/soranoha-reproducibility-architecture.dot"
    :svg-path "docs/figures/soranoha-reproducibility-architecture.svg"}
   {:id :publication
    :dot-path "docs/figures/soranoha-publication-pipeline.dot"
    :svg-path "docs/figures/soranoha-publication-pipeline.svg"}])

(defn- atomic-spit! [target content]
  (let [parent (fs/path (fs/parent target))]
    (fs/create-dirs parent)
    (let [temp (Files/createTempFile parent ".presentation-" ".tmp"
                                     (make-array FileAttribute 0))]
      (try
        (spit (.toFile temp) content)
        (Files/move temp (fs/path target)
                    (into-array StandardCopyOption
                                [StandardCopyOption/ATOMIC_MOVE
                                 StandardCopyOption/REPLACE_EXISTING]))
        (finally
          (Files/deleteIfExists temp))))))

(defn- drift [path expected]
  (when (or (not (fs/regular-file? path))
            (not= expected (slurp (fs/file path))))
    (str "DRIFT: " path
         " is stale; run `clojure -M:abc/presentation-diagrams`")))

(defn run-with! [entries graphs {:keys [check?]}
                 {:keys [dot-render svg-render]}]
  (try
    (fs/with-temp-dir [temp {}]
      (let [by-id (into {} (map (juxt :id identity)) graphs)
            rendered
            (mapv (fn [{:keys [id] :as entry}]
                    (let [graph (or (by-id id)
                                    (throw (ex-info "presentation registry has no graph"
                                                    {:id id})))
                          dot (dot-render graph)
                          svg (svg-render graph dot temp)]
                      (assoc entry :dot dot :svg svg)))
                  entries)]
        (if check?
          (let [drifts (vec
                        (mapcat (fn [{:keys [dot-path svg-path dot svg]}]
                                  (keep identity [(drift dot-path dot)
                                                  (drift svg-path svg)]))
                                rendered))]
            {:ok? (empty? drifts) :problems [] :drifts drifts :wrote []})
          (do
            (doseq [{:keys [dot-path svg-path dot svg]} rendered]
              (atomic-spit! dot-path dot)
              (atomic-spit! svg-path svg))
            {:ok? true :problems [] :drifts []
             :wrote (vec (mapcat (juxt :dot-path :svg-path) rendered))}))))
    (catch Exception ex
      {:ok? false
       :problems [(or (ex-message ex) (str ex))]
       :exception ex :drifts [] :wrote []})))

(defn run! [options]
  (run-with! registry (figures/graphs) options
             {:dot-render graphviz/dot
              :svg-render svg/render-svg!}))

(defn -main [& args]
  (let [unknown (remove #{"--check"} args)]
    (when (seq unknown)
      (binding [*out* *err*]
        (println "usage: clojure -M:abc/presentation-diagrams [--check]"))
      (System/exit 2))
    (let [{:keys [ok? problems drifts wrote]}
          (run! {:check? (boolean (some #{"--check"} args))})]
      (binding [*out* *err*]
        (doseq [problem problems] (println "ERROR:" problem))
        (doseq [item drifts] (println item)))
      (doseq [path wrote] (println "wrote" path))
      (when (and ok? (empty? wrote))
        (println "all presentation diagrams current"))
      (System/exit (if ok? 0 1)))))
```

The write test expects no target artifacts after render failure. The temporary directory itself may contain candidates before cleanup; assert only the target paths are absent rather than asserting the temp root is empty if `fs/with-temp-dir` creates bookkeeping entries.

- [ ] **Step 4: Add the Clojure alias**

Add beside `:abc/diagrams` in `abc/deps.edn`:

```clojure
:abc/presentation-diagrams
{:main-opts ["-m" "abc.tools.diagram.presentation-registry"]}
```

- [ ] **Step 5: Pin the renderer in the ABC app and development shell**

In the `apps` system-local `let` in `abc/flake.nix`, add an isolated Python environment and a dedicated caller-working-directory launcher. Do not modify `mkCljLauncher`; its existing callers retain their current working-directory behavior and therefore have no shared-helper blast radius.

```nix
presentationFontTools = pkgs.python3.withPackages (ps: [
  ps.fonttools
  ps.brotli
]);
presentationLauncher = pkgs.writeShellScript "abc-presentation-diagrams" ''
  set -euo pipefail
  export HOME="${cljDepsCache}"
  export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
  export CLJ_CONFIG="${cljDepsCache}/.clojure"
  export GITLIBS="${cljDepsCache}/.gitlibs"
  export CLJ_CACHE="$(mktemp -d)"
  if [ -f abc/deps.edn ] && [ -f abc/docs/architecture-stages.edn ]; then
    cd abc
  elif [ -f deps.edn ] && [ -f docs/architecture-stages.edn ]; then
    :
  else
    echo "presentation-diagrams: run from the monorepo root or abc/" >&2
    exit 2
  fi
  export ABC_GRAPHVIZ_DOT="${pkgs.graphviz}/bin/dot"
  export ABC_FONTTOOLS_SUBSET="${presentationFontTools}/bin/pyftsubset"
  export ABC_PRESENTATION_FONT="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc"
  export ABC_PRESENTATION_FONT_DIR="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk"
  export DOTFONTPATH="$ABC_PRESENTATION_FONT_DIR"
  exec ${pkgs.clojure}/bin/clojure -M:abc/presentation-diagrams "$@"
'';
```

Add this app next to `validate-design-bundle`:

```nix
presentation-diagrams = {
  type = "app";
  program = toString presentationLauncher;
  meta.description = "Generate or drift-check academic presentation SVG diagrams";
};
```

In the `devShells` system-local `let`, define the same `presentationFontTools` expression. Add `graphviz`, `noto-fonts-cjk-sans`, and `presentationFontTools` to `devShells.default.packages`. Add these shell variables to the same shell:

```nix
ABC_GRAPHVIZ_DOT = "${pkgs.graphviz}/bin/dot";
ABC_FONTTOOLS_SUBSET = "${presentationFontTools}/bin/pyftsubset";
ABC_PRESENTATION_FONT = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc";
ABC_PRESENTATION_FONT_DIR = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk";
DOTFONTPATH = "${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk";
```

- [ ] **Step 6: Add the Nix integration test before committing generated figures**

Add `presentation-diagram-renderer` in `abc/flake.nix` checks. It exercises rendering into a writable copy but does not yet require committed artifact equality:

First add this binding beside `cljDepsCache` in the checks `let`:

```nix
presentationFontTools = pkgs.python3.withPackages (ps: [
  ps.fonttools
  ps.brotli
]);
```

```nix
presentation-diagram-renderer =
  pkgs.runCommand "abc-presentation-diagram-renderer"
    {
      nativeBuildInputs = [
        pkgs.clojure
        pkgs.graphviz
        pkgs.imagemagick
        pkgs.librsvg
        pkgs.noto-fonts-cjk-sans
        presentationFontTools
      ];
    }
    ''
      cp -R ${./.} source
      chmod -R u+w source
      cd source
      export HOME="${cljDepsCache}"
      export JAVA_TOOL_OPTIONS="-Duser.home=${cljDepsCache}"
      export CLJ_CONFIG="$HOME/.clojure"
      export CLJ_CACHE="$TMPDIR/cp-cache"
      export XDG_CONFIG_HOME="$TMPDIR/xdg-config"
      export GITLIBS="$HOME/.gitlibs"
      export ABC_GRAPHVIZ_DOT="${pkgs.graphviz}/bin/dot"
      export ABC_FONTTOOLS_SUBSET="${presentationFontTools}/bin/pyftsubset"
      export ABC_PRESENTATION_FONT="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc"
      export ABC_PRESENTATION_FONT_DIR="${pkgs.noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk"
      export DOTFONTPATH="$ABC_PRESENTATION_FONT_DIR"
      clojure -M:abc/presentation-diagrams
      for svg in docs/figures/*.svg; do
        png="$TMPDIR/$(basename "$svg" .svg).png"
        rsvg-convert --width 1920 --height 1080 "$svg" --output "$png"
        test "$(magick identify -format '%wx%h' "$png")" = "1920x1080"
        test "$(magick identify -format '%k' "$png")" -gt 1
      done
      mkdir -p "$out"
      cp docs/figures/*.dot docs/figures/*.svg "$out"/
    '';
```

- [ ] **Step 7: Run focused tests and the pinned renderer check**

```sh
git add abc/deps.edn abc/flake.nix \
  abc/src/abc/tools/diagram/presentation_registry.clj \
  abc/test/abc/tools/diagram/presentation_registry_test.clj
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-registry-test
nix build .#checks.x86_64-linux.presentation-diagram-renderer --no-link
nixfmt --check flake.nix
cljfmt check src/abc/tools/diagram/presentation_registry.clj \
  test/abc/tools/diagram/presentation_registry_test.clj
```

Expected: all commands exit 0; the Nix derivation produces four non-empty artifacts in its output.

- [ ] **Step 8: Commit registry and pinned rendering integration**

```sh
git commit -m "feat(diagram): add pinned presentation SVG registry"
```

---

### Task 6: Generate and Review the Two Presentation Proofs

**Files:**
- Create generated: `abc/docs/figures/soranoha-reproducibility-architecture.dot`
- Create generated: `abc/docs/figures/soranoha-reproducibility-architecture.svg`
- Create generated: `abc/docs/figures/soranoha-publication-pipeline.dot`
- Create generated: `abc/docs/figures/soranoha-publication-pipeline.svg`
- Modify only if a proof fails the approved checklist: `abc/docs/architecture-presentation.edn`, `abc/src/abc/tools/diagram/presentation_figures.clj`, or theme tokens in that namespace.

**Interfaces:**
- Consumes: the pinned app and complete presentation registry.
- Produces: four reviewed, committed generated artifacts.

- [ ] **Step 1: Generate both figures through the pinned root app**

Run from the monorepo root:

```sh
nix run .#abc-presentation-diagrams
nix run .#abc-presentation-diagrams -- --check
```

Expected: the first command reports four written paths; the second prints `all presentation diagrams current`.

- [ ] **Step 2: Rasterize temporary 1920×1080 proof images without committing them**

Run:

```sh
nix shell nixpkgs#librsvg -c rsvg-convert \
  --width 1920 --height 1080 \
  abc/docs/figures/soranoha-reproducibility-architecture.svg \
  --output /tmp/soranoha-reproducibility-architecture.png
nix shell nixpkgs#librsvg -c rsvg-convert \
  --width 1920 --height 1080 \
  abc/docs/figures/soranoha-publication-pipeline.svg \
  --output /tmp/soranoha-publication-pipeline.png
```

Expected: both PNG files are exactly 1920×1080 and non-empty.

- [ ] **Step 3: Perform the required visual proof checkpoint**

Inspect both PNGs at original size with the image-viewing tool. Report each checklist item separately:

- title and main labels readable at 100 percent;
- clear left-to-right entry point;
- no connector crosses a label;
- no clipped glyph, group, arrow, title, subtitle, or footer;
- at most eight primary conceptual units;
- the five coordinate families are subordinate to the central identity argument;
- the publication figure's stable path is dominant;
- the dashed current-parser inset is visible but subordinate;
- balanced whitespace and consistent alignment;
- only the approved palette is visible; and
- both figures look like one professional set.

For the primary-unit count, the five coordinate-family nodes are subordinate
items inside the single **15-coordinate identity contract** cluster; they count
as one primary unit, not five. Figure 1 therefore has four primary units:
sources/evidence, identity contract, validated manifest, and scholarly views.

If any item fails, do not patch generated DOT/SVG. Change only presentation metadata, figure projection data, or shared theme/layout tokens; rerun Steps 1–3. Stop at this checkpoint and show the user the proofs or a concise visual review before committing.

- [ ] **Step 4: Run semantic and structural checks after visual approval**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test \
  --focus abc.tools.diagram.presentation-figures-test \
  --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-svg-test \
  --focus abc.tools.diagram.presentation-registry-test
clojure -M:abc/diagrams --check
cd ..
nix run .#abc-presentation-diagrams -- --check
```

Expected: all commands exit 0; existing Mermaid diagrams remain current.

- [ ] **Step 5: Commit the user-approved generated proofs and any source-level style refinements**

```sh
git add abc/docs/figures \
  abc/docs/architecture-presentation.edn \
  abc/src/abc/tools/diagram/presentation_figures.clj
git commit -m "docs(diagram): add academic presentation architecture figures"
```

---

### Task 7: Drift Gate, Identity Exclusion, Documentation, and Full Verification

**Files:**
- Modify: `abc/flake.nix`
- Modify: `abc/docs/architecture.md`
- Modify: `abc/test/abc/tools/schema_test.clj`
- Test generated: all four `abc/docs/figures/*.{dot,svg}` files.

**Interfaces:**
- Consumes: committed reviewed artifacts from Task 6.
- Produces: `checks.<system>.presentation-diagram-drift`, architecture documentation links, explicit identity exclusion regression, and green root validation.

- [ ] **Step 1: Write the failing manifest-identity exclusion regression**

Add inside `manifest-schema-requires-tokenizer-profile-hash-coordinate-test` in `abc/test/abc/tools/schema_test.clj`, after the valid-manifest assertion:

```clojure
(is (seq (schema/validation-errors
          manifest-schema
          (assoc-in manifest
                    ["manifest_identity_object" "presentation_diagram"]
                    "docs/figures/soranoha-publication-pipeline.svg")))
    "presentation metadata and figures are not manifest identity coordinates")
```

- [ ] **Step 2: Run the schema test and verify the exclusion already bites**

```sh
cd abc
bin/kaocha --focus abc.tools.schema-test/manifest-schema-requires-tokenizer-profile-hash-coordinate-test
```

Expected: PASS because the manifest identity schema is closed. This is a characterization test of the required non-identity boundary, not a production schema change.

- [ ] **Step 3: Replace the renderer smoke check with the committed drift check**

Rename `presentation-diagram-renderer` to `presentation-diagram-drift` in `abc/flake.nix` and replace its final generation/copy lines with:

```nix
clojure -M:abc/presentation-diagrams --check
for svg in docs/figures/*.svg; do
  png="$TMPDIR/$(basename "$svg" .svg).png"
  rsvg-convert --width 1920 --height 1080 "$svg" --output "$png"
  test "$(magick identify -format '%wx%h' "$png")" = "1920x1080"
  test "$(magick identify -format '%k' "$png")" -gt 1
done
mkdir -p "$out"
echo "Academic presentation DOT and SVG artifacts are current." > "$out/result.txt"
```

Keep Graphviz, Noto, the combined fonttools+Brotli environment, librsvg,
ImageMagick, the offline Clojure cache, and all renderer environment variables
unchanged. Rasterization is a permanent drift-gate responsibility, not only a
manual-proof step.

- [ ] **Step 4: Link the figures and document their distinct purpose**

Add after the generated Mermaid link in `abc/docs/architecture.md`:

```markdown
For academic presentations, two source-validated SVG projections provide a
larger-type, audience-facing view of the same contracts:

- [Soranoha Reproducibility Architecture](figures/soranoha-reproducibility-architecture.svg)
- [Soranoha Publication Pipeline](figures/soranoha-publication-pipeline.svg)

They are generated presentation views, not additional architecture sources.
Regenerate them from the monorepo root with
`nix run .#abc-presentation-diagrams`; verify drift with
`nix run .#abc-presentation-diagrams -- --check`. The pipeline figure keeps the
stable Parser-IR publication contract prominent and marks the current AAT path
as implementation detail so parser consolidation can simplify it honestly.
```

- [ ] **Step 5: Run all focused diagram and identity tests**

```sh
cd abc
bin/kaocha --focus abc.tools.diagram.presentation-model-test \
  --focus abc.tools.diagram.presentation-figures-test \
  --focus abc.tools.diagram.graphviz-test \
  --focus abc.tools.diagram.presentation-svg-test \
  --focus abc.tools.diagram.presentation-registry-test \
  --focus abc.tools.diagram.registry-test \
  --focus abc.tools.schema-test/manifest-schema-requires-tokenizer-profile-hash-coordinate-test
clojure -M:abc/adr-governance
clojure -M:abc/diagrams --check
cd ..
nix run .#abc-presentation-diagrams -- --check
```

Expected: all commands exit 0; both old and new diagram registries are current.

- [ ] **Step 6: Run Clojure, Nix, and full monorepo verification**

```sh
cd abc
bin/kaocha
nix build .#checks.x86_64-linux.clj-kondo --no-link
nix build .#checks.x86_64-linux.presentation-diagram-drift --no-link
nix build .#checks.x86_64-linux.diagram-drift --no-link
cd ..
just nix-format-check
just validate-migration
```

Expected:

- Kaocha reports zero failures and errors.
- All three Nix builds succeed.
- Nix formatting passes.
- `just validate-migration` ends with `all checks passed!`.

- [ ] **Step 7: Verify generated-source and identity boundaries directly**

Run:

```sh
rg -n 'presentation|figures/' abc/src/abc/tools/manifest.clj \
  abc/schemas/manifest.schema.json
git diff --exit-code -- abc/docs/figures
git status --short
```

Expected:

- The first command returns no production identity reference. The test's
  `presentation_diagram` mutation exists only under `abc/test` and therefore is
  not in this search scope.
- Generated figures have no post-check diff.
- Status contains only the intended Task 7 changes.

- [ ] **Step 8: Commit final gates and documentation**

```sh
git add abc/flake.nix abc/docs/architecture.md abc/test/abc/tools/schema_test.clj
git commit -m "build(diagram): enforce presentation SVG drift"
```

---

## Final Review Checklist

- [ ] Both titles exactly match the approved concise titles.
- [ ] Both SVGs have an explicit black 1920×1080 canvas.
- [ ] Every element QName resolves to the SVG namespace, including the root.
- [ ] The pinned Nix gate rasterizes both SVGs to non-blank 1920×1080 PNGs.
- [ ] Both SVGs embed Noto Sans CJK JP WOFF2 data and preserve text elements.
- [ ] Figure 1 shows all fifteen live identity coordinates in five families.
- [ ] Figure 1 distinguishes ArtifactID from content hash.
- [ ] Figure 2 keeps the stable source → parser process → Parser-IR → manifest → outputs path dominant.
- [ ] Figure 2 labels the AAT path as **Current producer implementation**.
- [ ] Every node, citation, connector, and edge endpoint passes canonical validation.
- [ ] Existing Mermaid diagrams remain unchanged and current.
- [ ] Presentation artifacts are excluded from manifest identity.
- [ ] Generated DOT/SVG files are byte-current under pinned Nix inputs.
- [ ] The human proof checkpoint is explicitly approved.
- [ ] Full Kaocha and `just validate-migration` pass.
