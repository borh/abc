(ns abc.tools.diagram.figure-theme
  "Canonical visual language for Graphviz presentation figures.

  This is the retained style contract for presentation-quality figures
  after the JADH 2026 rendering pipeline was retired: the frozen figures
  under docs/figures/ were rendered with exactly these tokens. Scope is
  Graphviz presentation figures only; the committed Mermaid diagrams keep
  their own ADR-governed conventions.")

(def presentation
  "Semantic theme consumed by abc.tools.diagram.graphviz/dot.

  Roles carry meaning beyond color: validation nodes additionally render
  as diamonds, so role distinctions survive grayscale reproduction."
  {:canvas "#000000"
   :text "#F5F7FA"
   :secondary "#A7B0BE"
   :identity "#48CAE4"
   :evidence "#F2B84B"
   :output "#7BC47F"
   :font-family "Noto Sans CJK JP"
   :primary-size 34
   :secondary-size 24
   :stroke-width 2})
