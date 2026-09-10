(ns soranoha.snh.schema-walk
  "One way for a gate to read a protocol schema.

  Several tests compare something the build states with what a schema will
  accept, and each needs the same thing first: every subschema, wherever it
  sits. Written once because the recursion is easy to get subtly wrong, and a
  copy that forgets to descend into a vector misses every `oneOf` branch while
  still passing.")

(defn nodes
  "Every subschema of `schema`, itself included, in no particular order."
  [schema]
  (cond
    (map? schema) (cons schema (mapcat nodes (vals schema)))
    (sequential? schema) (mapcat nodes schema)
    :else nil))

(defn enums
  "Every closed vocabulary `schema` defines, as value sets."
  [schema]
  (keep #(when (sequential? (get % "enum")) (set (get % "enum"))) (nodes schema)))

(defn objects
  "Every object `schema` defines, as {:required :properties} name sets."
  [schema]
  (keep #(when-let [properties (get % "properties")]
           {:required (set (get % "required"))
            :properties (set (keys properties))})
        (nodes schema)))
