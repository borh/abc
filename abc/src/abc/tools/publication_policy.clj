(ns abc.tools.publication-policy
  "Fail-closed release policy for publication-facing commands.

  Rendering functions used by development fixture validation remain separate;
  callers that can create release artifacts must cross assert-release-allowed!."
  (:require [abc.tools.files :as files]))

(def policy-path "data/publication-policy.edn")

(defn rights-publication-state
  ([] (rights-publication-state policy-path))
  ([path] (:rights-publication (files/read-edn path))))

(defn assert-release-allowed!
  "Return :ok only after the rights migration enables assessed publication.
  Every other or missing state fails closed and names the policy reason."
  ([] (assert-release-allowed! policy-path))
  ([path]
   (let [state (rights-publication-state path)]
     (if (= :assessment-required state)
       :ok
       (throw (ex-info
               (str "release publication blocked by rights policy: " state)
               {:reason (or state :missing-rights-publication-policy)
                :policy-path path}))))))
