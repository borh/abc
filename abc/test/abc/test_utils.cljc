(ns abc.test-utils
  (:require [clojure.pprint :as pprint]
            [clojure.test :as t]
            [malli.core :as m]
            [malli.error :as me]))

(defmethod t/assert-expr 'schema-valid
  [msg [_ schema data]]
  `(let [is-valid?# (m/validate ~schema ~data)]
     (t/do-report {:actual   ~data
                   :expected (-> ~schema (m/explain ~data) (me/humanize))
                   :message  ~msg
                   :type     (if is-valid?# :pass :fail)})))

(defn schema-validate
  [s v]
  (let [r (me/humanize (m/explain s v))]
    (if (empty? r)
      true
      (do (pprint/pprint {:error r}) false))))
