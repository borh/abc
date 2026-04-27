(ns abc.test-utils
  (:require [clojure.pprint :as pprint]
            [clojure.test :as t]
            [malli.core :as m]
            [malli.error :as me]))

(defmethod t/assert-expr 'schema-valid
  [msg [_ schema data registry]]
  (let [schema [:schema {:registry registry} schema]]
    `(let [is-valid?# (m/validate ~schema ~data)]
       (t/do-report {:actual   ~data
                     :expected (-> ~schema
                                   (m/explain ~data)
                                   (me/humanize))
                     :message  ~msg
                     :type     (if is-valid?# :pass :fail)}))))

(defn schema-validate
  [s v registry]
  (let [r (me/humanize (m/explain [:schema {:registry registry} s] v))]
    (if (empty? r)
      true
      (do (pprint/pprint {:error r #_:input #_v}) false))))
