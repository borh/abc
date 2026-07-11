(ns abc.sim.model
  "Ground-truth upstream model and total event algebra for the aozora
  evolution simulation harness. Pure. The oracle reads applied intents;
  inapplicable events are recorded no-ops so sequence shrinking always
  yields valid histories.
  Spec: docs/superpowers/specs/2026-07-11-aozora-evolution-simulation-testing-design.md"
  (:require [clojure.string :as string]))

(def person-fields
  [:family_name :given_name :family_name_reading :given_name_reading
   :family_name_sort :given_name_sort :family_name_romaji :given_name_romaji
   :date_of_birth :date_of_death :copyright_expired])

(def work-fields
  [:title :title_reading :sort_reading :ndc :orthography :copyright_expired
   :available :modified :edition_title :edition_publisher])

(defn base-person []
  {:family_name "旧" :given_name "人"
   :family_name_reading "きゅう" :given_name_reading "ひと"
   :family_name_sort "きゆう" :given_name_sort "ひと"
   :family_name_romaji "Old" :given_name_romaji "Person"
   :date_of_birth "1900-01-01" :date_of_death "1970-01-01"
   :copyright_expired true})

(defn base-work [wid]
  {:title (str "作品" wid) :title_reading "てすとさくひん"
   :sort_reading "てすとさくひん" :ndc "NDC 913" :orthography "新字新仮名"
   :copyright_expired true :available "1997-10-29" :modified "2022-07-16"
   :edition_title "テスト作品" :edition_publisher "テスト出版社"})

(defn- fmt6 [n] (format "%06d" n))

(defn fresh-pid [m] (fmt6 (+ 900000 (:next-id m))))
(defn fresh-wid [m] (fmt6 (+ 800000 (:next-id m))))

(defn bootstrap
  "n works numbered 000101.., each with a distinct sole author 000001..,
  relation 著者."
  [n]
  {:persons (into (sorted-map)
                  (for [i (range 1 (inc n))]
                    [(fmt6 i) (assoc (base-person) :given_name (str "人" i))]))
   :works (into (sorted-map)
                (for [i (range 1 (inc n))] [(fmt6 (+ 100 i)) (base-work (fmt6 (+ 100 i)))]))
   :edges (into (sorted-map)
                (for [i (range 1 (inc n))] [[(fmt6 (+ 100 i)) "著者"] #{(fmt6 i)}]))
   :next-id 1})

(defn check-invariants!
  "Throws ex-info on violation; nil otherwise. Harness defect if it fires."
  [m event]
  (doseq [[[wid rel] pids] (:edges m)]
    (when (or (not (contains? (:works m) wid))
              (not (string? rel))
              (string/blank? rel)
              (empty? pids)
              (not-every? #(contains? (:persons m) %) pids))
      (throw (ex-info "model invariant violated"
                      {:edge [wid rel] :pids pids :event event}))))
  nil)

(defn- valid-relation? [relation]
  (and (string? relation) (not (string/blank? relation))))

(defn- no-op [m] {:model m :applied nil})

(defn- applied [m' e edge-keys]
  {:model (update m' :next-id inc)
   :applied {:intent (:event/type e) :event e :edges (vec edge-keys)}})

(defmulti apply-event* (fn [_m e] (:event/type e)))

(defmethod apply-event* :add-work-with-edge
  [m {:keys [wid work pid person relation] :as e}]
  (if (or (contains? (:works m) wid) (contains? (:persons m) pid)
          (not (map? work)) (not (map? person)) (not (valid-relation? relation)))
    (no-op m)
    (applied (-> m
                 (assoc-in [:works wid] work)
                 (assoc-in [:persons pid] person)
                 (assoc-in [:edges [wid relation]] #{pid}))
             e [[wid relation]])))

(defmethod apply-event* :add-person-with-edge
  [m {:keys [pid person wid relation] :as e}]
  (if (or (contains? (:persons m) pid) (not (contains? (:works m) wid))
          (not (map? person)) (not (valid-relation? relation)))
    (no-op m)
    (applied (-> m
                 (assoc-in [:persons pid] person)
                 (update-in [:edges [wid relation]] (fnil conj #{}) pid))
             e [[wid relation]])))

(defmethod apply-event* :add-person
  [m {:keys [pid person] :as e}]
  (if (or (contains? (:persons m) pid) (not (map? person)))
    (no-op m)
    (applied (assoc-in m [:persons pid] person) e [])))

(defmethod apply-event* :add-work
  [m {:keys [wid work] :as e}]
  (if (or (contains? (:works m) wid) (not (map? work)))
    (no-op m)
    (applied (assoc-in m [:works wid] work) e [])))

(defmethod apply-event* :add-edge
  [m {:keys [wid relation pid] :as e}]
  (if (or (not (contains? (:works m) wid))
          (not (contains? (:persons m) pid))
          (not (valid-relation? relation))
          (contains? (get-in m [:edges [wid relation]] #{}) pid))
    (no-op m)
    (applied (update-in m [:edges [wid relation]] (fnil conj #{}) pid)
             e [[wid relation]])))

(defmethod apply-event* :remove-edge
  [m {:keys [wid relation pid] :as e}]
  (let [k [wid relation]
        pids (get-in m [:edges k] #{})]
    (if-not (contains? pids pid)
      (no-op m)
      (let [pids' (disj pids pid)
            m' (if (empty? pids')
                 (update m :edges dissoc k)
                 (assoc-in m [:edges k] pids'))]
        (applied m' e [k])))))

(defmethod apply-event* :edit-person
  [m {:keys [pid field value] :as e}]
  (if (or (not (contains? (:persons m) pid))
          (= value (get-in m [:persons pid field])))
    (no-op m)
    (applied (assoc-in m [:persons pid field] value) e [])))

(defmethod apply-event* :edit-work
  [m {:keys [wid field value] :as e}]
  (if (or (not (contains? (:works m) wid))
          (= value (get-in m [:works wid field])))
    (no-op m)
    (applied (assoc-in m [:works wid field] value) e [])))

(defmethod apply-event* :remove-work
  [m {:keys [wid] :as e}]
  (if-not (contains? (:works m) wid)
    (no-op m)
    (let [edge-keys (filter #(= wid (first %)) (keys (:edges m)))]
      (applied (-> m
                   (update :works dissoc wid)
                   (update :edges #(apply dissoc % edge-keys)))
               e edge-keys))))

(defmethod apply-event* :default [m _e] (no-op m))

(defn apply-event [m e] (apply-event* m e))

(defn fold-history
  [{:keys [initial events]}]
  (reduce (fn [{:keys [states applied]} event]
            (let [m (peek states)
                  {m' :model intent :applied} (apply-event m event)]
              (check-invariants! m' event)
              {:states (conj states m')
               :applied (if intent (conj applied intent) applied)}))
          {:states [initial] :applied []}
          events))
