(ns thesis.clausal-normal-form
  (:require
    [clojure.string :as s]
    [thesis.algebra :refer [expr-transform expr-atom? expr-clauses expr?]]
    [thesis.simplify :refer [simplify-expression]]
    [clojure.set :as set]))

(defn- de-implify
  [predicate]
  (expr-transform [:implies "p" "q"] [:or [:not "p"] "q"] predicate))

(defn- move-nots-inward
  [predicate]
  (let [transformed (->> predicate
                        (expr-transform [:not [:and "p" "q"]] [:or [:not "p"] [:not "q"]])
                        (expr-transform [:not [:or "p" "q"]] [:and [:not "p"] [:not "q"]]))]
    (if (not= predicate transformed)
      (move-nots-inward transformed)
      (expr-transform [:not [:not "x"]] "x" transformed))))

(defn- flip-not-expressions [predicate]
  (->> predicate
       (expr-transform [:not [:> "x" "y"]] [:<= "x" "y"])
       (expr-transform [:not [:< "x" "y"]] [:>= "x" "y"])
       (expr-transform [:not [:>= "x" "y"]] [:< "x" "y"])
       (expr-transform [:not [:<= "x" "y"]] [:> "x" "y"])
       (expr-transform [:not [:== "x" "y"]] [:!= "x" "y"])
       (expr-transform [:not [:!= "x" "y"]] [:== "x" "y"])))

(defn- distribute-ors
  [predicate]
  (expr-transform [:or "p" [:and "q" "r"]] [:and [:or "p" "q"] [:or "p" "r"]] predicate))

(defn- negation-normal-form [predicate]
  (-> predicate de-implify move-nots-inward flip-not-expressions))

; See https://en.wikipedia.org/wiki/Conjunctive_normal_form#Converting_from_first-order_logic
(defn- conjunctive-normal-form [predicate]
  (-> predicate negation-normal-form distribute-ors))

(defn- simplified-conjunctive-normal-form [predicate]
  (-> predicate simplify-expression conjunctive-normal-form simplify-expression))

(defn- yes-no-clauses [expressions]
  (let [groups (group-by #(and (expr? %) (= :not (:operator %))) expressions)]
    [ (set (get groups false)) (set (map #(first (:params %)) (get groups true)))]
    )
  )

(defn- clauses
  "Returns a two-dimensional array - top level is AND and second level is OR, and elements are expressions"
  [conjunctive-normal-form]
  (map #(yes-no-clauses (expr-clauses :or %)) (expr-clauses :and conjunctive-normal-form)))

(defn remove-true-clauses [clauses]
  (filter (fn [[yes no]] (empty? (set/intersection yes no))) clauses)
  )

(defn compare-clauses [c1 c2]
  (let [[y1 n1] c1
        [y2 n2] c2
        c (compare (+ (count y1) (count n1)) (+ (count y2) (count n2)))]
    (if (= c 0)
      (compare (hash c1) (hash c2))
      c)))

(defn clause-set [clauses]
  (apply sorted-set-by compare-clauses clauses))

(defn clausal-normal-form [expression]
  (-> expression simplified-conjunctive-normal-form clauses remove-true-clauses clause-set))