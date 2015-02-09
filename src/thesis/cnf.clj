(ns thesis.cnf
  (:require
    [clojure.string :as s]
    [thesis.algebra :refer [expr-transform expr-atom?]]
    [thesis.simplify :refer [simplify-expression]]))

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

(defn group-expression-by [operator expression]
  (if (or (expr-atom? expression) (not= operator (:operator expression)))
    (list expression)
    (apply concat (map (partial group-expression-by operator) (:params expression)))))
