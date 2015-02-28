(ns thesis.polynomial
  (:require [thesis.algebra :refer [expr-transform expr-clauses identifier?]]
            [clojure.string :as s]))

;; implementation of clausal-polynomial form

(defn differences-to-sums
  "Convert expression A - B to A + (-1 * B) for further normalization"
  [expression]
  (expr-transform [:- "a" "b"] [:+ "a" [:* -1 "b"]] expression))

(defn distribute-products
  "Convert A * (B + C) to A*B + A*C"
  [expression]
  (let [transformed (expr-transform [:* "a" [:+ "b" "c"]] [:+ [:* "a" "b"] [:* "a" "c"]] expression)]
    (if (not= expression transformed)
      (recur transformed)
      transformed)))

; TODO maybe (/ a const) = (* a 1/const)
(defn move-division-outward
  [predicate]
  (let [transformed (->> predicate
                         (expr-transform [:+ [:/ "a" "b"] [:/ "c" "d"]]
                                         [:/ [:+ [:* "a" "d"] [:* "c" "b"]] [:* "b" "d"]])
                         (expr-transform [:* "a" [:/ "b" "c"]] [:/ [:* "a" "b"] "c"])
                         (expr-transform [:+ "a" [:/ "b" "c"]] [:/ [:+ [:* "a" "c"] "b"] "c"])
                         (expr-transform [:/ "a" [:/ "b" "c"]] [:/ "a" [:* "b" "c"]])
                         (expr-transform [:/ [:/ "a" "b"] "c"] [:/ "a" [:* "b" "c"]])
                         )]
    (if (not= predicate transformed)
      (recur transformed)
      transformed)))


(defn polynomial-clauses [normalized-polynomial]
  (map (partial expr-clauses :*) (expr-clauses :+ normalized-polynomial)))

(defn join-similar-clauses
  "Returns a normalized hash of (sorted variable list) -> numeric factor"
  [polynomial-terms]
  (reduce (fn [term-factors new-term]
            (let [factor-key (s/join "*" (reverse (sort (filter identifier? new-term))))
                  factor (reduce * 1 (filter number? new-term))
                  existing-factor (get term-factors factor-key 0)]
              (assoc term-factors factor-key (+ existing-factor factor))))
          (sorted-map-by #(compare %2 %1)) polynomial-terms))

(defn remove-zeroes-from-cpf [terms]
  (into {} (remove (fn [[t-vars t-factor]] (zero? t-factor)) terms)))

(defn clausal-polynomial-form
  "Given an algebraic expression, produce sorted map of (variable multiplier) => (constant multiplier)"
  [expression]
  (-> expression differences-to-sums distribute-products polynomial-clauses join-similar-clauses remove-zeroes-from-cpf))

;; implementation of normalize-cpf

(defn multiply-cpf [factor terms]
  (into {} (map (fn [[t-vars t-factor]] [t-vars (/ t-factor factor)]) terms)))

(defn normalize-cpf
  "Given CPF, return [factor-for-highest-power-clause clauses-with-highest-power-clause-equal-to-1]"
  [clausal-polynomial-form]
  (let [lex-first-factor (first (vals clausal-polynomial-form))]
    [lex-first-factor (multiply-cpf lex-first-factor clausal-polynomial-form)]))

;; implemetation of separate-scalar

(defn separate-scalar
  "Given clausal polynomial form, returns [cpf-without-scalar-factor scalar-factor-or-0]"
  [cpf]
  [(dissoc cpf "") (get cpf "" 0)])

;; implementation of cpf->str

(defn addend->str [[addend-variables addend-factor]]
  (if (= "" addend-variables)
    (str addend-factor)
    (case addend-factor
      1 addend-variables
      -1 (str "-" addend-variables)
      (str addend-factor "*" addend-variables))))

(defn join-addends [addend1 addend2]
  (if (= \- (first addend2))
    (str addend1 addend2)
    (str addend1 "+" addend2)))

(defn cpf->str [clausal-polynomial-form]
  (reduce join-addends (map addend->str clausal-polynomial-form)))
