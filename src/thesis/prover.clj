(ns thesis.prover
  (:use thesis.algebra thesis.cnf thesis.normalize-inequalities)
  (:require [thesis.wp :as wp]
            [clojure.set :as set]
            [clojure.string :as s])
  (:import (thesis.algebra Expression)))

; (map #(map str %) (-> "division.js" slurp parse parse-tree->program prove))

(defn conjuncts
  "Returns a two-dimensional array - top level is AND and second level is OR, and elements are expressions"
  [conjunctive-normal-form]
  (map (partial group-expression-by :or) (group-expression-by :and conjunctive-normal-form)))

(defn contradicts? [^Expression ineq1 ^Expression ineq2]
  (let [[lside1 rside1] (:params ineq1)
        [lside2 rside2] (:params ineq2)
        op1 (:operator ineq1)
        op2 (:operator ineq2)]
    (and (= lside1 lside2)
         (case [op1 op2]
           [:== :==] (not= rside1 rside2)
           [:== :>] (<= rside1 rside2)                      ; x==1 x>2 ; x==1 x>1
           [:== :<] (>= rside1 rside2)                      ; x==2 x<1; x==1 x<1
           [:> :==] (>= rside1 rside2)                      ; x>2 x==1; x>1 x==1
           [:> :>] false
           [:> :<] (>= rside1 rside2)                       ; x>2 x<1; x>1 x<1
           [:< :==] (<= rside1 rside2)                      ; x<1 x==2; x<1 x==1
           [:< :>] (<= rside1 rside2)                       ; x<1 x>2; x<1 x>1
           [:< :<] false))))

(defn derive-conjunct [conjunct1 conjunct2]
  (into #{} (concat
    (filter (fn [ineq] (not-any? (partial contradicts? ineq) conjunct2)) conjunct1)
    (filter (fn [ineq] (not-any? (partial contradicts? ineq) conjunct1)) conjunct2)
    )))

(defn derive-all-conjuncts [conjuncts]
  (set/difference
    (->> (for [conjunct1 conjuncts conjunct2 conjuncts]
           (and (not= conjunct1 conjunct2) (derive-conjunct conjunct1 conjunct2)))
         (filter coll?)
         (into #{}))
    conjuncts))

(defn resolution-method [conjuncts iteration]
  (if (> iteration 1000)
    :exceeded-iteration-limit
    (if-let [best-new-conjunct (some->> conjuncts
                                         (derive-all-conjuncts)
                                         (not-empty)
                                         (apply min-key count))]
      (resolution-method (conj conjuncts best-new-conjunct) (inc iteration))
      :disproved)))

(defn unwrap-double-inequality [inequality]
  (let [operator (:operator inequality) params (:params inequality)]
    (case operator
      :>= (list (apply expr :> params) (apply expr :== params))
      :<= (list (apply expr :< params) (apply expr :== params))
      :!= (list (apply expr :< params) (apply expr :> params))
      (list inequality) ; default
      )))

(defn prove
  [program]
  (let [weakest-predicate (wp/weakest-predicate program)
        counterexample (expr :and (:precondition program) (expr :not weakest-predicate))
        conjuncts (-> counterexample
                      simplified-conjunctive-normal-form
                      conjuncts)
        normalized-conjuncts (into #{} (map #(apply concat (map unwrap-double-inequality (map normal-inequality-form %))) conjuncts))
        ]
      (= (resolution-method normalized-conjuncts 0) :disproved)
    ))
