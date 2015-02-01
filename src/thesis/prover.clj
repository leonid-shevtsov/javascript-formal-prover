(ns thesis.prover
  (:use thesis.types thesis.normalize)
  (:require [thesis.wp :as wp]
            [clojure.set :as set]
            [clojure.string :as s]
            [clojure.core.logic :as logic]
            [clojure.core.logic.fd :as fd]) )

(defn predicate-identifiers [predicate]
  (if (expr? predicate)
    (apply set/union (map predicate-identifiers (:params predicate)))
    (if (identifier? predicate)
      #{predicate}
      #{})))


(defn- group-predicate-by [operator predicate]
  (if (or (pred-atom? predicate) (not= operator (:operator predicate)))
    (list predicate)
    (apply concat (map (partial group-predicate-by operator) (:params predicate)))))

(defn conjuncts
  "Returns a two-dimensional array - top level is AND and second level is OR, and elements are expressions"
  [conjunctive-normal-form]
  (map (partial group-predicate-by :or) (group-predicate-by :and conjunctive-normal-form)))

(def fd-symbols {:+  `+
                    :-  `-
                    :*  `*
                    (keyword "/") `/
                    :==  `=
                    :!= `not=
                    :<= `<=
                    :<  `<
                    :>= `>=
                    :>  `>})

(defn expression->fd [expression]
  (cond
    (expr? expression) `(~((:operator expression) fd-symbols) ~@(map expression->fd (:params expression)))
    (string? expression) (symbol expression)
    (true? expression) `(logic/succeed)
    (false? expression) `(logic/fail)
    true expression))

(defn conjunct->goal [conjunct]
  `(logic/conde ~@(map (fn [ex] `(fd/eq ~(expression->fd ex))) conjunct)))

(defn prove
  [program]
  (let [weakest-predicate (wp/weakest-predicate program)
        counterexample (expr :and (:precondition program) (expr :not weakest-predicate))
        conjuncts (conjuncts (conjunctive-normal-form counterexample))
        identifiers (map symbol (predicate-identifiers counterexample))]
    ;`(logic/run 1 [~'q]
    ;  (logic/fresh [~@identifiers]
    ;               (fd/in ~@identifiers (fd/interval 1 1000))
    ;               ~@(map conjunct->goal conjuncts)
    ;               (logic/== ~'q [~@identifiers])
    ;   )
    ;   ))
    conjuncts)
    )
