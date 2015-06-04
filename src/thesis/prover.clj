(ns thesis.prover
  (:require [clojure.tools.logging :as log]
            [clojure.string :as s]
            [thesis.algebra :refer [expr]]
            [thesis.clausal-normal-form :refer [clausal-normal-form clause-set]]
            [thesis.simplify :refer [simplify-expression]]
            [clojure.set :as set]))

; https://en.wikipedia.org/wiki/Resolution_(logic)

(defn derive-resolutions [clauses]
  (clause-set (for [c1 clauses c2 clauses :when (not= c1 c2)]
    (let [[yes1 no1] c1
          [yes2 no2] c2
          all-yes (set/union yes1 yes2)
          all-no (set/union no1 no2)]
      (with-meta
        [(set/difference all-yes all-no) (set/difference all-no all-yes)]
        {:parents [c1 c2]})))))

(defn clause->str [[yes-disjuncts no-disjuncts]]
  (if (every? empty? [yes-disjuncts no-disjuncts])
    "FALSE"
    (s/join " OR " (concat yes-disjuncts
                           (map (partial str "NOT ") no-disjuncts)))))

(defn clauses->str [cnf]
  (s/join "\nAND\n" (map clause->str cnf)))

(defn empty-clause? [[yes no]]
  (and (empty? yes) (empty? no)))

(defn log-clause [label clause]
  (log/debugf (str label ": %s") (clause->str clause))
  (when-let [[c1 c2] (get (meta clause) :parents)]
    (log/debugf "From: %s  ~AND~  %s" (clause->str c1) (clause->str c2)))
  clause)

(defn resolution-method [clauses iteration]
  (if (> iteration 1000)
    :exceeded-iteration-limit
    (if-let [best-clause
             (log-clause "Best derived clause"
                         (first (set/difference (derive-resolutions clauses)
                                                clauses)))]
      (if (empty-clause? best-clause)
        :proved
        (recur (conj clauses best-clause) (inc iteration))
        )
      (do
        (log/debug "Failed to disprove: " (clauses->str clauses))
        :failed-to-prove)
      )))

(defn trivial-solution [[yes no]]
  (cond
    ; Take note - trivial solution deals with the counter example
    (and (= yes #{true}) (empty? no)) :disproved
    (and (= yes #{false}) (empty? no)) :proved
    :default :failed-to-prove))

(defn resolution-prover [facts hypothesis]
  (let [counter-hypothesis (expr :not hypothesis)

        _ (log/spyf "Simplified counter hypothesis: %s"
                    (simplify-expression counter-hypothesis))

        provable-statement (reduce #(expr :and %1 %2) counter-hypothesis facts)

        cnf (clausal-normal-form provable-statement)

        _ (log/spyf "Initial clauses: %s" (clauses->str cnf))]
    (if (= 1 (count cnf))
      (trivial-solution (first cnf))
      (resolution-method cnf 0))))