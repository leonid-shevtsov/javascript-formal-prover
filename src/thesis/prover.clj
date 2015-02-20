(ns thesis.prover
  (:require [clojure.tools.logging :as log]
            [clojure.string :as s]
            [thesis.algebra :refer [expr]]
            [thesis.clausal-normal-form :refer [clausal-normal-form clause-set]]
            [clojure.set :as set]))

(defn derive-resolutions [clauses]
  (clause-set (for [c1 clauses c2 clauses :when (not= c1 c2)]
    (let [[yes1 no1] c1
          [yes2 no2] c2
          all-yes (set/union yes1 yes2)
          all-no (set/union no1 no2)]
        [(set/difference all-yes all-no) (set/difference all-no all-yes)]))))

(defn clause->str [[yes-disjuncts no-disjuncts]]
  (s/join " OR " (concat yes-disjuncts (map (partial str "NOT ") no-disjuncts))))

(defn clauses->str [cnf]
  (s/join "\nAND\n" (map clause->str cnf)))

(defn empty-clause? [[yes no]]
  (and (empty? yes) (empty? no)))

(defn resolution-method [clauses iteration]
  (if (> iteration 1000)
    :exceeded-iteration-limit
    (if-let [best-clause (first (set/difference (derive-resolutions clauses) clauses))]
      (if (empty-clause? best-clause)
        :disproved
        (resolution-method (conj clauses best-clause) (inc iteration))
        )
      (do
        (log/debug "Failed to disprove: " (clauses->str clauses))
        :failed-to-disprove)
      )))

(defn resolution-prover [hypothesis]
  (let [counter-hypothesis (expr :not hypothesis)
        cnf (clausal-normal-form counter-hypothesis)
        _ (log/debug (clauses->str cnf))
        ]
    (resolution-method cnf 0)))