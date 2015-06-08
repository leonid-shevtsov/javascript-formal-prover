(ns thesis.core
  (:gen-class)
  (:require [thesis.parser :refer [parse]]
            [thesis.program-semantics :refer [parse-tree->program]]
            [thesis.wp :refer [program-correctness-hypothesis]]
            [thesis.comparison-semantics :refer [factualize-comparisons]]
            [thesis.prover :refer [prover]]
            [thesis.simplify :refer [simplify-expression]]
            [clojure.tools.logging :as log]
            [clojure.string :as s])
  (:import (org.antlr.v4.runtime.misc ParseCancellationException)
           (java.io FileNotFoundException)))

(defn parse-program-from-filename [filename]
  (try
    (if-let [parse-tree (-> filename slurp parse)]
      (parse-tree->program parse-tree)
      (log/errorf "Failed to parse: %s" filename))
    (catch FileNotFoundException _
      (log/errorf "File not found: %s" filename))))

(defn prove-program [program]
  (let [correctness-hypothesis (program-correctness-hypothesis program)

        _ (log/spyf "Program correctness hypothesis:\n%s"
                    correctness-hypothesis)

        [comparison-axioms factualized-hypothesis]
          (factualize-comparisons correctness-hypothesis)

        _ (log/spyf "After abstracting comparisons:\n%s"
                    factualized-hypothesis)
        _ (log/spyf "Simplified:\n%s"
                    (simplify-expression factualized-hypothesis))
        _ (log/spyf "Axioms about comparisons:\n%s"
                    (s/join "\n" (map str comparison-axioms)))]
    (prover comparison-axioms factualized-hypothesis)))

(defn -main [filename & args]
  (if-let [program (parse-program-from-filename filename)]
    (println (prove-program program))
    (println "Error")))
