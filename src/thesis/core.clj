(ns thesis.core
  (:gen-class)
  (:require [thesis.parser :refer [parse]]
            [thesis.program-semantics :refer [parse-tree->program]]
            [thesis.wp :refer [program-correctness-hypothesis]]
            [thesis.comparison-semantics :refer [factualize-comparisons]]
            [thesis.prover :refer [resolution-prover]]
            [thesis.simplify :refer [simplify-expression]]
            [clojure.tools.logging :as log]
            [clojure.string :as s]))

(defn prove-program-from-filename [filename]
  (let [source-code (slurp filename)
        parse-tree (parse source-code)
        program (parse-tree->program parse-tree)
        correctness-hypothesis (log/spyf "Program correctness hypothesis: %s"
                                         (program-correctness-hypothesis program))
        [comparison-facts pure-correctness-hypothesis] (factualize-comparisons correctness-hypothesis)
        _ (log/spyf "Pure hypothesis: %s" (simplify-expression pure-correctness-hypothesis))
        _ (log/spyf "Comparison facts:\n%s" (s/join "\n" (map str comparison-facts)))
        prover-conclusion (resolution-prover comparison-facts pure-correctness-hypothesis)]
    prover-conclusion))

(defn -main [filename & args]
  (println (if (prove-program-from-filename filename) "Proved" "Disproved")))