(ns thesis.core
  (:gen-class)
  (:require [thesis.parser :refer [parse]]
            [thesis.program-semantics :refer [parse-tree->program]]
            [thesis.wp :refer [program-correctness-hypothesis]]
            [thesis.comparison-semantics :refer [factualize-comparisons]]
            [thesis.prover :refer [resolution-prover]]
            [clojure.tools.logging :as log]
            ))

(defn prove-program-from-filename [filename]
  (let [source-code (slurp filename)
        parse-tree (parse source-code)
        program (parse-tree->program parse-tree)
        correctness-hypothesis (log/spyf "Program correctness hypothesis: %s"
                                         (program-correctness-hypothesis program))
        pure-correctness-hypothesis (factualize-comparisons correctness-hypothesis)
        _ (log/spyf "Pure hypothesis: %s" pure-correctness-hypothesis)
        prover-conclusion (resolution-prover pure-correctness-hypothesis)]
    prover-conclusion))

(defn -main [filename & args]
  (println (if (prove-program-from-filename filename) "Proved" "Disproved")))