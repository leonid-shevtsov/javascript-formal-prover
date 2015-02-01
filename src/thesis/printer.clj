(ns thesis.printer)

(declare print-tree)

(def pt print-tree)
(def print-fns
  {:and-predicate #(str "(and " (pt %1) " " (pt %3) ")")
   :or-predicate #(str "(or " (pt %1) " " (pt %3) ")")
   :impl-predicate #(str "(implies " (pt %1) " " (pt %3) ")")
   :mult-expression #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :sum-expression #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :comparison #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :not-atom #(str "(not " (pt %2) ")")
   :parens-predicate (fn [_ p _] (pt p))
   :numeric-constant identity
   :identifier identity
   :atom-predicate #(pt %1)
   :atom-expression #(pt %1)
   :atom #(pt %1)
   :predicate-atom #(pt %1)
   })

(defn print-tree [tree]
  (let [tree-type (first tree)
        tree-data (next tree)
        ]
    (apply (tree-type print-fns) tree-data)))
