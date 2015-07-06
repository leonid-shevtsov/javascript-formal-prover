(ns jsfp.identifiers
  (:require [jsfp.algebra :refer [expr? expr-map identifier?]]))

(defn replace-identifier-in-expression
  [identifier value construct]
  (cond
    (expr? construct)
    (expr-map
      (partial replace-identifier-in-expression identifier value) construct)

    (identifier? construct)
    (if (= construct identifier)
      value
      construct)

    :else construct))

(defn identifiers-in-expression
  [expression]
  (cond
    (identifier? expression)
    [expression]

    (expr? expression)
    (distinct (mapcat identifiers-in-expression (:params expression)))

    :else
    []))
