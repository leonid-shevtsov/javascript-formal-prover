(ns jsfp.t-simplify
  (:use midje.sweet)
  (:require [jsfp.algebra :refer [expr]]
            [jsfp.simplify :as simplify]))

(facts
  "about simplify-expression"

  (fact
    "simplify-expressions evaluates nested constant expressions and reduces
    logical equalities"
    (simplify/simplify-expression (expr :and "x" [:> [:+ 2 3] 1]))
    => "x")

  (fact
    "simplify-expressions can eliminate variable dependencies using
    logical equalities"
    (simplify/simplify-expression (expr :and true [:and true [:or true "y"]]))
    => true))
