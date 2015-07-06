(ns jsfp.t-comparison-semantics
  (:use midje.sweet)
  (:require [jsfp.comparison-semantics :as subj]
            [jsfp.algebra :refer [expr]]))

(fact
  "make-comparisons-one-sided carries everything to one side of the comparison"
  (subj/make-comparison-one-sided (expr :> "x" "y"))
  => (expr :> [:+ "x" [:* -1 "y"]] 0)
  )

(facts
  "about record-comparison"

  (subj/record-comparison {} (expr :> "x" 1))
  => [{{"x" 1} #{[:> 1]}} "x>1"]

  (subj/record-comparison {} (expr :!= "x" 1))
  => [{{"x" 1} #{[:== 1]}} (expr :not "x==1")]
  )

(facts
  "about extract-comparisons"

  (subj/extract-comparisons {} (expr :and [:> "x" 1] [:< [:* 2 "y"] 0]))
  => [{{"y" 1} #{[:< 0]} {"x" 1} #{[:> 1]}} (expr :and "x>1" "y<0")]
  )
