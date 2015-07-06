(ns jsfp.t-prover
  (:use midje.sweet)
  (:require [jsfp.prover :as subj]
            [jsfp.algebra :refer [expr]]))

(fact
  "prover can prove truth"
  (subj/resolution-method [] true)
  => :proved
  )
