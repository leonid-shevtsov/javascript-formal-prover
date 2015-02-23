(ns thesis.t-prover
  (:use midje.sweet)
  (:require [thesis.prover :as subj]
            [thesis.algebra :refer [expr]]))

(fact
  "prover can prove truth"
  (subj/resolution-prover true)
  => :disproved
  )