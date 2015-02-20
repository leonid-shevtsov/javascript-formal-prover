(ns thesis.t-polynomial
  (:use midje.sweet)
  (:require [thesis.polynomial :as subj]
            [thesis.algebra :refer [expr]]))

(fact
  "clausal polynomial form produces CPF"
  (subj/clausal-polynomial-form (expr :+ "x" [:* 2 [:- "x" "y"]]))
  => {"y" -2 "x" 3}

  (subj/clausal-polynomial-form (expr :* "x" [:* 2 [:- "x" "y"]]))
  => {"y*x" -2 "x*x" 2}

  (subj/clausal-polynomial-form (expr :* [:+ "x" 1] [:+ "x" 1]))
  => {"x*x" 1, "x" 2, "" 1}
  )

(let [cpf (sorted-map-by #(compare %2 %1) "y*x" -2 "x*x" 2)]
  (fact
    "normalize-cpf normalizes CPF"
    (subj/normalize-cpf cpf)
    => [-2 {"y*x" 1 "x*x" -1}]
    ))

(let [cpf (sorted-map-by #(compare %2 %1) "x*x" -1, "x" -2, "" 1)]
  (fact
    "separate-scalar separates scalar"
    (subj/separate-scalar cpf)
    => [{"x*x" -1 "x" -2} 1]
    )

  (fact
    "cpf->str stringifies CPF"
    (subj/cpf->str cpf)
    => "-x*x-2*x+1"
    ))

(facts
  "about addend->str"

  (subj/addend->str ["x*x" 1])
  => "x*x"

  (subj/addend->str ["x*x" -1])
  => "-x*x"

  (subj/addend->str ["x*x" 2])
  => "2*x*x"

  (subj/addend->str ["x*x" -2])
  => "-2*x*x"

  (subj/addend->str ["" 1])
  => "1"
  )