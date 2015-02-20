(ns thesis.t-algebra
  (:use midje.sweet)
  (:require [thesis.algebra :as algebra]))

(fact
  "expr builds expressions"
  (algebra/expr :+ 1 2)
  => (algebra/->Expression :+ [1 2])

  (algebra/expr :+ 1 [:- 2 3])
  => (algebra/->Expression :+ [1 (algebra/->Expression :- [2 3])])

  (algebra/expr :+ 1 (algebra/->Expression :- [2 3]))
  => (algebra/->Expression :+ [1 (algebra/->Expression :- [2 3])]))

(facts
  "about expr-map"
  (fact
    "expr-map applies a function to all of an expression's parameters"
    (algebra/expr-map #(+ 2 %) (algebra/expr :+ 1 2))
    => (algebra/expr :+ 3 4)))

(facts
  "about expr-match"
  (fact
    "expr-match matches expressions and returns the map of matching values"
    (algebra/expr-match [:not "x"] (algebra/expr :not 1))
    => {"x" 1}

    (algebra/expr-match [:and "x" "y"] (algebra/expr :and "a" "b"))
    => {"x" "a", "y" "b"}

    (algebra/expr-match [:not "x"] (algebra/expr :and "x" "y"))
    => falsey

    (algebra/expr-match [:and "x" "y"] (algebra/expr :or "a" "b"))
    => falsey)

  (fact
    "expr-match matches nested expressions"
    (algebra/expr-match [:not [:and "x" "y"]] (algebra/expr :not '(:and "a" "b")))
    => {"x" "a", "y" "b"})

  (fact
    "expr-match matches placeholders to non-primitive values"
    (algebra/expr-match [:not "z"] (algebra/expr :not '(:and "a" "b")))
    => {"z" (algebra/->Expression :and ["a" "b"])})

  (future-fact
    "expr-match can handle commutative sub-expressions in different orientation"
    ; Right now each sub-expression is matched separately
    ; Need to search and back-track instead.
    ; For example - from x OR y = a OR b we bind x=a, y=b, but we could also bind x=b, y=a
    ; which would then let x => y = b => a
    (algebra/expr-match [:and [:or "x" "y"] [:implies "x" "y"]] (algebra/expr :and [:or "a" "b"] [:implies "b" "a"]))
    => {"x" "b", "y" "a"}
    )

  (fact
    "for commutative expressions, expr-match matches in reverse, too"
    (algebra/expr-match [:and "x" [:not "y"]] (algebra/expr :and [:not "a"] "b"))
    => {"x" "b", "y" "a"})

  (fact
    "for non-commutative expressions, expr-match does NOT match in reverse"
    (algebra/expr-match [:implies "x" [:not "y"]] (algebra/expr :implies [:not "a"] "b"))
    => falsey)

  (fact
    "expr-match matches booleans and numbers as their actual value"
    (algebra/expr-match [:+ "x" 1] (algebra/expr :+ "a" 1))
    => {"x" "a"}

    (algebra/expr-match [:+ "x" 1] (algebra/expr :+ "a" 2))
    => falsey

    (algebra/expr-match [:and "x" true] (algebra/expr :and "a" true))
    => {"x" "a"}

    (algebra/expr-match [:and "x" true] (algebra/expr :and "a" false))
    => falsey))

(facts
  "about expr-construct"
  (fact
    "expr-construct builds expressions from a pattern and a set of bindings"
    (algebra/expr-construct {"x" "a", "y" "b"} [:and "x" [:not "y"]])
    => (algebra/expr :and "a" [:not "b"])))

(facts
  "about expr-transform"
  (fact
    "transforms expressions"
     (algebra/expr-transform [:not [:and "x" "y"]]
                             [:or [:not "x"] [:not "y"]]
                             (algebra/expr :not [:and "a" "b"]))
     => (algebra/expr :or [:not "a"] [:not "b"])

     (algebra/expr-transform [:not [:not "x"]]
                             "x"
                             (algebra/expr :not [:not "x"]))
     => "x")

  (fact
    "transforms parts of expressions"
    (algebra/expr-transform [:not [:not "x"]]
                            "x"
                            (algebra/expr :and [:not [:not "x"]] [:not [:not "y"]]))
    => (algebra/expr :and "x" "y")

    (algebra/expr-transform [:not [:not "x"]]
                            "x"
                            (algebra/expr :not [:not [:not "x"]]))
    => (algebra/expr :not "x")

    (algebra/expr-transform [:not [:not "x"]]
                            "x"
                            (algebra/expr :not [:not [:not [:not "x"]]]))
    => "x"))

(fact
  "expr-clauses groups expression by an operator"
  (algebra/expr-clauses :* (algebra/expr :* [:+ 1 2] [:* [:+ 3 4] [:+ 5 6]]))
  => [(algebra/expr :+ 1 2) (algebra/expr :+ 3 4) (algebra/expr :+ 5 6)])

(fact
  "expr-clauses with just one expression will return its params"
  (algebra/expr-clauses :* (algebra/expr :* 1 2))
  => [1 2]
  )

(fact
  "top-level expression for expr-clauses must match clause-operator, or the result will have only one clause"
  (algebra/expr-clauses :* (algebra/expr :+ [:+ 1 2] [:* [:+ 3 4] [:+ 5 6]]))
  => [ (algebra/expr :+ [:+ 1 2] [:* [:+ 3 4] [:+ 5 6]]) ])