(ns jsfp.simplify
  (:require [jsfp.algebra :refer [expr expr-map expr-transform expr?
                                    primitive?]]))

; Reduces a 500-char expression into a 120-char expression through the power
; of logic and arithmetic equalities and evaluation of constant expressions

; (str (-> "division.js"
;          slurp parse parse-tree->program weakest-predicate
;          conjunctive-normal-form))
; (AND (== x (+ x (* y 0))) (AND (AND (AND (AND (OR (OR (>= y x) (!= x (+ x (* y
; 0)))) (== x (+ (* (+ 1 0) y) (- x y)))) (OR (OR (>= y x) (!= x (+ x (* y 0))))
; (< (- x y) y))) (OR (OR (>= y x) (!= x (+ x (* y 0)))) (>= (+ 1 0) 0))) (OR
; (OR (>= y x) (!= x (+ x (* y 0)))) (>= (- x y) 0))) (AND (AND (AND (OR (OR
; (< y x) (!= x (+ x (* y 0)))) (== x (+ (* 0 y) x))) (OR (OR (< y x) (!= x (+ x
; (* y 0)))) (< x y))) (OR (OR (< y x) (!= x (+ x (* y 0)))) (>= 0 0))) (OR (OR
; (< y x) (!= x (+ x (* y 0)))) (>= x 0)))))

; (str (-> "division.js"
;          slurp parse parse-tree->program weakest-predicate
;          conjunctive-normal-form simplify-expression))
; (AND (AND (OR (>= y x) (< (- x y) y)) (OR (>= y x) (>= (- x y) 0))) (AND (OR
; (< y x) (< x y)) (OR (< y x) (>= x 0))))

(defn- simplify-expression-once [expression]
  (let [simplification-rules {; Logic rules
                              [:implies true "x"]      "x"
                              [:implies "x" true]      true
                              [:implies false "x"]     true
                              [:implies "x" false]     [:not "x"]
                              [:not [:not "x"]]        "x"
                              [:or "x" false]          "x"
                              [:or "x" true]           true
                              [:and "x" false]         false
                              [:and "x" true]          "x"
                              [:or "x" "x"]            "x"
                              [:and "x" "x"]           "x"
                              [:or "x" [:not "x"]]     true
                              [:and "x" [:not "x"]]    false
                              [:and "x" [:or "x" "y"]] "x"
                              [:or "x" [:and "x" "y"]] "x"
                              ; Arithmetic rules
                              [:+ "x" 0]               "x"
                              [:* "x" 0]               0
                              [:* "x" 1]               "x"
                              [:+ [:- "x" "y"] "y"]    "x"
                              [:- "x" "x"]             0
                              [:- "x" 0]               "x"
                              [:/ "x" 1]               "x"
                              [:/ 0 "x"]               0
                              [:== "x" "x"]            true
                              [:<= "x" "x"]            true
                              [:>= "x" "x"]            true
                              [:!= "x" "x"]            false
                              [:< "x" "x"]             false
                              [:> "x" "x"]             false
                              }]
    (reduce
      (fn [expression [p-from p-to]] (expr-transform p-from p-to expression))
      expression simplification-rules)))

(defn evaluate-constants
  "Find constant sub-expressions in expression and evaluate them"
  [expression]
  (let [expression-operators {:and     #(and %1 %2)
                              :or      #(or %1 %2)
                              :not     not
                              :implies #(or (not %1) %2)
                              :+       +
                              :-       -
                              :*       *
                              :/       /
                              :==      =
                              :!=      not=
                              :>       >
                              :<       <
                              :>=      >=
                              :<=      <=}]
    (if (expr? expression)
      (let [with-evaluated-params (expr-map evaluate-constants expression)]
        (if (every? primitive? (:params with-evaluated-params))
          (let [operator-fn
                  (get expression-operators (:operator with-evaluated-params))
                params (:params with-evaluated-params)]
            (apply operator-fn params))
          with-evaluated-params))
      expression)))

(defn simplify-expression [expression]
  (let [simplified (-> expression simplify-expression-once evaluate-constants)]
    (if (not= expression simplified)
      (recur simplified)
      simplified)))
