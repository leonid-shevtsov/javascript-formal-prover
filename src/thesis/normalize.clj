(ns thesis.normalize
  (:use thesis.semantic-analyzer thesis.types))

; Reduces a 500-char expression into a 120-char expression through the power
; of logic and arithmetic equalities and evaluation of constant expressions
; (str (-> "division.js" slurp parse parse-tree->program weakest-predicate conjunctive-normal-form))
; (AND (== x (+ x (* y 0))) (AND (AND (AND (AND (OR (OR (>= y x) (!= x (+ x (* y 0)))) (== x (+ (* (+ 1 0) y) (- x y)))) (OR (OR (>= y x) (!= x (+ x (* y 0)))) (< (- x y) y))) (OR (OR (>= y x) (!= x (+ x (* y 0)))) (>= (+ 1 0) 0))) (OR (OR (>= y x) (!= x (+ x (* y 0)))) (>= (- x y) 0))) (AND (AND (AND (OR (OR (< y x) (!= x (+ x (* y 0)))) (== x (+ (* 0 y) x))) (OR (OR (< y x) (!= x (+ x (* y 0)))) (< x y))) (OR (OR (< y x) (!= x (+ x (* y 0)))) (>= 0 0))) (OR (OR (< y x) (!= x (+ x (* y 0)))) (>= x 0)))))
; (str (-> "division.js" slurp parse parse-tree->program weakest-predicate conjunctive-normal-form simplify-expression evaluate-constants simplify-expression))
; (AND (AND (OR (>= y x) (< (- x y) y)) (OR (>= y x) (>= (- x y) 0))) (AND (OR (< y x) (< x y)) (OR (< y x) (>= x 0))))


(defn de-implify
  [predicate]
  (expr-transform [:implies "p" "q"] [:or [:not "p"] "q"] predicate))

(defn move-nots-inward
  [predicate]
  (let [transformed (->> predicate
                        (expr-transform [:not [:and "p" "q"]] [:or [:not "p"] [:not "q"]])
                        (expr-transform [:not [:or "p" "q"]] [:and [:not "p"] [:not "q"]]))]
    (if (not= predicate transformed)
      (move-nots-inward transformed)
      (expr-transform [:not [:not "x"]] "x" transformed))))

(defn flip-not-expressions [predicate]
  (->> predicate
       (expr-transform [:not [:> "x" "y"]] [:<= "x" "y"])
       (expr-transform [:not [:< "x" "y"]] [:>= "x" "y"])
       (expr-transform [:not [:>= "x" "y"]] [:< "x" "y"])
       (expr-transform [:not [:<= "x" "y"]] [:> "x" "y"])
       (expr-transform [:not [:== "x" "y"]] [:!= "x" "y"])
       (expr-transform [:not [:!= "x" "y"]] [:== "x" "y"])))

(defn distribute-ors
  [predicate]
  (expr-transform [:or "p" [:and "q" "r"]] [:and [:or "p" "q"] [:or "p" "r"]] predicate))

(defn negation-normal-form [predicate]
  (-> predicate de-implify move-nots-inward flip-not-expressions))

; see http://sakharov.net/logic.html
(def simplification-rules
  {[:implies true "x"]      "x"
   [:implies "x" true]      true
   [:implies false "x"]     true
   [:implies "x" false]     [:not "x"]
   [:not true]              false
   [:not false]             true
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
   [:+ "x" 0]               "x"
   [:* "x" 0]               0
   [:* "x" 1]               "x"
   [:+ [:- "x" "y"] "y"]    "x"
   [:== "x" "x"]            true
   [:<= "x" "x"]            true
   [:>= "x" "x"]            true
   [:!= "x" "x"]            false
   [:< "x" "x"]             false
   [:> "x" "x"]             false
   [:- "x" "x"]             0
   })

(defn simplify-expression [expression]
  (let [simplified (reduce
                     (fn [expression [p-from p-to]] (expr-transform p-from p-to expression))
                     expression simplification-rules)]
    (if (not= expression simplified)
      (simplify-expression simplified) ; continue reducing
      simplified)))

(def expression-operators
  {:and          #(and %1 %2)
   :or           #(or %1 %2)
   :not          not
   :implies      #(or (not %1) %2)
   :+            +
   :-            -
   :*            *
   (keyword "/") /
   :==           =
   :!=           not=
   :>            >
   :<            <
   :>=           >=
   :<=           <=})

(defn evaluate-constants [expression]
  (if (expr? expression)
    (let [with-evaluated-params (map-expr evaluate-constants expression)]
      (if (every? primitive? (:params with-evaluated-params))
        (apply ((:operator expression) expression-operators) (:params with-evaluated-params))
        with-evaluated-params))
    expression))

; See https://en.wikipedia.org/wiki/Conjunctive_normal_form#Converting_from_first-order_logic
(defn conjunctive-normal-form [predicate]
  (-> predicate negation-normal-form distribute-ors))
