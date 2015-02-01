(ns thesis.normalize
  (:use thesis.semantic-analyzer thesis.types)
  (:require [clojure.string :as s]))

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

(defn simplify-expression-once [expression]
  (reduce
    (fn [expression [p-from p-to]] (expr-transform p-from p-to expression))
    expression simplification-rules)
  )

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

(defn simplify-expression [expression]
  (let [simplified (-> expression simplify-expression-once evaluate-constants)]
    (if (not= expression simplified)
      (simplify-expression simplified)
      simplified)))

; See https://en.wikipedia.org/wiki/Conjunctive_normal_form#Converting_from_first-order_logic
(defn conjunctive-normal-form [predicate]
  (-> predicate negation-normal-form distribute-ors))

(defn simplified-conjunctive-normal-form [predicate]
  (-> predicate simplify-expression conjunctive-normal-form simplify-expression))

(defn group-expression-by [operator expression]
  (if (or (expr-atom? expression) (not= operator (:operator expression)))
    (list expression)
    (apply concat (map (partial group-expression-by operator) (:params expression)))))

(defn make-inequality-one-sided
  "Convert A _sign_ B to (A - B) _sign_ 0"
  [inequality]
  (let [[lside rside] (:params inequality)]
    (if (= 0 rside)
      inequality
      (expr (:operator inequality) (expr :+ lside (expr :* -1 rside)) 0))))

(defn differences-to-sums [expression]
  (expr-transform [:- "a" "b"] [:+ "a" [:* -1 "b"]] expression))

(defn distribute-products [expression]
  (let [transformed (expr-transform [:* "a" [:+ "b" "c"]] [:+ [:* "a" "b"] [:* "a" "c"]] expression)]
    (if (not= expression transformed)
      (distribute-products transformed)
      transformed)))

(defn collect-terms [normalized-polynomial]
  (map (partial group-expression-by :*) (group-expression-by :+ normalized-polynomial)))

(defn join-similar-terms
  "Returns a normalized hash of (sorted variable list) -> numeric factor"
  [polynomial-terms]
  (reduce (fn [term-factors new-term]
            (let [factor-key (s/join "*" (sort (filter identifier? new-term)))
                  factor (reduce * 1 (filter number? new-term))
                  existing-factor (get term-factors factor-key 0)]
              (assoc term-factors factor-key (+ existing-factor factor))))
          (sorted-map-by #(compare %2 %1)) polynomial-terms))

(defn multiply-terms [factor terms]
  (into {} (map (fn [[t-vars t-factor]] [t-vars (/ t-factor factor)]) terms)))

(defn normalize-terms
  "Picks first term (by reverse default sort order), and divides all factors by its factor"
  [polynomial-terms]
  (let [lex-first-term-key (first (keys polynomial-terms))
        lex-first-factor (Math/abs (get polynomial-terms lex-first-term-key))]
    (multiply-terms lex-first-factor polynomial-terms)))

(defn polynomial-terms
  "Given an algebraic expression, produce sorted 2-dimensional array;
  first level of array is the terms of the polynomial; second level is the factors of
  each term"
  [expression]
  (-> expression differences-to-sums distribute-products collect-terms join-similar-terms normalize-terms))

(defn term->str [[term-variables term-factor]]
  (if (= "" term-variables)
    term-factor
    (case term-factor
      1 term-variables
      -1 (str "-" term-variables)
      (str term-factor "*" term-variables))))

(defn join-terms [term1 term2]
  (if (= \- (first term2))
    (str term1 term2)
    (str term1 "+" term2)))

(defn normalize-inequality [inequality]
  (let [operator (:operator inequality)
        lside-terms (polynomial-terms (first (:params inequality)))
        flip-sign (> 0 (first (vals lside-terms)))
        flipped-operator (if flip-sign
                           (get {:> :<, :>= :<=, :< :>, :<= :>=} operator operator)
                           operator)
        positive-lside-terms (if flip-sign
                         (multiply-terms -1 lside-terms)
                         lside-terms)
        free-factor (get positive-lside-terms "" 0)
        lside-without-free-factor (dissoc positive-lside-terms "")
        lside-string (reduce join-terms (map term->str lside-without-free-factor))
        ]
    (expr flipped-operator lside-string (- free-factor))))

(defn normal-inequality-form [inequality]
  (-> inequality make-inequality-one-sided normalize-inequality))