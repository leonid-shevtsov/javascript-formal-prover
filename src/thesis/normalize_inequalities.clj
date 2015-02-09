(ns thesis.normalize-inequalities
  (:use thesis.algebra thesis.cnf)
  (:require [clojure.string :as s]))

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