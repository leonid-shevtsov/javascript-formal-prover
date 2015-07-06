(ns jsfp.comparison-semantics
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [jsfp.algebra :refer [expr expr-transform expr-construct
                                    expr-clauses expr? identifier?]]
            [jsfp.polynomial :refer [clausal-polynomial-form normalize-cpf
                                       cpf->str separate-scalar]]
            [clojure.tools.logging :as log]))

(defn comparison? [expr] (-> expr :operator #{:> :< :>= :<= :!= :==}))

(defn make-comparison-one-sided
  "Convert A _sign_ B to (A - B) _sign_ 0"
  [comparison]
  (let [[lside rside] (:params comparison)]
    (if (and (number? rside) (zero? rside))
      comparison
      (expr (:operator comparison) (expr :+ lside (expr :* -1 rside)) 0))))

(def flipped-operator
  {:>  :<
   :>= :<=
   :<  :>
   :<= :>=
   :== :==
   :!= :!=})

(def operators-pattern
  {:>  [[">"]    "l>r"]
   :<  [["<"]    "l<r"]
   :<= [[">"]    [:not "l>r"]]
   :>= [["<"]    [:not "l<r"]]
   :== [["=="]  "l==r"]
   :!= [["=="] [:not "l==r"]]
   })

(defn record-comparison [bindings comparison]
  (let [one-sided (make-comparison-one-sided comparison)]
    (if-let [cpf (-> one-sided :params first clausal-polynomial-form not-empty)]
      (let [[normalizing-factor normalized-clauses] (normalize-cpf cpf)
            normalized-operator (if (pos? normalizing-factor)
                                  (:operator one-sided)
                                  (get flipped-operator (:operator one-sided)))
            [non-scalar scalar] (separate-scalar normalized-clauses)
            rside (* -1 scalar)]
        (if (empty? non-scalar)                             ; if left side empty
          [bindings (expr normalized-operator 0 rside)]
          (let [string-lside (cpf->str non-scalar)
                [operators pattern] (get operators-pattern normalized-operator)
                expr-params
                  (into {} (map #(vector (str "l" % "r")
                                         (str string-lside % rside))
                                operators))
                new-expression (expr-construct expr-params pattern)
                new-bindings
                  {non-scalar
                   (into #{} (map #(vector (keyword %) rside) operators))}]
            [(merge-with set/union bindings new-bindings) new-expression])))
      ; if left side is empty (0), expression is true if operator allows
      ; equality (because right side is 0)
      [bindings (expr (:operator one-sided) 0 0)])))


;; Implementation of augment-hypothesis-with-comparison-implications

(defn comparison-implications
  "Input:
   2 comparisons: lside ox x, and lside oy y, where ox,oy - one of [>, <],
   x, y - scalars
   Output:
   if there is an implication from lside ox x being true or false,
   to lside oy y being true or false, then
   returns list of such implications, otherwise empty list
   "
  [lside [[ox x] [oy y]]]
  (let [exprx (str lside (name ox) x)
        expry (str lside (name oy) y)]
    (remove false?
            (case [ox oy]
              [:> :<] [(and (>= x y) (expr :implies exprx [:not expry]))
                       (and (< x y) (expr :implies [:not exprx] expry))]
              [:> :>] [(and (> x y) (expr :implies exprx expry))
                       (and (< x y) (expr :implies [:not exprx] [:not expry]))]
              [:< :<] [(and (< x y) (expr :implies exprx expry))
                       (and (> x y) (expr :implies [:not exprx] [:not expry]))]
              [:< :>] [(and (<= x y) (expr :implies exprx [:not expry]))
                       (and (> x y) (expr :implies [:not exprx] expry))]
              [:> :==] [(and (>= x y) (expr :implies exprx [:not expry]))
                        (and (< x y) (expr :implies [:not exprx] [:not expry]))]
              [:< :==] [(and (<= x y) (expr :implies exprx [:not expry]))
                        (and (> x y) (expr :implies [:not exprx] [:not expry]))]
              [:== :==] [(and (not= x y) (expr :implies exprx [:not expry]))]
              [:== :>] [(and (> x y) (expr :implies exprx expry))
                        (and (<= x y) (expr :implies exprx [:not expry]))]
              [:== :<] [(and (< x y) (expr :implies exprx expry))
                        (and (>= x y) (expr :implies exprx [:not expry]))]
              ))))

(defn binding-implications
  "Given one binding produced by extract-comparisons, build a collection of
  axioms about the comparisons mentioned in the binding"
  [[clauses comparisons]]
  (let [string-clause
          (cpf->str clauses)
        permutations
          (for [c1 comparisons c2 comparisons :when (not= c1 c2)] [c1 c2])]
    (mapcat (partial comparison-implications string-clause) permutations)))

(defn extract-comparisons
  [bindings expression]
  (if (expr? expression)
    (if (comparison? expression)
      (record-comparison bindings expression)
      (let [[bindings params]
            (reduce (fn [[bindings params] param]
                      (let [[new-bindings new-param]
                            (extract-comparisons bindings param)]
                        [new-bindings (conj params new-param)]))
                    [bindings []]
                    (:params expression))]
        [bindings (apply expr (:operator expression) params)]
        ))
    [bindings expression]))

(defn factualize-comparisons [expression]
  (let [[bindings expression] (extract-comparisons {} expression)]
    [(mapcat binding-implications bindings) expression]))
