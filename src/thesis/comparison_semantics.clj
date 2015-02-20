(ns thesis.comparison-semantics
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [thesis.algebra :refer [expr expr-transform expr-construct expr-clauses expr? identifier?]]
            [thesis.polynomial :refer [clausal-polynomial-form normalize-cpf cpf->str separate-scalar]]
            [clojure.tools.logging :as log]))

(defn comparison? [expr] (-> expr :operator #{:> :< :>= :<= :!= :==}))

(defn make-comparison-one-sided
  "Convert A _sign_ B to (A - B) _sign_ 0"
  [comparison]
  (let [[lside rside] (:params comparison)]
    (if (= 0 rside)
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
   :== [[">" "<"] [:and [:not "l>r"] [:not "l<r"]]]
   :!= [[">" "<"] [:or "l>r" "l<r"]]
   })

(defn record-comparison [bindings comparison]
  (let [one-sided (make-comparison-one-sided comparison)]
    (if-let [cpf (-> one-sided :params first clausal-polynomial-form not-empty)]
      (let [[normalizing-factor normalized-clauses] (normalize-cpf cpf)
            normalized-operator (if (> normalizing-factor 0)
                                  (:operator one-sided)
                                  (get flipped-operator (:operator one-sided)))
            [non-scalar scalar] (separate-scalar normalized-clauses)
            rside (* -1 scalar)]
        (if (empty? non-scalar)                             ; if left side empty
          [bindings (expr normalized-operator 0 rside)]
          (let [string-lside (cpf->str non-scalar)
                [operators pattern] (get operators-pattern normalized-operator)
                expr-params (into {} (map #(vector (str "l" % "r") (str string-lside % rside)) operators))
                new-expression (expr-construct expr-params pattern)
                new-bindings {non-scalar (into #{} (map #(vector (keyword %) rside) operators))}]
            [(merge-with set/union bindings new-bindings) new-expression])))
      ; if left side is empty (0), expression is true if operator allows equality (because right side is 0)
      [bindings (expr (:operator one-sided) 0 0)])))


;; Implementation of augment-hypothesis-with-comparison-implications

(defn comparison-implies [lside [[ox x] [oy y]]]
  (let [exprx (str lside (name ox) x)
        expry (str lside (name oy) y)]
    (case [ox oy]
      [:> :<] (and (>= x y) (expr :implies exprx [:not expry]))
      [:> :>] (and (> x y) (expr :implies exprx expry))
      [:< :<] (and (< x y) (expr :implies exprx expry))
      [:< :>] (and (<= x y) (expr :implies exprx [:not expry]))
      )))

(defn augment-one [hypothesis [clauses comparisons]]
  (let [string-clause (cpf->str clauses)
        permutations (for [c1 comparisons c2 comparisons :when (not= c1 c2)] [c1 c2])]
    (reduce (fn [hypothesis permutation]
              (if-let [implication (comparison-implies string-clause permutation)]
                (expr :and hypothesis implication)
                hypothesis))
            hypothesis
            permutations)))

(defn augment-hypothesis-with-comparison-implications
  "Given a hypothesis and comparison bindings from factualize-comparisons,
  builds implications between mentioned comparisons, and augments hypothesis with
  these implications that grow out of the semantics of > and <"
  [bindings hypothesis]
  (reduce augment-one hypothesis bindings))

(defn extract-comparisons
  [bindings expression]
  (if (expr? expression)
    (if (comparison? expression)
      (record-comparison bindings expression)
      (let [[bindings params] (reduce (fn [[bindings params] param]
                                        (let [[new-bindings new-param] (extract-comparisons bindings param)]
                                          [new-bindings (conj params new-param)]))
                                      [bindings []] (:params expression))]
        [bindings (apply expr (:operator expression) params)]
        ))
    [bindings expression]))

(defn factualize-comparisons [expression]
  (apply augment-hypothesis-with-comparison-implications (extract-comparisons {} expression)))


(comment "Not used anymore, but might be useful"
  (defn normalize-comparison-sign
    "Express comparison as a combination of >, < and not"
    [comparison]
    (let [[lside rside] (:params comparison)]
      (case (:operator comparison)
        (:> :<) comparison
        :>= (expr :not [:< lside rside])
        :<= (expr :not [:> lside rside])
        :== (expr :and [:not [:> lside rside]] [:not [:< lside rside]])
        :!= (expr :or [:> lside rside] [:< lside rside]))))

  (defn normalize-inequality [inequality]
    (let [operator (:operator inequality)
          lside-terms (clausal-polynomial-form (first (:params inequality)))
          flip-sign (> 0 (first (vals lside-terms)))
          flipped-operator (if flip-sign
                             (get {:> :<, :>= :<=, :< :>, :<= :>=} operator operator)
                             operator)
          positive-lside-terms (if flip-sign
                                 (multiply-addends -1 lside-terms)
                                 lside-terms)
          free-factor (get positive-lside-terms "" 0)
          lside-without-free-factor (dissoc positive-lside-terms "")
          lside-string (cpf->str lside-without-free-factor)
          ]
      (expr flipped-operator lside-string (- free-factor))))

  (defn normal-inequality-form [inequality]
    (-> inequality make-comparison-one-sided normalize-inequality))
  )


