(ns thesis.algebra
  (:require [clojure.string :as s]))

; An Expression is any kind of logical expression, it consists of an operator and its params,
; which in turn may be Expression, string identifiers, or numeric and boolean primitive constants
(defrecord Expression
  [operator params]

  Object
  (toString [this]
    (str "(" (-> this :operator name s/upper-case) " " (s/join " " (map str (:params this))) ")")))

(defmethod print-method Expression [expr ^java.io.Writer writer]
  (.write writer (str expr)))

(defn expr? [expression] (isa? (class expression) Expression))

(defn expr
  "Simplified expression constructor. Accepts nested structure of operator1 [operator2 param1 param2] param3 etc"
  [operator & params]
  {:pre [(keyword? operator) (not (empty? params))]}
  (let [realized-params (map (fn[param] (if (and (not (expr? param)) (coll? param)) (apply expr param) param)) params)]
    (->Expression operator realized-params)))

(defn expr-map [f expression] (apply expr (:operator expression) (map f (:params expression))))

(defn expr-atom? [expression] (not (expr? expression)))

(defn logical-expr? [predicate] (and (expr? predicate) (#{:and :or :not :implies} (:operator predicate))))

(defn logical-atom? [predicate] (not (logical-expr? predicate)))

(defn identifier? [identifier] (string? identifier))

(defn primitive? [predicate] (or (number? predicate) (true? predicate) (false? predicate)))

(defn expr-commutative? [expression]
  (#{:and :or :+ :* :==} (:operator expression)))

(defn merge-bindings [bindings-seq]
  (let [merged-bindings (apply merge-with #(if (= %1 %2) %1 nil) bindings-seq)]
    (and (not-any? #(nil? (fnext %)) merged-bindings) merged-bindings)))

(defn expr-match
  "Attempts to match expression by a pattern.
  Pattern is a structure of [operator param1 [operator param2 param3]...
  String params in pattern are matched against any sub-expression, identifier or primitive
  Boolean and numeric params in pattern are matched by equality.
  Returns a map of string params to their values, or false if there is no match"
  [pattern expression]
  (if (identifier? pattern)
    {pattern expression}
    (if (primitive? pattern)
      (and (= expression pattern) {})
      (and (expr? expression)
           (let [operator (first pattern) params (next pattern)]
             (and (= operator (:operator expression))
                  (let [forward-param-bindings (map expr-match params (:params expression))
                        forward-match (not-any? false? forward-param-bindings)
                        reverse-param-bindings (and (not forward-match)
                                                    (expr-commutative? expression)
                                                    (map expr-match params (reverse (:params expression))))
                        reverse-match (and reverse-param-bindings (not-any? false? reverse-param-bindings))
                        param-bindings (or (and forward-match forward-param-bindings)
                                          (and reverse-match reverse-param-bindings))]
                    (and param-bindings (merge-bindings param-bindings)))))))))

(defn expr-construct
  "Constructs expression from a pattern. For pattern format see expr-match
  String params in pattern are replaced by the values specified in param-bindings"
  [param-bindings pattern]
  (cond
    (primitive? pattern)
      pattern
    (identifier? pattern)
      (or (get param-bindings pattern)
          (throw (Exception. (str "Undefined expression param " pattern))))
    (coll? pattern)
      (let [operator (first pattern)
            params (map (partial expr-construct param-bindings) (next pattern))]
        (apply expr operator params))))


(defn expr-transform
  "Replaces parts of expression that match pattern-from, with expressions constructed from pattern-to
  and the matched param bindings"
  [pattern-from pattern-to expression]
  (if (expr? expression)
    (if-let [param-bindings (expr-match pattern-from expression)]
      (recur pattern-from pattern-to (expr-construct param-bindings pattern-to))
      (let [expr-with-transformed-params (expr-map (partial expr-transform pattern-from pattern-to) expression)]
        (if-let [param-bindings (expr-match pattern-from expr-with-transformed-params)]
          (recur pattern-from pattern-to (expr-construct param-bindings pattern-to))
          expr-with-transformed-params)))
    expression))

(defn expr-clauses
  "Given expression subexpr-A <clause-operator> subexpr-B <clause-operator> ..., produces a seq of (subexpr-A subexpr-B ... )"
  [clause-operator expression]
  (if (or (expr-atom? expression) (not= clause-operator (:operator expression)))
    [expression]
    (apply concat (map (partial expr-clauses clause-operator) (:params expression)))))
