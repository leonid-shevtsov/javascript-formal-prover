(ns thesis.types
  (:require [clojure.string :as s]))

(defrecord Expression [operator params]
  Object
  (toString [this]
    (str "(" (-> this :operator name s/upper-case) " " (s/join " " (map str (:params this))) ")")))

(defrecord Command [type params])

(defn expr [operator & params] (->Expression operator params))

(defn map-expr [fn expression] (apply expr (:operator expression) (map fn (:params expression))))

(defn expr? [expression] (isa? (class expression) Expression))

(defn pred? [predicate] (and (expr? predicate) (#{:and :or :not :implies} (:operator predicate))))

(defn identifier? [identifier] (string? identifier))

(defn primitive? [predicate] (or (number? predicate) (true? predicate) (false? predicate)))

(defn pred-atom? [predicate] (not (pred? predicate)))

(defn expr-atom? [expression] (not (expr? expression)))

(defn expr-commutative? [expression]
  (#{:and :or :+ :* :==} (:operator expression)))

(defn merge-bindings [bindings-seq]
  (let [merged-bindings (apply merge-with #(if (= %1 %2) %1 nil) bindings-seq)]
    (and (not-any? #(nil? (fnext %)) merged-bindings) merged-bindings)))

(defn expr-match
  "Returns a map of named bindings for given pattern."
  [pattern expression]
  (if (string? pattern)
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

(defn pattern->expr [param-bindings pattern]
  (cond
    (primitive? pattern)
      pattern
    (identifier? pattern)
      (or (get param-bindings pattern)
          (throw (Exception. (str "Undefined expression param " pattern))))
    (coll? pattern)
      (let [operator (first pattern)
            params (map (partial pattern->expr param-bindings) (next pattern))]
        (apply expr operator params))))


(defn expr-transform
  [pattern-from pattern-to expression]
  (if (expr? expression)
    (if-let [param-bindings (expr-match pattern-from expression)]
      (expr-transform pattern-from pattern-to (pattern->expr param-bindings pattern-to))
      (let [expr-with-transformed-params (map-expr (partial expr-transform pattern-from pattern-to) expression)]
        (if-let [param-bindings (expr-match pattern-from expr-with-transformed-params)]
          (expr-transform pattern-from pattern-to (pattern->expr param-bindings pattern-to))
          expr-with-transformed-params)))
    expression))


(comment "Examples:"
  (expr-transform [:not [:and "x" "y"]]
                  [:or [:not "x"] [:not "y"]]
                  predicate)

  (expr-transform [:or "x" [:and "y" "z"]]
                  [:and [:or "x" "y"] [:or "x" "z"]]
                  predicate)

  (expr-transform [:not [:not "x"]] "x" predicate)
  )
