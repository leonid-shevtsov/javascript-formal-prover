(ns thesis.wp
  (:require [thesis.algebra :refer [expr expr-map expr? identifier?]]))

(declare command-wp)

(defn replace-identifier-in-expression
  [identifier value construct]
  (cond
    (expr? construct)
      (expr-map
        (partial replace-identifier-in-expression identifier value) construct)

    (identifier? construct)
      (if (= construct identifier)
          value
          construct)

    :else construct))


(defn conditional-wp [predicate if-command else-command postcondition]
  (expr :and
        (expr :implies predicate (command-wp postcondition if-command))
        (expr :implies (expr :not predicate) (command-wp postcondition else-command))))

; TODO need to check variant, too
(defn loop-wp [ predicate loop-command invariant postcondition]
  (expr :and
        invariant
        (expr :and
              (expr :implies (expr :and predicate invariant) (command-wp postcondition loop-command))
              (expr :implies (expr :and [:not predicate] invariant) postcondition))))

(defn command-wp [postcondition command]
  (let [c-type (:type command)
        c-params (:params command)]
    (case c-type
      :noop postcondition
      :sequence (reduce command-wp postcondition (reverse c-params))
      :assign (let [[identifier value] c-params]
                (replace-identifier-in-expression identifier value postcondition))
      :if (let [[predicate if-command else-command] c-params] (conditional-wp predicate if-command else-command postcondition))
      :while (let [[predicate loop-command invariant] c-params] (loop-wp predicate loop-command invariant postcondition)))))

(defn weakest-predicate [program]
  (command-wp (:postcondition program) (:commands program)))

(defn program-correctness-hypothesis
  "Returns hypothesis that the program is correct, that is:
  precondition => weakest-predicate(commands, postcondition)"
  [program]
  (expr :implies (:precondition program) (weakest-predicate program)))