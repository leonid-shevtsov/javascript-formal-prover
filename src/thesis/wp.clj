(ns thesis.wp
  (use thesis.semantic-analyzer thesis.types)
  (:import (thesis.types Command Expression)))

(declare command-wp)

(defn replace-identifier-in-expression
  [identifier value construct]
  (cond
    (expr? construct)
      (map-expr
        (partial replace-identifier-in-expression identifier value) construct)

    (identifier? construct)
      (if (= construct identifier)
          value
          construct)

    true construct))


(defn conditional-wp [^Expression predicate ^Command if-command ^Command else-command ^Expression postcondition]
  (expr :and
        (expr :implies predicate (command-wp postcondition if-command))
        (expr :implies (expr :not predicate) (command-wp postcondition else-command))))

; TODO need to check variant, too
(defn loop-wp [^Expression predicate ^Command loop-command ^Expression invariant ^Expression postcondition]
  (expr :and
        invariant
        (expr :and
              (expr :implies (expr :and predicate invariant) (command-wp postcondition loop-command))
              (expr :implies (expr :and (expr :not predicate) invariant) postcondition))))

(defn command-wp [^Expression postcondition ^Command command]
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
