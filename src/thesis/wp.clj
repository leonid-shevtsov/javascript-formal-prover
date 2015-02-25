(ns thesis.wp
  (:require [thesis.algebra :refer [expr expr-map expr? identifier?]]
            [clojure.tools.logging :as log]))

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
(defn loop-wp [loop-id loop-condition loop-command invariant bound postcondition]
  (let [bound-variable (str "bound_" loop-id)]
    (log/spyf "Loop condition %s" (reduce (partial expr :and)
            [invariant
             [:implies [:and loop-condition invariant] (command-wp invariant loop-command)]
             [:implies [:and loop-condition invariant] [:> bound 0]]
             [:implies [:and loop-condition invariant] (replace-identifier-in-expression bound-variable bound
                                                                                         (command-wp (expr :< bound bound-variable) loop-command))]
             [:implies [:and [:not loop-condition] invariant] postcondition]]))))

(defn command-wp [postcondition command]
  (let [c-type (:type command)
        c-params (:params command)]
    (case c-type
      :noop postcondition
      :sequence (reduce command-wp postcondition (reverse c-params))
      :assign (let [[identifier value] c-params]
                (replace-identifier-in-expression identifier value postcondition))
      :if (let [[predicate if-command else-command] c-params] (conditional-wp predicate if-command else-command postcondition))
      :while (let [[predicate loop-command invariant bound] c-params] (loop-wp (hash command) predicate loop-command invariant bound postcondition)))))

(defn weakest-precondition [program]
  (command-wp (:postcondition program) (:commands program)))

(defn program-correctness-hypothesis
  "Returns hypothesis that the program is correct, that is:
  precondition => weakest-predicate(commands, postcondition)"
  [program]
  (expr :implies (:precondition program) (weakest-precondition program)))