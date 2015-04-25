(ns thesis.program-semantics
  (:use thesis.algebra))

(defrecord Command [type params])

(defn- find-child
  "Find a child seq with a given name inside the context seq
  For example: find (:plus 2 3) in (:multiply 4 (:plus 2 3))"
  [parse-seq child-name]
  (first (filter #(= (first %) child-name) (next parse-seq))))

(defn- conjoin-predicates
  "Given a list of predicates, produce a conjunction of all of them"
  [predicates]
  (reduce (fn [& predicate-pair] (apply expr :and predicate-pair)) predicates))


(defn translate-expression
  "Convert expression from the parse tree structure to a structure for further processing"
  [expression-tree]
  (let [e-type (first expression-tree)
        e-params (next expression-tree)
        tr translate-expression]
    (case e-type
      :and-predicate (let [[x _and y] e-params]
                       (expr :and (tr x) (tr y)))
      :or-predicate (let [[x _or y] e-params]
                      (expr :or (tr x) (tr y)))
      :implies-predicate (let [[x _implies y] e-params]
                      (expr :implies (tr x) (tr y)))
      (:mult-expression :sum-expression :comparison) (let [[x operator y] e-params]
                                                       (expr (keyword operator) (tr x) (tr y)))

      :not-atom (let [[_not x] e-params]
                  (expr :not (tr x)))
      (:parens-predicate :parens-expression) (let [[_paren expression _paren] e-params]
                          (tr expression))

      :identifier (first e-params)
      :numeric-constant (Integer/parseInt (first e-params))
      :boolean-constant (Boolean/parseBoolean (first e-params))

      ; pass-through nodes
      (:atom-predicate :atom-expression :atom :predicate-atom) (tr (first e-params)))))

(defn translate-command
  [command-tree]
  (let [c-type (first command-tree)
        c-params (next command-tree)]
    (case c-type
      :empty-command (->Command :noop [])

      :assignment-command (let [[[_identifier identifier] _equals expression] c-params]
                            (->Command :assign [identifier (translate-expression expression)]))

      :sequence-command (let [[_bracket [_commands & commands] _bracket] c-params]
                          (if (== 1 (count commands))
                            (translate-command (first commands))
                            (->Command :sequence (map translate-command commands))))

      :conditional-command (let [[_if _paren predicate _paren command] c-params]
                             (->Command :if [(translate-expression predicate) (translate-command command) (->Command :noop [])]))

      :full-conditional-command (let [[_if _paren predicate _paren if-command _else else-command] c-params]
                                  (->Command :if [(translate-expression predicate) (translate-command if-command) (translate-command else-command)]))

      :loop-command (let [[[_loop-comment _star [_invariant _inv invariant] [_bound _bound bound-function] _star] _while _paren predicate _paren command] c-params]
                             (->Command :while [(translate-expression predicate) (translate-command command) (translate-expression invariant) (translate-expression bound-function)])))))

(defn parse-tree->program
  "Convert parse tree into a semantic structure"
  [parse-tree]
  (let [assertion-comment (find-child parse-tree :assertion-comment)
        [precondition postcondition] (map (fn [key-name] (-> (find-child (find-child assertion-comment key-name) :predicates)
                                               next
                                               (#(map translate-expression %))
                                               conjoin-predicates))
                                          [:preconditions :postconditions])
        commands (next (find-child parse-tree :commands))]
    {:precondition precondition, :postcondition postcondition, :commands (->Command :sequence (map translate-command commands))}))
