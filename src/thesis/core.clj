(ns thesis.core
  (:import
    (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)
    (thesis ImperativeLanguageLexer ImperativeLanguageParser))
  (:require [clojure.string :as s])
  (:gen-class))

(defn parse-structure [s]
  (let [lexer (ImperativeLanguageLexer. (ANTLRInputStream. s))
        tokens (CommonTokenStream. lexer)
        parser (ImperativeLanguageParser. tokens)]
    (.provingStructure parser)))

(defn context-keyword [context]
  (keyword (.toLowerCase (s/replace (s/replace (str (.getClass context)) #"^.+\$(.+)Context$" "$1")
                                    #"([a-z])([A-Z])" "$1-$2"
                                    ))))

(defn context->seq [context]
  (if (zero? (.getChildCount context))
    (.getText context)
    (cons (context-keyword context)
          (map #(context->seq (.getChild context %)) (range 0 (.getChildCount context))))))

(defn find-child [context-seq child-name]
  (first (filter #(= (first %) child-name) (next context-seq))))

(defn skip-semicolons [seq]
  (remove #{";"} seq))

(defn join-predicates [predicate-ary]
  (if (= 1 (count predicate-ary))
    (first predicate-ary)
    (reduce #(list :and-predicate %1 "&&" %2) predicate-ary)))

(defn replace-assignment-in-tree [identifier expression tree]
  (if (string? tree)
    tree
    (let [tree-type (first tree)
          tree-data (next tree)]
    (if (and (= :atom-expression tree-type)
             (= :atom (ffirst tree-data))
             (= identifier (fnext (first tree-data))))
      expression
      (cons tree-type (map (partial replace-assignment-in-tree identifier expression) tree-data))
      ))))

(def wp-fns
  {:assignment-command #(replace-assignment-in-tree %2 %4 %1)
;   :sequence-command (fn [*commands] (remove #{"{" "}" ";"} commands))
;   :conditional-command identity
   :loop-command identity})

(defn build-wp [predicate command]
  (let [command-type (first command)
        command-data (next command)]
    (apply (command-type wp-fns) predicate command-data)))

(declare print-tree)

(def pt print-tree)
(def print-fns
  {:and-predicate #(str "(and " (pt %1) " " (pt %3) ")")
   :or-predicate #(str "(or " (pt %1) " " (pt %3) ")")
   :mult-expression #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :sum-expression #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :comparison #(str "(" %2 " " (pt %1) " " (pt %3) ")")
   :numeric-constant identity
   :identifier identity
   :atom-predicate #(pt %1)
   :atom-expression #(pt %1)
   :atom #(pt %1)
   :predicate-atom #(pt %1)
  })

(defn print-tree [tree]
  (let [tree-type (first tree)
        tree-data (next tree)
        ]
    (apply (tree-type print-fns) tree-data)))

(defn program->predicate [program]
  (let [assertion-comment (find-child program :assertion-comment)
        [precondition postcondition] (map #(-> (find-child (find-child assertion-comment %) :predicates)
                                                 next
                                                 skip-semicolons
                                                 join-predicates)
                                            [:preconditions :postconditions])
        commands (skip-semicolons (next (find-child program :commands)))
        ]
    (reduce #(build-wp %1 %2) postcondition (reverse commands))))
