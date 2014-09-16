(ns thesis.core
  (:import
    (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)
    (thesis ImperativeLanguageLexer ImperativeLanguageParser))
  (:require [clojure.string :as s])
  (:gen-class))

(defn parse-expr [s]
  (let [lexer (ImperativeLanguageLexer. (ANTLRInputStream. s))
        tokens (CommonTokenStream. lexer)
        parser (ImperativeLanguageParser. tokens)]
    (.provingStructure parser)))

(defn context->seq [context]
  (if (zero? (.getChildCount context))
    (.getText context)
    (cons (s/replace (str (.getClass context)) #"^.+\$(.+)Context$" "$1") (map #(context->seq (.getChild context %)) (range 0 (.getChildCount context))))))


; (defn -main
;   "I don't do a whole lot ... yet."
;   [& args]
;   (println "Hello, World!"))
