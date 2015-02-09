(ns thesis.parser
  (:require [clojure.string :as s])
  (:import (org.antlr.v4.runtime ANTLRInputStream CommonTokenStream)
           (org.antlr.v4.runtime.tree ParseTree)
           (thesis ImperativeLanguageLexer ImperativeLanguageParser)))

(defn- antlr-parse
  "Invokes ANTLR parser to produce a raw parse structure from source code"
  [source-code]
  (let [lexer (ImperativeLanguageLexer. (ANTLRInputStream. source-code))
        tokens (CommonTokenStream. lexer)
        parser (ImperativeLanguageParser. tokens)]
    (.provingStructure parser)))

(defn- context-keyword
  "For object of class Something$SomeParseContext, return :some-parse"
  [^ParseTree context]
  (keyword (.toLowerCase (s/replace (s/replace (str (.getClass context))
                                               #"^.+\$(.+)Context$" "$1")
                                    #"([a-z])([A-Z])" "$1-$2"))))

(defn zap-semicolons
  "Remove all semicolons from context sequence"
  [parse-seq]
  (remove #{";"} parse-seq))

(defn- context->seq
  "Convert a parse context into a nested list"
  [^ParseTree context]
  (if (zero? (.getChildCount context))
    (.getText context)
    (cons (context-keyword context)
          (zap-semicolons (for [child-index (range 0 (.getChildCount context))]
            (context->seq (.getChild context child-index)))))))

(defn parse
  "Produce a parse tree from source code"
  [source-code]
  (-> source-code antlr-parse context->seq))