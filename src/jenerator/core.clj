(ns jenerator.core
  (:require [jenerator
             [declarations :as jdecl]
             [literals :as jliterals]
             [operations :as jops]
             [statements :as jstats]
             [types :as jtypes]
             [util :as u]]))

(declare
  primitive-data?)

(defn jen
  "Map -> String
   Function takes a map of AST data and poduces a string of generated sources"
  [data]
  (cond
    (nil? data) "null"
    (primitive-data? data) (str data)
    (ratio? data) (str (double data))
    (char? data) (jliterals/jenerate-char-literal data)
    (string? data) (jliterals/jenerate-string-literal data)
    (or (class? data) (symbol? data)) (jtypes/jenerate-class-ref data)
    :else (condp = (:jtag data)
            :int (jliterals/jenerate-integer-literal data)
            :float (jliterals/jenerate-float-literal data)
            :annotation (jtypes/jenerate-annotation jen data)
            :type (jtypes/jenerate-type data)
            :cast (jtypes/jenerate-cast jen data)
            :var (jdecl/jenerate-var jen data)
            :decl (jdecl/jenerate-decl jen data)
            :pre (jops/jenerate-prefix jen data)
            :post (jops/jenerate-postfix jen data)
            :bin (jops/jenerate-binary jen data)
            :br (jops/jenerate-brackets jen data)
            :field (jops/jenerate-field-access jen data)
            :method (jops/jenerate-method-call jen data)
            :block (jstats/jenerate-code-block jen data)
            :if (jstats/jenerate-if jen data)
            :for (jstats/jenerate-for jen data)
            nil (u/error "Failed to find `:jtag` tag in data: " data)
            (u/error "Illegal `:jtag` tag in data: " data))))

(def primitive-data?
  "; Any -> Boolean
   Returns `true` if specified value is of primitive type,
   and may be jenerated by a simple stringification."
  (u/anyp integer? float? u/boolean?))
