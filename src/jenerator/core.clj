(ns jenerator.core
  (:require [jenerator.util :as u]
            [jenerator.literals :as jliterals]
            [jenerator.types :as jtypes]))

(declare
  primitive-data?)

(defn jenerate
  "Map -> String
   Function takes a map of AST data and poduces a string of generated sources"
  [data]
  (cond
    (nil? data) "null"
    (primitive-data? data) (str data)
    (ratio? data) (str (double data))
    (char? data) (jliterals/jenerate-char-literal data)
    (string? data) (jliterals/jenerate-string-literal data)
    (class? data) (jtypes/jenerate-class-ref data)
    :else (condp = (:jenerate data)
            :int (jliterals/jenerate-integer-literal data)
            :float (jliterals/jenerate-float-literal data)
            :annotation (jtypes/jenerate-annotation jenerate data)
            :type (jtypes/jenerate-type data)
            nil (u/error "Failed to find `:jenerate` tag in data: " data)
            (u/error "Illegal `:jenerate` tag in data: " data))))

(def primitive-data?
  "; Any -> Boolean
   Returns `true` if specified value is of primitive type,
   and may be jenerated by a simple stringification."
  (u/anyp integer? float? u/boolean?))