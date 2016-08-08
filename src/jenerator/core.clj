(ns jenerator.core
  (:require [jenerator
             [declarations :as jdecl]
             [literals :as jliterals]
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
    (class? data) (jtypes/jenerate-class-ref data)
    :else (condp = (:jtag data)
            :int (jliterals/jenerate-integer-literal data)
            :float (jliterals/jenerate-float-literal data)
            :annotation (jtypes/jenerate-annotation jen data)
            :type (jtypes/jenerate-type data)
            :cast (jtypes/jenerate-cast jen data)
            :var (jdecl/jenerate-var jen data)
            nil (u/error "Failed to find `:jtag` tag in data: " data)
            (u/error "Illegal `:jtag` tag in data: " data))))

(def primitive-data?
  "; Any -> Boolean
   Returns `true` if specified value is of primitive type,
   and may be jenerated by a simple stringification."
  (u/anyp integer? float? u/boolean?))
