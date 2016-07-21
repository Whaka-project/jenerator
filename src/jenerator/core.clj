(ns jenerator.core
  (:require [jenerator.util :as u]
            [jenerator.literals :as jliterals]
            [jenerator.types :as jtypes]))

(defn jenerate
  "Map -> String
   Function takes a map of AST data and poduces a string of generated sources"
  [data]
  (cond
    (nil? data) "null"
    (contains? #{true false} data) (str data)
    (char? data) (jliterals/jenerate-char-literal data)
    (string? data) (jliterals/jenerate-string-literal data)
    (integer? data) (str data)
    (float? data) (str data)
    (ratio? data) (str (double data))
    :else (condp = (:jenerate data)
            :int (jliterals/jenerate-integer-literal data)
            :float (jliterals/jenerate-float-literal data)
            :annotation (jtypes/jenerate-annotation jenerate data)
            nil (u/error "Failed to find `:jenerate` tag in data: " data)
            (u/error "Illegal `:jenerate` tag in data: " data))))