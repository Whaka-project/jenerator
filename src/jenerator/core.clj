(ns jenerator.core
  (:require [jenerator.util :as u]
            [jenerator.literals :as jliterals]))

(defn jenerate
  "Map -> String
   Function takes a map of AST data and poduces a string of generated sources"
  [data]
  (cond
    (nil? data) "null"
    (contains? #{true false} data) (str data)
    (char? data) (str "'" data "'")
    :else (condp = (:jenerate data)
            :int (jliterals/jenerate-integer-literal data)
            :float (jliterals/jenerate-float-literal data)
            nil (u/error "Failed to find `:jenerate` tag in data: " data)
            (u/error "Illegal `:jenerate` tag in data: " data))))