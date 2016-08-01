(ns jenerator.util
  (:require [clojure.string :as s]))

(defn abs
  "; Number -> Number
   Returns absolute value"
  [x] (if (neg? x) (- x) x))

(defn error
  "; Any* -> Nothing
   Throws runtime exception with string message
   constructed from specified variadic values."
  [& msg-parts] (throw (RuntimeException. (apply str msg-parts))))

(defn boolean?
  "; Any -> Boolean
   Returns true if specified value is either `true` or `false`"
  [x] (or (= x true) (= x false)))

(defn anyp
  "; [(Any* -> Boolean)* -> (Any* -> Boolean)]
   Takes any number of predicates that take equal number of arguments.
   Retuns function that takes any number of arguments and applies all predicates to them,
   until one of them returns `truthy` value. Returns `true` if at least one predicate returned truthy value."
  [& predicates] (fn [& args] (if (some #(apply % args) predicates) true false)))

(defn joiner
  "; String -> ([Any] -> String)
   Takes single delimiter string. Returns function that takes a collection
   and joins it into a string with the specified delimiter.

   ; String -> String -> String -> ([Any] -> String)
   Takes a delimter string, a prefix string, and a suffix string.
   Returns function that takes a collection and joins it into a string,
   with the specified delimiter, a prefix, and a suffix."
  ([delimiter]
    (joiner delimiter "" ""))
  ([delimiter pre post]
    (fn[coll] (str pre (s/join delimiter coll) post))))

(def jn-comma
  "; [Any] -> String
   Joins collection with `, ` delimiter."
  (joiner ", "))

(def jn-args
  "; [Any] -> String
   Joins collection with `, ` delimiter and round brackets."
  (joiner ", " "(" ")"))

(def jn-curly
  "; [Any] -> String
   Joins collection with `, ` delimiter and curly brackets."
  (joiner ", " "{" "}"))

(def jn-generics
  "; [Any] -> String
   Joins collection with `, ` delimiter and triangle brackets."
  (joiner ", " "<" ">"))

(defn map-entry
  "; (Any -> Any) -> (Any -> Any) -> {Any, Any}
   Takes two functions and a map.
   Applies first function to each key in the map,
   and second function to each value in the map.
   Return new map with result values."
  [key-fn val-fn m]
  (into {} (map (fn[[k v]] [(key-fn k) (val-fn v)]) m)))

(defn map-keys
  "; (Any -> Any) -> {Any, Any}
   Takes a function and a map.
   Applies the function to each key in the map.
   Returns new map with result values."
  [f map] (map-entry f identity map))

(defn map-values
  "; (Any -> Any) -> {Any, Any}
   Takes a function and a map.
   Applies the function to each value in the map.
   Returns new map with result values."
  [f map] (map-entry identity f map))
