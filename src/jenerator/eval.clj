(ns jenerator.eval
  (:refer-clojure :exclude [eval])
  (:require [jenerator.util :as u]
            [jenerator.fns :as jm]
            [clojure.core.match :refer [match]]))

(declare
  simple-data?)

(defn eval [data]
  (cond
    (simple-data? data) data
    :else (match [data]
         
      [[:int x & rest]] (apply jm/int (int x) rest)
      [[:long x & rest]] (apply jm/long (long x) rest)

      [[:double & rest]] (apply jm/double rest)
      [[:float & rest]] (apply jm/float rest)
      
      [[:ann class]] (jm/ann class)
      [[:ann class value]] (jm/ann class (eval value))
      [[:ann class :args args]] (jm/ann class :args (eval args))
      [[:ann class key val & args]] (apply jm/ann class key (eval val) (eval args))

      :else (cond
        (map? data) (u/map-entry eval eval data)
        (list? data) (->> data (map eval) (into '()) reverse)
        (coll? data) (->> data (map eval) (into (empty data)))
        :else data))))

(def simple-data?
  "Any -> Boolean
   Takes one argument - returns true if argument is of the simple type and should not be specially evaluated."
  (u/anyp nil? keyword? symbol? u/boolean? integer? float? ratio? char? string? class?))