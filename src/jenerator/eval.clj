(ns jenerator.eval
  (:require [jenerator.util :as u]
            [jenerator.fns :as jm]
            [clojure.core.match :refer [match]]))

(declare
  simple-data?
  eval-map-vals)

(defn eval [data]
  (cond
    (keyword? data) data
    (simple-data? data) data
    :else (match [data]
         
            [[:int x & rest]] (apply jm/int (int x) rest)
            [[:long x & rest]] (apply jm/long (long x) rest)
    
            [[:double & rest]] (apply jm/double rest)
            [[:float & rest]] (apply jm/float rest)
            
            [[:ann class]] (jm/ann class)
            [[:ann class value]] (jm/ann class (eval value))
            [[:ann class :args args]] (jm/ann class :args (if (map? args) (eval-map-vals args) args))
            [[:ann class key val & args]] (apply jm/ann class key (eval val) (map eval args))
    
            :else data)))

(defn simple-data? [data]
  (or (contains? #{true false nil} data)
      (->> data
        ((juxt integer? float? ratio? char? string? class?))
        (some identity))))

(defn eval-map-vals [map]
  (u/map-values eval map))