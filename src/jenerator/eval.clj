(ns jenerator.eval
  (:require [jenerator.util :as u]
            [jenerator.macro :as jm]
            [clojure.core.match :refer [match]]))

(defn- eval-float [num]
  (let [s (str num)
        ss (clojure.string/split s #"[\.eE]")
        nums (map read-string ss)]
    (match [nums]
      [([a b c] :seq)] (jm/float a b c)
      [([a b] :seq)] (jm/float a b)
      :else (u/error "Faled to eval float: '" s "'!"))))

(defn eval [data]
  (match [data]
    [(x :guard integer?)] (jm/int x)
    [([(x :guard integer?)] :seq)] (jm/int x)
    [([(x :guard integer?) (base :guard keyword?)] :seq)] (jm/int x base)
    [([(x :guard integer?) (base :guard keyword?) long] :seq)] (jm/int x base long)
    [(x :guard float?)] (eval-float x)
    [(x :guard ratio?)] (eval-float (double x))
    [([(x :guard float?)] :seq)] (eval-float x)
    [([(x :guard ratio?)] :seq)] (eval-float (double x))
    [([(x :guard integer?) (y :guard integer?)] :seq)] (jm/float x y)
    [([(x :guard integer?) (y :guard integer?) (z :guard integer?)] :seq)] (jm/float x y z)
    :else (u/error "Unknown data-type: " data)
    ))