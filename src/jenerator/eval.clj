(ns jenerator.eval
  (:require [jenerator.util :as u]
            [jenerator.macro :as jm]
            [clojure.core.match :refer [match]]))

(defn- find-shift [exp]
  (let [shift (count (take-while #{\0} exp))
        trim (apply str (drop shift exp))]
    [(if (empty? trim) "0" trim) (if (= shift (count exp)) 0 shift)]))

(defn- eval-float-parts [whole fraction exponent]
  (let [[trim shift] (find-shift fraction)]
    (if exponent
      (jm/float (read-string whole) (read-string trim) :e (read-string exponent) :shift shift)
      (jm/float (read-string whole) (read-string trim) :shift shift))))

(defn- eval-double-parts [whole fraction exponent]
  (let [[trim shift] (find-shift fraction)]
    (if exponent
      (jm/double (read-string whole) (read-string trim) :e (read-string exponent) :shift shift)
      (jm/double (read-string whole) (read-string trim) :shift shift))))

(defn- eval-float [num eval-fn]
  (let [s (str num)
        ss (clojure.string/split s #"[\.eE]")]
    (match [ss]
      [([a] :seq)] (eval-fn a nil nil)
      [([a b] :seq)] (eval-fn a b nil)
      [([a b c] :seq)] (eval-fn a b c)
      :else (u/error "Faled to eval float: '" s "'!"))))

(defn eval [data]
  (cond
    (contains? #{true false nil} data) data
    (char? data) data
    (string? data) data
    :else (match [data]
         
            ; (eval 12) -> (jm/int 12)
            [(x :guard integer?)] (jm/int x)
    
            [[:int x]] (jm/int x)
            [[:int x base]] (jm/int x base)
            
            [[:long x]] (jm/long x)
            [[:long x base]] (jm/long x base)
    
            [(x :guard float?)] (eval-float x
                                  (if ((u/classp Float) x)
                                    eval-float-parts eval-double-parts))
            
            [(x :guard ratio?)] (eval-float (double x) eval-double-parts)
    
            [[:double (x :guard ratio?)]] (eval-float (double x) eval-double-parts)
            [[:float (x :guard ratio?)]] (eval-float (double x) eval-float-parts)
            
            [[:double x]] (eval-float x eval-double-parts)
            [[:float x]] (eval-float x eval-float-parts)
    
            [[:double x y]] (jm/double x y)
            [[:double x y :e z]] (jm/double x y :e z)
            
            [[:float x y]] (jm/float x y)
            [[:float x y :e z]] (jm/float x y :e z)
    
            :else (u/error "Unknown data-type: " data))))