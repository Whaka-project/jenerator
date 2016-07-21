(ns jenerator.eval
  (:require [jenerator.util :as u]
            [jenerator.fns :as jm]
            [clojure.core.match :refer [match]]))

(defn- find-shift [exp]
  (let [shift (count (take-while #{\0} exp))
        trim (apply str (drop shift exp))]
    [(if (empty? trim) "0" trim) (if (= shift (count exp)) 0 shift)]))

(defn- eval-float-parts [whole fraction exponent map-fn]
  (let [[trim shift] (find-shift fraction)]
    (if exponent
      (map-fn (read-string whole) (read-string trim) :e (read-string exponent) :shift shift)
      (map-fn (read-string whole) (read-string trim) :shift shift))))

(defn- eval-float [num eval-fn]
  (let [s (str num)
        ss (clojure.string/split s #"[\.eE]")]
    (match [ss]
      [([a] :seq)] (eval-float-parts a nil nil eval-fn)
      [([a b] :seq)] (eval-float-parts a b nil eval-fn)
      [([a b c] :seq)] (eval-float-parts a b c eval-fn)
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
                                    jm/float jm/double))
            
            [(x :guard ratio?)] (eval-float (double x) jm/double)
    
            [[:double (x :guard ratio?)]] (eval-float (double x) jm/double)
            [[:float (x :guard ratio?)]] (eval-float (double x) jm/float)
            
            [[:double x]] (eval-float x jm/double)
            [[:float x]] (eval-float x jm/float)
    
            [[:double x y]] (jm/double x y)
            [[:double x y :e z]] (jm/double x y :e z)
            
            [[:float x y]] (jm/float x y)
            [[:float x y :e z]] (jm/float x y :e z)
    
            :else (u/error "Unknown data-type: " data))))