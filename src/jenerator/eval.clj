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

(defn- eval-float [num]
  (let [s (str num)
        ss (clojure.string/split s #"[\.eE]")]
    (match [ss]
      [([a b c] :seq)] (eval-float-parts a b c)
      [([a b] :seq)] (eval-float-parts a b nil)
      :else (u/error "Faled to eval float: '" s "'!"))))

(defn eval [data]
  (match [data]
         
    ; (eval 12) -> (jm/int 12)
    [(x :guard integer?)] (jm/int x)
    
    ; (eval [12]) -> (jm/int 12)
    [([(x :guard integer?)] :seq)] (jm/int x)
    
    ; (eval [12 :long]) -> (jm/long 12)
    [([(x :guard integer?) :long] :seq)] (jm/long x)
    
    ; (eval [12 :oct]) -> (jm/int 12 :oct)
    [([(x :guard integer?) (base :guard keyword?)] :seq)] (jm/int x base)
    
    ; (eval [12 :oct :long]) -> (jm/long 12 :oct)
    [([(x :guard integer?) (base :guard keyword?) :long] :seq)] (jm/long x base)
    
    ; (eval 12.2) -> (jm/float 12 2)
    ; (eval 12.2e-5) -> (jm/loat 12 2 -5)
    [(x :guard float?)] (eval-float x)
    
    ; (eval 5/2) -> (eval (double 5/2))
    [(x :guard ratio?)] (eval-float (double x))
    
    ; (eval [12.2] -> (eval 12.2)
    ; (eval [12.2e-5] -> (eval 12.2e-5)
    [([(x :guard float?)] :seq)] (eval-float x)
    
    ; (eval [5/2]) -> (eval 5/2)
    [([(x :guard ratio?)] :seq)] (eval-float (double x))
    
    ; (eval [12 2]) -> (jm/float 12 2)
    [([(x :guard integer?) (y :guard integer?)] :seq)] (jm/float x y)
    
    ; (eval [12 2 -5]) -> (jm/float 12 2 -5)
    [([(x :guard integer?) (y :guard integer?) :e (z :guard integer?)] :seq)] (jm/float x y :e z)
    
    :else (u/error "Unknown data-type: " data)))