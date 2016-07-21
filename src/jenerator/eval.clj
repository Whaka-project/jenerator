(ns jenerator.eval
  (:require [jenerator.util :as u]
            [jenerator.fns :as jm]
            [clojure.core.match :refer [match]]))

(defn- find-shift [exp]
  (let [shift (count (take-while #{\0} exp))
        trim (apply str (drop shift exp))]
    [(if (empty? trim) "0" trim) (if (= shift (count exp)) 0 shift)]))

(defn- translate-float-parts [[whole-str fraction-str exponent-str]]
  (let [[trim-str shift] (find-shift fraction-str)]
    [(read-string whole-str) (read-string trim-str) :e (read-string (or exponent-str "0")) :shift shift]))

(defn- eval-float [num]
  (let [ss (clojure.string/split (str num) #"[\.eE]")
        eval-fn (if ((u/classp Float) num) jm/float jm/double)]
    (if (or (empty? ss) (> (count ss) 3))
      (u/error "Faled to eval float: '" (str num) "'!")
      (apply eval-fn (translate-float-parts ss)))))

(defn eval [data]
  (cond
    (contains? #{true false nil} data) data
    (char? data) data
    (string? data) data
    :else (match [data]
         
            ; (eval 12) -> (jm/int 12)
            [(x :guard integer?)] (jm/int (int x))
    
            [[:int x]] (jm/int (int x))
            [[:int x base]] (jm/int (int x) base)
            
            [[:long x]] (jm/long (long x))
            [[:long x base]] (jm/long (long x) base)
    
            [(x :guard float?)] (eval-float x)
            [(x :guard ratio?)] (eval-float (double x))
    
            [[:double (x :guard ratio?)]] (eval-float (double x))
            [[:float (x :guard ratio?)]] (eval-float (float x))
            
            [[:double x]] (eval-float (double x))
            [[:float x]] (eval-float (float x))
    
            [[:double x y]] (jm/double x y)
            [[:double x y :e z]] (jm/double x y :e z)
            
            [[:float x y]] (jm/float x y)
            [[:float x y :e z]] (jm/float x y :e z)
    
            :else data)))