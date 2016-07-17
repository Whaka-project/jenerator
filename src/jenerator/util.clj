(ns jenerator.util)

(defn abs [x]
  (if (neg? x) (- x) x))

(defn error [& msg-parts]
  (throw (RuntimeException. (apply str msg-parts))))