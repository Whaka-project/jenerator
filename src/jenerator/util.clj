(ns jenerator.util)

(defn error [& msg-parts]
  (throw (RuntimeException. (apply str msg-parts))))