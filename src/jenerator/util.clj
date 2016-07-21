(ns jenerator.util
  (:require [clojure.string :as s]))

(defn abs [x]
  (if (neg? x) (- x) x))

(defn error [& msg-parts]
  (throw (RuntimeException. (apply str msg-parts))))

(defn classp [expected-class]
  (fn [x] (= (class x) expected-class)))

(defn joiner
  ([delimiter]
    (joiner delimiter "" ""))
  ([delimiter pre post]
    (fn[coll] (str pre (s/join delimiter coll) post))))

(def jn-comma (joiner ", "))
(def jn-args (joiner ", " "(" ")"))
(def jn-curly (joiner ", " "{" "}"))
(def jn-generics (joiner ", " "<" ">"))