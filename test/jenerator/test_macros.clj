(ns jenerator.test-macros
  (:require  [clojure.test :as t]))

(defmacro are= [& body]
  `(t/are [x y] (= x y) ~@body))

(defmacro testing= [name & body]
  `(t/testing ~name (are= ~@body)))

(defmacro deftest= [name & body]
  `(t/deftest ~name (are= ~@body)))
