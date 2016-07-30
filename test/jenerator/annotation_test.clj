(ns jenerator.annotation_test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm])
  (:import [java.lang.annotation Target Retention]))

(deftest jenerate-annotation
  
  (testing "jenerate empty annotations from java.lang"
    (are [x y] (= x y)
      "@Deprecated()" (jen {:jtag :annotation :class Deprecated})
      "@Deprecated()" (jen {:jtag :annotation :class Deprecated :args {}})
      "@SuppressWarnings()" (jen {:jtag :annotation :class SuppressWarnings})
      "@SuppressWarnings()" (jen {:jtag :annotation :class SuppressWarnings :args {}})))
  
  (testing "jenerate empty annotations from other packages"
    (are [x y] (= x y)
      "@java.lang.annotation.Target()" (jen {:jtag :annotation :class Target})
      "@java.lang.annotation.Target()" (jen {:jtag :annotation :class Target :args {}})
      "@java.lang.annotation.Retention()" (jen {:jtag :annotation :class Retention})
      "@java.lang.annotation.Retention()" (jen {:jtag :annotation :class Retention :args {}})))
  
  (testing "jenerate annotation with single argument"
    (is (= "@Deprecated(value = 12)"
           (jen {:jtag :annotation :class Deprecated :args {:value 12}}))))
  
  (testing "jenerate annotation with multiple arguments"
    (is (= "@Deprecated(value = 12, name = \"qwe\")"
           (jen {:jtag :annotation :class Deprecated :args {:value 12 :name "qwe"}}))))
  
  (testing "jenerate annotation with array argument"
    (is (= "@Deprecated(value = {10, 20, 30})"
           (jen {:jtag :annotation :class Deprecated :args {:value [10 20 30]}}))))
  
  (testing "jenerate annotation with multiple array arguments"
    (is (= "@Deprecated(value = {10, 20, 30}, names = {\"qwe\", \"rty\", \"qaz\"})"
           (jen {:jtag :annotation :class Deprecated :args {:value [10 20 30]
                                                                     :names ["qwe" "rty" "qaz"]}}))))
  
  (testing "annotation arguments are also jenerated"
    (is (= "@Deprecated(value = 12L)"
           (jen {:jtag :annotation
                      :class Deprecated
                      :args {:value {:jtag :int :value 12 :long true}}}))))
  
  (testing "multiple annotation arguments are also jenerated"
    (is (= "@Deprecated(value = {12L, 12.22d, null})"
           (jen {:jtag :annotation
                      :class Deprecated
                      :args {:value [(jm/long 12) (jm/double 12 22) nil]}})))))

(deftest ann-function
  
  (testing "ann with a single class argument produces annotation AST with empty args map"
    (are [x y] (= x y)
      (jm/ann Deprecated) {:jtag :annotation :class Deprecated :args {}}))
  
  (testing "ann with a class argument and another single argument produces annotation AST with a `value` arg"
    (are [x y] (= x y)
      (jm/ann Deprecated 12) {:jtag :annotation :class Deprecated :args {:value 12}}
      (jm/ann Deprecated [12 22]) {:jtag :annotation :class Deprecated :args {:value [12 22]}}))
  
  (testing "ann with a class argument and any number of key value pairs produces AST with multiple names args"
    (are [x y] (= x y)
      (jm/ann Deprecated :name 12) {:jtag :annotation :class Deprecated :args {:name 12}}
      (jm/ann Deprecated :some [12 22]) {:jtag :annotation :class Deprecated :args {:some [12 22]}}
      (jm/ann Deprecated :name "qwe" :age 42) {:jtag :annotation :class Deprecated :args {:name "qwe" :age 42}}))
  
  (testing "ann with a class argument and :args key and NON-map value - produces AST with a single `args` arg"
    (are [x y] (= x y)
      (jm/ann Deprecated :args 12) {:jtag :annotation :class Deprecated :args {:args 12}}
      (jm/ann Deprecated :args [12 22]) {:jtag :annotation :class Deprecated :args {:args [12 22]}}
      (jm/ann Deprecated :args Long) {:jtag :annotation :class Deprecated :args {:args Long}}))
  
  (testing "ann with a class argument and :args key and MAP value - produces AST with the map as args"
    (are [x y] (= x y)
      (jm/ann Deprecated :args {}) {:jtag :annotation :class Deprecated :args {}}
      (jm/ann Deprecated :args {:value 12}) {:jtag :annotation :class Deprecated :args {:value 12}}
      (jm/ann Deprecated :args {:name "qwe" :age 42}) {:jtag :annotation :class Deprecated :args {:name "qwe" :age 42}})))
