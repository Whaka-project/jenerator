(ns jenerator.types_test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm]
            [jenerator.eval :as je])
  (:import [java.lang.annotation Target Retention]))

(deftest jenerate-annotation
  
  (testing "jenerate empty annotations from java.lang"
    (are [x y] (= x y)
      "@Deprecated()" (jenerate {:jenerate :annotation :class Deprecated})
      "@Deprecated()" (jenerate {:jenerate :annotation :class Deprecated :args {}})
      "@SuppressWarnings()" (jenerate {:jenerate :annotation :class SuppressWarnings})
      "@SuppressWarnings()" (jenerate {:jenerate :annotation :class SuppressWarnings :args {}})))
  
  (testing "jenerate empty annotations from other packages"
    (are [x y] (= x y)
      "@java.lang.annotation.Target()" (jenerate {:jenerate :annotation :class Target})
      "@java.lang.annotation.Target()" (jenerate {:jenerate :annotation :class Target :args {}})
      "@java.lang.annotation.Retention()" (jenerate {:jenerate :annotation :class Retention})
      "@java.lang.annotation.Retention()" (jenerate {:jenerate :annotation :class Retention :args {}})))
  
  (testing "jenerate annotation with single argument"
    (is (= "@Deprecated(value = 12)"
           (jenerate {:jenerate :annotation :class Deprecated :args {:value 12}}))))
  
  (testing "jenerate annotation with multiple arguments"
    (is (= "@Deprecated(value = 12, name = \"qwe\")"
           (jenerate {:jenerate :annotation :class Deprecated :args {:value 12 :name "qwe"}}))))
  
  (testing "jenerate annotation with array argument"
    (is (= "@Deprecated(value = {10, 20, 30})"
           (jenerate {:jenerate :annotation :class Deprecated :args {:value [10 20 30]}}))))
  
  (testing "jenerate annotation with multiple array arguments"
    (is (= "@Deprecated(value = {10, 20, 30}, names = {\"qwe\", \"rty\", \"qaz\"})"
           (jenerate {:jenerate :annotation :class Deprecated :args {:value [10 20 30]
                                                                     :names ["qwe" "rty" "qaz"]}}))))
  
  (testing "annotation arguments are also jenerated"
    (is (= "@Deprecated(value = 12L)"
           (jenerate {:jenerate :annotation
                      :class Deprecated
                      :args {:value {:jenerate :int :value 12 :long true}}}))))
  
  (testing "multiple annotation arguments are also jenerated"
    (is (= "@Deprecated(value = {12L, 12.22d, null})"
           (jenerate {:jenerate :annotation
                      :class Deprecated
                      :args {:value [(jm/long 12) (jm/double 12 22) nil]}}))))
    
  
  )