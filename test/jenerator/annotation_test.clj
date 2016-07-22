(ns jenerator.annotation_test
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
                      :args {:value [(jm/long 12) (jm/double 12 22) nil]}})))))

(deftest ann-function
  
  (testing "ann with a single class argument produces annotation AST with empty args map"
    (are [x y] (= x y)
      (jm/ann Deprecated) {:jenerate :annotation :class Deprecated :args {}}))
  
  (testing "ann with a class argument and another single argument produces annotation AST with a `value` arg"
    (are [x y] (= x y)
      (jm/ann Deprecated 12) {:jenerate :annotation :class Deprecated :args {:value 12}}
      (jm/ann Deprecated [12 22]) {:jenerate :annotation :class Deprecated :args {:value [12 22]}}))
  
  (testing "ann with a class argument and any number of key value pairs produces AST with multiple names args"
    (are [x y] (= x y)
      (jm/ann Deprecated :name 12) {:jenerate :annotation :class Deprecated :args {:name 12}}
      (jm/ann Deprecated :some [12 22]) {:jenerate :annotation :class Deprecated :args {:some [12 22]}}
      (jm/ann Deprecated :name "qwe" :age 42) {:jenerate :annotation :class Deprecated :args {:name "qwe" :age 42}}))
  
  (testing "ann with a class argument and :args key and NON-map value - produces AST with a single `args` arg"
    (are [x y] (= x y)
      (jm/ann Deprecated :args 12) {:jenerate :annotation :class Deprecated :args {:args 12}}
      (jm/ann Deprecated :args [12 22]) {:jenerate :annotation :class Deprecated :args {:args [12 22]}}
      (jm/ann Deprecated :args Long) {:jenerate :annotation :class Deprecated :args {:args Long}}))
  
  (testing "ann with a class argument and :args key and MAP value - produces AST with the map as args"
    (are [x y] (= x y)
      (jm/ann Deprecated :args {}) {:jenerate :annotation :class Deprecated :args {}}
      (jm/ann Deprecated :args {:value 12}) {:jenerate :annotation :class Deprecated :args {:value 12}}
      (jm/ann Deprecated :args {:name "qwe" :age 42}) {:jenerate :annotation :class Deprecated :args {:name "qwe" :age 42}})))

(deftest eval-ann
  
  (testing "Simple ann AST equal to calling `ann` function"
    (are [x y] (= x y)
      (je/eval [:ann Deprecated]) (jm/ann Deprecated)
      (je/eval [:ann Deprecated 12]) (jm/ann Deprecated 12)
      (je/eval [:ann Deprecated :age 12]) (jm/ann Deprecated :age 12)
      (je/eval [:ann Deprecated :ages [12 22]]) (jm/ann Deprecated :ages [12 22])
      (je/eval [:ann Deprecated :name "qwe" :age 42]) (jm/ann Deprecated :name "qwe" :age 42)
      (je/eval [:ann Deprecated :args 42]) (jm/ann Deprecated :args 42)
      (je/eval [:ann Deprecated :args {}]) (jm/ann Deprecated :args {})
      (je/eval [:ann Deprecated :args {:name "qwe"}]) (jm/ann Deprecated :args {:name "qwe"})))
  
  (testing "Recursive eval - each ann argument's value is also evaluated"
    (is (= (je/eval [:ann Deprecated [:long 12]])
           (jm/ann Deprecated (jm/long 12)))))
  
  (testing "Recursive eval - ann argument values evaluated from collection"
    (is (= (je/eval [:ann Deprecated [[:long 42] [:int 12] [:double 12 22]]])
           (jm/ann Deprecated [(jm/long 42) (jm/int 12) (jm/double 12 22)]))))
  
  (testing "Recursive eval - ann argument values evaluated from implicit map"
    (is (= (je/eval [:ann Deprecated :age [:long 42] :height [:double 6 2]])
           (jm/ann Deprecated :age (jm/long 42) :height (jm/double 6 2)))))
  
  (testing "Recursive eval - ann argument values evaluated from implicit map collection value"
    (is (= (je/eval [:ann Deprecated :ages [[:long 42] [:int 12] [:double 12 22]]])
           (jm/ann Deprecated :ages [(jm/long 42) (jm/int 12) (jm/double 12 22)]))))
  
  (testing "Recursive eval - ann argument values evaluated from explicit map"
    (is (= (je/eval [:ann Deprecated :args {:age [:long 42] :height [:double 6 2]}])
           (jm/ann Deprecated :args {:age (jm/long 42) :height (jm/double 6 2)}))))
  
  (testing "Recursive eval - ann argument values evaluated from explicit map collection value"
    (is (= (je/eval [:ann Deprecated :ages [[:long 42] [:int 12] [:double 12 22]]])
           (jm/ann Deprecated :args {:ages [(jm/long 42) (jm/int 12) (jm/double 12 22)]})))))