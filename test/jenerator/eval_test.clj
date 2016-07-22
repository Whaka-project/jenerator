(ns jenerator.eval_test
  (:require [clojure.test :refer :all]
            [jenerator.eval :as je]
            [jenerator.fns :as fns]))

(deftest eval-simple-types
  
  (testing "Eval does not change simple types"
    (are [x y] (= x y)
      (je/eval nil) nil
      (je/eval :key) :key
      (je/eval 'sym) 'sym
      (je/eval true) true
      (je/eval 12) 12
      (je/eval 12.2) 12.2
      (je/eval 12/5) 12/5
      (je/eval \c) \c
      (je/eval "s") "s"
      (je/eval Long) Long))
  
  (testing "Eval does not change simple types in a collection or map"
    (are [x y] (= x y)
      (je/eval [nil 12 \c]) [nil 12 \c]
      (je/eval (list nil 12 \c)) (list nil 12 \c)
      (je/eval #{nil 12 \c}) #{nil 12 \c}
      (je/eval {:a nil, :b 12, :c \c}) {:a nil, :b 12, :c \c})))

(deftest eval-collections
 
  (testing "Eval processes matching ASTs in collections"
    (are [x y] (= x y)
      (je/eval [:int 12]) (fns/int 12)
      (je/eval [[:int 12] [:int 13]]) [(fns/int 12) (fns/int 13)]
      (je/eval [[:int 12] [:long 13] [:ann Deprecated]]) [(fns/int 12) (fns/long 13) (fns/ann Deprecated)]))
  
  (testing "Collections elements are evaluated recursively"
    (is (= (je/eval [ [:int 12] [ [:int 42] [:int 43] ] ])
           [ (fns/int 12) [ (fns/int 42) (fns/int 43) ]  ])))
  
  (testing "Collections eval preserves types"
    (are [x y] (= x y)
      (je/eval [[:int 12] [:int 13]]) [(fns/int 12) (fns/int 13)]
      (je/eval (list [:int 12] [:int 13])) (list (fns/int 12) (fns/int 13))
      (je/eval #{[:int 12] [:int 13]}) #{(fns/int 12) (fns/int 13)}))
  
  (testing "Eval recursively processes both keys and values in a map"
    (are [x y] (= x y)
      (je/eval {:key [:int 12]}) {:key (fns/int 12)}
      (je/eval {[:int 12] :val}) {(fns/int 12) :val}
      (je/eval {[:int 12] [:long 13]}) {(fns/int 12) (fns/long 13)})))