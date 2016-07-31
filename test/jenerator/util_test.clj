(ns jenerator.util-test
  (:require [jenerator.util :as u]
            [clojure.test :refer :all]))

(deftest test-abs
  (are [x y] (= x y)
    (u/abs 12)  12
    (u/abs -12) 12
    (u/abs -0)  0
    (u/abs -5.5) 5.5
    (u/abs 5.5)  5.5
    (u/abs 12/7) 12/7
    (u/abs -12/7) 12/7))

(deftest test-error
  (is (thrown-with-msg? RuntimeException #"msg"
                        (u/error "msg")))
  (is (thrown-with-msg? RuntimeException #"msg42"
                        (u/error "msg" 42)))
  (is (thrown-with-msg? RuntimeException #"12\[1 2 3\]"
                        (u/error 1 2 [1 2 3]))))
