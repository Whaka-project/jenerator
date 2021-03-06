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

(deftest test-boolean?
  (are [x y] (= x y)
    (u/boolean? true) true
    (u/boolean? false) true
    (u/boolean? 12) false
    (u/boolean? "") false
    (u/boolean? nil) false
    (u/boolean? (boolean "")) true
    (u/boolean? (boolean nil)) true))

(deftest test-anyp
  (let [number-or-string? (u/anyp number? string?)
        boolean-or-list? (u/anyp u/boolean? list?)]
    (are [x y] (= x y)
      (number-or-string? 12) true
      (number-or-string? "") true
      (number-or-string? []) false
      (number-or-string? nil) false
      (boolean-or-list? 12) false
      (boolean-or-list? false) true
      (boolean-or-list? '(1 2 3)) true
      (boolean-or-list? nil) false)))

(deftest test-joiner
  (let [joiner-a (u/joiner "a")
        joiner-bac (u/joiner "a" "b" "c")]
    (are [x y] (= x y)
      (joiner-a []) ""
      (joiner-a [1 2 3]) "1a2a3"
      (joiner-a ["a" "b" "c"]) "aabac"
      (joiner-bac []) "bc"
      (joiner-bac [1 2 3]) "b1a2a3c"
      (joiner-bac ["a" "b" "c"]) "baabacc")))

(deftest test-specific-joiners
  (are [x y] (= x y)
    (u/jn-comma [1 2 3]) "1, 2, 3"
    (u/jn-comma []) ""
    (u/jn-args [1 2 3]) "(1, 2, 3)"
    (u/jn-args []) "()"
    (u/jn-curly [1 2 3]) "{1, 2, 3}"
    (u/jn-curly []) "{}"
    (u/jn-generics [1 2 3]) "<1, 2, 3>"
    (u/jn-generics []) "<>"))

(deftest test-map-entry
  (are [x y] (= x y)
    (u/map-entry inc dec {1 2 3 4}) {2 1 4 3}
    (u/map-entry dec inc {1 2 3 4}) {0 3 2 5}
    (u/map-entry name keyword {:qwe "aaa" :rty "bbb"}) {"qwe" :aaa "rty" :bbb}))

(deftest test-map-keys
  (are [x y] (= x y)
    (u/map-keys inc {1 2 3 4}) {2 2 4 4}
    (u/map-keys dec {1 2 3 4}) {0 2 2 4}
    (u/map-keys name {:qwe "aaa" :rty "bbb"}) {"qwe" "aaa" "rty" "bbb"}))

(deftest test-map-values
  (are [x y] (= x y)
    (u/map-values inc {1 2 3 4}) {1 3 3 5}
    (u/map-values dec {1 2 3 4}) {1 1 3 3}
    (u/map-values keyword {:qwe "aaa" :rty "bbb"}) {:qwe :aaa :rty :bbb}))
