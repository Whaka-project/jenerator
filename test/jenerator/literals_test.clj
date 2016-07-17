(ns jenerator.literals-test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.macro :as jm]
            [jenerator.eval :as je]))

(deftest jenerate-int
  
  (testing "jenerate int with default values"
    (is (= "0" (jenerate {:jenerate :int}))))
  
  (testing "jenerate int with default base"
    (is (= "12" (jenerate {:jenerate :int :value 12}))))
  
  (testing "jenerate long with default base"
    (is (= "12L" (jenerate {:jenerate :int :value 12 :long true}))))
  
  (testing "jenerate decimal int with explicit base"
    (is (= "42" (jenerate {:jenerate :int :value 42 :base :dec}))))
  
  (testing "jenerate decimal int with explicit base"
    (is (= "42L" (jenerate {:jenerate :int :value 42 :base :dec :long true}))))
  
  (testing "jenerate octal int"
    (is (= "012" (jenerate {:jenerate :int :value 10 :base :oct}))))
  
  (testing "jenerate octal long"
    (is (= "012L" (jenerate {:jenerate :int :value 10 :base :oct :long true}))))
  
  (testing "jenerate hex int"
    (is (= "0x12" (jenerate {:jenerate :int :value 18 :base :hex}))))
  
  (testing "jenerate hex long"
    (is (= "0x12L" (jenerate {:jenerate :int :value 18 :base :hex :long true}))))
  
  (testing "jenerate binary int"
    (is (= "0b1010" (jenerate {:jenerate :int :value 10 :base :bin}))))
  
  (testing "jenerate binary long"
    (is (= "0b1010L" (jenerate {:jenerate :int :value 10 :base :bin :long true})))))

(deftest int-long-macros
  
  (testing "Int macro"
    (is (= {:jenerate :int :value 12} (jm/int 12))))
  
  (testing "Int macro with explicit base"
    (is (= {:jenerate :int :value 12 :base :dec} (jm/int 12 :dec))))
  
  (testing "Int macro with octal explicit base"
    (is (= {:jenerate :int :value 42 :base :oct} (jm/int 42 :oct))))
  
  (testing "Long macro"
    (is (= {:jenerate :int :value 42 :long true} (jm/long 42))))
  
  (testing "Long macro with explicit base"
    (is (= {:jenerate :int :value 42 :long true :base :dec} (jm/long 42 :dec))))
  
  (testing "Long macro with explicit octal base"
    (is (= {:jenerate :int :value 42 :long true :base :oct} (jm/long 42 :oct)))))

(deftest int-long-eval
  
  (testing "Eval ints and longs"
    (is (= (jm/int 12) (je/eval 12)))
    (is (= (jm/int 12) (je/eval [12])))
    (is (= (jm/int 12 :oct) (je/eval [12 :oct])))
    (is (= (jm/long 12) (je/eval [12 :long])))
    (is (= (jm/long 12 :oct) (je/eval [12 :oct :long])))))

(deftest jenerate-float
  
  (testing "jenerate float with default values"
    (is (= "0.0" (jenerate {:jenerate :float}))))
  
  (testing "jenerate float with default fraction"
    (is (= "12.0" (jenerate {:jenerate :float :whole 12}))))
  
  (testing "jenerate float with default whole"
    (is (= "0.12" (jenerate {:jenerate :float :fraction 12}))))
  
  (testing "jenerate float with no exponent"
    (is (= "12.22" (jenerate {:jenerate :float :whole 12 :fraction 22}))))
  
  (testing "jenerate float with shift"
    (is (= "12.0022" (jenerate {:jenerate :float :whole 12 :fraction 22 :shift 2}))))
  
  (testing "jenerate float with exponent"
    (is (= "12.22e-5" (jenerate {:jenerate :float :whole 12 :fraction 22 :exponent -5}))))

  (testing "jenerate float with exponent and shift"
    (is (= "12.0022e-5" (jenerate {:jenerate :float :whole 12 :fraction 22 :exponent -5 :shift 2})))))

(deftest float-macro
  
  (testing "Macro with a whole"
    (is (= {:jenerate :float :whole 12} (jm/float 12))))
  
  (testing "Macro with a whole and fraction"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent nil :shift 0} (jm/float 12 22))))
  
  (testing "Macro with a whole and fraction and exponent"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent -5 :shift 0} (jm/float 12 22 :e -5))))

  (testing "Macro with a whole and fraction and exponent and shift"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent -5 :shift 2} (jm/float 12 22 :e -5 :shift 2)))))

(deftest float-eval
  
  (testing "Eval floats"
    (is (= (jm/float 2 5) (je/eval 2.5)))
    (is (= (jm/float 2 5 :e -5) (je/eval 2.5e-5)))
    (is (= (jm/float 0 25 :shift 1) (je/eval 2.5e-2)))
    (is (= (jm/float 2 5) (je/eval 5/2)))
    (is (= (jm/float 2 5) (je/eval [2.5])))
    (is (= (jm/float 2 5) (je/eval [5/2])))
    (is (= (jm/float 12 22) (je/eval [12 22])))
    (is (= (jm/float 12 22 :e -5) (je/eval [12 22 :e -5])))))

(deftest simple-literals
  
  (testing "Nil"
    (is (= "null" (jenerate nil)))
    (is (= nil (je/eval nil))))
  
  (testing "Booleans"
    (is (= "true" (jenerate true)))
    (is (= "false" (jenerate false)))
    (is (= true (je/eval true)))
    (is (= false (je/eval false))))
  
  (testing "Chars"
    (is (= "'a'" (jenerate \a)))
    (is (= "'1'" (jenerate \1)))
    (is (= "'.'" (jenerate \.)))
    (is (= "'\\''" (jenerate \')))
    (is (= "'\\\"'" (jenerate \")))
    (is (= "'\\t'" (jenerate \tab)))
    (is (= "'\\b'" (jenerate \backspace)))
    (is (= "'\\f'" (jenerate \formfeed)))
    (is (= "'\\r'" (jenerate \return)))
    (is (= "'\\\\'" (jenerate \\)))
    (is (= \a (je/eval \a)))
    (is (= \1 (je/eval \1)))
    (is (= \. (je/eval \.)))
    (is (= \tab (je/eval \tab))))
  
  (testing "Strings"
    (is (= "\"qwe\"" (jenerate "qwe")))
    (is (= "\"q\\te\"" (jenerate "q\te")))
    (is (= "\"q\\be\"" (jenerate "q\be")))
    (is (= "\"q\\re\"" (jenerate "q\re")))
    (is (= "\"q\\fe\"" (jenerate "q\fe")))
    (is (= "\"q\\\\e\"" (jenerate "q\\e")))
    (is (= "\"q'e\"" (jenerate "q'e")))
    (is (= "\"q\\\"e\"" (jenerate "q\"e")))))