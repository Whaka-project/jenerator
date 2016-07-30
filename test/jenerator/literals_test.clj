(ns jenerator.literals-test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm]))

(deftest jenerate-int
  
  (testing "jenerate int with default values"
    (is (= "0" (jen {:jtag :int}))))
  
  (testing "jenerate int with default base"
    (is (= "12" (jen {:jtag :int :value 12}))))
  
  (testing "jenerate long with default base"
    (is (= "12L" (jen {:jtag :int :value 12 :long true}))))
  
  (testing "jenerate decimal int with explicit base"
    (is (= "42" (jen {:jtag :int :value 42 :base :dec}))))
  
  (testing "jenerate decimal int with explicit base"
    (is (= "42L" (jen {:jtag :int :value 42 :base :dec :long true}))))
  
  (testing "jenerate octal int"
    (is (= "012" (jen {:jtag :int :value 10 :base :oct}))))
  
  (testing "jenerate octal long"
    (is (= "012L" (jen {:jtag :int :value 10 :base :oct :long true}))))
  
  (testing "jenerate hex int"
    (is (= "0x12" (jen {:jtag :int :value 18 :base :hex}))))
  
  (testing "jenerate hex long"
    (is (= "0x12L" (jen {:jtag :int :value 18 :base :hex :long true}))))
  
  (testing "jenerate binary int"
    (is (= "0b1010" (jen {:jtag :int :value 10 :base :bin}))))
  
  (testing "jenerate binary long"
    (is (= "0b1010L" (jen {:jtag :int :value 10 :base :bin :long true})))))

(deftest int-long-fns
  
  (testing "Int macro"
    (is (= {:jtag :int :value 12} (jm/int 12))))
  
  (testing "Int macro with explicit base"
    (is (= {:jtag :int :value 12 :base :dec} (jm/int 12 :dec))))
  
  (testing "Int macro with octal explicit base"
    (is (= {:jtag :int :value 42 :base :oct} (jm/int 42 :oct))))
  
  (testing "Long macro"
    (is (= {:jtag :int :value 42 :long true} (jm/long 42))))
  
  (testing "Long macro with explicit base"
    (is (= {:jtag :int :value 42 :long true :base :dec} (jm/long 42 :dec))))
  
  (testing "Long macro with explicit octal base"
    (is (= {:jtag :int :value 42 :long true :base :oct} (jm/long 42 :oct)))))

(deftest jenerate-float
  
  (testing "jenerate float with default values"
    (is (= "0.0" (jen {:jtag :float}))))
  
  (testing "jenerate float with double suffix"
    (is (= "0.0d" (jen {:jtag :float :suffix :d}))))
  
  (testing "jenerate float with float suffix"
    (is (= "0.0f" (jen {:jtag :float :suffix :f}))))
  
  (testing "jenerate float with default fraction"
    (is (= "12.0" (jen {:jtag :float :whole 12}))))
  
  (testing "jenerate float with default whole"
    (is (= "0.12" (jen {:jtag :float :fraction 12}))))
  
  (testing "jenerate float with no exponent"
    (is (= "12.22" (jen {:jtag :float :whole 12 :fraction 22}))))
  
  (testing "jenerate float with shift"
    (is (= "12.0022" (jen {:jtag :float :whole 12 :fraction 22 :shift 2}))))
  
  (testing "jenerate float with exponent"
    (is (= "12.22e-5" (jen {:jtag :float :whole 12 :fraction 22 :exponent -5}))))

  (testing "jenerate float with exponent and shift"
    (is (= "12.0022e-5" (jen {:jtag :float :whole 12 :fraction 22 :exponent -5 :shift 2})))))

(deftest float-fns
  
  (testing "Macro with a whole"
    (is (= {:jtag :float :whole 12 :suffix :f} (jm/float 12))))
  
  (testing "Double macro with a whole"
    (is (= {:jtag :float :whole 12 :suffix :d} (jm/double 12))))
  
  (testing "Macro with a whole and fraction"
    (is (= {:jtag :float :whole 12 :fraction 22 :exponent 0 :shift 0 :suffix :f} (jm/float 12 22))))
  
  (testing "Macro with a whole and fraction and exponent"
    (is (= {:jtag :float :whole 12 :fraction 22 :exponent -5 :shift 0 :suffix :f} (jm/float 12 22 :e -5))))

  (testing "Macro with a whole and fraction and exponent and shift"
    (is (= {:jtag :float :whole 12 :fraction 22 :exponent -5 :shift 2 :suffix :f} (jm/float 12 22 :e -5 :shift 2)))))

(deftest simple-literals
  
  (testing "Nil"
    (are [x y] (= x y)
      (jen nil) "null"))   
  
  (testing "Booleans"
    (are [x y] (= x y)
      (jen true)  "true" 
      (jen false) "false"))  
  
  (testing "Numbers"
    (are [x y] (= x y)
      (jen 12)   "12"  
      (jen 12.2) "12.2"))  
  
  (testing "Ratio produces imprecise double"
    (are [x y] (= x y)
      (jen 12/5) (str (double 12/5))))
  
  (testing "Chars"
    (are [x y] (= x y)
      "'a'"    (jen \a)
      "'1'"    (jen \1)
      "'.'"    (jen \.)
      "'\\''"  (jen \')
      "'\\\"'" (jen \")
      "'\\t'"  (jen \tab)
      "'\\b'"  (jen \backspace)
      "'\\f'"  (jen \formfeed)
      "'\\r'"  (jen \return)
      "'\\\\'" (jen \\)))
  
  (testing "Strings"
    (are [x y] (= x y)
      "\"qwe\""    (jen "qwe")
      "\"q\\te\""  (jen "q\te")
      "\"q\\be\""  (jen "q\be")
      "\"q\\re\""  (jen "q\re")
      "\"q\\fe\""  (jen "q\fe")
      "\"q\\\\e\"" (jen "q\\e")
      "\"q'e\""    (jen "q'e")
      "\"q\\\"e\"" (jen "q\"e"))))
