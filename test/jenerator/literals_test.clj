(ns jenerator.literals-test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm]
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

(deftest int-long-fns
  
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
    (are [x y] (= x y)
      (jm/int 12)  (je/eval 12)
      (jm/int 12)  (je/eval [:int 12])
      (jm/long 12) (je/eval [:long 12])
      (jm/int 12 :oct)  (je/eval [:int 12 :oct])
      (jm/long 12 :oct) (je/eval [:long 12 :oct]))))

(deftest jenerate-float
  
  (testing "jenerate float with default values"
    (is (= "0.0" (jenerate {:jenerate :float}))))
  
  (testing "jenerate float with double suffix"
    (is (= "0.0d" (jenerate {:jenerate :float :suffix :d}))))
  
  (testing "jenerate float with float suffix"
    (is (= "0.0f" (jenerate {:jenerate :float :suffix :f}))))
  
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

(deftest float-fns
  
  (testing "Macro with a whole"
    (is (= {:jenerate :float :whole 12 :suffix :f} (jm/float 12))))
  
  (testing "Double macro with a whole"
    (is (= {:jenerate :float :whole 12 :suffix :d} (jm/double 12))))
  
  (testing "Macro with a whole and fraction"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent 0 :shift 0 :suffix :f} (jm/float 12 22))))
  
  (testing "Macro with a whole and fraction and exponent"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent -5 :shift 0 :suffix :f} (jm/float 12 22 :e -5))))

  (testing "Macro with a whole and fraction and exponent and shift"
    (is (= {:jenerate :float :whole 12 :fraction 22 :exponent -5 :shift 2 :suffix :f} (jm/float 12 22 :e -5 :shift 2)))))

(deftest float-eval
  
  (testing "Eval floats"
    (are [x y] (= x y)
           
      (jm/double 2 5) (je/eval 2.5)
      (jm/double 2 5) (je/eval 5/2)
      (jm/double 2 5 :e -5)     (je/eval 2.5e-5)
      (jm/double 0 25 :shift 1) (je/eval 2.5e-2)
     
      (jm/float 2 5) (je/eval (float 2.5))
      (jm/float 2 5) (je/eval (float 5/2))
     
      (jm/double 2 5) (je/eval [:double 2.5])
      (jm/float 2 5)  (je/eval [:float 2.5])
      (jm/float 2 5)  (je/eval [:float 5/2])
     
      (jm/double 12 22) (je/eval [:double 12 22])
      (jm/float 12 22)  (je/eval [:float 12 22])
     
      (jm/double 12 22 :e -5) (je/eval [:double 12 22 :e -5])
      (jm/float 12 22 :e -5)  (je/eval [:float 12 22 :e -5]))))

(deftest simple-literals
  
  (testing "Nil"
    (are [x y] (= x y)
      "null" (jenerate nil)
      nil    (je/eval nil)))
  
  (testing "Booleans"
    (are [x y] (= x y)
      "true"  (jenerate true)
      "false" (jenerate false)
      true  (je/eval true)
      false (je/eval false)))
  
  (testing "Chars"
    (are [x y] (= x y)
      "'a'"    (jenerate \a)
      "'1'"    (jenerate \1)
      "'.'"    (jenerate \.)
      "'\\''"  (jenerate \')
      "'\\\"'" (jenerate \")
      "'\\t'"  (jenerate \tab)
      "'\\b'"  (jenerate \backspace)
      "'\\f'"  (jenerate \formfeed)
      "'\\r'"  (jenerate \return)
      "'\\\\'" (jenerate \\)
      \a   (je/eval \a)
      \1   (je/eval \1)
      \.   (je/eval \.)
      \tab (je/eval \tab)))
  
  (testing "Strings"
    (are [x y] (= x y)
      "\"qwe\""    (jenerate "qwe")
      "\"q\\te\""  (jenerate "q\te")
      "\"q\\be\""  (jenerate "q\be")
      "\"q\\re\""  (jenerate "q\re")
      "\"q\\fe\""  (jenerate "q\fe")
      "\"q\\\\e\"" (jenerate "q\\e")
      "\"q'e\""    (jenerate "q'e")
      "\"q\\\"e\"" (jenerate "q\"e"))))