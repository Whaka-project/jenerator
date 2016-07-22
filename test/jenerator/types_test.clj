(ns jenerator.types_test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm]
            [jenerator.eval :as je]))

(deftest jenerate-type
  
  (testing "jenerate primitive type with a keyword"
    (are [x y] (= x y)
      "int"     (jenerate {:jenerate :type :type :int})
      "boolean" (jenerate {:jenerate :type :type :boolean})))
  
  (testing "jenerate reference type with a class"
    (are [x y] (= x y)
      "Long" (jenerate {:jenerate :type :type Long})
      "Byte" (jenerate {:jenerate :type :type Byte})))
  
  (testing "jenerate generic type with a :generics key"
    (are [x y] (= x y)
         
      ; Note how empty :generics seq produces a "diamond" type
      "Long<>" (jenerate {:jenerate :type :type Long :generics []})
      
      ; Note that elements of the :generics seq may only be another type AST map
      ; Types jeneration isn't fully-recursive, so you cannot specify plain types here.
      ; But at the same time you don't have to specify :jenerate tag, since only reference
      "Byte<Float>" (jenerate {:jenerate :type :type Byte :generics [{:type Float}]})))

  (testing "Jenerate primitive array type using :array key"
    (are [x y] (= x y)
      "int[]"    (jenerate {:jenerate :type :type :int :array 1})
      "long[][]" (jenerate {:jenerate :type :type :long :array 2})))
  
  (testing "Jenerate reference array type using :array key"
    (are [x y] (= x y)
      "Long[]"         (jenerate {:jenerate :type :type Long :array 1})
      "Long<>[]"       (jenerate {:jenerate :type :type Long :array 1 :generics []})
      "Long<Byte>[]"   (jenerate {:jenerate :type :type Long :array 1 :generics [{:type Byte}]})
      "Long<Byte>[][]" (jenerate {:jenerate :type :type Long :array 2 :generics [{:type Byte}]})))
  
  (testing "Jenerate reference type with array as generic"
    (are [x y] (= x y)
      "Long<Byte[]>"   (jenerate {:jenerate :type :type Long :generics [{:type Byte :array 1}]})
      "Long<Byte[]>[]" (jenerate {:jenerate :type :type Long :generics [{:type Byte :array 1}] :array 1})
      "Long<int[]>"    (jenerate {:jenerate :type :type Long :generics [{:type :int :array 1}]})
      
      ; Note that nothing in jenerator itself stops you from jenerating primitive generic type.
      ; You compilation (at least on Java8 or earlier) will fail, but just know that you can do it.
      ; In case `valhalla` ever finally comes around.
      "Long<int>"      (jenerate {:jenerate :type :type Long :generics [{:type :int}]})))
  
  (testing "Errors"
           
    ; `:type` value must be a keyword or a class 
    (is (thrown? Error (jenerate {:jenerate :type :type 42})))
    
    ; If `:type` is a keyword - it's must be a valid primitive type
    (is (thrown? Exception (jenerate {:jenerate :type :type :qwe})))
    
    ; `:array` value must be an integer
    (is (thrown? Error (jenerate {:jenerate :type :type :int :array "qwe"})))
    
    ; `:array` value must be a POSITIVE or zero integer
    (is (thrown? Error (jenerate {:jenerate :type :type :int :array -2})))
    
    ; `:generics` must be a nil or a sequential collection
    (is (thrown? Error (jenerate {:jenerate :type :type Long :generics "qwe"})))
    (is (thrown? Error (jenerate {:jenerate :type :type Long :generics {}})))))