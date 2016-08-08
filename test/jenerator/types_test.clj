(ns jenerator.types_test
  (:require [clojure.test :refer :all]
            [jenerator.core :refer :all]
            [jenerator.fns :as jm]))

(deftest jenerate-type
  
  (testing "jenerate primitive type with a keyword"
    (are [x y] (= x y)
      "int"     (jen {:jtag :type :type :int})
      "boolean" (jen {:jtag :type :type :boolean})))
  
  (testing "jenerate reference type with a class"
    (are [x y] (= x y)
      "Long" (jen {:jtag :type :type Long})
      "Byte" (jen {:jtag :type :type Byte})))
  
  (testing "jenerate generic type with a :generics key"
    (are [x y] (= x y)
         
      ; Note how empty :generics seq produces a "diamond" type
      "Long<>" (jen {:jtag :type :type Long :generics []})
      
      ; Note that elements of the :generics seq may only be another type AST map
      ; Types jeneration isn't fully-recursive, so you cannot specify plain classes here.
      ; But at the same time you don't have to specify :jtag tag, since only another type may be used at the place.
      "Byte<Float>" (jen {:jtag :type :type Byte :generics [{:type Float}]})))

  (testing "Jenerate primitive array type using :array key"
    (are [x y] (= x y)
      "int[]"    (jen {:jtag :type :type :int :array 1})
      "long[][]" (jen {:jtag :type :type :long :array 2})))
  
  (testing "Jenerate reference array type using :array key"
    (are [x y] (= x y)
      "Long[]"         (jen {:jtag :type :type Long :array 1})
      "Long<>[]"       (jen {:jtag :type :type Long :array 1 :generics []})
      "Long<Byte>[]"   (jen {:jtag :type :type Long :array 1 :generics [{:type Byte}]})
      "Long<Byte>[][]" (jen {:jtag :type :type Long :array 2 :generics [{:type Byte}]})))
  
  (testing "Jenerate reference type with array as generic"
    (are [x y] (= x y)
      "Long<Byte[]>"   (jen {:jtag :type :type Long :generics [{:type Byte :array 1}]})
      "Long<Byte[]>[]" (jen {:jtag :type :type Long :generics [{:type Byte :array 1}] :array 1})
      "Long<int[]>"    (jen {:jtag :type :type Long :generics [{:type :int :array 1}]})
      
      ; Note that nothing in jenerator itself stops you from jenerating primitive generic type.
      ; You compilation (at least on Java8 or earlier) will fail, but just know that you can do it.
      ; In case `valhalla` ever finally comes around.
      "Long<int>"      (jen {:jtag :type :type Long :generics [{:type :int}]})))
  
  (testing "Errors"
           
    ; `:type` value must be a keyword or a class 
    (is (thrown? Error (jen {:jtag :type :type 42})))
    
    ; If `:type` is a keyword - it's must be a valid primitive type
    (is (thrown? Exception (jen {:jtag :type :type :qwe})))
    
    ; `:array` value must be an integer
    (is (thrown? Error (jen {:jtag :type :type :int :array "qwe"})))
    
    ; `:array` value must be a POSITIVE or zero integer
    (is (thrown? Error (jen {:jtag :type :type :int :array -2})))
    
    ; `:generics` must be a nil or a sequential collection
    (is (thrown? Error (jen {:jtag :type :type Long :generics "qwe"})))
    (is (thrown? Error (jen {:jtag :type :type Long :generics {}})))))

(deftest type-fn
  
  (testing "Simple type jen"
    (are [x y] (= x y)
      (jm/type Long)   {:jtag :type :type Long :generics nil :array 0}
      (jm/type Number) {:jtag :type :type Number :generics nil :array 0}))
  
  (testing "Array type jen"
    (are [x y] (= x y)
      (jm/type Long 1)   {:jtag :type :type Long :generics nil :array 1}
      (jm/type Number 2) {:jtag :type :type Number :generics nil :array 2}))
  
  (testing "Generic type jen"
    (are [x y] (= x y)
      (jm/type [Long])        {:jtag :type :type Long :generics [] :array 0}
      (jm/type [Number Byte]) {:jtag :type :type Number :generics [(jm/type Byte)] :array 0}))
  
  (testing "Deep generic type jen"
    (are [x y] (= x y)
      (jm/type [Long [Iterable]])       {:jtag :type :type Long :generics [(jm/type [Iterable])] :array 0}
      (jm/type [Long [Iterable Byte]])  {:jtag :type :type Long :generics [(jm/type [Iterable Byte])] :array 0}))
  
  (testing "String test"
    (are [x y] (= x y)
      (jen (jm/type Long))                       "Long"
      (jen (jm/type Long 1))                     "Long[]"
      (jen (jm/type [Long] 1))                   "Long<>[]"
      (jen (jm/type [Long Byte]))                "Long<Byte>"
      (jen (jm/type [Long Byte Short]))          "Long<Byte, Short>"
      (jen (jm/type [Long Byte [Short]]))        "Long<Byte, Short<>>"
      (jen (jm/type [Long Byte [Short String]])) "Long<Byte, Short<String>>")))

(deftest jenerate-cast
  (are [x y] (= x y)

    ; Simple cast - value
    (jen {:jtag :cast :value 12 :type (jm/type :int)})
    "((int) 12)"

    ; Value is also jenerated
    (jen {:jtag :cast :value (jm/int 12 :oct) :type (jm/type Long)})
    "((Long) 014)"

    ; Nested casts
    (jen {:jtag :cast :type (jm/type :int 1) :value {:jtag :cast :type (jm/type Object) :value nil}})
    "((int[]) ((Object) null))"))

(deftest cast-fn
  (are [x y] (= x y)

    ; Cast may be called with a sigle value - then value returned unchanged
    (jm/cast 12) 12
    (jm/cast (jm/int 22)) (jm/int 22)

    ; Cast may be called with a value and a type - then cast-AST is produce
    (jm/cast 12 (jm/type :int)) {:jtag :cast :value 12 :type (jm/type :int)}
    (jm/cast 12 (jm/type :int 1)) {:jtag :cast :value 12 :type (jm/type :int 1)}

    ; Calling cast with a non-map type[s] will result in implicit call to 'fns/type'
    (jm/cast 12 :int) (jm/cast 12 (jm/type :int))
    (jm/cast 12 [Long String]) (jm/cast 12 (jm/type [Long String]))
    (jm/cast 12 Long String) (jm/cast 12 (jm/type Long) (jm/type String))

    ; Calling cast with multiple types is equal to calling multiple nested casts
    (jm/cast 12 :int :long) (jm/cast (jm/cast 12 :int) :long)
    (jm/cast 12 Long String Object) (jm/cast (jm/cast (jm/cast 12 Long) String) Object)

    ; Cast jeneration is pretty straightforward
    (jen (jm/cast 12 :int)) "((int) 12)"
    (jen (jm/cast 12 [Class])) "((Class<>) 12)"
    (jen (jm/cast nil Object [Class Long] (jm/type :int 1))) "((int[]) ((Class<Long>) ((Object) null)))"))
