(ns jenerator.operations-test
  (:require [clojure.test :refer :all]
            [jenerator
             [core :refer [jen]]
             [fns :as j]
             [test-macros :refer [deftest=]]]))

(deftest= jenerate-unary
  (jen {:jtag :pre :op '++ :value 12}) "++12"
  (jen {:jtag :post :op '++ :value nil}) "null++"
  (jen {:jtag :pre :op '! :value true}) "!true"
  )

(deftest= unary-fn
  (j/unary 12 '++) {:jtag :post :op '++ :value 12}
  (j/unary '++ 12) {:jtag :pre :op '++ :value 12}
  (j/unary '++ '--) {:jtag :pre :op '++ :value '--}
  )

(deftest= jenerate-binary
  (jen {:jtag :bin :left 12 :right 22 :op '%}) "12 % 22"
  (jen {:jtag :bin :left "qwe" :right nil :op '+}) "\"qwe\" + null"
  )

(deftest= bin-fn
  (j/bin 12 '* 22) {:jtag :bin :left 12 :right 22 :op '*}
  (j/bin "qwe" '+ nil) {:jtag :bin :left "qwe" :right nil :op '+}

  ; Operand passed as a vector will automatically simulate `bin` and `br` call and produce nested AST
  (j/bin 12 '* [5 '+ 6]) (j/bin 12 '* (j/br (j/bin 5 '+ 6)))
  (j/bin 12 '* [5 '+ 6]) {:jtag :bin :left 12 :op '* :right {:jtag :br :value {:jtag :bin :left 5 :op '+ :right 6}}}

  (jen (j/bin 12 '* [22 '+ 13])) "12 * (22 + 13)"
  )

(deftest= jenerate-brackets
  (jen {:jtag :br :value nil}) "(null)"
  (jen {:jtag :br :value 12}) "(12)"
  (jen {:jtag :br :value "qwe"}) "(\"qwe\")"
  )

(deftest= br-fn
  (j/br nil) {:jtag :br :value nil}
  (j/br 12) {:jtag :br :value 12}
  (j/br "qwe") {:jtag :br :value "qwe"}
  )

(deftest= not-fn
  (j/not true) {:jtag :pre :op '! :value {:jtag :br :value true}}
  (jen (j/not (j/bin 12 '< 22))) "!(12 < 22)"
  )

(deftest= jenerate-field-ref
  (jen {:jtag :field :target Integer :field "MAX_VALUE"}) "Integer.MAX_VALUE"
  (jen {:jtag :field :target Boolean :field "TRUE"}) "Boolean.TRUE"
  (jen {:jtag :field :target "qwe" :field "x"}) "\"qwe\".x"
  )

(deftest= field-fn
  (j/field Integer "MAX_VALUE") {:jtag :field :target Integer :field "MAX_VALUE"}
  (j/field String "class") {:jtag :field :target String :field "class"}
  (jen (j/field Boolean "TRUE")) "Boolean.TRUE"
  ; Chained field access is available
  (j/field Boolean "FALSE" "value") (j/field (j/field Boolean "FALSE") "value")
  (jen (j/field Boolean "FALSE" "value")) "Boolean.FALSE.value"
  )

(deftest= clref-fn
  (j/clref String) {:jtag :field :target String :field "class"}
  (jen (j/clref Byte)) "Byte.class"
  (jen (j/clref (j/type :int 1))) "int[].class"
  )

(deftest= jenerate-method-call
  (jen {:jtag :method :target (j/field System "out") :method "println"}) "System.out.println()"
  (jen {:jtag :method :target (j/field System "out") :method "println" :args [12]}) "System.out.println(12)"
  (jen {:jtag :method :target (j/field System "out") :method "printf" :args ["qwe" nil]}) "System.out.printf(\"qwe\", null)"
  )

(deftest= call-fn
  (j/call (j/field System "out") "println") {:jtag :method :target (j/field System "out") :method "println" :args nil}
  (j/call "qwe" "length") {:jtag :method :target "qwe" :method "length" :args nil}

  ; Method may be placed in a vector
  (j/call "qwe" ["length"]) {:jtag :method :target "qwe" :method "length" :args nil}

  ; To add argumens - method MUST be placed in a vector
  (j/call "qwe" ["charAt" 2]) {:jtag :method :target "qwe" :method "charAt" :args [2]}
  (jen (j/call "qwe" ["charAt" 2])) "\"qwe\".charAt(2)"

  ; Method calls may be chained
  (jen (j/call String ["valueOf" nil] ["substring" 1 3] "toUpperCase" ["charAt" 1])) "String.valueOf(null).substring(1, 3).toUpperCase().charAt(1)"
  )
