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

  ; Operand passed as a vector will automatically simulate `bin` call and produce nested AST
  (j/bin 12 '* [5 '+ 6]) (j/bin 12 '* (j/bin 5 '+ 6))
  (j/bin 12 '* [5 '+ 6]) {:jtag :bin :left 12 :op '* :right {:jtag :bin :left 5 :op '+ :right 6}}
  )
