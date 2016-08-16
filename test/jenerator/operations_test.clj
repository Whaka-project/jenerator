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
