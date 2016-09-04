(ns jenerator.statements-test
  (:require [jenerator.core :refer [jen]]
            [jenerator.fns :as j]
            [jenerator.test-macros :refer [deftest=]]
            [clojure.test :as t]))


(deftest= jenerate-block
  (jen {:jtag :block}) "{}"
  (jen {:jtag :block :statements []}) "{}"
  (jen {:jtag :block :statements [(j/unary 12 '++)]}) "{\n\t12++;\n}"
  (jen {:jtag :block :statements [(j/unary 12 '++)]}) "{\n\t12++;\n}"
  )

(deftest= block-fn
  (j/block) {:jtag :block :statements nil}
  (j/block (j/unary 12 '++) (j/unary 13 '--)) {:jtag :block :statements [(j/unary 12 '++) (j/unary 13 '--)]}
  (jen (j/block (j/unary 12 '++) (j/unary 13 '--))) "{\n\t12++;\n\t13--;\n}"
  (jen (j/block (j/unary 12 '++) (j/block (j/unary 13 '--)))) "{\n\t12++;\n\t{\n\t\t13--;\n\t}\n}"
  )
