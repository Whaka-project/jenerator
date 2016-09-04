(ns jenerator.statements-test
  (:require [jenerator.core :refer [jen]]
            [jenerator.fns :as j]
            [jenerator.test-macros :refer [deftest=]]
            [clojure.test :as t]))

(def st (j/unary 12 '++))

(deftest= jenerate-block
  (jen {:jtag :block}) "{}"
  (jen {:jtag :block :statements []}) "{}"
  (jen {:jtag :block :statements [st]}) "{\n\t12++;\n}"
  (jen {:jtag :block :statements [st]}) "{\n\t12++;\n}"
  )

(deftest= block-fn
  (j/block) {:jtag :block :statements nil}
  (j/block st st) {:jtag :block :statements [st st]}
  (jen (j/block st st)) "{\n\t12++;\n\t12++;\n}"
  (jen (j/block st (j/block st))) "{\n\t12++;\n\t{\n\t\t12++;\n\t}\n}"
  )

(deftest= jenerate-if

  (jen {:jtag :if :cond 12 :then st})
  "if (12) {\n\t12++;\n}"

  (jen {:jtag :if :cond 12 :then st :else st})
  "if (12) {\n\t12++;\n}\nelse {\n\t12++;\n}"

  (jen {:jtag :if :cond 12 :then st :else {:jtag :if :cond 13 :then st}})
  "if (12) {\n\t12++;\n}\nelse if (13) {\n\t12++;\n}"

  (jen {:jtag :if :cond 12 :then st :else {:jtag :if :cond 13 :then st :else st}})
  "if (12) {\n\t12++;\n}\nelse if (13) {\n\t12++;\n}\nelse {\n\t12++;\n}"
  )

(deftest= if-fn

  (j/if 12 st)
  {:jtag :if :cond 12 :then st}

  (j/if 12 st st)
  {:jtag :if :cond 12 :then st :else st}

  (j/if 12 st 13 st)
  {:jtag :if :cond 12 :then st :else {:jtag :if :cond 13 :then st}}

  (j/if 12 st 13 st st)
  {:jtag :if :cond 12 :then st :else {:jtag :if :cond 13 :then st :else st}}
  )
