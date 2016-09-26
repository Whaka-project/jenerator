(ns jenerator.statements-test
  (:require [jenerator.core :refer [jen]]
            [jenerator.fns :as j]
            [jenerator.test-macros :refer [deftest=]]
            [clojure.test :as t]))

(def st (j/unary 12 '++))

(deftest= jenerate-block
  (jen {:jtag :block}) "{\n}"
  (jen {:jtag :block :statements []}) "{\n}"
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

(deftest= jenerate-for

  (jen {:jtag :for
        :decl (j/decl [:int "i"] 0)
        :test (j/bin 12 '< 0)
        :iters (j/unary 12 '++)
        :body (j/call System ["exit" 0])})
  "for (int i = 0; 12 < 0; 12++) {\n\tSystem.exit(0);\n}"

  (jen {:jtag :for
        :decl (j/decl [:int "i"] 0 [["j" 1]])
        :test (j/bin [12 '< 0] '&& [13 '> 100])
        :iters [(j/unary 12 '++) (j/unary '-- 13)]
        :body (j/block
                (j/call System ["exit" 0])
                (j/call (j/field System "out") ["println" nil]))})
  "for (int i = 0, j = 1; (12 < 0) && (13 > 100); 12++, --13) {\n\tSystem.exit(0);\n\tSystem.out.println(null);\n}"

  (jen {:jtag :for
        :decl nil
        :test (j/bin 12 '< 0)
        :iters (j/unary 12 '++)
        :body (j/call System ["exit" 0])})
  "for (; 12 < 0; 12++) {\n\tSystem.exit(0);\n}"

  (jen {:jtag :for
        :decl (j/decl [:int "i"] 0)
        :test nil
        :iters (j/unary 12 '++)
        :body (j/call System ["exit" 0])})
  "for (int i = 0; ; 12++) {\n\tSystem.exit(0);\n}"

  (jen {:jtag :for
        :decl (j/decl [:int "i"] 0)
        :test (j/bin 12 '< 0)
        :iters nil
        :body (j/call System ["exit" 0])})
  "for (int i = 0; 12 < 0; ) {\n\tSystem.exit(0);\n}"

  (jen {:jtag :for
        :decl (j/decl [:int "i"] 0)
        :test (j/bin 12 '< 0)
        :iters (j/unary 12 '++)
        :body nil})
  "for (int i = 0; 12 < 0; 12++) {\n}"

  (jen {:jtag :for})
  "for (; ; ) {\n}"
  )

(deftest= for-fn

  (j/for [(j/decl [String "s"] "qwe")
          (j/bin 12 '< 0)
          (j/unary 12 '++)]
    (j/call System ["exit" 0]))
  {:jtag :for
   :decl (j/decl [String "s"] "qwe")
   :test (j/bin 12 '< 0)
   :iters (list (j/unary 12 '++))
   :body (j/call System ["exit" 0])}

  (j/for [(j/decl [String "s"] "qwe")
          (j/bin 12 '< 0)
          (j/unary 12 '++)
          (j/unary 13 '--)]
    (j/call System ["exit" 0])
    (j/call System ["exit" 1]))
  {:jtag :for
   :decl (j/decl [String "s"] "qwe")
   :test (j/bin 12 '< 0)
   :iters (list (j/unary 12 '++) (j/unary 13 '--))
   :body (j/block
           (j/call System ["exit" 0])
           (j/call System ["exit" 1]))}

  (j/for [(j/decl [String "s"] "qwe")
          (j/bin 12 '< 0)]
    )
  {:jtag :for
   :decl (j/decl [String "s"] "qwe")
   :test (j/bin 12 '< 0)
   :iters nil
   :body nil}

  (j/for [nil nil])
  {:jtag :for
   :decl nil
   :test nil
   :iters nil
   :body nil}
  )
