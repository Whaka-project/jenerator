(ns jenerator.statements-test
  (:require [jenerator.core :refer [jen]]
            [jenerator.fns :as j]
            [jenerator.test-macros :refer [deftest=]]
            [clojure.test :refer :all]))

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

(deftest= jenerate-label

  (jen {:jtag :label :name "qwe" :statement 12})
  "qwe: 12"

  ; 'as-statement' meta is propagated to the 'statement' value
  (jen (j/block {:jtag :label :name "zzz" :statement (j/call System ["exit" 0])}))
  "{\n\tzzz: System.exit(0);\n}"
  )

(deftest= label-fn

  (j/label "qwe" 12)
  {:jtag :label :name "qwe" :statement 12}

  (jen
   (j/label "zzz"
    (j/block
     (j/call System ["exit" 0]))))
  "zzz: {\n\tSystem.exit(0);\n}"
  )

(deftest= jenerate-branches

  (jen {:jtag :branch :mode :break}) "break;"
  (jen {:jtag :branch :mode :break :target "qwe"}) "break qwe;"

  (jen {:jtag :branch :mode :continue}) "continue;"
  (jen {:jtag :branch :mode :continue :target "qwe"}) "continue qwe;"

  ; In ':return' mode - target is also jenerated
  (jen {:jtag :branch :mode :return}) "return;"
  (jen {:jtag :branch :mode :return :target "qwe"}) "return \"qwe\";"

  ; ':force-jen' and ':skip-check?' flags are possible
  (jen {:jtag :branch :mode :break :target "qwe" :force-jen true}) "break \"qwe\";"
  (jen {:jtag :branch :mode :qweqwe :target "qwe" :skip-check? true}) "qweqwe qwe;"
  )

(deftest jenerate-branch-error
  ; Without a ':skip-check?' flag - unknown mode causes an exception
  (is (thrown? AssertionError (jen {:jtag :branch :mode :qweqwe})))
  )

(deftest= branches-fns

  (j/break) {:jtag :branch :mode :break}
  (j/break "qwe") {:jtag :branch :mode :break :target "qwe"}

  (j/continue) {:jtag :branch :mode :continue}
  (j/continue "qwe") {:jtag :branch :mode :continue :target "qwe"}

  (j/return) {:jtag :branch :mode :return}
  (j/return "qwe") {:jtag :branch :mode :return :target "qwe"}
  )

(deftest= jenerate-switch

(jen {:jtag :switch :target 42 :cases [[0 (j/call System ["exit" 0])]]})
"switch (42) {
	case 0: {
		System.exit(0);
	}
}"

(jen {:jtag :switch :target 42
      :cases [[0 (j/call System ["exit" 0])]
              [1 (j/call System ["exit" 1])]]})
"switch (42) {
	case 0: {
		System.exit(0);
	}
	case 1: {
		System.exit(1);
	}
}"

(jen {:jtag :switch :target 42
      :cases [[0 (j/call System ["exit" 0])]
              [1 (j/call System ["exit" 1])]
              [:def (j/break)]]})
"switch (42) {
	case 0: {
		System.exit(0);
	}
	case 1: {
		System.exit(1);
	}
	default: {
		break;
	}
}"

(jen {:jtag :switch :target "qwe"
      :cases [[nil (j/call System ["exit" 0])]
              [:def (j/call System ["exit" 1])]]})
"switch (\"qwe\") {
	case null: {
		System.exit(0);
	}
	default: {
		System.exit(1);
	}
}"
)

(deftest switch-asserts

  ; ':cases' key is required
  (is (thrown? AssertionError (jen {:jtag :switch :target 42})))

  ; ':cases' key is required to be a sequential
  (is (thrown? AssertionError (jen {:jtag :switch :target 42 :cases 42})))

  ; ':cases' key is required to be a NON-EMPTY sequential
  (is (thrown? AssertionError (jen {:jtag :switch :target 42 :cases []})))

  ; ':cases' key is required to contain sequentials
  (is (thrown? AssertionError (jen {:jtag :switch :target 42 :cases [42]})))

  ; ':cases' key is required to contain sequentials of the size 2 each
  (is (thrown? AssertionError (jen {:jtag :switch :target 42 :cases [[42]]})))

  ; ':target' key is required
  (is (thrown? AssertionError (jen {:jtag :switch :cases [[42 (j/break)]]})))
  )

(deftest= switch-fn

  (j/switch 42
    0 (j/break "a")
    1 (j/break "b"))
  {:jtag :switch :target 42 :cases [[0 (j/break "a")] [1 (j/break "b")]]}

  (j/switch 42
    0 (j/break "a")
    1 (j/break "b")
    (j/break "c"))
  {:jtag :switch :target 42 :cases [[0 (j/break "a")] [1 (j/break "b")] [:def (j/break "c")]]}
  )
