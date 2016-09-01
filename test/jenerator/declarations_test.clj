(ns jenerator.declarations-test
  (:require [clojure.test :refer :all]
            [jenerator
             [core :refer [jen]]
             [fns :as fns]
             [test-macros :refer [deftest=]]]
            [jenerator.fns :as j]))

(deftest= jenerate-var

    (jen {:jtag :var :type Long :name "qwe"}) "Long qwe"
    (jen {:jtag :var :type (fns/type [Class Long]) :name "qwe"}) "Class<Long> qwe"
    (jen {:jtag :var :type Long :name "qwe" :modifiers [:private]}) "private Long qwe"
    (jen {:jtag :var :type Long :name "qwe" :modifiers [:private] :annotations [(fns/ann Deprecated)]}) "@Deprecated private Long qwe"
    )

(deftest= var-fn

  (fns/var Long "qwe")
  {:jtag :var :type (fns/type Long) :name "qwe" :annotations [] :modifiers []}

  (fns/var :final Long "qwe")
  {:jtag :var :type (fns/type Long) :name "qwe" :annotations [] :modifiers [:final]}

  (fns/var :private :final Long "qwe")
  {:jtag :var :type (fns/type Long) :name "qwe" :annotations [] :modifiers [:private :final]}

  (fns/var (fns/ann Deprecated) Long "qwe")
  {:jtag :var :type (fns/type Long) :name "qwe" :annotations [(fns/ann Deprecated)] :modifiers []}

  (fns/var (fns/ann Deprecated) :private Long "qwe")
  {:jtag :var :type (fns/type Long) :name "qwe" :annotations [(fns/ann Deprecated)] :modifiers [:private]}
  )

(deftest var-fn-assertions
  ; 'var' with less than two arguments throws assertion
  (is (thrown? AssertionError (fns/var)))
  (is (thrown? AssertionError (fns/var String)))
  (is (thrown? AssertionError (fns/var "s")))
  )

(deftest= jenerate-decl
  (jen {:jtag :decl :var (fns/var String "s") :value nil}) "String s = null"
  (jen {:jtag :decl :var (fns/var :final String "s") :value "qwe"}) "final String s = \"qwe\""
  (jen {:jtag :decl :var (fns/var String "s") :value nil :values [["s2" nil] ["s3" nil]]}) "String s = null, s2 = null, s3 = null"
  (jen {:jtag :decl :var (fns/var String "s") :value nil :values {"s2" nil "s3" nil}}) "String s = null, s2 = null, s3 = null"
  )

(deftest= decl-fn

  (fns/decl (fns/var String "s") nil)
  {:jtag :decl :var (fns/var String "s") :value nil :values nil}

  (fns/decl [String "s"] nil)
  {:jtag :decl :var (fns/var String "s") :value nil :values nil}

  (fns/decl [[String Long] "s"] nil)
  {:jtag :decl :var (fns/var [String Long] "s") :value nil :values nil}

  (fns/decl [String "s"] nil [["s1" nil] ["s2" nil]])
  {:jtag :decl :var (fns/var String "s") :value nil :values [["s1" nil] ["s2" nil]]}

  (fns/decl [String "s"] nil {"s1" nil "s2" nil})
  {:jtag :decl :var (fns/var String "s") :value nil :values {"s1" nil "s2" nil}}
  )
