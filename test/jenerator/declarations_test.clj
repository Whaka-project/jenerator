(ns jenerator.declarations-test
  (:require [jenerator.core :refer [jen]]
            [jenerator.fns :as fns]
            [clojure.test :refer :all]))

(deftest jenerate-var
  (are [x y] (= x y)
    (jen {:jtag :var :type Long :name "qwe"}) "Long qwe"
    (jen {:jtag :var :type (fns/type [Class Long]) :name "qwe"}) "Class<Long> qwe"
    (jen {:jtag :var :type Long :name "qwe" :modifiers [:private]}) "private Long qwe"
    (jen {:jtag :var :type Long :name "qwe" :modifiers [:private] :annotations [(fns/ann Deprecated)]}) "@Deprecated private Long qwe"
    ))

(deftest var-fn
  (are [x y] (= x y)

    (fns/var Long "qwe")
    {:jtag :var :type Long :name "qwe" :annotations [] :modifiers []}

    (fns/var :final Long "qwe")
    {:jtag :var :type Long :name "qwe" :annotations [] :modifiers [:final]}

    (fns/var :private :final Long "qwe")
    {:jtag :var :type Long :name "qwe" :annotations [] :modifiers [:private :final]}

    (fns/var (fns/ann Deprecated) Long "qwe")
    {:jtag :var :type Long :name "qwe" :annotations [(fns/ann Deprecated)] :modifiers []}

    (fns/var (fns/ann Deprecated) :private Long "qwe")
    {:jtag :var :type Long :name "qwe" :annotations [(fns/ann Deprecated)] :modifiers [:private]}
    ))

