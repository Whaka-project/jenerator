(ns jenerator.types
  (:require [jenerator.util :as u]))

(defn jenerate-class-ref [class-ref]
  (let [package (-> class-ref .getPackage .getName)]
    (if (= package "java.lang") (.getSimpleName class-ref) (.getName class-ref))))

(defn- jenerate-annotation-arg [jenerate-fn [arg-name value]]
  {:pre [(keyword? arg-name)]}
  (let [value-str (if (sequential? value)
                    (u/jn-curly (map jenerate-fn value))
                    (jenerate-fn value))]
    (str (name arg-name) " = " value-str)))

(defn jenerate-annotation [jenerate-fn {:keys [class args] :or {args {}}}]
  {:pre [(fn? jenerate-fn) (class? class) (map? args)]}
  (let [body (str "@" (jenerate-class-ref class))]
    (if (empty? args) (str body "()")
      (str body (u/jn-args (map (partial jenerate-annotation-arg jenerate-fn) args))))))