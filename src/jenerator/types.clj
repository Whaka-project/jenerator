(ns jenerator.types
  (:require [jenerator.util :as u]))

(defn jenerate-class-ref
  "Class -> String
   Takes a class. Returns either a full name, or a simple name,
   based on whether class is from `java.lang` package."
  [class-ref]
  (let [package (-> class-ref .getPackage .getName)]
    (if (= package "java.lang") (.getSimpleName class-ref) (.getName class-ref))))

(defn- jenerate-annotation-arg [jenerate-fn [arg-name value]]
  {:pre [(keyword? arg-name)]}
  (let [value-str (if (sequential? value)
                    (u/jn-curly (map jenerate-fn value))
                    (jenerate-fn value))]
    (str (name arg-name) " = " value-str)))

(defn jenerate-annotation
  "({Key -> Any} -> String) -> {Key -> Any} -> String
   Takes a jeneration function and an annotation AST map.
   Uses function to jenerate all annotation arguments (if any).
   Produces string source for Java annotation instantiation."
  [jenerate-fn {:keys [class args] :or {args {}}}]
  {:pre [(fn? jenerate-fn) (class? class) (map? args)]}
  (let [body (str "@" (jenerate-class-ref class))]
    (if (empty? args) (str body "()")
      (str body (u/jn-args (map (partial jenerate-annotation-arg jenerate-fn) args))))))