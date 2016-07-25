(ns jenerator.types
  (:require [jenerator.util :as u]))

(def valid-primitive-types
  #{:boolean :byte :short :char :int :long :float :double})

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
  "(Any -> String) -> {Key -> Any} -> String
   Takes a jeneration function and an annotation AST map.
   Uses function to jenerate all annotation arguments (if any).
   Produces string source for Java annotation instantiation."
  [jenerate-fn {:keys [class args] :or {args {}}}]
  {:pre [(fn? jenerate-fn) (class? class) (map? args)]}
  (let [body (str "@" (jenerate-class-ref class))]
    (if (empty? args) (str body "()")
      (str body (u/jn-args (map (partial jenerate-annotation-arg jenerate-fn) args))))))

(declare
  jenerate-type)

(defn- jenerate-primitive-type
  [type]
  {:pre [(keyword? type)]}
  (if (valid-primitive-types type)
    (name type)
    (u/error "Illegal primitive type: " type "! Valid: " valid-primitive-types)))

(defn- jenerate-reference-type
  [{:keys [type generics] :or {generics nil}}]
  {:pre [(class? type) (or (nil? generics) (sequential? generics))]}
  (str (jenerate-class-ref type)
       (if-not (sequential? generics) ""
         (u/jn-generics (map jenerate-type generics)))))

(defn jenerate-type
  [{:keys [type array] :or {array 0} :as map}]
  {:pre [(or (keyword? type) (class? type))
         (and (integer? array) (>= array 0))]}
  (let [type-str (if (keyword? type) (jenerate-primitive-type type) (jenerate-reference-type map))]
    (str type-str (apply str (take array (repeat "[]"))))))