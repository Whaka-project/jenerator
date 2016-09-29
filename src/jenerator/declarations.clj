(ns jenerator.declarations
  (:require [jenerator
             [util :as u]
             [statements :as statements]]))

(def ^:private jn-modifiers
  #(if (empty? %) "" ((u/joiner " " "" " ") %)))

(defn- jenerate-modifiers
  [jen-fn {:keys [annotations modifiers type annotations-newline]}]
  {:pre [(every? keyword? modifiers) (some? type)]}
  (let [jen-annotatons (map jen-fn annotations)
        jen-modifiers (map clojure.core/name modifiers)
        jen-type (jen-fn type)
        newline (if (and (not-empty jen-annotatons) annotations-newline) \newline nil)
        str-annotations (jn-modifiers jen-annotatons)
        str-annotations (if newline (str (apply str (butlast str-annotations)) newline) str-annotations)]
    (str str-annotations (jn-modifiers jen-modifiers) jen-type)))

(defn jenerate-var
  "; (Any -> String) -> Var-AST -> String
   Function takes a jen function and a var AST map.
   Produces string surces of the Java var declaration.

   Var-AST:
     :annotations - [Annotation-AST]
     :modifiers - [Key]
     :type - Type-AST
     :name - String

   Annotations and type are also jenerated.
   Modifiers are not validated, but must be keywords.
   Name is not validated, so be sure to produce valid Java identifier."
  [jen-fn {:keys [annotations modifiers type name] :as data}]
  (str (jenerate-modifiers jen-fn data) " " name))

(defn- valid-additional-decl?
  "; Any -> Boolean
   Returns true if specified value is a sequential with 2 elements and the first one of them is a String.
   'Addional' declarations are all but first vars in a variable declarations.
   For example: 'String s1 = null, s2 = null, s3 = null' has 2 additional declarations."
  [seq]
  (and (sequential? seq)
       (= (count seq) 2)
       (string? (first seq))))

(defn jenerate-decl
  "; (Any -> String) -> Decl-AST -> String
   Function takes a jen function and a decl AST map.
   Returns string representation of a Java variable declaration.
   Decl-AST:
     :var - Var-AST
     :value - Any
     :values - [[String, Any]]

   Var will be jenerated into a declaration 'header' (like 'final String s').
   Value will be jenerated into the initializer for the first 'header' declaration.
   Values will be jenerated into additional inline declarations (like 's2 = null, s3 = null')"
  [jen-fn {:keys [var value values]}]
  {:pre [(every? valid-additional-decl? values)]}
  (let [var-str (jen-fn var)
        value-str (jen-fn value)
        main-str (str var-str " = " value-str)]
    (str (reduce (fn [main-str [name value]]
               (str main-str ", " name " = " (jen-fn value)))
             main-str
             values) ";")))

(defn jenerate-method
  [jen-fn {:keys [annotations modifiers type name args exceptions body] :as data}]
  {:pre [(some? name)]}
  (let [data (if (some? type) data (assoc data :type 'void))
        jen-prefix (jenerate-modifiers jen-fn (assoc data :annotations-newline true))
        jen-args (map jen-fn args)
        jen-exceptions (map jen-fn exceptions)
        jen-throws (if (empty? jen-exceptions) ""
                     (str "throws " (u/jn-comma jen-exceptions)))]
    (str jen-prefix " " name (u/jn-args jen-args) jen-throws " "
      (jen-fn (statements/ensure-block body)))))
