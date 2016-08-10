(ns jenerator.declarations
  (:require [jenerator.util :as u]))

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
  [jen-fn {:keys [annotations modifiers type name]}]
  {:pre [(string? name) (every? keyword? modifiers) (not (nil? type))]}
  (let [jen-annotatons (map jen-fn annotations)
        jen-modifiers (map clojure.core/name modifiers)
        jen-type (jen-fn type)
        join #(if (empty? %) "" ((u/joiner " " "" " ") %))]
    (str (join jen-annotatons) (join jen-modifiers) jen-type " " name)))

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
    (reduce (fn [main-str [name value]]
              (str main-str ", " name " = " (jen-fn value)))
            main-str
            values)))
